# Distributed Query Count with Threshold Keys

## The problem

Several sites each hold a private table of patient records. A aggregator
wants the answer to a single aggregate query — *how many patients across
all sites satisfy some condition?* — without any site revealing its own
records, and without the aggregator learning any individual site’s
count. Only the grand total should ever become visible, and it should be
**exact**: a count is an integer, and an answer of “about 4” is not a
count.

We simulate three sites, each with `sex`, `age`, and a biomarker `bm`.

``` r

set.seed(130)
sample_size <- c(60, 15, 25)
query_data <- local({
    tmp   <- c(0, cumsum(sample_size))
    start <- tmp[1:3] + 1
    end   <- tmp[-1]
    id_list <- Map(seq, from = start, to = end)
    lapply(seq_along(sample_size), function(i) {
        data.frame(
            id  = sprintf("P%4d", id_list[[i]]),
            sex = sample(c("F", "M"), sample_size[i], replace = TRUE),
            age = sample(40:70,      sample_size[i], replace = TRUE),
            bm  = rnorm(sample_size[i]),
            stringsAsFactors = FALSE)
    })
})
```

The query we will run is `age < 50 & sex == "F" & bm < 0.2`.

## The target answer

If the data could be pooled in one place, the query is a single line of
R. We compute it here only to have a reference to check the distributed
protocol against.

``` r

query <- quote(age < 50 & sex == "F" & bm < 0.2)

pooled <- do.call(rbind, query_data)
cleartext_count <- sum(eval(query, pooled))
cleartext_count
```

    ## [1] 11

The pooled answer is 11. No site will actually pool its data; this
number is the ground truth the encrypted protocol must reproduce.

## Why the classic solution needed two non-cooperating parties

The additive structure of the problem — the grand total is the sum of
the per-site counts — is exactly what an *additively homomorphic*
encryption scheme computes. Let \\c_i\\ be site \\i\\’s local count and
\\E(\cdot)\\ the encryption function. Given \\E(c_1), E(c_2), E(c_3)\\,
the scheme lets anyone compute \\E(c_1) + E(c_2) + E(c_3) = E(c_1 +
c_2 + c_3)\\ without decrypting the parts.

The historical difficulty was not the arithmetic but the *trust model*.
Whoever holds the private key can decrypt anything — including a lone
\\E(c_i)\\. So if the aggregator holds the key, it can read each site’s
count individually, defeating the purpose.

The workaround was two **non-cooperating parties**, NCP1 and NCP2, who
do not collude. The full details are in the `QueryNCP` vignette,
archived in the `paillier-archive/` directory of the source repository.

## Threshold encryption removes the need for masking

`openfhe.R` supports **threshold** (multiparty) encryption, and it
changes the trust model at the root. There is no single private key. Key
generation is chained across the sites: each site holds only a secret
*share* \\sk_i\\, and the joint public key \\pk\_{1..n}\\ is built by
passing the running public key from one site to the next. Everything is
encrypted under \\pk\_{1..n}\\, but **decryption requires every site to
contribute a partial decryption** — no one, aggregator included, can
decrypt anything alone.

Under this model a lone \\E(c_i)\\ is simply not decryptable by any
single party, so the random masks and the two non-cooperating parties
are no longer needed. The aggregator sums the encrypted per-site counts
and asks the sites to jointly decrypt only the total. Individual counts
are protected not because they are hidden behind random noise, but
because the ability to decrypt them does not exist in any one place.

We use the **BFV** scheme rather than CKKS here. BFV is exact integer
arithmetic: the decrypted total is the integer sum, bit for bit, with no
approximation. CKKS, used elsewhere in this package for real-valued
likelihoods, would return the count as a floating-point value close to
the integer and require rounding. For a count, exactness is the whole
point, so BFV is the right instrument.

## The threshold-BFV implementation

The encryption context is BFV with the `MULTIPARTY` feature enabled. BFV
works with integers modulo a fixed bound, and that bound only has to
exceed the largest total the query could return, so the default is
comfortable for counts.

``` r

library(homomorpheR)

cc <- openfhe.R::fhe_context("BFV",
                             plaintext_modulus    = 65537L,
                             multiplicative_depth = 1L,
                             features             = c(openfhe.R::Feature$MULTIPARTY))
```

Each site is a worker whose local function evaluates the query against
its own private data and returns a count. The master broadcasts the
query; no site sees another site’s data, and the counts it returns are
encrypted before they leave.

``` r

local_count <- function(data, query) sum(eval(query, data))

workers <- Map(
    function(nm, d) make_worker(nm, data = d, contribution_fn = local_count),
    c("Site 1", "Site 2", "Site 3"),
    query_data)
```

The sites have to exist before the aggregator does, because the joint
public key is built *from* them:
[`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
walks the chain, each site generating and keeping its own share and
passing on only a public key. What comes back is an aggregator holding
the joint public key and nothing secret, already wired to the sites. The
same master class drives CKKS or BFV — it reads the scheme back from the
context — so the only thing that changed from the real-valued vignettes
is the context above.

``` r

master <- make_threshold_master("Aggregator",
                                crypto_context = cc,
                                sites          = workers)
```

One call runs the protocol: each worker’s encrypted count is summed
homomorphically, and the total is jointly decrypted by the three
secret-share holders.

``` r

encrypted_count <- master_aggregate(master, theta = query)
encrypted_count
```

    ## [1] 11

## The encrypted answer is exact

``` r

comparison <- data.frame(
    method = c("pooled cleartext", "threshold-BFV distributed"),
    count  = c(cleartext_count, as.integer(encrypted_count)))
ktab(comparison, col.names = c("Method", "Count"),
             caption = "Distributed encrypted query count vs. the pooled answer")
```

| Method                    | Count |
|:--------------------------|------:|
| pooled cleartext          |    11 |
| threshold-BFV distributed |    11 |

Distributed encrypted query count vs. the pooled answer {.table .table
.table-striped .table-condensed
style="margin-left: auto; margin-right: auto;"}

The threshold-BFV protocol returns 11, identical to the pooled answer of
11. Because BFV is exact integer arithmetic, the two agree with no
tolerance to argue about — the equality is bit-for-bit, not approximate.

``` r

stopifnot(identical(as.integer(encrypted_count), as.integer(cleartext_count)))
```

## Summary

- No single party holds a decryption key, so no single party —
  aggregator or site — can decrypt an individual site’s count.

- Only the aggregate is ever revealed. The sites jointly decrypt the
  total and nothing else.

- BFV gives exact counts. For integer-valued aggregates — counts, sums,
  contingency-table cells — BFV decrypts to the exact integer.

- BFV arithmetic is modular. A site that tries to contribute something
  the scheme cannot carry — a non-integer, a non-finite value, or one at
  or beyond `plaintext_modulus / 2` — is refused by
  [`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
  rather than having its value rounded or wrapped silently. What no
  party can check is the *total*: a sum that exceeds the modulus wraps,
  and the wrapped value decrypts as an ordinary integer with nothing to
  mark it as wrong. The remedy is to choose `plaintext_modulus` for the
  largest total the protocol can produce, not for the largest single
  contribution.

- As with the other distributed protocols in this package, sites are
  assumed to follow the protocol. A malicious site could submit a wrong
  count or a corrupted partial decryption; detecting that requires
  verifiable decryption, which is beyond this demonstration.
