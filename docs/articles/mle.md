# Distributed Maximum Likelihood Estimation

## The statistical problem

Suppose we have count data $`y_1, y_2, \ldots, y_n`$ that we model as
independent draws from a Poisson distribution with unknown parameter
$`\lambda`$. The maximum likelihood estimate is
$`\hat{\lambda} = \bar{y}`$, obtained by minimizing the negative
log-likelihood

``` math
-\ell(\lambda \mid y) \;=\; -\sum_{i=1}^{n} \log p(y_i \mid \lambda).
```

In R, this is a one-liner using
[`stats4::mle()`](https://rdrr.io/r/stats4/mle.html):

[`library`](https://rdrr.io/r/base/library.html)`(``stats4``)`` `[`set.seed`](https://rdrr.io/r/base/Random.html)`(``17822``)`` ``y`` ``<-`` `[`rpois`](https://rdrr.io/r/stats/Poisson.html)`(``n ``=`` ``40``, lambda ``=`` ``10``)`` `` ``nLL`` ``<-`` ``function``(``lambda``)`` ``-`[`sum`](https://rdrr.io/r/base/sum.html)`(``stats``::`[`dpois`](https://rdrr.io/r/stats/Poisson.html)`(``y``, ``lambda``, log ``=`` ``TRUE``)``)`` ``fit0`` ``<-`` `[`mle`](https://rdrr.io/r/stats4/mle.html)`(``nLL``, start ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``lambda ``=`` ``5``)``, nobs ``=`` `[`NROW`](https://rdrr.io/r/base/nrow.html)`(``y``)``)`` `[`summary`](https://rdrr.io/r/base/summary.html)`(``fit0``)`

    ## Maximum likelihood estimation
    ## 
    ## Call:
    ## mle(minuslogl = nLL, start = list(lambda = 5), nobs = NROW(y))
    ## 
    ## Coefficients:
    ##        Estimate Std. Error
    ## lambda    9.175  0.4789311
    ## 
    ## -2 log L: 199.5328

[`logLik`](https://rdrr.io/r/stats/logLik.html)`(``fit0``)`

    ## 'log Lik.' -99.76641 (df=1)

## The privacy constraint

Now suppose the same data is **distributed across three sites** — say,
three hospitals counting adverse events. None will share its raw counts
with the others or with a central aggregator, but they are willing to
*jointly* compute the same MLE provided no party learns anything about
another party’s contribution.

To simulate this, partition `y`:

`y1`` ``<-`` ``y``[``1``:``20``]`` ``y2`` ``<-`` ``y``[``21``:``27``]`` ``y3`` ``<-`` ``y``[``28``:``40``]`

The negative log-likelihood factorises additively:

``` math
-\ell(\lambda \mid y) \;=\; -\ell_1(\lambda \mid y_1) - \ell_2(\lambda \mid y_2) - \ell_3(\lambda \mid y_3)
```

so each site can compute its local term in the clear and only the *sum*
of the three local likelihoods needs to travel between parties — and the
sum must not reveal the individual addends.

## The protocol

We use the **master/worker** topology that distcomp- and
DataSHIELD-style federated analyses actually deploy: a star with the
master at the center and one independent worker per site. There is no
chain and no inter-site communication.

In words:

0.  The master generates a CKKS context and key pair, distributes the
    public key to the three workers, keeps the secret key.
1.  The master broadcasts the current $`\lambda`$ to each worker.
2.  Each worker computes its local negative log-likelihood
    $`\ell_i(\lambda)`$ on its private data, encrypts the result under
    the master’s public key, and returns $`E(\ell_i)`$ to the master.
3.  The master sums the encrypted contributions homomorphically:
    $`E(\ell) = E(\ell_1) \boxplus E(\ell_2) \boxplus E(\ell_3)
    = E(\ell_1 + \ell_2 + \ell_3)`$.
4.  The master decrypts $`E(\ell)`$ to recover $`\ell`$.
5.  The master hands $`\ell`$ to the optimizer; the protocol repeats for
    each new guess of $`\lambda`$ until convergence.

This is the realistic shape: each worker independently does its local
computation and ships an encrypted summary; the master only sees the
encrypted summaries (and, after homomorphic summation, the decrypted
total). With a single-decrypter master, the master *could* decrypt
individual $`E(\ell_i)`$ in principle; the cryptographic story
strengthens when paired with threshold key generation, where no single
party holds the secret key. We will revisit that in the Cox threshold
vignette.

(The companion Paillier vignette `vignette("homomorphing")` uses an
older *round-robin* protocol with a random offset traveling around a
chain. That idiom was a Paillier-era pedagogical artifact; with proper
FHE plus threshold key generation we no longer need it.)

## Why CKKS, not Paillier?

The companion `homomorphing` vignette uses Paillier, which is purely
*additive*. Real-valued log-likelihoods had to be split into integer and
fractional parts and rationally approximated with a denominator of
$`2^{256}`$. CKKS encrypts real numbers natively and the protocol
becomes much cleaner: each step is just `+` between two encrypted
values. CKKS also supports multiplication, which the Cox vignette will
need.

## Implementation

The actor surface — `Site` (worker), `Master`, the master/worker runner
— is the same code as in any other distributed-stats vignette in this
package; only the master’s *backend* changes. `homomorpheR` exports
[`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)
that takes an `openfhe.R` `CryptoContext` and `KeyPair` and routes
encryption/decryption through CKKS. The worker class and the
[`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
runner are backend-agnostic.

The per-site negative log-likelihood is the same plain R function it
would be in the cleartext case:

[`library`](https://rdrr.io/r/base/library.html)`(`[`homomorpheR`](https://bnaras.github.io/homomorpheR/)`)`` `` ``local_nll`` ``<-`` ``function``(``data``, ``lambda``)`` ``{`` `` ``-`[`sum`](https://rdrr.io/r/base/sum.html)`(``stats``::`[`dpois`](https://rdrr.io/r/stats/Poisson.html)`(``data``, ``lambda``, log ``=`` ``TRUE``)``)`` ``}`

### 1. Generate a CKKS key pair

We use `openfhe.R` qualified rather than
[`library(openfhe.R)`](https://openfheorg.github.io/openfhe.R/): both
packages export `encrypt`/`decrypt` generics, and we want
`homomorpheR`’s on the search path so the protocol code below reads
naturally.

`cc`` ``<-`` ``openfhe.R``::`[`fhe_context`](https://openfheorg.github.io/openfhe.R/reference/fhe_context.html)`(``"CKKS"``,`` `` multiplicative_depth ``=`` ``1L``,`` `` scaling_mod_size ``=`` ``50L``,`` `` batch_size ``=`` ``8L``)`` ``keys`` ``<-`` ``openfhe.R``::`[`key_gen`](https://openfheorg.github.io/openfhe.R/reference/key_gen.html)`(``cc``)`

### 2. Build workers and master

`worker1`` ``<-`` `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``"Site 1"``, data ``=`` ``y1``, local_fn ``=`` ``local_nll``)`` ``worker2`` ``<-`` `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``"Site 2"``, data ``=`` ``y2``, local_fn ``=`` ``local_nll``)`` ``worker3`` ``<-`` `[`make_worker`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)`(``"Site 3"``, data ``=`` ``y3``, local_fn ``=`` ``local_nll``)`` ``master`` ``<-`` `[`make_ckks_master`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)`(``"Master"``, crypto_context ``=`` ``cc``, keypair ``=`` ``keys``)`` `[`set_workers`](https://bnaras.github.io/homomorpheR/reference/set_workers.md)`(``master``, `[`list`](https://rdrr.io/r/base/list.html)`(``worker1``, ``worker2``, ``worker3``)``)`

### 3. Run `mle()` through the encrypted channel

`fit1`` ``<-`` `[`mle`](https://rdrr.io/r/stats4/mle.html)`(``function``(``lambda``)`` `[`master_aggregate`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)`(``master``, ``lambda``)``,`` `` start ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(``lambda ``=`` ``5``)``)`` `[`summary`](https://rdrr.io/r/base/summary.html)`(``fit1``)`

    ## Maximum likelihood estimation
    ## 
    ## Call:
    ## mle(minuslogl = function(lambda) master_aggregate(master, lambda), 
    ##     start = list(lambda = 5))
    ## 
    ## Coefficients:
    ##        Estimate Std. Error
    ## lambda    9.175  0.4789311
    ## 
    ## -2 log L: 199.5328

[`logLik`](https://rdrr.io/r/stats/logLik.html)`(``fit1``)`

    ## 'log Lik.' -99.76641 (df=1)

The CKKS-based estimate matches the cleartext estimate to within
floating-point precision. No site ever revealed its individual counts to
any other party.

## Beyond MLE

For Poisson MLE the protocol uses only additions, so even the purely
additive Paillier scheme suffices (see `vignette("homomorphing")`). CKKS
is a clean improvement but not strictly necessary here. The story
changes for likelihoods that need products or higher-order statistics —
distributed Cox regression, for example — where CKKS’s multiplicative
homomorphism becomes a prerequisite.

## CAVEAT

This is a teaching example. In production you would want a real
communication transport, threshold key generation so no single party
holds the full secret, and persistent serialization at site boundaries.
The point of this vignette is not the deployment story but the
*structure* of a privacy-preserving distributed computation built on
homomorphic encryption.
