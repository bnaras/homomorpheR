## Site / Master / NCParty actor surface.

library(homomorpheR)

keys <- paillier_keypair(512)

## ---- Round-robin: encrypted sum matches cleartext sum --------------------
master <- make_master("Master", keys)
site1  <- make_site("S1", c(2, 3),
                    function(d, lambda) -sum(stats::dpois(d, lambda, log = TRUE)))
site2  <- make_site("S2", c(4, 5),
                    function(d, lambda) -sum(stats::dpois(d, lambda, log = TRUE)))
round_robin_chain(master, list(site1, site2))

direct <- -sum(stats::dpois(c(2, 3, 4, 5), 3.5, log = TRUE))
encrypted <- run_round_robin(master, 3.5)
expect_true(abs(encrypted - direct) < 1e-9)

## Determinism across repeated calls (modulo the random offset the master
## generates and removes — the result must be the same value).
v <- replicate(4, run_round_robin(master, 3.5))
expect_true(max(abs(v - direct)) < 1e-9)

## ---- NA propagation: a site that returns NA bubbles up to NA_real_ ------
master2 <- make_master("Master2", keys)
site_bad <- make_site("Sbad", c(2, 3),
                      function(d, lambda) if (lambda < 0.01) NA else
                          -sum(stats::dpois(d, lambda, log = TRUE)))
round_robin_chain(master2, list(site_bad))

expect_true(is.na(run_round_robin(master2, 0.001)))
expect_true(!is.na(run_round_robin(master2, 1.0)))

## ---- mle() converges to the cleartext MLE through the encrypted channel --
master3 <- make_master("Master3", keys)
s1 <- make_site("S1", c(2, 3),
                function(d, lambda) -sum(stats::dpois(d, lambda, log = TRUE)))
s2 <- make_site("S2", c(4, 5),
                function(d, lambda) -sum(stats::dpois(d, lambda, log = TRUE)))
round_robin_chain(master3, list(s1, s2))
fit <- stats4::mle(function(lambda) run_round_robin(master3, lambda),
                   start = list(lambda = 5))
expect_true(abs(stats4::coef(fit) - mean(c(2, 3, 4, 5))) < 1e-3)
