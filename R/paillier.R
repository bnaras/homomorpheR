#' Construct a Paillier public key with the given modulus.
#' @docType class
#' @seealso `PaillierPrivateKey` which goes hand-in-hand with this object
#' @importFrom R6 R6Class
#' @importFrom gmp add.bigz
#' @importFrom gmp mod.bigz
#' @importFrom gmp mul.bigz
#' @importFrom gmp powm
#' @importFrom gmp as.bigz
#' @field bits the number of bits in the modulus
#' @field n the modulus
#' @field nSquared the square of the modulus
#' @field nPlusOne one more than the modulus
#' @section Methods:
#' \describe{
#'   \item{`PaillierPublicKey$new(bits, n)`}{Create a new public key with given `bits` and modulus
#'         \code{n}. It also precomputes a few values for more efficient computations}
#'   \item{`PaillierPublicKey$encrypt(m)`}{Encrypt a message. The value `m` should be less than
#'         the modulus, not checked}
#'   \item{`PaillierPublicKey$add(a, b)`}{Return the sum of two encrypted messages `a` and `b`}
#'   \item{`PaillierPublicKey$sub(a, b)`}{Return the difference, a - b, of two encrypted messages `a` and `b`}
#'   \item{`PaillierPublicKey$add_real(den, r, a)`}{Return the sum of an encrypted real message `r`, a list consisting of a encrypted integer part (named `int`) and an encrypted fractional part (named `frac`), and a real number `a`; `den` is a denominator to use for the rational approximation.}
#'   \item{`PaillierPublicKey$sub_real(den, r, a)`}{Return the difference r - a, of an encrypted real message `r`, a list consisting of a encrypted integer part (named `int`) and an encrypted fractional part (named `frac`), and a real number `a`; `den` is a denominator to use for the rational approximation.}
#'   \item{`PaillierPublicKey$mult(a, b)`}{Return the product of two encrypted messages `a` and `b`}
#' }
#'
#' @export
#' @format An \code{\link{R6Class}} generator object

PaillierPublicKey <- R6Class("PaillierPublicKey",
                            private = list(
                                randomize = function(a) {
                                    repeat {
                                        r <- random.bigz(nBits = self$bits)
                                        ## make sure r <= n
                                        if (r < self$n)
                                            break
                                    }
                                    rn <- powm(r, self$n, self$nSquared)
                                    mod.bigz(mul.bigz(a, rn), self$nSquared)
                                }),
                            public = list (
                                ## fields
                                bits = NULL,
                                n = NULL,
                                nSquared = NULL,
                                nPlusOne = NULL,
                                ## methods
                                initialize = function(bits, n) {
                                    ## bits
                                    self$bits <- bits
                                    ## n
                                    self$n <- n
                                    ## n squared
                                    self$nSquared <- mul.bigz(n, n)
                                    ## n plus 1
                                    self$nPlusOne <- add.bigz(n, ONE)
                                },
                                encrypt = function(m) {
                                    private$randomize(
                                        mod.bigz(
                                            add.bigz(
                                                mul.bigz(self$n, m),
                                                ONE),
                                            self$nSquared))
                                },
                                ## a + b
                                add = function(a, b) {
                                    mod.bigz(mul.bigz(a, b), self$nSquared)
                                },
                                ## a - b
                                sub = function(a, b) {
                                    self$add(a, self$nSquared - b)
                                },
                                ## below a is a list with int and frac parts
                                ## while b is a real number
                                ## den is a denominator
                                ## a + b
                                add_real = function(den, a, b) {
                                    int_value <- floor(b)
                                    frac_value <- b - int_value
                                    frac_number  <- gmp::as.bigq(gmp::numerator(gmp::as.bigq(frac_value) * den))
                                    enc_int_value <- self$encrypt(int_value)
                                    enc_frac_number <- pubkey$encrypt(frac_number)
                                    list(int = self$add(enc_int_value, a$int),
                                         frac = self$add(enc_frac_number, a$frac))
                                },
                                ## below a is a list with int and frac parts
                                ## while b is a real number
                                ## den is a denominator
                                ## a - b
                                sub_real = function(den, a, b) {
                                    int_value <- floor(b)
                                    frac_value <- b - int_value
                                    frac_number  <- gmp::as.bigq(gmp::numerator(gmp::as.bigq(frac_value) * den))
                                    enc_int_value <- self$encrypt(int_value)
                                    enc_frac_number <- pubkey$encrypt(frac_number)
                                    list(int = self$sub(enc_int_value, a$int),
                                         frac = self$sub(enc_frac_number, a$frac))
                                },
                                mult = function(a, b) {
                                    powm(a, b, self$nSquared)
                                })
                            )

#' Construct a Paillier private key with the given secret and a public key
#' @docType class
#' @seealso `PaillierPublicKey` which goes hand-in-hand with this object
#' @importFrom R6 R6Class
#' @importFrom gmp add.bigz
#' @importFrom gmp mod.bigz
#' @importFrom gmp mul.bigz
#' @importFrom gmp div.bigz
#' @importFrom gmp sub.bigz
#' @importFrom gmp powm
#' @importFrom gmp inv.bigz
#' @importFrom gmp as.bigz
#' @field pubkey the Paillier public key
#'
#' @section Methods:
#' \describe{
#'   \item{`PaillierPrivateKey$new(lambda, pubkey)`}{Create a new private key with given secret
#'         `lambda` and the public key}
#'   \item{`PaillierPrivateKey$getLambda()`}{Return the secret `lambda`}
#'   \item{`PaillierPrivateKey$decrypt(c)`}{Decrypt a message. The value `c` should be an
#'         encrypted value}
#' }
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
PaillierPrivateKey <- R6Class("PaillierPrivateKey",
                              private = list(
                                  lambda = NA,
                                  ## cached value for decryption
                                  x = NA
                              ),
                              public = list(
                                  ## fields
                                  pubkey = NULL,
                                  ## methods
                                  initialize = function(lambda, pubkey) {
                                      ## lambda
                                      private$lambda <- lambda
                                      self$pubkey <- pubkey
                                      ## x (cached) for decryption
                                      private$x <- inv.bigz(
                                          div.bigz(
                                              sub.bigz(
                                                  powm(pubkey$nPlusOne, private$lambda, pubkey$nSquared),
                                                  ONE),
                                              pubkey$n),
                                          pubkey$n)
                                  },
                                  getLambda = function() {
                                      private$lambda
                                  },
                                  decrypt = function(c) {
                                      mod.bigz(
                                          mul.bigz(
                                              div.bigz(
                                                  sub.bigz(
                                                      powm(c, private$lambda, self$pubkey$nSquared),
                                                      ONE),
                                                  self$pubkey$n),
                                              private$x),
                                          self$pubkey$n)
                                  })
                              )

#' Construct a Paillier public and private key pair given a fixed number of bits
#' @docType class
#' @seealso \code{\link{PaillierPublicKey}} and \code{\link{PaillierPrivateKey}}
#' @importFrom R6 R6Class
#' @importFrom gmp add.bigz
#' @importFrom gmp sub.bigz
#' @importFrom gmp mod.bigz
#' @importFrom gmp mul.bigz
#' @importFrom gmp powm
#' @importFrom gmp isprime
#' @importFrom gmp sizeinbase
#' @importFrom gmp lcm.bigz
#' @importFrom gmp as.bigz
#' @field pubkey the Paillier public key
#'
#' @section Methods:
#' \describe{
#'   \item{`PaillierKeyPair$new(modulusBits)}`}{Create a new private key with specified
#'         number of modulus bits}
#'   \item{`PaillierKeyPair$getPrivateKey()`}{Return the private key}
#' }
#'
#' @examples
#' keys <- PaillierKeyPair$new(1024)
#' keys$pubkey
#' keys$getPrivateKey()
#'
#' @export
#' @format An \code{\link{R6Class}} generator object
PaillierKeyPair <- R6Class("PaillierKeyPair",
                          private = list(
                              privkey = NULL
                          ),
                          public = list(
                              ## fields
                              pubkey = NULL,
                              ## methods
                              initialize = function(modulusBits) {
                                  repeat {
                                      repeat {
                                          p <- random.bigz(nBits = modulusBits %/% 2)
                                          if (isprime(n = p, reps = 10))
                                              break
                                      }
                                      repeat {
                                          q <- random.bigz(nBits = modulusBits %/% 2)
                                          if (isprime(n = q, reps = 10))
                                              break
                                      }
                                      n <- mul.bigz(p, q)
                                      if ( ( p != q ) && ( sizeinbase(a = n, b = 2) == modulusBits) )
                                          break
                                  }

                                  pubkey <- PaillierPublicKey$new(modulusBits, n)
                                  self$pubkey <- pubkey
                                  lambda <- lcm.bigz(sub.bigz(p, ONE), sub.bigz(q, ONE))
                                  private$privkey <- PaillierPrivateKey$new(lambda, pubkey)
                              },
                              getPrivateKey = function() {
                                  private$privkey
                              })
                          )



