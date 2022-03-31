#' Construct a Paillier public key with the given modulus.
#' @docType class
#' @seealso `PaillierPrivateKey` which goes hand-in-hand with this object
#' @importFrom R6 R6Class
#' @importFrom gmp add.bigz
#' @importFrom gmp mod.bigz
#' @importFrom gmp mul.bigz
#' @importFrom gmp powm
#' @importFrom gmp as.bigz
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
                                 rn <- gmp::powm(r, self$n, self$nSquared)
                                 mod.bigz(mul.bigz(a, rn), self$nSquared)
                               }),

                             public = list (

                               #' @field bits the number of bits in the modulus
                               bits = NULL,

                               #' @field n the modulus
                               n = NULL,

                               #' @field nSquared the square of the modulus
                               nSquared = NULL,

                               #' @field nPlusOne one more than the modulus
                               nPlusOne = NULL,

                               #' @description
                               #' Create a new public key and precompute some internal values for efficiency
                               #' @param bits number of bits to use
                               #' @param n the modulus to use
                               #' @return a new `PaillierPublicKey` object
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

                               #' @description
                               #' Encrypt a message
                               #' @param m the message
                               #' @return the encrypted message
                               encrypt = function(m) {
                                 private$randomize(
                                   mod.bigz(
                                     add.bigz(
                                       mul.bigz(self$n, m),
                                       ONE),
                                     self$nSquared))
                               },

                               #' @description
                               #' Add two encrypted messages
                               #' @param a a message
                               #' @param b another message
                               #' @return the sum of `a` and `b`
                               add = function(a, b) {
                                 mod.bigz(mul.bigz(a, b), self$nSquared)
                               },

                               #' @description
                               #' Subtract one encrypted message from another
                               #' @param a a message
                               #' @param b another message
                               #' @return the difference `a - b`
                               sub = function(a, b) {
                                 self$add(a, self$nSquared - b)
                               },

                               #' @description
                               #' Return the sum `a + b`
                               #' of an encrypted real message `a`,
                               #' a list consisting of a encrypted
                               #' integer part (named `int`) and an
                               #' encrypted fractional part (named `frac`),
                               #' and a real number `a` using
                               #' `den` as denominator in the rational
                               #' approximation.
                               #' @param den the denominator to use for rational approximations
                               #' @param a the _real_ message, a list consisting of the integer and fractional parts named `int` and `frac` respectively
                               #' @param b a simple real number
                               add_real = function(den, a, b) {
                                 int_value <- floor(b)
                                 frac_value <- b - int_value
                                 frac_number  <- gmp::as.bigq(gmp::numerator(gmp::as.bigq(frac_value) * den))
                                 enc_int_value <- self$encrypt(int_value)
                                 enc_frac_number <- pubkey$encrypt(frac_number)
                                 list(int = self$add(enc_int_value, a$int),
                                      frac = self$add(enc_frac_number, a$frac))
                               },

                               #' @description
                               #' Return the difference `a - b`
                               #' of an encrypted real message `a`,
                               #' a list consisting of a encrypted
                               #' integer part (named `int`) and an
                               #' encrypted fractional part (named `frac`),
                               #' and a real number `b` using
                               #' `den` as denominator in the rational
                               #' approximation.
                               #' @param den the denominator to use for rational approximations
                               #' @param a the _real_ message, a list consisting of the integer and fractional parts named `int` and `frac` respectively
                               #' @param b a simple real number
                               sub_real = function(den, a, b) {
                                 int_value <- floor(b)
                                 frac_value <- b - int_value
                                 frac_number  <- gmp::as.bigq(gmp::numerator(gmp::as.bigq(frac_value) * den))
                                 enc_int_value <- self$encrypt(int_value)
                                 enc_frac_number <- pubkey$encrypt(frac_number)
                                 list(int = self$sub(a$int, enc_int_value),
                                      frac = self$sub(a$frac, enc_frac_number))
                               },

                               #' @description
                               #' Return the product of two encrypted
                               #' messages `a` and `b`
                               #' @param a a message
                               #' @param b another message
                               #' @return the product of `a` and `b`
                               mult = function(a, b) {
                                 gmp::powm(a, b, self$nSquared)
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
#' @export
#' @format An \code{\link{R6Class}} generator object
PaillierPrivateKey <- R6Class("PaillierPrivateKey",
                              private = list(

                                lambda = NA,
                                x = NA
                              ),

                              public = list(

                                #' @field pubkey the public key
                                pubkey = NULL,

                                #' @description
                                #' Create a new private key with given secret `lambda` and the public key
                                #' @param lambda the secret
                                #' @param pubkey the public key
                                initialize = function(lambda, pubkey) {
                                  ## lambda
                                  private$lambda <- lambda
                                  self$pubkey <- pubkey

                                  ## x (cached) for decryption
                                  private$x <- inv.bigz(
                                    div.bigz(
                                      sub.bigz(
                                        gmp::powm(pubkey$nPlusOne, private$lambda, pubkey$nSquared),
                                        ONE),
                                      pubkey$n),
                                    pubkey$n)
                                },

                                #' @description
                                #' Return the secret lambda
                                #' @return lambda
                                getLambda = function() {
                                  private$lambda
                                },

                                #' @description
                                #' Decrypt a message
                                #' @param c the message
                                #' @return the decrypted message
                                decrypt = function(c) {
                                  mod.bigz(
                                    mul.bigz(
                                      div.bigz(
                                        sub.bigz(
                                          gmp::powm(c, private$lambda, self$pubkey$nSquared),
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

#'
#' @section Methods:
#' \describe{
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

                             #' @field pubkey the public key
                             pubkey = NULL,

                             #' @description
                             #' Create a new public private key pair with specified number of modulus bits
                             #' @param modulusBits the number of bits to use
                             #' @return a `PaillierKeyPair` object
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

                             #' @description
                             #' Return the private key
                             #' @return the private key
                             getPrivateKey = function() {
                               private$privkey
                             })
                           )



