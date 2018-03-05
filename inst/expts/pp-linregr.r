library(homomorpheR)
######################################
# Set up a simple artificial dataset
######################################
n = 50
x1 = 11:(10+n)
x2 = runif(n,5,95)
x3 = rbinom(n,1,0.5)

x = data.frame(x1,x2,x3)
x = scale(x)
x = data.frame(1,x)    # the intercept is handled using a column of 1's

sigma = 1.4
eps = rnorm(x1, 0, sigma)  # generate noise vector

b = c(17, -2.5, 0.5,-5.2)   # the real model coefficient

y = b[1] + b[2] * x[,2] + b[3] * x[,3] + b[4] * x[,4] + scale(eps)  # target variable

##############################################################################
# Batch Gradient Descent algorithm, here for benchmarking and error checking
# For a refresher on the algorithm, read the first 6 pages of
#    http://cs229.stanford.edu/notes/cs229-notes1.pdf
##############################################################################

lm_sgd <- function(iter, rate) {
  theta = c(0,0,0,0)
  alpha = rate
  for (iter in 1:iter) {
     adj = c(0,0,0,0)
     for (i in 1:4) {
         for (j in 1:n) {
             adj[i] = adj[i] + (sum(x[j,] * theta) - y[j]) * x[j,i]
         }
         # theta[i] = theta[i] - (alpha / n) * adj[i]         # adjust as we go is faster
     }
     for (i in 1:4) theta[i] = theta[i] - (alpha / n) * adj[i]
     print(adj)
  }
  print(theta)
}

lm_sgd(100, 0.1)

##########################################################################################
# Distributed Privacy-Preserving Gradient Descent algorithm for Linear Regression
##########################################################################################
# Basic setup:
# FIU holds the vector y of true labels
# RE1 holds the data x[,1:3]
# RE2 holds the data x[,4]
# Together, they want to learn a linear model to predict y using x but in such a way that
# - nobody sees each other's data,
# - RE1 holds the coefficients for the variables it holds, which is not visible to others
# - RE2 holds the coefficients for the variables it holds, which is not visible to others
# - Prediction of a new instance requires the collaboration of RE1 and RE2
###########################################################################################

###############################################################################################
# FIU first generates a public-private key pair using the Paillier scheme
# Public key is used by RE1 and RE2 to encrypt their data and do maths on them.
# Private key is visible only to FIU and is used by the FIU to decrypt data sent from the REs.
###########

library(homomorpheR)

keypair = PaillierKeyPair$new(modulusBits = 1024)
pubkey = keypair$pubkey
privkey = keypair$getPrivateKey()

# Convenience function
zipf = function(x1,x2,x3) {
  res = c()
  for (i in 1:length(x1)) res = c(res,x1[i],x2[i],x3[i])
  res
}

# zipf(5,10,1)
# zipf(c(1,2,3), c(4,5,6), c(1,1,1))

######################################################################
# Paillier is defined only on positive integers.
# We approximate a floating point number X by 3 integers (a,b,c)
# such that X is approximately equal to (a + b/largenum) / 10^c
#
# Example: Assuming largenum = 2^10, 5.2 can be represented by
#  (5, 204, 0) = (5 + 204/1024) / 1 = 5.19922
#  or (50, 2040, 1) = (50 + 2040/1024) / 10 = 5.19922
#
# In general, the larger largenum is, the better the approximation.
#################

largenum = 2^50
encodeFloat = function(x, e=0) {
  x = x * 10^e
  x1 = floor(x)
  x2 = x - x1
  x2 = floor(x2 * largenum)
  exps = rep(e, length(x))
  zipf(x1,x2,exps)
}

# encodeFloat(5.2)
# encodeFloat(5.2, 2)
# encodeFloat(c(4.5,5.2,7), 1)

# Some useful constants
zero = pubkey$encrypt('0')
one = pubkey$encrypt('1')

############################################################################
# We encrypt each number (a,b,c) by encrypting a and b with the public key
# The input can be a vector of numbers
################

encrypt = function(z, e=0) {
  y = encodeFloat(z, e)
  res = rep(zero, length(y))
  for (i in 1:length(y)) {
      if (i %% 3 == 0) { res[i] = y[i] }
      else { res[i] = pubkey$encrypt(as.character(y[i])) }
  }
  res
}

############################################################################
# We decrypt each encrypted number (enc(a), enc(b),c) using the private key
# by computing (dec(enc(a)) + dec(enc(b))/largenum) / 10^c
# The input can be a vector of numbers.
# We need to deal with negative numbers too.
##################

decrypt = function(en) {
  uen = en
  for (i in 1:length(uen)) {
      if (i %% 3 != 0) uen[i] = privkey$decrypt(en[i])
  }
  # deal with negative results;
  # see https://crypto.stackexchange.com/questions/19457/how-can-i-do-minus-on-plaintexts-in-the-paillier-cryptosystem
  for (i in 1:length(uen)) {
    if (i %% 3 != 0 &&    # don't process exponents
        uen[i] >= as.double(pubkey$n/3.0)) { uen[i] = uen[i] - pubkey$n }
  }
  res = c()
  for (i in seq(1, length(uen), 3)) {
      res = c(res,
             (as.double(uen[i]) + as.double(uen[i+1]/(largenum))) / as.double(10^uen[i+2]))
  }
  return(res)
}

# decrypt(encrypt(4.7))
# decrypt(encrypt(0.5))
# decrypt(encrypt(-2.3))
# (z = encrypt(c(5.2,4.7,-2.6)))
# decrypt(z)

############################################################################
# We next define addition of two encrypted numbers: (enc(a1), enc(b1), c1) and (enc(a2), enc(b2), c2).
# Addition can be done on the encrypted a's and encrypted b's if c1 = c2. We make c1 = c2 true by
# multiplying by powers of 10 when necessary.
# The need to make c1 = c2 is the reason why we can't have c1 and c2 encrypted.
# The function works on vectors.
###############

addenc = function(x, y) {
  res = x
  for (i in seq(1, length(x), 3)) {
       if (x[i+2] != y[i+2]) {  # if different exponent, make the exponents equal by multiplying
          res[i+2] = max(x[i+2], y[i+2])
          xdiff = res[i+2] - x[i+2]
          ydiff = res[i+2] - y[i+2]
          if (xdiff > 0) {
              x[i] = pubkey$mult(x[i], 10^(xdiff))
              x[i+1] = pubkey$mult(x[i+1], 10^(xdiff))
          }
          if (ydiff > 0) {
              y[i] = pubkey$mult(y[i], 10^(ydiff))
              y[i+1] = pubkey$mult(y[i+1], 10^(ydiff))
          }
       }
       res[i] = pubkey$add(x[i], y[i])
       # print(privkey$decrypt(c(x[i], y[i], res[i])))
       res[i+1] = pubkey$add(x[i+1], y[i+1])
  }
  res
}

############################################################################
# We next define subtraction of two encrypted numbers: (enc(a1), enc(b1), c1) and (enc(a2), enc(b2), c2).
# We simply multiply the second encrypted number by -1 and then add it to the first encrypted number.
############

subenc = function(x, y) {
  for (i in seq(1, length(x), 3)) {
      # y[i:i+1] = pubkey$mult(y[i:i+1], -1)  # this doesn't work, why?
      y[i] = pubkey$mult(y[i], -1)
      y[i+1] = pubkey$mult(y[i+1], -1)
  }
  addenc(x, y)
}

#############################################################################################
# We next define the multiplication of an encrypted number en = (enc(a),enc(b),c) by a scalar y
# Again, Paillier only allows y to be an integer. To deal with floating point numbers, we use
# en * y = en * (y_int + y_frac)
#        = en * y_int + (enc(a) * floor(y_frac * 10^n), enc(b) * floor(y_frac * 10^n), n)
# where n is a parameter with larger n leading to better approximation. We use n = 3 below.
#######

smultenc = function(x,y) {
  yint = floor(y * 1000)
  res = x
  for (i in seq(1, length(x), 3)) {
      res[i] = pubkey$mult(x[i], yint)
      res[i+1] = pubkey$mult(x[i+1], yint)
      res[i+2] =  x[i+2] + 3
  }
  res
}

################
# Here are some test cases
####
# five = encrypt(5.4)
# ten = encrypt(10.2)
# five
# ten
# decrypt(addenc(five,ten))
# decrypt(subenc(ten,five))
# decrypt(subenc(five,ten))
# decrypt(addenc(c(five,ten), c(ten,ten)))
#
# five = encrypt(5.4,1)
# ten = encrypt(10.2,2)
# decrypt(addenc(five,ten))
# decrypt(subenc(ten,five))
# decrypt(addenc(c(five,ten), c(ten,ten)))
#
# decrypt(smultenc(five, 4))
# decrypt(smultenc(ten, 6))
# decrypt(smultenc(c(five,ten), 4))
# decrypt(smultenc(c(five,ten), -4))

##################################################################
# Here are the functions to setup the parties in the computation
#################

x1 = c()  # these 3 vectors belong to RE1
x2 = c()
x3 = c()

re1setup = function() {
  x1 <<- encrypt(x[,1])
  x2 <<- encrypt(x[,2])
  x3 <<- encrypt(x[,3])
}

x4 = c() # this belong to RE2

re2setup = function() {
  x4 <<- encrypt(x[,4])
}

mastersetup = function() {
  re1setup()
  re2setup()
}

############
# This function computes the gradient: the difference between predicted values (encrypted) by REs and the true values
# Note the gradient is unencrypted. We could encrypt it if necessary.
#########

master_grad <- function(x) {
  xd = decrypt(x)
  xd - y
  # subenc(xd, y)
}

###########
# This is used to compute the partial predictions based on RE1's data only. The values are encrypted.
###########
re1pred <- function() {
   # return (theta1 * x[,1] + theta2 * x[,2] + theta3 * x[,3])
  return (addenc(smultenc(x1, theta1),
          addenc(smultenc(x2, theta2),
                 smultenc(x3, theta3)))
          )
}

#############
# Here, RE2 takes the partial prediction of RE1 and then add its contribution to the prediction.
# The final prediction values are encrypted.
###########

re2pred <- function(z) {
   # return (z + theta4 * x[,4])
  return (addenc(z, smultenc(x4, theta4)))
}

cadj = c(0,0,0,0) # a variable we want to print to keep track of progress

#############
# This is RE1's model update function and implements the gradient descent update formula.
# It takes the gradient (unencrypted) from the master and then adjust its own theta.
# The whole computation is assumed to take place within RE1 and no encryption is required.
# We can encrypt the whole thing if we want to.
###########

re1update <- function(z) {
   theta1 <<- theta1 - (alpha / n) * sum(z * x[,1])
   theta2 <<- theta2 - (alpha / n) * sum(z * x[,2])
   theta3 <<- theta3 - (alpha / n) * sum(z * x[,3])
   cadj[1] <<- sum(z * x[,1])
   cadj[2] <<- sum(z * x[,2])
   cadj[3] <<- sum(z * x[,3])
}

#############
# This is RE2's model update function and implements the gradient descent update formula.
# It takes the gradient (unencrypted) from the master and then adjust its own theta.
# The whole computation is assumed to take place within RE1 and no encryption is required.
# We can encrypt the whole thing if we want to.
###########

re2update <- function(z) {
   theta4 <<- theta4 - (alpha / n) * sum(z * x[,4])
   cadj[4] <<- sum(z * x[,4])
}

##########################################################################
# Finally, this is the privacy-preserving linear regression algorithm.
# The algorithm can be sensitive to the learning rate.
##########################################################################

pp_lm_sgd <- function(iter, rate) {

  theta1 <<- 0
  theta2 <<- 0
  theta3 <<- 0
  theta4 <<- 0
  alpha <<- rate

  mastersetup()  # encrypt the data and set up communication

  for (i in 1:iter) {
      p1 = re1pred()     # partial prediction
      px = re2pred(p1)   # full prediction
      grad = master_grad(px)  # compute gradient based on difference between true values and predicted values
      re1update(grad)    # update models independently
      re2update(grad)
      print(cadj)
  }
  print(c(theta1,theta2,theta3,theta4))
}

model0 = lm_sgd(100, 0.1)
model1 = pp_lm_sgd(100, 0.1)

