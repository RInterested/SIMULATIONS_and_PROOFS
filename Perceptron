# DATA PRE-PROCESSING:
dat = read.csv("perceptron.txt", header=F)
dat[,1:2] = apply(dat[,1:2], MARGIN = 2, FUN = function(x) scale(x)) # scaling the data
data = data.frame(rep(1,nrow(dat)), dat) # introducing the "bias" column
colnames(data) = c("bias","test1","test2","y")
data$y[data$y==0] = -1 # Turning 0/1 dependent variable into -1/1.
data = as.matrix(data) # Turning data.frame into matrix to avoid mmult problems.

no.iter = 10 

palette(c("red","yellowgreen"))
plot(test2 ~ test1, col = as.factor(y), pch = 20, data=data,
     main="College admissions", xlab="Results on test 1",
     ylab = "Results on test 2")
(x = c(min(data[,2])-.2,  max(data[,2])+ .2))


# PERCEPTRON:

set.seed(62416)
                                         
theta = rnorm(ncol(data) - 1)            # Starting a random vector of coefficients.
theta = theta/sqrt(sum(theta^2))         # Normalizing the vector.
save_for_later = theta

(y = c((-1/theta[3]) * (theta[2] * x + theta[1])))
lines(x, y, lwd=2, col=rgb(0,0,1,.2))

h = theta %*% t(data[,1:3])              # Performing the first f(theta^T X)

for (i in 1:no.iter){                    # We will recalculate 1,000 times
  for (j in 1:nrow(data)){               # Each time we go through each example.
    if(h[j] * data[j, 4] < 0){         # If the hypothesis disagrees with the sign of y,
      theta = theta + (sign(data[j,4]) * data[j, 1:3]) # We + or - the example from theta.
      }
    else
      theta = theta                      # Else we let it be.
  }
  h = theta %*% t(data[,1:3])            # Calculating h() after iteration.
  (y = c((-1/theta[3]) * (theta[2] * x + theta[1])))
  lines(x, y, lwd=2, col=rgb(0,0,1,.2))
}
theta                                    # Final coefficients
mean(sign(h) == data[,4])                # Accuracy

#Movie:

plot(test2 ~ test1, col = as.factor(y), pch = 20, data=data,
     main="College admissions", xlab="Results on test 1",
     ylab = "Results on test 2")
theta = save_for_later
(y = c((-1/theta[3]) * (theta[2] * x + theta[1])))
lines(x, y, lwd=2, col=rgb(0,0,1,.8))

h = theta %*% t(data[,1:3])              # Performing the first f(theta^T X)

for (i in 1:no.iter){                    # We will recalculate 1,000 times
  for (j in 1:nrow(data)){               # Each time we go through each example.
    if(h[j] * data[j, 4] < 0){         # If the hypothesis disagrees with the sign of y,
      theta = theta + (sign(data[j,4]) * data[j, 1:3]) # We + or - the example from theta.
    }
    else
      theta = theta                      # Else we let it be.
  }
  h = theta %*% t(data[,1:3])            # Calculating h() after iteration.
  (y = c((-1/theta[3]) * (theta[2] * x + theta[1])))
  plot(test2 ~ test1, col = as.factor(y), pch = 20, data=data,
       main="College admissions", xlab="Results on test 1",
       ylab = "Results on test 2")
  lines(x, y, lwd=2, col=rgb(0,0,1,.8))
}

# Learning curve:

plot(NULL, xlim=c(0,no.iter), ylim=c(0,1), ylab="Accuracy", 
     xlab="Iteration", main="Learning curve")

theta = save_for_later
h = theta %*% t(data[,1:3])              # Performing the first f(theta^T X)

for (i in 1:no.iter){                    # We will recalculate 1,000 times
  for (j in 1:nrow(data)){               # Each time we go through each example.
    if(h[j] * data[j, 4] < 0){         # If the hypothesis disagrees with the sign of y,
      theta = theta + (sign(data[j,4]) * data[j, 1:3]) # We + or - the example from theta.
    }
    else
      theta = theta                      # Else we let it be.
  }
  h = theta %*% t(data[,1:3])            # Calculating h() after iteration.
  points(i, mean(sign(h) == data[,4]), pch=24, col = 'darkred', bg="darkred")
}


# LOGISTIC REGRESSION:

dat = read.csv("perceptron.txt", header=F)
colnames(dat) = c("test1","test2","y")
plot(test2 ~ test1, col = as.factor(y), pch = 20, data=dat,
     main = "College admissions", xlab = "Results test 1",
     ylab = "Results test 2")
fit = glm(y ~ test1 + test2, family = "binomial", data = dat)
(coefs = coef(fit))
(x = c(min(dat[,1])-2,  max(dat[,1])+2))
(y = c((-1/coefs[3]) * (coefs[2] * x + coefs[1])))
lines(x, y, lwd = 2, col = rgb(0,0,1,.5))
mean(sign(predict(fit))==data[,4])
