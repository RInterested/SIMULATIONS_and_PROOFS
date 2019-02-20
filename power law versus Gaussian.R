plot(x,dnorm(x), type ="l", col=3, main="Decay Std Normal pdf with doubling of x", cex.main=.8, ylab="density", xlab="", cex.lab=0.8, ylim=c(0,1), cex.axis=.6)
lines(x,(dnorm(2*x)/dnorm(x)),type="l", col=2)
