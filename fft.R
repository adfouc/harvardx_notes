
######################## fft tests



set.seed(101)
acq.freq <- 200
time     <- 1
w        <- 2*pi/time
ts       <- seq(0,time,1/acq.freq)
trajectory <- 3*rnorm(101) + 3*sin(3*w*ts)
plot(trajectory, type="l")

X.k <- fft(trajectory)

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}


plot.frequency.spectrum(X.k,xlimits=c(0,acq.freq/2))

X.n <-fft(X.k, inverse=TRUE)
max(Mod(X.n/length(X.k) - trajectory))

myft <- function( xn ){
  N<-length(xn)
  sapply( 1:N, function(k) {
    1/N * sum ( xn * exp (-2i*pi * (1:N-1) *(k-1) / N)) 
  })
}
X.n<-trajectory
X.k<-fft(X.n)  # unormalized
max( Mod( myft(X.n) - X.k / length(X.n) ) )
