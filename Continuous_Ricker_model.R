#Saving for github

### Parameters ####
alpha_l <- 4.5 # to avoid numerical issues, must be less than 5
K <- 1
d <- 1
No <- 0.1
years <- 30

### These are parameters that specify how the numerical intergration is to be done.  Do not change! ####
inc <- 4 # must be an integer greater than or equal to one
deltat <- 1 / inc


### Run this function to load it into memory.  This does the numerical integration ####
runge_kutta <- function(alpha_l, K, d, No, nt, deltat) {
  tlist <- seq(from = 0, to = nt, by = deltat)
  Nt <- rep(NA, length(tlist))
  Nt[1] <- No
  
  nbfun <- function(alpha_l, K, d, N) N * exp(alpha_l * ( 1 - N / K)) - d * N
  
  for (i in 2:length(tlist)) {
    K1 <- nbfun(alpha_l, K, d, Nt[i-1])
    Nmid1 <- Nt[i -1] + 0.5 * deltat * K1
    K2 <- nbfun(alpha_l, K, d, Nmid1)
    Nmid2 <- Nt[i - 1] + 0.5 * deltat * K2
    K3 <- nbfun(alpha_l, K, d, Nmid2)
    Nend <- Nt[i-1] + deltat * K3
    K4 <- nbfun(alpha_l, K, d, Nend)
    Nt[i] <- Nt[i-1] + deltat * (K1 / 6 + K2 / 3 + K3 / 3 + K4 / 6)
  }
  return(Nt)
}

### Run function and plot N(t) vs. t ####
Nt <- runge_kutta(alpha_l, K, d, No, nt = years, deltat = deltat)
plotindex <- 1 + seq(0, length(Nt), by = 1 / deltat)
years.2.plot <- seq(0, years, by = deltat)[plotindex]
Nt.2.plot <- Nt[plotindex]
plot(years.2.plot, 
     Nt.2.plot,
     type = "l",
     lwd = 2,
     xlim = c(0, 30),
     ylim = c(0, 2),
     xlab = "time",
     ylab = "N(t)",
     xaxs = "i",
     yaxs = "i",
     main = bquote(alpha[l] ==  .(alpha_l))
     
)
points(years.2.plot, 
       Nt.2.plot,
       pch = 21,
       bg = "black")


