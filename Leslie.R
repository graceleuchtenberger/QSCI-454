# Template for Age Structured Model
# 
# # Define Parameters

F3 <- 10
F4 <- 15
F5 <- 20
S1 <- 0.2
S2 <- 0.3
S3 <- 0.4
S4 <- 0.5

# Initial Conditions
N1.start <- 20
N2.start <- 15
N3.start <- 10
N4.start <- 5
N5.start <- 1

tmax <- 50
years <- 0:tmax
n_ages <- 5

# Setup matrix to hold model output, with each year in row, each age in column
output <- matrix(NA, nrow = length(years), ncol = 5)
output[1,] <- c(N1.start, N2.start, N3.start, N4.start, N5.start)
colnames(output)<- c("Age 1", "Age 2", "Age 3", "Age 4", "Age 5")


# Create A matrix

A <- matrix(0, nrow = n_ages, ncol = n_ages)
A[1,3] <- F3
A[1,4] <- F4
A[1,5] <- F5
A[2,1] <- S1
A[3,2] <- S2
A[4,3] <- S3
A[5,4] <- S4

# A generic loop, Insert new equations on the right hand side for each row

for (i in 1:tmax){
  output[i+1,]<- A %*% output[i,] 
}


# plot the output
par(las =1)
color.list <- c("black","purple", "blue","grey","red")
plot(years, output[,1], 
     type = "l",
     lwd = 2,
     xaxs = "i",
     yaxs = "i",
     ylab = "Abundance",
     xlab = "Year",
     col = "black")


for (i in 1:4) lines(years, output[,i], lwd = 2, col = color.list[i])

legend("topleft", legend = c("N1", "N2", "N3", "N4", "N5"), lty = "solid", col = color.list, lwd = 2)
ev <- eigen(A)
eigen.vals <- ev$values
eigen.vects <- ev$vectors
# find the dominant eigenvalue
lambda <- Re(eigen.vals[1])
lambda

w <- Re(eigen.vects[,1])
stable.age <- w / sum(w)

# perform calculations for elasticity analysis
# first, give left eigenvector
evl <- eigen(t(A))
v <- Re(evl$vectors[,1])

# caclulate scalar product
sp <- sum(v * w)
# numerator of elasticity matrix
vi.wj <- v %o% w

e <- A / lambda * (vi.wj / sp)

# prepare output
eigen.output<-list(lambda=lambda,stable.age=w/sum(w),elasticities=e)       
print(eigen.output)


