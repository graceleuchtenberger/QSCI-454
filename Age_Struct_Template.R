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


# Setup matrix to hold model output, with each year in row, each age in column

output <- matrix (NA, nrow = length(years), ncol = 5)
colnames(output) <- c("Age 1", "Age 2", "Age 3", "Age 4", "Age 5")

# Input starting values into the first row

output[1,]<- c(N1.start, N2.start, N3.start, N4.start, N5.start)

####################################################################
# A generic loop, Insert new equations on the right hand side for each row
## MODIFY THIS LOOP and code below TO GENERATE POPULATION DYNAMICS.
for (i in 1:tmax){
  output[i+1,1]<- (F3*output[i,3] + F4*output[i,4] + F5*output[i,5]) # put in equation to calculate number of age 1 
  output[i+1,2]<- (S1*output[i,1]) # put in equation to calculate number of age 2 
  output[i+1,3]<- (S2*output[i,2]) # put in equation to calculate number of age 3 
  output[i+1,4]<- (S3*output[i,3]) # put in equation to calculate number of age 4 
  output[i+1,5]<- (S4*output[i,4]) # put in equation to calculate number of age 5 
}

# calculate stable age distribution in year 50
N.stable <- c(output[50,1]/sum(output[50,1:5]),
              output[50,2]/sum(output[50,1:5]),
              output[50,3]/sum(output[50,1:5]),
              output[50,4]/sum(output[50,1:5]),
              output[50,5]/sum(output[50,1:5]))

print(N.stable)

# Calculate population growth rate in year 50
lambda <- sum(output[50,1:5])/sum(output[49,1:5])


##### End of code region that you must modify
###############################################################
# print out lambda
print(lambda)  



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

