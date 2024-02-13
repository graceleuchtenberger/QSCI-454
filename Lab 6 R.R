# Create objhects and assign values to them
x <- 10
p <- 0.5
N <- 20

#demo: get the probability for a single outcome
dbinom(x = x, size = N, prob = p)

#demo part 2: get probability for a range of outcomes
x <- 0:N

# get the probability for each value of x
prob <- dbinom(x = x, size = N, prob = p)

#print and plot the
cbind(x, prob)
plot(x, prob,
     type = "h",
     lwd = 2,
     xlab = "Number of deaths"
     )

x <- 8:12


## Question 1A #####

# N represents the number of salamanders
# x represents the number of salamanders that die
# p represents the probability of a salamander dying


## Question 1b ####

N <- 20
x <- 0:N
p <- 0.5


prob <- dbinom(x = x, size = N, prob = p)
# values between 0.4 and 0.6
prob.8.12 <- prob[8:12]
sum(prob.8.12)


## Question 1c ####

# Create a function that takes N and p, and the desired range around p (call this deltap),
# and returns the probability of getting an outcome within those limits

prob_within_deltap <- function(N, p, deltap){
  # salamanders that die
  x <- 0:N
  prob <- dbinom(x = x, size = N, prob = p)
  
  x_index <- which(x>=N * (p - deltap) & x<=N * (p + deltap))
  
  prob_within <- sum(prob[x_index])

  return(prob_within)
}

# p = 0.25
prob_within_deltap(N = 20, p = 0.25, deltap = 0.1)
# probability: 0.8069277

# p = 0.5
prob_within_deltap(N = 20, p = 0.5, deltap = 0.1)
# probability: 0.736824

# p = 0.75
prob_within_deltap(N = 20, p = 0.75, deltap = 0.1)
# probability: 0.8069277


## Question 1d ####

# If P either small or large (close to 1 or 0), the variance is minimal, and variance is highest at 0.5. Larger variance means that theres a wider spread of outcomes, so within a set of x values, the probability of getting a certain set of outcomes declines. Whereas, with a smaller spread of outcomes (smaller variance), the probability of getting a certain set of outcomes is higher, if the spread is smaller. 

#### Part 2 ####

## Question 2a ####
#We should use a Possion we are describing a continous set of successes over the sampling frame, so at any time frame there is a chance of success (a bear sighting), we aren't talking about patchiness so we don't need to use a negative binomial. 

x <- 0:100
prob.bear <- dpois(x, lambda = 0.14 * 194.4)
plot(x, prob.bear,
     type = "h",
     lwd = 2,
     xlab = "Number of bears"
)

## Question 2b ####
#2B. Your code that generates a plot of the probability function where the outcome is on the x-axis and the probability of each outcome is on the y axis and calculates the probability of seeing 51 or more grizzly bears. (2).
 

sum(prob.bear[52:100])

## Question 2c ####

# The probability of seeing 51 or more grizzly bears is very low, so it is unlikely that the true density is 0.14 bears. We beleive the true density is likely higher, because with this distribution it is unlikely to see a value above 51. So likely the true distribution of probability values is shifted higher than what we are observing.

# If we had a negative binomial and we had patchiness in the data than we would expect to see a lower probability of seeing 51 or more grizzly bears, because in theory this data collection would've just included some patches of grizzly bears, because there is more oppertuntiy for variability in the data.

## Bonus #####
x <- 0:100
lambda <- 0.14 * 194.4
k <- 1
dbinom <- dnbinom(x=x, size = k, mu = lambda)
sum(dbinom[52:100])

plot(x, dbinom,
     type = "h",
     lwd = 2,
     xlab = "Number of bears"
)
