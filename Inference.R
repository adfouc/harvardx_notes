##
## Inference
# Module 4

library(tidyverse)

#------------------------------
# section 1
##
# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p<-seq(0,1,length=100)

# Create a variable `se` that contains the standard error of each sample average
se<-sapply(p, function(x) sqrt(x*(1-x)/N))

# Plot `p` on the x-axis and `se` on the y-axis
plot(p,se)

##
# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for (N in sample_sizes)
{
  se<-sqrt(p*(1-p)/N)
  plot(p,se, ylim=c(0,0.1))
  
}


##



#------------------------------
# section 2

## Code: Computing the probability of  X¯  being within .01 of  p 
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)

## Code: Monte Carlo simulation using a set value of p
p <- 0.45    # unknown p to estimate
N <- 1000

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

# Code: Histogram and QQ-plot of Monte Carlo results
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

#
# Code: Plotting margin of error in an extremely large poll over a range of values of p

library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()
#-------------------------------
# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.

take_sample<-function(p,N){mean(sample(c(0,1),N,replace=TRUE,prob=c(1-p,p)))}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.

take_sample(p,N)

#
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications

errors<-replicate(B,p-take_sample(p,N))

# Calculate the mean of the errors. Print this value to the console.
mean(errors)
hist(errors)
mean(abs(errors))
# Calculate the standard deviation of `errors`: calcul de SE par montecarlo 
sqrt(mean(errors^2))
# valeur de SE theorique
sqrt(p*(1-p)/N)

#-- Estimation de la valeur SE theorique
p <- 0.45
N <- 100
set.seed(1)

# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X<-sample(c(0,1),N,replace=TRUE,prob=c(1-p,p))

# Define `X_bar` as the average sampled proportion
X_bar<-mean(X)

# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar*(1-X_bar)/N)

## Create a plot of the largest standard error for N ranging from 100 to 5,000. Based on this plot, how large does the sample size have to be to have a standard error of about 1%?
  
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N,se)

# Generate a qq-plot of `errors` with a qq-line showing a normal distribution

qqnorm(errors)
qqline(errors)

# Generate a qq-plot of `errors` with a qq-line showing a normal distribution

qqnorm(errors)
qqline(errors)

# estimated proportion of Democrats in the population is greater than 0.5. 
1-pnorm(0.5,p,sqrt(p*(1-p)/N))

# Estimating the probability of a specific error size
N <-100
X_hat <- 0.51
se_hat<-sqrt(X_hat*(1-X_hat)/N)

# Calculate the probability that the error is 0.01 or larger
1-pnorm(q=0.01,sd=se_hat)+pnorm(q=-0.01,sd=se_hat)

#
# Code: geom_smooth confidence interval example
# The shaded area around the curve is related to the concept of confidence intervals.

data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

#
#Code: Monte Carlo simulation of confidence intervals
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.

p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean

# Code: Solving for  z  with qnorm
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval

# A Monte Carlo Simulation for Confidence Intervals
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

# Power
# Confidence interval for the spread with sample size of 25
# Note that to compute the exact 95% confidence interval, we would use c(-qnorm(.975), qnorm(.975)) instead of 1.96.

N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)
 
# pValue
#The null hypothesis is the hypothesis that there is no effect. In this case, the null hypothesis is that the spread is 0, or  p=0.5 .
#The p-value is the probability of detecting an effect of a certain size or larger when the null hypothesis is true.
#We can convert the probability of seeing an observed value under the null hypothesis into a standard normal random variable. We compute the value of  z  that corresponds to the observed result, and then use that  z  to compute the p-value.
#If a 95% confidence interval does not include our observed value, then the p-value must be smaller than 0.05.
#It is preferable to report confidence intervals instead of p-values, as confidence intervals give information about the size of the estimate and p-values do not.

# Computing a p-value for observed spread of 0.02
N <- 100    # sample size
# null hypothesis: Xhat=0.5, SE_hat = sqrt (0.5*(1-0.5)/N) = 0.5/sqrt(N)
# P(|X-Xhat|>0.02)=P(Z > 0.02/SE_hat)
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))

# section 3 assessment

# Load the data
library(dslabs)
library(tidyverse)
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls<-polls_us_election_2016%>%filter(state=="U.S." & enddate >= "2016-10-31")


# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N<-polls[1,"samplesize"]
N
# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat<-polls[1,"rawpoll_clinton"]/100
X_hat

# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat<-sqrt(X_hat*(1-X_hat)/N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<-c(X_hat-se_hat*qnorm(1-0.05/2),X_hat+se_hat*qnorm(1-0.05/2))

# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
pollster_results<-polls%>%mutate(X_hat=rawpoll_clinton/100,se_hat=sqrt(X_hat*(1-X_hat)/samplesize),lower=X_hat-se_hat*qnorm(0.975), upper=X_hat+se_hat*qnorm(0.975))%>%select(pollster,enddate,X_hat,se_hat,lower,upper)
pollster_results
#-
avg_hit <- pollster_results%>%mutate(hit=lower <= 0.482 & upper>= 0.482)%>%summarise(mean(hit))

#-Confidence interval for d
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 
polls<-polls%>%mutate(d_hat = (rawpoll_clinton -rawpoll_trump)/100)

N<-polls[1,"samplesize"]
N
d_hat<-polls[1,"d_hat"]
d_hat
X_hat<-(d_hat+1)/2

se_hat<-2*sqrt(X_hat*(1-X_hat)/N)
se_hat
ci<-d_hat+c(-1,+1)*se_hat*qnorm(0.975)
ci

#--Pollster results for d
pollster_results <- polls %>% mutate(X_hat=(d_hat+1)/2, se_hat=2*sqrt(X_hat*(1-X_hat)/samplesize), 
                                     lower=d_hat-se_hat*qnorm(0.975), upper=d_hat+se_hat*qnorm(0.975)) %>% 
  select(pollster,enddate,d_hat,lower,upper)
#-- Comparing to actual results - d
avg_hit <- pollster_results %>% mutate(hit= 0.021<=upper & 0.021>=lower) %>% summarise(mean(hit))
avg_hit

#-Comparing to actual results by pollster
polls<- polls %>% mutate(error=d_hat-0.021 )
polls %>% ggplot(aes(pollster,error))+geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#-Comparing to actual results by pollster - multiple polls
# only for pollsters that took five or more polls.
polls<- polls %>% mutate(error=d_hat-0.021 )
polls %>% group_by(pollster) %>% filter(n()>=5) %>% ggplot(aes(pollster,error))+geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Section 4

#
# Code: Simulating polls
#Note that to compute the exact 95% confidence interval, we would use qnorm(.975)*SE_hat instead of 2*SE_hat.

d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

#
#Code: Calculating the spread of combined polls

d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)

## Pollster bias
#
# Code: Generating simulated poll data
library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat

# compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

## Code: Investigating poll data and pollster bias
# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())

# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

## data-driven models

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)


#-- SECTION 4 ASSESSMENT
library(dslabs)
data(heights)

x <- heights %>% filter(sex == "Male") %>%
  .$height

mean(x)
sd(x)
##
N <- 50
X<-sample(x,N,replace=TRUE)
# sample average : It is a random variable with expected value mu and standard error sigma
mean(X)
sd(X)
##
X <- sample(x, N, replace = TRUE)
#  standard error of the estimate
se<-sd(X)/sqrt(N)
se

#  95% confidence interval for the population average based on our sample
ci<-mean(X)+c(-1,1)*qnorm(0.975)*se
ci

## Now run a Monte Carlo simulation in which you compute 10,000 confidence intervals 
## as you have just done. What proportion of these intervals include mu?
# Define `mu` as the population average
mu <- mean(x)
set.seed(1)
N <- 50
B <- 10000

# logical vector for simulated intervals that contain mu
res<-replicate(B,{ 
  X<-sample(x,N,replace=TRUE)
  se<-sd(X)/sqrt(N)
  between((mu-mean(X))/(qnorm(0.975)*se), -1,1)
})

#  proportion of results in `res` that include mu
mean(res)

##
polls <- polls_us_election_2016 %>% 
filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
         enddate >= "2016-10-15" &
         state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls%>% ggplot(aes(pollster,spread))+geom_boxplot()+geom_point()

## model : Yij = d + bi +epsilon(i,j)
# expected value of Y1_hat is d+b1, stderror is sigma1/sqrt(N1)
# expected value of Y2_hat - Y1hat = b2-b1
# stderror of Y2_hat - Y1hat = sqrt (sigma1^2/N1+sigma2^2/N2)

## compute the estimates of sigma(i)
sigma<-polls%>%group_by(pollster)%>%summarize(s=sd(spread))%>%select(pollster,s)
sigma

## What does the central limit theorem tell us about the distribution of the differences between the pollster averages?
## => If we assume N2 and N1 are large enough, Y2_hat and Y1_hat, and their difference, are approximately normal.

## We have constructed a random variable that has expected value b2−b1, the pollster bias difference. 
# If our model holds, then this random variable has an approximately normal distribution. 
# The standard error of this random variable depends on sigma1 and sigma2, but we can use the sample standard deviations we computed earlier. 
# We have everything we need to answer our initial question: is b2−b1 different from 0?


#  summarizes the average, standard deviation, and number of polls for the two pollsters.
res<-polls%>%group_by(pollster)%>%summarize(avg=mean(spread),sd=sd(spread),n=n())

#  difference between the larger average and the smaller 
estimate<-max(res["avg"])-min(res["avg"])
estimate

#  standard error of the estimates 
se_hat<-sqrt(sum(res["sd"]^2/res["n"]))
se_hat

#  95% confidence interval of the spreads. 
ci<-estimate+c(-1,1)*qnorm(0.975)*se_hat
ci

##
## The confidence interval tells us there is relatively strong pollster effect resulting in a difference of about 5%. Random variability does not seem to explain it.
# Compute a p-value to relay the fact that chance does not explain the observed pollster effect.

res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# Calculate the p-value = 1E-13
2*(1-pnorm(estimate,0,se_hat))

## Comparing Within-Poll and Between-Poll Variability
# We compute statistic called the t-statistic by dividing our estimate of b2−b1 by its estimated standard error:
#  
#  Y¯2−Y¯ / sqrt(s2^2/N2+s1^2/N1)
#Later we learn will learn of another approximation for the distribution of this statistic for values of N2 and N1 that aren't large enough for the CLT.
# Note that our data has more than two pollsters. We can also test for pollster effect using all pollsters, not just two. The idea is to compare the variability across polls to variability within polls. We can construct statistics to test for effects and approximate their distribution. The area of statistics that does this is called Analysis of Variance or ANOVA. We do not cover it here, but ANOVA provides a very useful set of tools to answer questions such as: is there a pollster effect?
#Compute the average and standard deviation for each pollster and examine the variability across the averages and how it compares to the variability within the pollsters, summarized by the standard deviation.
# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.

var<-polls%>%group_by(pollster)%>%summarize(avg=mean(spread),s=sd(spread))%>%select(pollster,avg,s)
var

## SECTION 5
##


## Baye's Theorem

## Cystic fibrosis test probabilities
# Code: Monte Carlo simulation
prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)


# The techniques we have used up until now are referred to as frequentist statistics as they consider only the frequency of outcomes in a dataset and do not include any outside information. Frequentist statistics allow us to compute confidence intervals and p-values.
# Frequentist statistics can have problems when sample sizes are small and when the data are extreme compared to historical results.
# Bayesian statistics allows prior knowledge to modify observed results, which alters our conclusions about event probabilities.

# Hierarchical models
#  the first level is called the prior distribution and the second level is called the sampling distribution.

##
## Polls aggregators
library(tidyverse)
library(dslabs)
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d + 1) / 2

polls <- map_df(Ns, function(N) {
  x <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1 - x_hat) / N)
  list(estimate = 2 * x_hat - 1, 
       low = 2*(x_hat - 1.96*se_hat) - 1, 
       high = 2*(x_hat + 1.96*se_hat) - 1,
       sample_size = N)
}) %>% mutate(poll = seq_along(Ns))


d_hat <- polls %>% 
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>% 
  pull(avg)

sum(polls$sample_size)
[1] 11269
> n<-sum(polls$sample_size)
> p<-(d_hat+1)/2
> p
[1] 0.5208093
> se<-sqrt(p*(1-p)/n)
> se
[1] 0.004705989
> 4*se
[1] 0.01882395

## Polls aggregator
# polls Data
data(polls_us_election_2016)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade)))
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  pull(d_hat)
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat * (1 - p_hat) / sum(polls$samplesize))
moe

polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

# pollster bias
polls %>% group_by(pollster) %>% summarize(n())
polls%>%  group_by(pollster) %>% filter(n() >= 6) %>% ggplot(aes(spread,pollster))+geom_point()
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

# Data-driven models
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()
qplot(spread,data=one_poll_per_pollster,binwidth=0.01)
sd(one_poll_per_pollster$spread)
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread),
          se = sd(spread) / sqrt(length(spread))) %>%
  mutate(start = avg - 1.96 * se,
         end = avg + 1.96 * se)
round(results * 100, 1)

## Bayes theorem simulation
prev <- 0.00025
N <- 100000
outcome <- sample(c("Disease","Healthy"), N, replace = TRUE,
                  prob = c(prev, 1 - prev))
N_D <- sum(outcome == "Disease")
N_D
N_H <- sum(outcome == "Healthy")
N_H
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"]  <- sample(c("+", "-"), N_D, replace = TRUE, prob = c(accuracy, 1 - accuracy))
test[outcome == "Healthy"]  <- sample(c("-", "+"), N_H, replace = TRUE, prob = c(accuracy, 1 - accuracy))
table(outcome, test)


#
# section 5 assesment
#

## Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
results<-polls%>%summarize(avg=mean(spread),se=sd(spread)/sqrt(n()))
results

## Prior distribution
# Assume a Bayesian model sets the prior distribution for Florida's election night spread d to be normal
# with expected value mu and standard deviation tau.
# mu an tau summarize what we would predict for Florida before seeing any polls.

##
# estimate posterior distribution
# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma<-results$se

# Define a variable called `Y` that contains the average in the object `results`
Y<-results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B<-sigma^2/(sigma^2+tau^2)

# Calculate the expected value of the posterior distribution
exp_value <- B*mu + (1-B)*Y 
exp_value # 0.002

# Compute the standard error of the posterior distribution. 
se<-1/sqrt(1/sigma^2+1/tau^2)
se # 0.005

## Construct the 95% credible interval. 
B*mu+(1-B)*Y+c(-1,1)*qnorm(0.975)*se

pnorm(0,exp_value,se)

## change the priors
mu <- 0
sigma <- results$se
Y <- results$avg
taus <- seq(0.005, 0.05, len = 100)

p_calc<-function(tau){
  B<-sigma^2/(sigma^2+tau^2)
  exp_value <- B*mu + (1-B)*Y 
  se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
  pnorm(0,exp_value,se)
}

ps<-sapply(taus,p_calc)

plot(taus,ps)

##
## section 6. Election forecasting
# - Understand how pollsters use hierarchical models to forecast the results of elections.
# - Incorporate multiple sources of variability into a mathematical model to make predictions.
# - Construct confidence intervals that better model deviations such as those seen in election data using the t-distribution.

##
## Statistical Models
##

## Cas Study : election forecating
library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

## Bayesian approach / computing the posterior mean, standard error, credible interval and probability
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
posterior_mean
#> [1] 0.0281
posterior_se
#> [1] 0.00615
1 - pnorm(0, posterior_mean, posterior_se)
[1] 0.9999975
## =>  general bias

##  mathematical representation  of models
#  random variability of polls. X.j = d + eps.j
set.seed(3)
J <- 6
N <- 2000
d <- .021
p <- (d + 1)/2
X <- d + rnorm(J, 0, 2 * sqrt(p * (1 - p) / N))
# pollster effect X.i.j = d + h.i + eps.i.j
# for one specific pollster with sigma.h = 0.025:
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d + 1) / 2
h <- rnorm(I, 0, 0.025)
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J, 0, 2 * sqrt(p * (1 - p) / N))
})
> h
[1]  0.002135443  0.027915255 -0.030471435  0.031684218 -0.018619540
> X
[,1]       [,2]        [,3]       [,4]         [,5]
[1,] -0.002153795 0.03442330 -0.04672673 0.02727060  0.018152788
[2,]  0.007120713 0.07628570 -0.02030179 0.07281836  0.031450937
[3,]  0.028783676 0.05338219 -0.02603867 0.07172618  0.003235613
[4,]  0.026534539 0.03598282  0.01647500 0.06895284 -0.019512162
[5,]  0.016257553 0.02784942  0.01315408 0.06914927  0.020125587
[6,]  0.001830027 0.04436076 -0.01108280 0.04481210  0.019963410

# additional term (b) random variable that accounts for the election-to-election variability.
# Xi.j = d + b +h.i +eps.i.j
#  we can estimate b from previous elections and study the distribution of these values. 
# Based on this approach we assume that, across election years, b has expected value 0 and the standard error is about sigmab=0.025.
# Since the same b is in every measurement, the average does not reduce the variability introduced by the b term
# Xhat=d+b+sum(Xi)/N
# sd (Xhat) = sqrt[  sigma^2 / N + sigma.b^2 ]
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
1 - pnorm(0, posterior_mean, posterior_se)
[1] 0.8174373

#####################################
## Code: Top 5 states ranked by electoral votes
## 
  
  library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)
results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

## Code: Computing the average and standard deviation for each state
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", "state") &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

## Code: Calculating the posterior mean and posterior standard error
##
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))
## 
## Code: Monte Carlo simulation of Election Night results (no general bias)
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

##
## Code: Monte Carlo simulation including general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clinton_EV_2) %>%
  ggplot(aes(clinton_EV_2)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

##
# In poll results,  p  is not fixed over time. Variability within a single pollster comes from time variation.
# In order to forecast, our model must include a bias term  bt  to model the time effect.
# Pollsters also try to estimate  f(t) , the trend of  p  given time  t  using a model like:
#  Yi,j,t=d+b+hj+bt+f(t)+ϵi,j,t 
# Once we decide on a model, we can use historical data and current data to estimate the necessary parameters to make predictions

## Code: Variability across one pollster
# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")

## Code: Trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

## Code: Plotting raw percentages across time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))

##
## assesment 6.1
##

# Confidence Intervals of Polling Data

library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
cis<-polls%>%mutate(X_hat=(spread+1)/2, se= 2*sqrt(X_hat*(1-X_hat)/samplesize),
                    lower=spread-se*qnorm(0.975),upper=spread+se*qnorm(0.975))%>%
  select(state,startdate,enddate,pollster,grade,spread,lower,upper)

## compare to actual results

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
actual_spread<-0.03 # ??
p_hits<-ci_data%>%summarize(sum(ifelse(actual_spread>=lower& actual_spread<=upper,1,0))/n())
p_hits

## Stratify by Pollster and Grade

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
p_hits<- ci_data%>% mutate(hit=ifelse(actual_spread>=lower & actual_spread<=upper,1,0)) %>% group_by(pollster)%>%filter(n()>4)%>%
  summarize(proportion_hits=mean(hit),n=n(), grade=grade[1]) %>% 
  arrange(desc(proportion_hits))

## Stratify by State

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.
p_hits<- ci_data%>% mutate(hit=ifelse(actual_spread>=lower & actual_spread<=upper,1,0)) %>% 
  group_by(state)%>%filter(n()>4)%>%
  summarize(proportion_hits=mean(hit),n=n()) %>% 
  arrange(desc(proportion_hits))

## Plotting Prediction Results

p_hits%>%ggplot(aes(state,proportion_hits))+geom_bar(stat="identity")+coord_flip()

## Predicting the Winner

errors<-cis%>%mutate(error=spread-actual_spread,hit=ifelse(sign(spread)==sign(actual_spread),TRUE,FALSE))
tail(errors,6)

## plot
# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls
p_hits<-errors%>%group_by(state)%>%filter(n()>4)%>%summarize(proportion_hits=mean(hit),n=n())
p_hits %>% arrange(proportion_hits) %>% ggplot(aes(state,weight=proportion_hits)) + geom_bar()+coord_flip()

hist(errors$error)
#errors%>%summarize(median(error))
median(errors$error)

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors%>%filter(grade %in% c('B+','A-','A+','A'))%>% 
  group_by(state)%>%  filter(n()>4)%>%ungroup()%>%
  mutate(state=reorder(state,error,FUN=median))%>%
  ggplot(aes(state,error))+geom_boxplot()+geom_point()


##
## Code: Calculating 95% confidence intervals with the t-distribution
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)

## 
# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
2*pt(-2,df=3)

## plotting t-distrib
# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df<-3:50
# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func<-function(df){2*pt(-2,df)}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs<-sapply(df,pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df,probs)  
# => converge vers les 5% de la distrib normale


################################################ 
## Sampling From the Normal Distribution
# In a previous section, we repeatedly took random samples of 50 heights from a distribution of heights. 
# We noticed that about 95% of the samples had confidence intervals spanning the true population mean.
# Re-do this Monte Carlo simulation, but now instead of N=50, use N=15. 
# Notice what happens to the proportion of hits.
#
# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res<-replicate(B,{
  X<-sample(x,N,replace=TRUE)
  interval<-mean(X)+c(-1,1)*sd(X)/sqrt(N)*qnorm(0.975)
  mean(X)
  between(mu,interval[1],interval[2])
})
# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res) 
# 0.9323

##
## Sampling from the t-Distribution
mu <- mean(x)
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res<-replicate(B, {
  X<-sample(x,N, replace=TRUE)
  ifelse (abs(mu - mean(X))<= qt(0.975,df=N-1)*sd(X)/sqrt(N),1,0)
})
# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)
#0.9523

############################################
## association and chi squared test
##

## Code: Research funding rates example
# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

# Code: Two-by-two table and p-value for the Lady Tasting Tea problem
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

##
## Code: Chi-squared test
# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>% chisq.test()
chisq_test$p.value

## Code: Odds ratio
# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women

## Code: p-value and odds ratio responses to increasing sample size
# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()

##
## Assesment section 7
## comparing proportion of hits

str(errors) # exo precedent sur les polls
# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals<-errors%>%filter(grade %in% c('A-','C-'))%>%
  group_by(grade)%>%
  summarize(hits=sum(hit),misses=n()-hits)

# Print the proportion of hits for grade A- polls to the console
ah<-totals$hits[totals$grade=='A-']
am<-totals$misses[totals$grade=='A-']
ah/(ah+am)
# Print the proportion of hits for grade C- polls to the console
ch<-totals$hits[totals$grade=='C-']
cm<-totals$misses[totals$grade=='C-']
ch/(ch+cm)

## chi square test

# totals :
# A tibble: 2 x 3
# hit    `C-`  `A-`
# <lgl> <int> <int>
#  1 FALSE    50    26
#  2 TRUE    311   106

# Perform a chi-squared test on the hit data. 
chisq_test<-totals %>% select(-grade) %>% chisq.test()
chisq_test<-totals %>% select(-hit) %>% chisq.test() # my solution

chisq_test$p.value

## odds ratio

head(totals)
# A tibble: 2 x 3
hit    `C-`  `A-`
<lgl> <int> <int>
  1 FALSE    50    26
2 TRUE    311   106

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- totals$"C-"[totals$hit==TRUE]/totals$"C-"[totals$hit==FALSE]

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- totals$"A-"[totals$hit==TRUE]/totals$"A-"[totals$hit==FALSE]

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C

## Significance ## We did not find meaningful differences between the poll results from grade A- and grade C- polls in this
# subset of the data, which only contains polls for about a week before the election. 
# Imagine we expanded our analysis to include all election polls and we repeat our analysis.
# In this hypothetical scenario, we get that the p-value for the difference in prediction success if 0.0015 
# and the odds ratio describing the effect size of the performance of grade A- over grade B- polls is 1.07.
# which statement reflects the best interpretation of this result?
# => The p-value is below 0.05, but the odds ratio is very close to 1. There is not a scientifically significant difference in performance.

##
## Brexit poll analysis
##
# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

N<-1500
N*p
sqrt(p*(1-p)*N)
sqrt(p*(1-p)/N)
d
2*sqrt(p*(1-p)/N)

str(brexit_polls)
head(brexit_polls )

brexit_polls <- brexit_polls %>%   mutate(x_hat = (spread+1)/2)
mean(brexit_polls$spread)
sd(brexit_polls$spread)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

brexit_polls[1,]
x1<-brexit_polls[1,"x_hat"]
se1<-sqrt(x1*(1-x1)/brexit_polls[1,"samplesize"])
x1+c(-1,1)*qnorm(0.975)*se1

june_polls <- brexit_polls%>%filter(startdate >= "2016-06-01") %>% 
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize)) %>%
  mutate(se_d = 2*se_x_hat , lower=spread-qnorm(0.975)*se_d, upper=spread+qnorm(0.975)*se_d) %>%
  mutate(hit=ifelse(d>=lower & d<= upper,1,0))
nrow(june_polls)
mean(june_polls$lower<0 & june_polls$upper>0) # proportion of polls have a confidence interval that covers the value 0
mean(june_polls$lower>0) # proportion of polls predict "Remain" (confidence interval entirely above 0)
mean(june_polls$hit) #  proportion of polls have a confidence interval covering the true value of  d

june_polls%>%group_by(pollster)%>%summarise(n=n(),hits=mean(hit)) %>% arrange(hits)

#boxplot of the spread in june_polls by poll type.
str(june_polls)
june_polls%>% ggplot(aes(poll_type,spread))+geom_boxplot()

#Calculate the confidence intervals of the spread combined across all polls in june_polls, grouping by poll type
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
combined_by_type
combined_by_type%>%mutate(se=2*sqrt(p_hat*(1-p_hat)/N) , 
                          lower=spread - qnorm(0.975)*se, 
                          upper= spread + qnorm(0.975)*se, upper-lower)

#
# confidence intervals for all Brexit polls in 2016 and whether interval covers the actual value of the spread  
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
# two by two table
tbt<-brexit_hit %>% group_by(poll_type)%>% summarize (yeshit = sum(hit),nohit=sum(!hit))
tbt%>%select(-poll_type)%>%chisq.test()
# odds ratio between the hit rate of online and telephone polls
odd_ratio_online<-tbt$yeshit[tbt$poll_type=="Online"]/tbt$nohit[tbt$poll_type=="Online"]
odd_ratio_telephone<-tbt$yeshit[tbt$poll_type=="Telephone"]/tbt$nohit[tbt$poll_type=="Telephone"]
odd_ratio_online
odd_ratio_telephone
odd_ratio_online/odd_ratio_telephone
#
brexit_polls%>% ggplot(aes(enddate,spread,color=poll_type))+geom_point()+
  geom_smooth(method="loess", span = 0.4)+
  geom_hline(aes(yintercept = d))

#
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long%>% ggplot(aes(enddate,proportion,color=vote))+geom_point()+
  geom_smooth(method="loess", span = 0.3)