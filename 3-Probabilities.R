
# Probabilities
#

# birthday pb
# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays

## function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
n <- seq(1, 60)
prob <- sapply(n, compute_prob) 
# Code: Computing birthday problem probabilities with sapply
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob

---------------------------------------------------------
# section 1.2
  
# This line of example code simulates four independent random games where the Celtics either lose or win. Copy this example code to use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `celtic_wins` that replicates two steps for B iteration(1) generating a random four-game series `simulated_games` using the example code, then (2) determining whether the simulated series contains at least one win for the Celtics.
celtic_wins<-replicate(B,{
  
  simulated_games<-sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  c("win") %in% simulated_games 
})


# Calculate the frequency out of B iterations that the Celtics won at least one game. Print your answer to the console.
mean(celtic_wins)

---------------------------------------------------------
#Code: Monte Carlo simulation of stick strategy

B <- 10000

stick <- replicate(B, {
  
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})

mean(stick)    # probability of choosing prize door when sticking

# Code: Monte Carlo simulation of switch strategy

switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})

mean(switch)    # probability of choosing prize door when switching

#---------------------
# Assign a variable 'n' as the number of remaining games.
n<-6

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes<-c(0,1)

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
l<-rep(list(outcomes),n)

# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities<-expand.grid(l)

# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results<-rowSums(possibilities)>3

# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results)

#---------------------
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains at least four wins for the Cavs.
results<-replicate(B,{
games<-sample(c(0,1),6,replace=TRUE)
  sum(games)>3
})

# Calculate the frequency out of `B` iterations that the Cavs won at least four games in the remainder of the series. Print your answer to the console.
mean(results)
#---------------------
# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
Pr<-sapply(p,prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p,Pr)

#---------------------
# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
    
  })
  
  mean(result)
}

# Assign the variable 'N' as the vector of series lengths. Use only odd numbers ranging from 1 to 25 games.
N<-seq(1,25,2)

# Apply the 'prob_win' function across the vector of series lengths to determine the probability that team B will win. Call this object `Pr`.
Pr<-sapply(N,prob_win)

# Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
plot(N,Pr)

#---------------------

library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits

length(permutations(8,3))
combinations(8,3)
8*7*6
6/336

#---------------------

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)

jamVictory<-function(B=10000){
  probavector <- replicate(B, { sum(sample(runners,3) %in% c("Jamaica")) ==3})
  mean(probavector)
}

jamVictory(10000)

## restaurant
6*3*nrow(combinations(6,3))

sapply(1:12, function(x) x*3*15)
sapply(2:12, function(x) 18*nrow(combinations(x,2)))

#---------------------
# esophageal cancer
head(esoph)
str(esoph)
nrow(esoph)

all_cases<-sum(esoph$ncases)
all_controls<-sum(esoph$ncontrols)
esoph%>% group_by(alcgp)%>%summarize(sum(ncases)/sum(ncontrols+ncases))
esoph%>% filter( ncases>0) %>% mutate (more10g = ifelse(tobgp>="10-19",TRUE,FALSE))%>%group_by(more10g)%>%
  summarize(sum(ncases))%>%prop.table()
esoph%>% mutate (more10g = ifelse(tobgp>="10-19",TRUE,FALSE))%>%group_by(more10g)%>%
  summarize(sum(ncontrols))%>%prop.table()
esoph%>% filter (tobgp!="0-9g/day") %>%pull(ncases)%>%sum() / all_cases
esoph%>% filter (tobgp!="0-9g/day") %>%pull(ncontrols)%>%sum() / all_controls
esoph%>% group_by(alcgp)%>%summarize(sum(ncases)/all_cases)
esoph%>% group_by(tobgp)%>%summarize(sum(ncases)/all_cases)
esoph%>% group_by(alcgp, tobgp)%>%summarize(sum(ncases)/all_cases)
esoph%>%filter(alcgp=="120+" | tobgp=="30+")%>%summarize(sum(ncases)/all_cases, sum(ncontrols)/all_controls)
esoph%>% group_by(alcgp)%>%summarize(sum(ncontrols)/all_controls)
esoph%>% group_by(alcgp)%>%summarize(sum(ncases)/sum(ncontrols))
esoph%>% group_by(tobgp)%>%summarize(sum(ncontrols)/all_controls)
esoph%>% group_by(alcgp, tobgp)%>%summarize(sum(ncontrols)/all_controls)

#---------------------
# ACT Scores
set.seed(16,sample.kind = "Rounding")
library(tidyverse)
act_scores<-rnorm(10000,20.9, 5.7)
mean(act_scores)
sd(act_scores)
sum(act_scores>=36)
mean(act_scores>=30)
mean(act_scores<=10)
x<-1:36
f_x<-dnorm(x,20.9,5.7)
qplot(x,f_x,geom="line")
z_scores<-(act_scores-mean(act_scores))/sd(act_scores)
mean(z_scores)
sd(z_scores)
mean(z_scores>=2)
20.9+2*5.7
qnorm(0.975,mean(act_scores),sd(act_scores))
cdf<-function(v){ mean(act_scores<=v)}
cdfv<-sapply(1:36,cdf)
data.frame(x,cdfv)
qnorm(0.95,20.9,5.7)
p<-seq(0.01,0.99,0.01)
sample_quantiles<-quantile(act_scores,p)
theoretical_quantiles<-qnorm(p, 20.9,5.7)
qqplot(theoretical_quantiles,sample_quantiles)

#---------------------
# -----------------------------------------------------------
# CLT

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Create a model to predict the random variable `X`, your winnings from betting on green. Sample one time.
sample(c(17,-1),1,prob=c(p_green,p_not_green))

# Calculate the expected outcome if you win $17 if the ball lands on green and you lose $1 if the ball doesn't land on green
17*p_green-p_not_green

# Compute the standard error of the random variable
abs(17+1)*sqrt(p_green*p_not_green)

X<-sample(c(17,-1),1000,prob=c(p_green,p_not_green),replace=TRUE)
S<-sum(X)

n<-1000
n*(17*p_green-1*p_not_green)
abs(17+1)*sqrt(p_green*p_not_green)*sqrt(n)

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting on green 100 times.
1-pnorm(0,avg,se)

# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S<-replicate(B,{
  x<-sample(c(17,-1),100,replace=TRUE,prob=c(p_green,p_not_green))
  sum(x)
})

# Compute the average value for 'S'
mean(S)

# Calculate the standard deviation of 'S'
sd(S)

# Calculate the proportion of outcomes in the vector `S` that exceed $0
mean(S>=0)

#--
# =>The CLT does not work as well when the probability of success is small.
#--

n <- 10000

# Create a vector called `X` that contains the outcomes of `n` bets
X<-sample(c(17,-1),n,replace=TRUE,c(p_green,p_not_green))

# Define a variable `Y` that contains the mean outcome per bet. Print this mean to the console.
Y<-mean(X)

#--
# We defined the average using the following code
avg <- 17*p_green + -1*p_not_green

# We defined standard error using this equation
se <- 1/sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Given this average and standard error, determine the probability of winning more than $0. Print the result to the console.
1-pnorm(0,avg,se)

# --
## Make sure you fully follow instructions, including printing values to the console and correctly running the `replicate` loop. If not, you may encounter "Session Expired" errors.

# The variable `n` specifies the number of independent bets on green
n <- 10000

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation
set.seed(1)

# Generate a vector `S` that contains the the average outcomes of 10,000 bets modeled 10,000 times
S<-replicate(B, {
  x<-sample(c(17,-1),n,replace=TRUE,c(p_green,p_not_green))
  mean(x)
})

# Compute the average of `S`
mean(S)

# Compute the standard deviation of `S`
sd(S)

# Compute the proportion of outcomes in the vector 'S' where you won more than $0
mean(S>0)

# => The CLT works better when the sample size is larger.
# -----------------------------------------------------------

abs(1- -0.25)*sqrt(0.2*0.8)*sqrt(44)
1-pnorm(8,0,3.32)
set.seed(21, sample.kind = "Rounding")
X<-replicate(10000,{S<-sample(c(1,-0.25),44,prob=c(0.2,0.8),replace=TRUE ); sum(S) })
mean(X>=8)

# What is the lowest p such that the probability of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)
prob<-sapply(p,function(x) {
  1-pnorm(35,x*44,1*sqrt(44*x*(1-x)))
  })
data.frame(p,prob) # 0.85
# -------
p_fivepockets <- 5/38
payout<-p_fivepockets*6-(1-p_fivepockets)  
bet_se<-7*sqrt(p_fivepockets*(1-p_fivepockets))
bet_se/sqrt(500)
payout*500
bet_se*sqrt(500)
pnorm(0,500*payout,sqrt(500)*bet_se)

#-----------------------
# the big short
#4.1

#Code: Expected value with higher default rate and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)


# Code: Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

#Code: Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

#Code: Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

#Code: Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

# Code: Calculating interest rates for expected value of 0
x = - loss_per_foreclosure*p/(1-p)
x/180000

# Code: Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

# Code: Monte Carlo simulation for 1% probability of losing money
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

##
#Code: Calculating number of loans for desired probability of losing money
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

#Code: Monte Carlo simulation with known default probability
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

# Code: Monte Carlo simulation with unknown default probability
# estimates the expected profit given an unknown probability of default  0.03<p<0.05 , 
# modeling the situation where an event changes the probability of default for all borrowers simultaneously
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

###################
# bank earnings montecarlo
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Generate a list of summed losses 'S'. Replicate the code from the previous exercise over 'B' iterations to generate a list of summed losses for 'n' loans.  Ignore any warnings for now.

S<-replicate(B, {
  defaults<-sample(c(0,1),n,replace=TRUE,c(1-p_default,p_default))
  sum(defaults)*loss_per_foreclosure
})

# Plot a histogram of 'S'.  Ignore any warnings for now.
hist(S)

##

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Generate a variable `z` using the `qnorm` function
z<-qnorm(0.05)

# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
x<--loss_per_foreclosure*(p_default*n - z*sqrt(n*p_default*(1-p_default)))/((1-p_default)*n+z*sqrt(n*p_default*(1-p_default)))

# Convert `x` to an interest rate, given that the loan amount is $180,000. Print this value to the console.
x/180000

##############################
# Insurance rates
data(death_prob)
head(death_prob)
p<-death_prob$prob[death_prob$age==50 & death_prob$sex == "Female"]
expgain<-p*(-150000)+(1-p)*1150
stderr<-abs(1150--150000)*sqrt(p*(1-p))
expgain*1000
stderr*sqrt(1000)
pnorm(0,1000*expgain,stderr*sqrt(1000))
pm<-death_prob$prob[death_prob$age==50 & death_prob$sex == "Male"]
pm
# which premium for an expected profit of 700KUSD?
a<-150000
n<-1000
mu_s<-700000
b<-(mu_s/n-pm*(-a))/(1-pm)
b
se_m<-abs(b+a)*sqrt(n*pm*(1-pm))
se_m
pnorm(0,mu_s,se_m)
# pandemic disease
p<-0.015
mu<-1000*(p*(-150000)+(1-p)*1150)
mu
se<-sqrt(1000*p*(1-p))*(150000+1150)
se
pnorm(0,mu,se)
pnorm(-1E6,mu,se)
#What is the lowest death probability for which the chance of losing money exceeds 90% 
p <- seq(.01, .03, .001)
probloss<-function(p)
{
  mu<-1000*(p*(-150000)+(1-p)*1150)
  se<-sqrt(1000*p*(1-p))*(150000+1150)
  pnorm(0,mu,se)
}
vecprob<-sapply(p,probloss)
data.frame(p,vecprob)
# What is the lowest death probability for which the chance of losing over $1 million exceeds 90%?
p <- seq(.01, .03, .0025)
probloss1m<-function(p)
{
  mu<-1000*(p*(-150000)+(1-p)*1150)
  se<-sqrt(1000*p*(1-p))*(150000+1150)
  pnorm(-1E6,mu,se)
}
vecprob1m<-sapply(p,probloss1m)
data.frame(p,vecprob1m)

## simulation of one sampling model
set.seed(25, sample.kind = "Rounding")
p_loss <- .015
loss<--150000
profit<-1150
n<-1000
simu<-sample(c(loss,profit),n,c(p_loss,1-p_loss),replace=TRUE)
pl<-sum(simu)/1E6
pl
## montecarlo simulation of 10000 p&l over 1000 loans
set.seed(27, sample.kind = "Rounding")
p_loss <- .015
loss<--150000
profit<-1150
n<-1000
pl<-replicate(10000, {
  simu<-sample(c(loss,profit),n,c(p_loss,1-p_loss),replace=TRUE)
  sum(simu)/1E6
})
mean(pl)
sd(pl)
exp_pl<-n*(p_loss*loss+profit*(1-p_loss))/1E6
se_pl<-sqrt(n*p_loss*(1-p_loss))*abs(profit-loss)/1E6
# What is the OBSERVED probability of losing $1 million or more?
mean(pl<=-1)
# normal approx
pnorm(-1,exp_pl,se_pl)

## insurance rates part 3

#  premium x required for a 5% chance of losing money ?
n<-1000  # loans, 
p<-0.015 # probability of death
l<--150000 # and loss per claim
z<-qnorm(0.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x
p*l+(1-p)*x # expected profit per policy
(p*l+(1-p)*x)*n

#  probability of losing money on 1,000 policies given the new premium 
set.seed(28, sample.kind = "Rounding")
mc<-replicate(10000, {
  simu<-sample(c(l,x),n,c(p,1-p),replace=TRUE)
  sum(simu)
})
mean(mc<0)  # 0.0554

## random change of p (pandemic situation)
set.seed(29, sample.kind = "Rounding")
mc<-replicate(10000, {
  p<-0.015+sample(seq(-0.01,0.01,length=100),1)
  simu<-sample(c(l,x),n,c(p,1-p),replace=TRUE)
  sum(simu)
})
mean(mc) # expected value = 968K
mean(mc<0) # prob of loss = 0.19
mean(mc<(-1E6)) # prob loss +1m = 0.0424

