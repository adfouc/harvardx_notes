# 7-linear regression

## En resumé
#
#
#
# fit <- Teams_small %>% 
#   do ( tidy( lm (avg_attendance~W+R+HR+yearID, data = .)))#
#
# tidy(fit, conf.int = TRUE)
# predict(fit, data.frame(R = 5, HR= 1.2, W = 80, yearID = 2002))


# -----------------------------------------------
#  Code: Scatterplot of the relationship between HRs and wins
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#Code: Scatterplot of the relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#Code: Scatterplot of the relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

?Teams

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(runs_per_game = R / G, AB_per_game = AB / G) %>%
  ggplot(aes(AB_per_game, runs_per_game)) + 
  geom_point(alpha = 0.5)
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>%
  ggplot(aes(X2B_per_game, X3B_per_game)) + 
  geom_point(alpha = 0.5)
# -----------------------------------------------
# correlation
# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

rho <- mean(scale(galton_heights$father)*scale(galton_heights$son))
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

# Sample Correlation is a Random Variable
#
# Key points
# The correlation that we compute and use as a summary is a random variable.
# When interpreting correlations, it is important to remember that correlations derived from samples are estimates containing uncertainty.
# Because the sample correlation is an average of independent draws, the central limit theorem applies. 
# R ~  N (rho, sqrt((1-r^2)/(N-2)) )  avec r = moyenne (echantillon) et N = taille échantillon

# compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R
            
# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

# expected value and standard error
mean(R)
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

#
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(runs_per_game = R / G, AB_per_game = AB / G) %>% summarize(rho=cor(runs_per_game,AB_per_game))
# correlation coefficient between win rate (number of wins per game) and number of errors per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  summarize(rho=cor(W/G,E/G))
#
Teams %>% filter(yearID %in% 1961:2001) %>%
  summarize(rho=cor(X2B/G,X3B/G))

##
## Anscombe's Quartet/Stratification
##

# correlation is not always a useful summary
# https://rafalab.github.io/dsbook/regression.html#correlation-is-not-always-a-useful-summary
# conditional expectation
# https://rafalab.github.io/dsbook/regression.html#conditional-expectation
# the regression line
# https://rafalab.github.io/dsbook/regression.html#the-regression-line

# Key points
# Correlation is not always a good summary of the relationship between two variables.
# The general idea of conditional expectation is that we stratify a population into groups and compute summaries in each group.
# A practical way to improve the estimates of the conditional expectations is to define strata of with similar values of x.
# If there is perfect correlation, the regression line predicts an increase that is the same number of SDs for both variables. 
# If there is 0 correlation, then we don’t use x at all for the prediction and simply predict the average mu_y 
# For values between 0 and 1, the prediction is somewhere in between. If the correlation is negative, we predict a reduction instead of an increase.

# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

##
## Bivariate Normal Distribution

# Key points
# When a pair of random variables are approximated by the bivariate normal distribution, scatterplots look like ovals. They can be thin (high correlation) or circle-shaped (no correlation).
# When two variables follow a bivariate normal distribution, computing the regression line is equivalent to computing conditional expectations.
# We can obtain a much more stable estimate of the conditional expectation by finding the regression line and using it to make predictions.

# Here, we stratify the son height by the standardized father heights
# => verifier la normalité des distributions marginales (de y=son) i.e. pour chaque regroupement
# 
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)

# # Variance explained
# Conditioning on a random variable X can help to reduce variance of response variable Y.
# The standard deviation of the conditional distribution is  SD(Y | X ) = sigma_y *sqrt ( 1 - rho^2), 
# which is smaller than the standard deviation without conditioning sigma_y.
# Because variance is the standard deviation squared, the variance of the conditional distribution is sigma_y^2 * (1-rho^2).
# In the statement "X explains such and such percent of the variability," the percent value refers to the variance. 
# The variance decreases by rho^2  percent.
# The “variance explained” statement only makes sense when the data is approximated by a bivariate normal distribution.

#
# There are two different regression lines depending on whether we are taking the expectation of Y given X or taking the expectation of X given Y.

# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y

#
# In the second part of this assessment, you'll analyze a set of mother and daughter heights, also from GaltonFamilies.
# Define female_heights, a set of mother and daughter heights sampled from GaltonFamilies, as follows:
  
set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
#
m1<-mean(female_heights$mother)
s1<-sd(female_heights$mother)
m2<-mean(female_heights$daughter)
s2<-sd(female_heights$daughter)
rho<-cor(female_heights$daughter, female_heights$mother)
# Calculate the slope and intercept of the regression line predicting daughters' heights given mothers' heights. Given an increase in mother's height by 1 inch, how many inches is the daughter's height expected to change?
slope<-rho*s2/s1
intercept<-m2-m1*slope
slope*1
# What percent of the variability in daughter heights is explained by the mother's height?
# var Y|X = Var Y * (1-corr(X,Y)^2)
rho^2*100
# A mother has a height of 60 inches.What is the conditional expected value of her daughter's height given the mother's height?
60*slope+intercept

##
## Counfounding

# Association is not causation!
#   Although it may appear that BB cause runs, it is actually the HR that cause most of these runs. We say that BB are confounded with HR.
# Regression can help us account for confounding.

# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

##
## Stratification and Multivariate Regression

# A first approach to check confounding is to keep HRs fixed at a certain value and then examine the relationship between BB and runs.
# The slopes of BB after stratifying on HR are reduced, but they are not 0, which indicates that BB are helpful for producing runs, just not as much as previously thought.

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 


#
#
# https://rafalab.github.io/dsbook/linear-models.html#lse


lm(son ~ father, data = galton_heights)

# We want the intercept term for our model to be more interpretable, so we run the same model as before but now we subtract the mean of fathers’ heights from each individual father’s height to create a new variable centered at zero.

galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))

#We run a linear model using this centered fathers’ height variable.
lm(son ~ father_centered, data = galton_heights)

# Suppose we fit a multivariate regression model for expected runs based on BB and HR:
#  𝐸[𝑅|𝐵𝐵=𝑥1,𝐻𝑅=𝑥2]=𝛽0+𝛽1𝑥1+𝛽2𝑥2 
# Suppose we fix  𝐵𝐵=𝑥1 . Then we observe a linear relationship between runs and HR with intercept of:
#  b0 + b1.x1

##
## Least Squares Estimates (LSE)

# Once we find the values that minimize the RSS, we will call the values the least squares estimates (LSE) 

library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# residual sum of squares (RSS). 
# function that computes the RSS for any pair of values beta0 and 1
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# Here is a plot of the RSS as a function of  beta1 when we keep the  beta0 fixed at 25.

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

## lm
## in R, we can obtain the least squares estimates using the lm function. To fit the model:
# Yi = beta0 +beta1 . xi + epsilon_i
# with  Yi the son’s height and xi the father’s height, we can use this code to obtain the least squares estimates.

fit <- lm(son ~ father, data = galton_heights)
fit$coef
summary(fit)

# LSE are random variables
# The LSE is derived from the data y_i, which are a realization of random variables Yi  
# This implies that our estimates are random variables. To see this, we can run a Monte Carlo simulation in which we assume the son and father height data defines a population, take a random sample of size  
# N = 50, and compute the regression slope coefficient for each one:
#   
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 


# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

hist(lse$beta_0)
hist(lse$beta_1)

# standard errors estimates reported by the summary :
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>% .$coef
# ...they are close to the standard errors from the simulation:
lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

# The summary function also reports t-statistics (t value) and p-values (Pr(>|t|)). 
# The t-statistic is not actually based on the central limit theorem but rather on the assumption that the  epsilons follow a normal distribution. 
# Under this assumption, mathematical theory tells us that the LSE divided by their standard error, beta0_hat/SE_beta_0_hat  and beta1_hat/SE_beta_1_hat,
# , follow a t-distribution with  N-p degrees of freedom, with p the number of parameters in our model. In the case of height p=2.
# The two p-values are testing the null hypothesis that  beta_0 = 0 and beta_1 = 0, respectively.
# 
# Remember that, as we described in Section 16.10 for large enough N, the CLT works and the t-distribution becomes almost the same as the normal distribution. 
# Also, notice that we can construct confidence intervals, but we will soon learn about broom, an add-on package that makes this easy.
# Although we do not show examples in this book, hypothesis testing with regression models is commonly used in epidemiology and economics to make statements such as “the effect of A on B was statistically significant after adjusting for X, Y, and Z”. However, several assumptions have to hold for these statements to be true.

# Although interpretation is not straight-forward, it is also useful to know that the LSE can be strongly correlated, which can be seen using this code:

lse %>% summarize(cor(beta_0, beta_1)) # -0.9992294

# However, the correlation depends on how the predictors are defined or transformed.
# Here we standardize the father heights, which changes xi to (xi - x_bar)

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
# Observe what happens to the correlation in this case:
cor(lse[1,], lse[2,]) # 0.00897456

## Predicted Variables are Random Variables
##
# Key points
# The predicted value is often denoted as  Y_jat  , which is a random variable. Mathematical theory tells us what the standard error of the predicted value is.
# The predict() function in R can give us predictions directly.
# 
# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
fit
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()

galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>% arrange(Y_hat)

## asssesment
##
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

# Load the Lahman library and filter the Teams data frame to the years 1961-2001. 
# Run a linear model in R predicting the number of runs per game based on both the number of bases on balls per game and the number of home runs per game.
library(Lahman)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_pg = HR / G, R_pg = R / G, BB_pg = BB / G) %>%
  lm(R_pg~HR_pg+BB_pg, data=.) %>% summary
# Coefficients:
#   (Intercept)        HR_pg        BB_pg  
# 1.7443       1.5612       0.3874  

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_pg = HR / G, R_pg = R / G, BB_pg = BB / G) %>%
  
  mutate(R_hat = predict(lm(R_pg~HR_pg:BB_pg, data=.)))

# We run a Monte Carlo simulation where we repeatedly take samples of N = 100 from the Galton heights data and compute the regression slope coefficients for each sample:
# What does the central limit theorem tell us about the variables beta_0 and beta_1?
# -   They are approximately normally distributed.
# - The expected value of each is the true value of  𝛽0  and  𝛽1 (assuming the Galton heights data is a complete population).

#
# plot the predictions and confidence intervals for our linear model of sons’ heights
# -----------------------------------------------------------------------------------
# method 1
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

# method 2
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


##
##
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

model <- female_heights %>% lm(mother~daughter, data=.)
pred <- predict(model)
head(pred)
as_tibble(pred) %>% bind_cols(mother = female_heights$mother)

##

## keeping only players with more than 100 plate appearances
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
# Now compute a similar table but with rates computed over 1999-2001. 
# Keep only rows from 1999-2001 where players have 100 or more plate appearances, calculate each player's single rate and BB rate per season, 
# then calculate the average single rate (mean_singles) and average BB rate (mean_bb) per player over those three seasons.
bat_99 <- Batting %>% filter(between(yearID,1999,2001)) %>% # filter (playerID=="abbotje01") %>%
  group_by(playerID) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  summarize(mean_singles=mean(singles),mean_bb=mean(bb))
bat_99[bat_99$mean_singles>0.2,] %>% nrow()
sum(bat_99$mean_singles>0.2)
sum(bat_99$mean_bb>0.2)

bat <- inner_join(bat_02, bat_99, by = "playerID") 
# What is the correlation between 2002 singles rates and 1999-2001 average singles rates?
cor(bat$singles, bat$mean_singles)
cor(bat$bb, bat$mean_bb)

# Both distributions are bivariate normal.
library(gridExtra)
p1 <- qplot(bat$singles, bat$mean_singles)
p2 <- qplot(bat$bb, bat$mean_bb)
grid.arrange(p1, p2, ncol = 2)

#Fit a linear model to predict 2002 singles given 1999-2001 mean_singles
# What is the coefficient of mean_singles, the slope of the fit?
lm(singles~mean_singles, data=bat)
lm(bb~mean_bb, data=bat)

##
##
## Advanced dplyr: Tibbles

# Tibbles can be regarded as a modern version of data frames and are the default data structure in the tidyverse.
# Some functions that do not work properly with data frames do work with tibbles.

# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles : The lm function ignores the group_by !!
# This is expected because lm is not part of the tidyverse and does not know how to handle the outcome of a grouped tibble.
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# inspect a grouped tibble
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

##
## Tibbles: Differences from Data Frames
# Tibbles are more readable than data frames.
# If you subset a data frame, you may not get a data frame. If you subset a tibble, you always get a tibble.
# Tibbles can hold more complex objects such as lists or functions.
# Tibbles can be grouped.

# inspect data frame and tibble
Teams
as_tibble(Teams)

# subsetting a data frame sometimes generates vectors
class(Teams[,20])

# subsetting a tibble always generates tibbles
class(as.tibble(Teams[,20]))

# pulling a vector out of a tibble
class(as.tibble(Teams)$HR)

# access a non-existing column in a data frame or a tibble
Teams$hr
as.tibble(Teams)$hr

# create a tibble with complex objects
tibble(id = c(1, 2, 3), func = c(mean, median, sd))

##
# most R functions do not recognize grouped tibbles nor do they return data frames. The lm function is an example. 
# The do functions serves as a bridge between R functions, such as lm, and the tidyverse. 
# The do function understands grouped tibbles and always returns a data frame.
##
dat %>%  
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))
# The do function will create a data frame with the first column being the strata value and a column named fit 

# Also, if we do not name a column (note above we named it fit), then do will return the actual output of lm, not a data frame, 
# and this will result in an error since do is expecting a data frame as output.

dat %>%  
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))

# For a useful data frame to be constructed, the output of the function must be a data frame too. 
# We could build a function that returns only what we want in the form of a data frame:
  
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}
# And then use do without naming the output, since we are already getting a data frame:
  
dat %>%  
group_by(HR) %>%
do(get_slope(.))

# If we name the output, then we get something we do not want, a column containing data frames:

dat %>%  
  group_by(HR) %>%
  do(slope = get_slope(.))


# If the data frame being returned has more than one row, these will be concatenated appropriately. 
# Here is an example in which we return both estimated parameters:
  
get_lse <- function(data){
    fit <- lm(R ~ BB, data = data)
    data.frame(term = names(fit$coefficients),
               slope = fit$coefficients, 
               se = summary(fit)$coefficient[,2])
  }

dat %>%  
  group_by(HR) %>%
  do(get_lse(.))

##
## The broom package

# The broom package has three main functions, all of which extract information from the object returned by lm and return it in a tidyverse friendly data frame. 
# These functions are tidy, glance, and augment. The tidy function returns estimates and related information as a data frame:
  
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)
# We can add other important summaries, such as confidence intervals:
  
tidy(fit, conf.int = TRUE)

# Because the outcome is a data frame, we can immediately use it with do to string together the commands that produce the table we are after. 
# Because a data frame is returned, we can filter and select the rows and columns we want, which facilitates working with ggplot2:

dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE))

dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

# Now we return to discussing our original task of determining if slopes changed. The plot we just made, using do and tidy, shows that the confidence intervals overlap, which provides a nice visual confirmation that our assumption that the slope does not change is safe.
# 
# The other functions provided by broom, 
# glance: relate to model-specific
# augment : observation-specific outcomes
# Here, we can see the model fit summaries glance returns:
glance(fit)

## You want to know whether the relationship between home runs and runs per game varies by baseball league. 
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 
head(dat)

dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

# Assessment: Tibbles, do, and broom, part 2
# We have investigated the relationship between fathers' heights and sons' heights. But what about other parent-child relationships? Does one parent's height have a stronger association with child height? How does the child's gender affect this relationship in heights? Are any differences that we observe statistically significant?
#   
#   The galton dataset is a sample of one male and one female child from each family in the GaltonFamilies dataset. The pair column denotes whether the pair is father and daughter, father and son, mother and daughter, or mother and son.
# 
# Create the galton dataset using the code below:
#   
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

galton %>% group_by(pair) %>% summarise(n())

# Q9
galton %>% group_by(pair) %>% summarise(cor(childHeight,parentHeight))

fd <- galton %>% filter(pair=="father_daughter") 
cor(fd$childHeight,fd$parentHeight)

galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == max(cor))

# Use lm() and the broom package to fit regression lines for each parent-child pair type. 
# Compute the least squares estimates, standard errors, confidence intervals and p-values for the parentHeight coefficient for each pair.
galton %>%
  group_by(pair) %>%
  do( tidy( lm(childHeight~parentHeight, data=.) ,conf.int = TRUE) ) %>%
  filter (term=="parentHeight") %>% mutate(width=conf.high-conf.low)
  
## ------------------------------## ------------------------------## ------------------------------
## Building a Better Offensive Metric for Baseball
# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

#Imagine you have two teams. Team A is comprised of batters who, on average, get two bases on balls, four singles, one double, no triples, and one home run. 
#Team B is comprised of batters who, on average, get one base on balls, six singles, two doubles, one triple, and no home runs.
sum(coefs$estimate*c(1, 2,4,1,0,1))
sum(coefs$estimate*c(1, 1,6,2,1,0))

# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002 ) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

## ------------------------------#
# Linear Programming

# A way to actually pick the players for the team can be done using what computer scientists call linear programming. 
# Although we don't go into this topic in detail in this course, we include the code anyway:

library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

# This algorithm chooses these 9 players:

our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))

our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

#   nameFirst    nameLast POS   salary R_hat
# 1     Jason      Giambi  1B 10428571  7.99
# 2     Nomar Garciaparra  SS  9000000  7.51
# 3      Mike      Piazza   C 10571429  7.16
# 4      Phil       Nevin  3B  2600000  6.75
# 5      Jeff        Kent  2B  6000000  6.68

# We note that these players all have above average BB and HR rates while the same is not true for singles.

my_scale <- function(x) (x - median(x))/mad(x)

players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
    filter(playerID %in% our_team$playerID) %>%
    select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
    arrange(desc(R_hat))

#   nameFirst    nameLast    BB singles doubles triples    HR  AVG R_hat
# 1     Jason      Giambi 3.317 -0.5315   0.754  -0.675 2.067 2.63  3.54
# 2     Nomar Garciaparra 0.284  1.7330   2.651   0.471 1.003 3.95  2.97
# 3      Mike      Piazza 0.596 -0.0499  -0.177  -1.335 2.682 1.70  2.56
# 4      Phil       Nevin 0.790 -0.6751   0.670  -1.137 2.103 1.09  2.07
# 5      Jeff        Kent 0.875 -0.2717   1.833   1.210 0.967 1.66  2.00

# ------------------------------## ------------------------------
# Regression Fallacy
# 
# Regression can bring about errors in reasoning, especially when interpreting individual observations.
# The example showed in the video demonstrates that the "sophomore slump" observed in the data is caused by regressing to the mean.

# The code to create a table with player ID, their names, and their most played position:
library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)
# The code to create a table with only the ROY award winners and add their batting statistics:
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")
# The code to keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons:
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)
# The code to use the spread function to have one column for the rookie and sophomore years batting averages:
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY

#The code to calculate the proportion of players who have a lower batting average their sophomore year:
mean(ROY$sophomore - ROY$rookie <= 0)
#> [1] 0.677

#The code to do the similar analysis on all players that played the 2013 and 2014 seasons and batted more than 130 times (minimum to win Rookie of the Year):
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years

# The code to see what happens to the worst performers of 2013:
arrange(two_years, `2013`)

# The code to see  the correlation for performance in two separate years:
qplot(`2013`, `2014`, data = two_years)

summarize(two_years, cor(`2013`,`2014`))

# ------------------------------## ------------------------------
# Measurement Error Models

# Up to now, all our linear regression examples have been applied to two or more random variables. We assume the pairs are bivariate normal and use this to motivate a linear model.
# Another use for linear regression is with measurement error models, where it is common to have a non-random covariate (such as time). 
# Randomness is introduced from measurement error rather than sampling or natural variability.
# 
# The code to use dslabs function rfalling_object to generate simulations of dropping balls:
library(dslabs)
falling_object <- rfalling_object()
# The code to draw the trajectory of the ball:
falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")
#The code to use the lm() function to estimate the coefficients:
  fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)

tidy(fit)

# The code to check if the estimated parabola fits the data:
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")
#The code to see the summary statistic of the regression:
tidy(fit, conf.int = TRUE)

#Use the Teams data frame from the Lahman package. Fit a multivariate linear regression model to obtain the effects of BB and HR on Runs (R) in 1971. Use the tidy() function in the broom package to obtain the results in a data frame.
fit2 <- Teams %>% 
  filter(yearID %in% 1971) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% lm(R~HR+BB,data=.)
tidy(fit2,conf.int = TRUE)
# Interpret the p-values for the estimates using a cutoff of 0.05. : 
# The p-value for HR is less than 0.05, but the p-value of BB is greater than 0.05 (0.06), so the evidence is not strong enough to suggest that BB has a significant effect on runs at a p-value cutoff of 0.05.

# from 1961 to 2018; scatterplot of the estimate for the effect of BB on runs over time and add a trend line with confidence intervals
fitted <- Teams %>% 
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%  
  do ( tidy(lm(R~HR+BB,data=.), conf.int = TRUE) ) %>%
  ungroup() # a quoi sert-il?

# methode 1
coef <- fitted %>% filter(term=="BB") %>% 
  lm(estimate~yearID, data = .) %>% coef()

fitted %>% filter(term=="BB") %>% 
  ggplot(aes(yearID, estimate)) + geom_point() + 
  geom_abline(slope=coef[2], intercept = coef[1])

# methode 2
fitted %>% filter(term=="BB") %>% 
  ggplot(aes(yearID, estimate)) + geom_point() + geom_smooth(method = "lm")

#
fitted %>% filter(term=="BB") %>% 
  lm(estimate~yearID, data = .) %>%tidy() %>%
  filter(term == "yearID") %>% 
  pull(p.value)

# ------------------------------## ------------------------------
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G) %>%
  mutate(R = R/G, HR = HR/G)

# Use runs (R) per game to predict average attendance.
Teams_small %>% lm(avg_attendance~R, data =.)
Teams_small %>% lm(avg_attendance~HR, data =.)
# Use number of wins to predict average attendance; do not normalize for number of games.
str(Teams_small)
Teams_small %>% lm(avg_attendance~W, data =.) %>% coef()
Teams_small %>% lm(avg_attendance~yearID, data =.) %>% coef()
cor(Teams_small$W, Teams_small$R)
cor(Teams_small$W, Teams_small$HR)
# Stratify Teams_small by wins: divide number of wins by 10 and then round to the nearest integer. 
# Keep only strata 5 through 10, which have 20 or more data points.
Teams_small %>% mutate (W=round(W/10)) %>% 
  group_by(W) %>% 
  filter(W %in% 5:10 & n()>=20 ) %>% summarise(n())
# How many observations are in the 8 win strata? (Note that due to division and rounding, these teams have 75-85 wins.): 338
# Calculate the slope of the regression line predicting average attendance given runs per game for each of the win strata
dat <- Teams_small %>% mutate (W=round(W/10)) %>% 
  group_by(W) %>% 
  filter(W %in% 5:10 & n()>=20 ) %>% ungroup()
dat %>% 
  group_by(W) %>%
  do ( tidy(lm(avg_attendance~R, data=.))) %>% 
  filter(term=="R") %>%
  select(W, estimate)
# Calculate the slope of the regression line predicting average attendance given HR per game for each of the win strata.
dat %>% 
  group_by(W) %>%
  do ( tidy(lm(avg_attendance~HR, data=.))) %>% 
  filter(term=="HR") %>%
  select(W, estimate)
# alternative
dat %>%  
  group_by(W) %>%
  summarize(slope = cor(HR, avg_attendance)*sd(avg_attendance)/sd(HR))

# Looking at the data, we can see that runs per game are positively correlated with average attendance, 
# that home runs per game have the strongest effect on attendance when teams don't win many games, 
# and that teams with fewer wins have a larger average attendance with more home runs per game.
# We also see that runs per game have a stronger effect when teams win few, not many, games, 
# and that home runs per game are in fact positively correlated with attendance in all win strata.

# Fit a multivariate regression determining the effects of runs per game, home runs per game, wins, and year on average attendance. Use the original Teams_small wins column, not the win strata from question 3.
fit <- Teams_small %>% 
  do ( tidy( lm (avg_attendance~W+R+HR+yearID, data = .)))
fit
# Suppose a team averaged 5 runs per game, 1.2 home runs per game, and won 80 games in a season.
# What would this team's average attendance be in 2002?
fit$estimate[1]+fit$estimate[fit$term=="R"]*5+fit$estimate[fit$term=="HR"]*1.2+fit$estimate[fit$term=="W"]*80+fit$estimate[fit$term=="yearID"]*2002
# What would this team's average attendance be in 1960?
fit$estimate[1]+fit$estimate[fit$term=="R"]*5+fit$estimate[fit$term=="HR"]*1.2+fit$estimate[fit$term=="W"]*80+fit$estimate[fit$term=="yearID"]*1960

# alternative:
fit <- Teams_small %>% lm (avg_attendance~W+R+HR+yearID, data = .)
predict(fit, data.frame(R = 5, HR= 1.2, W = 80, yearID = 2002))

# predict average attendance for teams in 2002 in the original Teams data frame.
# What is the correlation between the predicted attendance and actual attendance?
pred <- Teams %>% filter(yearID %in% 2002) %>%
  mutate(avg_attendance = attendance/G) %>%
  mutate(R = R/G, HR = HR/G) %>%
  mutate (predicted_attendance = predict(fit, .) ) %>%
  select(teamID, avg_attendance, predicted_attendance)
pred
cor(pred$avg_attendance,pred$predicted_attendance)


# ------------------------------## ------------------------------
# Perso:  linear model

lmcoef <- function(x,y){
  fit <- lm(y~x ,data=data.frame(x=x, y=y)) %>% tidy()
  print(fit)
  data.frame(x,y) %>% ggplot(aes(x,y))+geom_point()+
    geom_abline(slope = fit$estimate[fit$term=="x"], intercept = fit$estimate[fit$term=="(Intercept)"], color="blue")
}

x<-1:100
y<-x^2
lmcoef(x, x^2)
lmcoef(x, 5*x^2-200*x+30)
lmcoef(x, (x-10)*(x-20)*(x-30))
x<-seq(0,2*pi,length.out = 100)
lmcoef(x, cos(x))
lmcoef(x, cos(x)+x/pi)


# ------------------------------## ------------------------------
# CONFOUNDING

# Correlation is Not Causation: Spurious Correlation

# Association/correlation is not causation.
# p-hacking is a topic of much discussion because it is a problem in scientific publications. Because publishers tend to reward statistically significant results over negative results, there is an incentive to report significant results.
# 
# generate the Monte Carlo simulation
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))

# calculate correlation between X,Y for each group
res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(x, y)) %>% 
  arrange(desc(r))
res

# plot points from the group with maximum correlation
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")

# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

sd(res$r)


# linear regression on group with maximum correlation
library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(y ~ x, data = .)))

# ------------------------------## ------------------------------
## Correlation is Not Causation: Outliers
# Correlations can be caused by outliers.
# The Spearman correlation is calculated based on the ranks of data.

# simulate independent X, Y and standardize all except entry 23
set.seed(1985)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

# plot shows the outlier
qplot(x, y, alpha = 0.5)

# outlier makes it appear there is correlation
cor(x,y)
cor(x[-23], y[-23])

# use rank instead
qplot(rank(x), rank(y))
cor(rank(x), rank(y))

# Spearman correlation with cor function
cor(x, y, method = "spearman")

# ------------------------------## ------------------------------
## Correlation is Not Causation: Reversing Cause and Effect
# Another way association can be confused with causation is when the cause and effect are reversed.
# As discussed in the video, in the Galton data, when father and son were reversed in the regression, the model was technically correct. The estimates and p-values were obtained correctly as well. What was incorrect was the interpretation of the model.

# cause and effect reversal using son heights to predict father heights
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>% 
  do(tidy(lm(father ~ son, data = .)))

# ------------------------------## ------------------------------
## Correlation is not Causation: Confounders
# If X and Y are correlated, we call Z a confounder if changes in Z causes changes in both X and Y.

# UC-Berkeley admission data
library(dslabs)
data(admissions)
admissions

# percent men and women accepted
admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

# test whether gender and admission are independent
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))

# percent admissions by major
admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)

# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# plot number of applicants admitted and not
admissions %>%
  mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>%
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major)

admissions %>% 
  mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

# condition on major and then look at differences
admissions %>% ggplot(aes(major, admitted, col = gender, size = applicants)) + geom_point()

# average difference by major
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))
# ------------------------------## ------------------------------

## Simpson’s Paradox happens when we see the sign of the correlation flip when comparing the entire dataset with specific strata. 

# ------------------------------## ------------------------------
#assessment
# Q1  In the videos, we ran one million tests of correlation for two random variables, X and Y.
# How many of these correlations would you expect to have a significant p-value ( 𝑝≤0.05 ), just by chance?
# => The p-value is defined as the probability of finding the observed result when the null hypothesis (no correlation) is true. 
# When we have a p-value of 0.05, this means the chance of finding a correlation when none exists is 5% - e.g., 0.05*1,000,000 chances, which is 50,000.

?admissions


# ------------------------------## ------------------------------
# For this set of exercises, we examine the data from a 2014 PNAS paper that analyzed success rates from funding agencies in the Netherlands
http://www.pnas.org/content/112/40/12349.abstract
# A response was published a few months later titled No evidence that gender contributes to personal research funding success in The Netherlands: A reaction to Van der Lee and Ellemers,
http://www.pnas.org/content/112/51/E7036.extract
#The main evidence for the conclusion of the original paper comes down to a comparison of the percentages. 
# The information we need was originally in Table S1 in the paper, which we include in dslabs:
  
library(dslabs)
data("research_funding_rates")
research_funding_rates

# Construct a two-by-two table of gender (men/women) by award status (awarded/not) using the total numbers across all disciplines.
research_funding_rates %>% mutate(fail_men = applications_men - awards_men, fail_women = applications_women - awards_women) %>%
  select(awards_men, awards_women, fail_men, fail_women) %>%  summarise_all(sum)

two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two

two_by_two %>% mutate(men=men/sum(men), women=women/sum(women))

two_by_two %>% select(-awarded) %>% chisq.test(.) %>% tidy() %>% pull(p.value)

# use this dataset with number of applications, awards, and success rate for each gender:
  
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat
# To check if this is a case of Simpson's paradox, plot the success rates versus disciplines, which have been ordered by overall success, with colors to denote the genders and size to denote the number of applications.
dat %>% ggplot(aes(x=discipline, y= success, col=gender))+geom_point(aes(size=applications)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# In which fields do men have a higher success rate than women?
# Which two fields have the most applications from women?
# Which two fields have the lowest overall funding rates? ????????????? Q4

# ------------------------------## ------------------------------
# Exercise
# 6. We definitely do not see the same level of confounding as in the UC Berkeley example. 
# It is hard to say there is a confounder here. H
# owever, we do see that, based on the observed rates, some fields favor men and others favor women and we do see that the two fields with the largest difference 
# favoring men are also the fields with the most applications. But, unlike the UC Berkeley example, women are not more likely to apply for the harder subjects. 
# So perhaps some of the selection committees are biased and others are not.
# 
# But, before we conclude this, we must check if these differences are any different than what we get by chance. 
# Are any of the differences seen above statistically significant? 
# Keep in mind that even when there is no bias, we will see differences due to random variability in the review process as well as random variability across candidates. 
# Perform a Chi-square test for each discipline. Hint: define a function that receives the total of a two-by-two table and returns a data frame with the p-value. Use the 0.5 correction. 
# Then use the do function.

chi<-function(x){ x %>% chisq.test(.) %>% tidy() %>% select(p.value)}

research_funding_rates %>% 
  group_by(discipline)%>%
  mutate( yes_men=awards_men, no_men=applications_men-awards_men,
         yes_women=awards_women, no_women=applications_women-awards_women) %>%
  select(discipline,yes_men,no_men, yes_men, yes_women, no_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value)  %>%
  do(chi(data.frame(.$yes,.$no)))

# 
# 7. For the medical sciences, there appears to be a statistically significant difference. 
# But is this a spurious correlation? We performed 9 tests. 
# Reporting only the one case with a p-value less than 0.05 might be considered an example of cherry picking. 
# Repeat the exercise above, but instead of a p-value, compute a log odds ratio divided by their standard error. 
# Then use qq-plot to see how much these log odds ratios deviate from the normal distribution we would expect: a standard normal distribution.

tbt <- research_funding_rates %>% 
  mutate( yes_men=awards_men, no_men=applications_men-awards_men,
          yes_women=awards_women, no_women=applications_women-awards_women) %>%
  select(discipline,yes_men,no_men, yes_men, yes_women, no_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value)

# statistical theory tells us that when all four entries of the two-by-two table are large enough, then the log of the odds ratio is approximately normal with standard error
# sqrt (1/a+1/b+...)
# voir: 15.10.5  https://rafalab.github.io/dsbook/inference.html#odds-ratio


logoddr <- function(x){
  men<-x %>% filter(gender=="men")
  women<-x %>% filter(gender=="women")
  log_or <- log(women$yes*men$no/(women$no * men$yes  ))
  se <- sqrt( 1/women$yes+1/men$no+1/women$no +1/men$yes  )
  data.frame(logoddratio=log_or, se = se, lower = log_or -1.96*se, upper = log_or + 1.96 * se ,
             pVal = 2*(1 - pnorm(abs(log_or), 0, se)))
} 

tbt %>% group_by(discipline)%>% do(logoddr(.))




# ------------------------------## ------------------------------
