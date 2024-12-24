rm(list = ls())
set.seed(519619)
library(ggplot2)
library(bayesrules)
library(cowplot)
library(tidyverse)
library(wesanderson)
library(ggplot2)
library(MASS)

#Data
growthDJ <- read.csv(file.choose(), header=T)
IMR <- read.csv(file.choose(), header=T)

################################################################################

## SUMMARY ##

#Summary Stats, we will use median to replace missing values.
summary(growthDJ$gdp60)
summary(growthDJ$gdp85)
summary(IMR$Rate)

# Density Plots
ggplot(data = growthDJ, aes(x = gdp60)) +
         geom_density(fill = "blue", color = "black", alpha = 0.5)
ggplot(data = growthDJ, aes(x = gdp85)) +
         geom_density(fill = "blue", color = "black", alpha = 0.5)
ggplot(data = IMR, aes(x = Rate)) +
         geom_density(fill = "blue", color = "black", alpha = 0.5)

# GDP 65 AND 80 Median Comparison
gdp1 <- growthDJ$gdp60
gdp2 <- growthDJ$gdp85
gdpData <- data.frame(Variable1 = gdp1, Variable2 = gdp2)

#qqnorm(gdp2)
#qqline(gdp2)

boxplot(newGdp60,
        main = "Boxplot of GDP IN 1960",
        xlab = "GDP",
        ylab = "Values",
        col = c("blue", "green"),
        names = c("GDP60")
)

summary(newGdp60)

boxplot(IMR,
        main = "Boxplot of IMR",
        xlab = "Rate",
        ylab = "Values",
        col = c("blue", "green"),
        names = c("Rate")
)

hist(IMR$Rate, breaks = 30, main = "Density Histogram of Sample Data", xlab = "Values", ylab = "Density", density = TRUE)
hist(growthDJ$gdp60, breaks = 30, main = "Density Histogram of Sample Data", xlab = "Values", ylab = "Density", density = TRUE)


newGdpData <- data.frame(Variable1 = newGdp60, Variable2 = newGdp85)
boxplot(newGdpData,
        main = "Boxplot of GDP IN 1965 and 1980",
        xlab = "GDP",
        ylab = "Values",
        col = c("blue", "green"),
        names = c("GDP60", "GDP85")
)

################################################################################

## Replace Missing Values for GDP with median ##

median_gdp60 <- median(growthDJ$gdp60, na.rm = TRUE)
growthDJ$gdp60[is.na(growthDJ$gdp60)] <- median_gdp60
sum(is.na(growthDJ$gdp60))

median_gdp85 <- median(growthDJ$gdp85, na.rm = TRUE)
growthDJ$gdp85[is.na(growthDJ$gdp85)] <- median_gdp85
sum(is.na(growthDJ$gdp85))

# BoxCox Transformation
transformedGdp60 <- boxcox(gdp1 ~ 1)
transformedGdp85 <- boxcox(gdp2 ~ 1)
lambda60 <- transformedGdp60$x[which.max(transformedGdp60$y)]
lambda60 # -0.2222222
lambda85 <- transformedGdp85$x[which.max(transformedGdp85$y)]
lambda85 # -0.02020202

newGdp60 <- (growthDJ$gdp60)^-0.2222222
newGdp85 <- (growthDJ$gdp85)^-0.02222222

################################################################################

## Normality Check ##

qqnorm(newGdp60)
qqline(newGdp60)

shapiro_test_result <- shapiro.test(newGdp60)
print(shapiro_test_result)

################################################################################

# Plots and summaries of other variables in dataset #

# Oil
ggplot(data = growthDJ, aes(x = oil)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Barplot of Oil Producers", x = "Oil Producers", y = "Frequency")

# OECD Members
ggplot(data = growthDJ, aes(x = oecd)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Barplot of OECD Member Count", x = "OECD", y = "Frequency")

# GDP Growth
summary(growthDJ$gdpgrowth)
ggplot(data = growthDJ, aes(x = gdpgrowth)) +
  geom_density(fill = "blue", color = "black", alpha = 0.5)

# Pop Growth
summary(growthDJ$popgrowth)
ggplot(data = growthDJ, aes(x = popgrowth)) +
  geom_density(fill = "blue", color = "black", alpha = 0.5)

# Literacy
summary(growthDJ$literacy60)
ggplot(data = growthDJ, aes(x = literacy60)) +
  geom_density(fill = "blue", color = "black", alpha = 0.5)

# School
summary(growthDJ$school)
ggplot(data = growthDJ, aes(x = school)) +
  geom_density(fill = "blue", color = "black", alpha = 0.5)

################################################################################

## Metropolis-Hastings Method to find Dist. of GDP ## OLD DATA

# For gdp60 #
# Reference: https://rpubs.com/ROARMarketingConcepts/1063733 
# https://www.nicksolomon.me/post/learn-metropolis-hastings-sampling-with-r/ 


# Define prior parameters for gamma distribution
# Define prior parameters
prior_shape <- 2  
prior_rate <- 1    

# Likelihood Function - Gamma
likelihood <- function(parameters, data) {
  shape <- parameters[1]
  rate <- parameters[2]
  dist <- sum(dgamma(data, shape = shape, rate = rate, log = TRUE))
  return(dist)
}

# Metropolis-Hastings Function
metropolis_hastings <- function(data, n_iter) {
  accepted_samples <- matrix(NA, nrow = n_iter, ncol = 2)
  current_parameters <- c(prior_shape, prior_rate)  
  
  for (i in 1:n_iter) {
    proposed_parameters <- current_parameters + rnorm(2, mean = 0, sd = c(0.1, 0.1))
    
    alpha <- exp(likelihood(proposed_parameters, data) - likelihood(current_parameters, data))
    
    # Accept or reject proposal
    if (runif(1) < alpha) {
      current_parameters <- proposed_parameters
    }
    
    accepted_samples[i,] <- current_parameters
  }
  
  return(accepted_samples)
}

# Monte Carlo Simulation
n <- 10000
posterior_samples <- metropolis_hastings(growthDJ$gdp60, n)


posterior_shape <- posterior_samples[,1]
posterior_rate <- posterior_samples[,2]
summary(posterior_shape)
summary(posterior_rate)

par(mfrow=c(1,2))
hist(posterior_shape, breaks = 50, main = "Posterior Distribution of Shape Parameter")
hist(posterior_rate, breaks = 50, main = "Posterior Distribution of Rate Parameter")

# Credible intervals
cred_intervals_shape <- quantile(posterior_shape, c(0.025, 0.975))
cred_intervals_rate <- quantile(posterior_rate, c(0.025, 0.975))

cred_intervals_shape
cred_intervals_rate

posterior_shape <- posterior_samples[,1]
posterior_rate <- 1/posterior_samples[,2]
posterior_mean_shape <- mean(posterior_shape)
posterior_mean_rate <- mean(posterior_rate)
posterior_mean_shape
posterior_mean_rate

################################################################################

## Metropolis-Hastings Method to find Dist. of IMR ##

# For IMR #

# Define Prior parameters for gamma distribution
prior_shape <- 2   # Shape parameter for gamma prior
prior_rate <- 1    # Rate parameter for gamma prior

# Likelihood Function - Gamma
likelihood <- function(parameters, data) {
  shape <- parameters[1]
  rate <- parameters[2]
  dist <- sum(dgamma(data, shape = shape, rate = rate, log = TRUE))
  return(dist)
}

# Metropolis-Hastings Function
metropolis_hastings <- function(data, n_iter) {
  accepted_samples <- matrix(NA, nrow = n_iter, ncol = 2)
  current_parameters <- c(prior_shape, prior_rate)  
  
  for (i in 1:n_iter) {
    proposed_parameters <- current_parameters + rnorm(2, mean = 0, sd = c(0.1, 0.1))
    
    # Calculate acceptance probability
    alpha <- exp(likelihood(proposed_parameters, data) - likelihood(current_parameters, data))
    
    # Accept or reject proposal
    if (runif(1) < alpha) {
      current_parameters <- proposed_parameters
    }
    
    accepted_samples[i,] <- current_parameters
  }
  
  return(accepted_samples)
}

# Monte Carlo Simulation
n <- 10000
posterior_samples <- metropolis_hastings(IMR$Rate, n)
par(mfrow=c(1,2))
hist(posterior_shape, breaks = 50, main = "Posterior Distribution of Shape Parameter")
hist(posterior_rate, breaks = 50, main = "Posterior Distribution of Rate Parameter")
plot(posterior_samples, col = "blue", pch = 20, xlab = "Shape Parameter", ylab = "Rate Parameter", main = "Posterior Samples")


posterior_shape <- posterior_samples[,1]
posterior_rate <- posterior_samples[,2]
posterior_mean_shape <- mean(posterior_shape)
posterior_mean_rate <- mean(posterior_rate)
cred_intervals_shape <- quantile(posterior_shape, c(0.025, 0.975))
cred_intervals_rate <- quantile(posterior_rate, c(0.025, 0.975))


posterior_mean_shape
posterior_mean_rate
cred_intervals_shape
cred_intervals_rate



################################################################################

## Gibbs Sampler for IMR (Market Power Proxy Variable)
## Reference: https://rpubs.com/andresmtnezglez/1040031#:~:text=Gibbs%20sampler%20is%20an%20example,are%20part%20of%20the%20model. 
# https://stats.stackexchange.com/questions/266665/gibbs-sampler-examples-in-r 


# Define prior parameters for gamma distribution
prior_shape <- 2  
prior_rate <- 1    

# Likelihood Function - Gamma
likelihood <- function(shape, rate, data) {
  dist <- sum(dgamma(data, shape = shape, rate = rate, log = TRUE))
  return(dist)
}

# Gibbs Sampler Function
gibbs_sampler <- function(data, n_iter) {
  accepted_samples <- matrix(NA, nrow = n_iter, ncol = 2)
  current_parameters <- c(prior_shape, prior_rate)  
  
  for (i in 1:n_iter) {
    # Update shape parameter
    current_parameters[1] <- rgamma(1, shape = sum(data) + prior_shape, rate = sum(data)/mean(data) + prior_rate)
    
    # Update rate parameter
    current_parameters[2] <- rgamma(1, shape = length(data) + prior_shape, rate = sum(data) + prior_rate)
    
    accepted_samples[i,] <- current_parameters
  }
  
  return(accepted_samples)
}

# Gibbs Sampling
n <- 10000
posterior_samples_gibbs <- gibbs_sampler(growthDJ$gdp60, n)

posterior_shape_gibbs <- posterior_samples_gibbs[,1]
posterior_rate_gibbs <- posterior_samples_gibbs[,2]

summary(posterior_shape_gibbs)
summary(posterior_rate_gibbs)

par(mfrow=c(1,2))
hist(posterior_shape_gibbs, breaks = 50, main = "Posterior Distribution of Shape Parameter (Gibbs)")
hist(posterior_rate_gibbs, breaks = 50, main = "Posterior Distribution of Rate Parameter (Gibbs)")

# Plot Joint Posterior Distribution
plot(posterior_samples_gibbs, xlab = "Shape Parameter (Alpha)", ylab = "Rate Parameter (Beta)", main = "Joint Posterior Distribution")



################################################################################


