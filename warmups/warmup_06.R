Year <- c(1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969)      
## assigning a vector, year
Population <- c(4835, 4970, 5085, 5160, 5310, 5260, 5235, 5255, 5235, 5210, 5175) 
## assigning a vector, population


sample1 <- data.frame(Year, Population)  
## creating a data frame, sample1
sample1
## prints sample1

sample1$Year <- sample1$Year - 1964  
## assigning sample year subtracting 1964
sample1


plot(sample1$Year, sample1$Population, type="b") 
## ploting the year vs. corresponding population)



fit1 <- lm(sample1$Population ~ sample1$Year)
## finding the best linear model for population vs year 
fit2 <- lm(sample1$Population ~ sample1$Year + I(sample1$Year^2))
## finding the best linear model while trying to remove variance 
fit3 <- lm(sample1$Population ~ sample1$Year + I(sample1$Year^2) + I(sample1$Year^3)) 
## finding the best linear model while trying to remove variance


# Pay particular attention to this one!
fit4 <- lm(sample1$Population ~ sample1$Year + I(sample1$Year^3)) 
## finding the best linear model while removing the second power of variance

summary(fit2) ## Gives the summary of each fit
summary(fit3)
summary(fit4)

plot(sample1$Year, sample1$Population, type="l", lwd=3) 
points(sample1$Year, predict(fit2), type="l", col="red", lwd=2) 
## creates predictive points from corresponding fit linear model
points(sample1$Year, predict(fit3), type="l", col="blue", lwd=2)
## creates predictive points from corresponding fit linear model
points(sample1$Year, predict(fit4), type="l", col="green", lwd=2)
## creates predictive points from corresponding fit linear model

# what is going on with fit4? Can you explain why it looks so different from fit2 and fit3?
## coefficient was throw out of whack because we took out the prior coefficient.  