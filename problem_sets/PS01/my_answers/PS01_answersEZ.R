#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# number of observations 

n = length(y) 

# descriptive statistics

mean_y <- sum(y)/n
sd_y <- sqrt(sum((y - mean_y) ^ 2) / (n - 1))
se_y <- sd_y/sqrt(n)
t_score <- qt(0.95, df = n - 1)

ci_90_lower= mean_y - t_score * se_y
ci_90_upper = mean_y + t_score * se_y


# hypothesis test 

# variables:
# explanatory - students (numeric)
# response - IQ scores (numeric)

# visualizing the distribution

# histogram and density plots
hist(y,
     breaks = 8,
     probability = TRUE,
     main = "IQ Scores of Students",
     xlab = "Scores")

lines(density(y), col = "blue")

# significance tests

# Question:
# Is the average IQ score in the counselor's school from the sample 
# higher than the average IQ score (100) among all the schools in the country?

# Hypotheses: 
# H0: Average IQ score of the students is 100                   (mu = 100)
# HA: Average IQ score of the students is greater than 100      (mu > 100)

t.test(y, mu = 100, alternative = "greater", conf.level = 0.95)

# Conclusion: fail to reject the null, so we do not have sufficient evidence to say that the average
# IQ score in the counselor's school is higher than the average IQ scoree among all schools in the country


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)

# quick overview 

head(expenditure)
str(expenditure)
summary(expenditure)

# Scatter plot

library(ggplot2)

# part 1

pairs(expenditure[, c("Y", "X1", "X2", "X3")], 
      upper.panel = NULL
)

vars <- c("Y", "X1", "X2", "X3")

# looping 
par(mfrow = c(2, 3))

for (i in 1:(length(vars) - 1)) {
  for (j in (i + 1):length(vars)) {
    
    plot(
      expenditure[[vars[i]]],
      expenditure[[vars[j]]],
      xlab = vars[i],
      ylab = vars[j],
      main = paste(vars[i], "vs", vars[j])
    )
    
  }
}



# part 2: 

ggplot(expenditure, aes(x = factor(Region), y = Y, color = factor(Region))) +
  geom_boxplot()
  

# part 3

ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  labs(title = "per capita expenditure on shelters/housing assistance in state",
       x = "",
       y = "per capita expenditure on shelters/housing assistance in state",
  ) 

ggplot(expenditure, aes(x = X1, y = Y, color = factor(Region), shape = factor(Region))) +
  geom_point() +
  labs(title = "per capita expenditure on shelters/housing assistance in state",
       x = "",
       y = "per capita expenditure on shelters/housing assistance in state",
       ) 


