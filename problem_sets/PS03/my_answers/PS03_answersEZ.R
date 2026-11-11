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
library(ggplot2)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")

# Question 1

# a)
model1 <- lm(inc.sub$voteshare ~ inc.sub$difflog)
summary(model1)

#b)
ggplot(inc.sub, aes(x=difflog, y=voteshare)) + 
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE)

# c)
residuals1 <- residuals(model1)
residuals1

#d)
intercept1 <- round(coef(model1)[1], 3)
slope1 <- round(coef(model1)[2], 3)

equation1 <- paste("voteshare =", intercept1, "+", slope1, "* difflog")
equation1


# Question 2

# a)
model2 <- lm(inc.sub$presvote ~ inc.sub$difflog)
summary(model2)

# b)
ggplot(inc.sub, aes(x=difflog, y=presvote)) + 
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE)

# c)
residuals2 <- residuals(model2)
residuals2

# d) 
intercept2 <- round(coef(model2)[1], 3)
slope2 <- round(coef(model2)[2], 3)

equation2 <- paste("presvote =", intercept2, "+", slope2, "* difflog")
equation2


# Question 3

# a) 
model3 <- lm(inc.sub$voteshare ~ inc.sub$presvote)
summary(model3)

# b)
ggplot(inc.sub, aes(x=presvote, y=voteshare)) + 
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE)

# c)
intercept3 <- round(coef(model3)[1], 3)
slope3 <- round(coef(model3)[2], 3)

equation3 <- paste("voteshare =", intercept3, "+", slope3, "* presvote")
equation3


# Question 4

# a)
residuals <- data.frame(residuals1, residuals2)

model4 <- lm(residuals$residuals1 ~ residuals$residuals2)
summary(model4)

# b)
ggplot(residuals, aes(x=residuals2, y=residuals1)) + 
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = FALSE)

# c)
intercept4 <- round(coef(model4)[1], 3)
slope4 <- round(coef(model4)[2], 3)

equation4 <- paste("voteshare_residual =", intercept4, "+", slope4, "* presvote_residual")
equation4


# Question 5

# a)
model5 <- lm(inc.sub$voteshare ~ inc.sub$difflog + inc.sub$presvote)
summary(model5)

# b) 
intercept5 <- round(coef(model5)[1], 3)
slope5_difflog <- round(coef(model5)[2], 3)
slope5_presvote <- round(coef(model5)[3], 3)

equation5 <- paste("voteshare =", intercept5, "+", slope5_difflog,"* difflog", "+", slope4, "* presvote")
equation5

# c)

# The coefficient for presvote is the same. 

# simple linear regression: voteshare_residual = 0 + 0.257 * presvote_residual
# multiregression: "voteshare = 0.449 + 0.036 * difflog + 0.257 * presvote"

# In the multi linear regression, we can interpret the coefficient of 0.257 as: holding all else constant, a one-unit increase in the vote share 
# of the presidential candidate will increase the difference between the incumbent and challenger's spending by $0.26. 

# In the simple linear regression of the residuals, the 0.257 coefficient of voteshare_residual represents that for a one-unit increase in the variation not explained by presvote, 
# the variation not explained by voteshare increases by 0.257. In other words, as the variation of 



