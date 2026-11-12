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

# importing stargazer package and loading library
install.packages("stargazer")
library(stargazer)

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")

# utilizing stargazer package and library to format output
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=FALSE)
}

# Question 1

# a)
# the effect of difference in campaign spending between incumbent and challenger (log scale) on the incumbent's share of the vote in the House election
# using lm function to perform simple linear regression
model1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(model1)
output_stargazer("q1_regression.tex", model1,
                 title = "Regression of voteshare on difflog",
                 label = "tab:q1")

# b)
# using ggplot and geom_point to display scatterplot 
# using geom_smooth to display regression line 
plot1 <- ggplot(inc.sub, aes(x=difflog, y=voteshare)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE)
ggsave("q1_plot.pdf", plot1, width = 6, height = 4)

# c)
# using residuals function to calculate the residuals
residuals1 <- residuals(model1)
summary(residuals1)

# d)
# using coef function to solve for intercept and slope value 
intercept1 <- round(coef(model1)[1], 3)
slope1 <- round(coef(model1)[2], 3)

# printing out the equation 
equation1 <- paste("voteshare =", intercept1, "+", slope1, "* difflog")
equation1


# Question 2

# a)
# the effect of difference in campaign spending between incumbent and challenger (log scale) on the share of vote that the 
# presidential candidate from the incumbent's party got
model2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model2)
output_stargazer("q2_regression.tex", model2,
                 title = "Regression of presvote on difflog",
                 label = "tab:q2")

# b)
# using ggplot and geom_point to display scatterplot 
# using geom_smooth to display regression line 
plot2 <- ggplot(inc.sub, aes(x=difflog, y=presvote)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE)
ggsave("q2_plot.pdf", plot2, width = 6, height = 4)

# c)
# using residuals function to calculate the residuals
residuals2 <- residuals(model2)
summary(residuals2)

# d)
# using coef function to solve for intercept and slope value 
intercept2 <- round(coef(model2)[1], 3)
slope2 <- round(coef(model2)[2], 3)

# printing out the equation 
equation2 <- paste("presvote =", intercept2, "+", slope2, "* difflog")
equation2


# Question 3

# a)
# the effect of the share of vote that the presidential candidate from the incumbent's party has on the incumbent's share of vote in the House election
model3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(model3)
output_stargazer("q3_regression.tex", model3,
                 title = "Regression of voteshare on presvote",
                 label = "tab:q3")

# b)
# using ggplot and geom_point to display scatterplot 
# using geom_smooth to display regression line 
plot3 <- ggplot(inc.sub, aes(x=presvote, y=voteshare)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE)
ggsave("q3_plot.pdf", plot3, width = 6, height = 4)

# c)
# using residuals function to calculate the residuals
intercept3 <- round(coef(model3)[1], 3)
slope3 <- round(coef(model3)[2], 3)

# using coef function to solve for intercept and slope value 
equation3 <- paste("voteshare =", intercept3, "+", slope3, "* presvote")
equation3


# Question 4

# a)
# voteshare residual (residual 1): how much better or worse the incumbent did than you had predict form campaign spending alone
# presvote residual (residual 2): how much better or worse the incumbent party's presidential candidate did in that district than you had predict from campaign spending along
residuals <- data.frame(residuals1, residuals2)

# voteshare residuals are regressed on presvote residuals
model4 <- lm(residuals1 ~ residuals2, data = residuals)
summary(model4)
output_stargazer("q4_regression.tex", model4,
                 title = "Regression of Q1 residuals on Q2 residuals",
                 label = "tab:q4")

# b)
# using ggplot and geom_point to display scatterplot 
# using geom_smooth to display regression line 
plot4 <- ggplot(residuals, aes(x=residuals2, y=residuals1)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE)
ggsave("q4_plot.pdf", plot4, width = 6, height = 4)

# c)
# using residuals function to calculate the residuals
intercept4 <- round(coef(model4)[1], 3)
slope4 <- round(coef(model4)[2], 3)

# using coef function to solve for intercept and slope value 
equation4 <- paste("voteshare_residual =", intercept4, "+", slope4, "* presvote_residual")
equation4


# Question 5

# a)
# the effect of both the difference in campaign spending between the incumbent and the challenger (log scale) 
# and the share of vote that the presidential candidate from the incumbent's party got on the proportion of incumbent's share in the House election
model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model5)
output_stargazer("q5_regression.tex", model5,
                 title = "Regression of voteshare on difflog and presvote",
                 label = "tab:q5")

# b)
# using coef function to solve for intercept and both slope values 
intercept5 <- round(coef(model5)[1], 3)
slope5_difflog <- round(coef(model5)[2], 3)
slope5_presvote <- round(coef(model5)[3], 3)

# using coef function to solve for intercept and slope value 
equation5 <- paste("voteshare =", intercept5, "+", slope5_difflog, "* difflog", "+", slope5_presvote, "* presvote")
equation5

# c)

# What is it in this output that is identical to the output in Question 4? Why do you think this is the case?






