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

# set working directory
setwd("/Users/emilyzhang/Documents/GitHub/StatsI_2026/problem_sets/PS01/my_answers")

# loading library (used in Question 2: Politcal Economy)
library(ggplot2)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# 1. Question 1: Education

# a. 

# number of observations 
n = length(y) 

# descriptive statistics
mean_y <- sum(y)/n
sd_y <- sqrt(sum((y - mean_y) ^ 2) / (n - 1))
se_y <- sd_y/sqrt(n)

# calculating the t-critical value: we use the t-distribution instead of the z-distribution because we observe that the length of y is less than 30 and the population mean and standard deviations are both unknown.
t_score <- qt(0.95, df = n - 1)

# calculating the lower and upper confidence bounds: confidence interval = sample statistic +/- margin of error 
ci_90_lower= mean_y - t_score * se_y
ci_90_upper = mean_y + t_score * se_y

# b. 

# hypothesis test 
# variables:
# explanatory: students (numeric)
# response: IQ scores (numeric)

# i. assumptions: 
# - numeric, continuous data
# - sample size = 25
# normal distribution 

# visualizing the distribution: we graph the observations to ensure for normality since the sample size is less than 30. We observe a relatively normal distribution, hence we can proceed with the significance test. 
pdf("plot_1.pdf")

hist(y,
     breaks = 8,
     probability = TRUE,
     main = "IQ Scores of Students",
     xlab = "Scores")

lines(density(y), col = "blue")

dev.off()

# ii. null and alternate hypothesis: 

# question:is the average IQ score in the counselor's school from the sample higher than the average IQ score (100) among all the schools in the country?
# hypotheses: 
# H0: average IQ score of the students is 100                   (mu = 100)
# HA: average IQ score of the students is greater than 100      (mu > 100)

mu_0 = 100

# iii. calculate a test statistic and t-critical value:
t_critical <- qt(0.95, df = n - 1)

t_score <- (mean_y - mu_0) / se_y

# iv. calculate a p-value 
p_value <- pt(t_score, df = n-1, lower.tail = FALSE)

# v. conclusion 
# We fail to reject the null because there is not sufficient evidence to say that the average IQ score in the counselor's school is higher than the average IQ score among all schools in the country.
# We draw this conclusion from observing a p-value = 0.7215, which is significantly greater than the alpha threshold value = 0.05. 
# In addition, we also observe that the t-score of -0.596 exceed the upper tail t-score of 1.711.
# Hence, in a world where the null is true, it is unlikely that we observe a value more extreme than the observed value. 

# checking my work with the built-in t.test function
t.test(y, mu = 100, alternative = "greater", conf.level = 0.95)

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt", header=T)

# quick overview 
head(expenditure)
str(expenditure)
summary(expenditure)

# 1. Question 2: Political Economy

# a. 

vars <- c("Y", "X1", "X2", "X3")

labels <- c(
  "Expenditure",
  "Income",
  "Financially Insecure",
  "Urban"
)

# use scatterplots for each panel because it most accurately represents the correlation between two continuous variables, displaying the magnitude and direction with the line of best fit
# scatterplots also efficiently visualise any outliers that may be skewing the data 
pdf("plot_2.pdf", width = 12, height = 8)

par(mfrow = c(2, 3))

# use for loop to create the same six graphs with its respective variables
# abline creates a line of best fit
# mtext displays the correlation r value 
for (i in 1:(length(vars) - 1)) {
  for (j in (i + 1):length(vars)) {
    
    plot(
      expenditure[[vars[j]]],
      expenditure[[vars[i]]],
      xlab = labels[j],
      ylab = labels[i],
      main = paste(labels[i], "vs", labels[j]),
      cex = 0.4
    )
    
    abline(
      lm(expenditure[[vars[i]]] ~ expenditure[[vars[j]]]),
      col = "blue",
    )
    
    r <- cor(
      expenditure[[vars[i]]],
      expenditure[[vars[j]]]
    )
    
    mtext(
      paste("r =", round(r, 2)),

    )
  }
}

dev.off()

# expenditure vs income (0.53): positive moderate linear relationship 
# expenditure vs financially insecure (0.45): positive moderate linear relationship
# expenditure vs urban (0.46): positive moderate linear relationship
# income vs financially insecure (0.21): positive weak linear relationship
# income vs urban (0.6): positive strong linear relationship
# financially insecure vs urban (0.22): positive weak linear relationship 

# Expenditure has a positive moderate linear relationship with income, "financially insecure" residents, and individuals who reside in urban areas. 
# Income and individuals who reside in urban areas have the strongest positive linear relationship .
# "Financially insecure" residents has a weak positive relationship with income and individuals who reside in urban areas, since in both graphs, the observations are widely around the fitted line.
# In general, higher per capita income, levels of financial insecurity, and urban population are each associated with higher per capita housing expenditure. 

# b. 

# use factor() to separate housing expenditure by region 
# labeling the four different regions to their respective names: 1 = northeast, 2 = north central, 3 = south, 4 = west
regions <- factor(
  expenditure$Region,
  levels = c(1, 2, 3, 4),
  labels = c("North East", "North Central", "South", "West")
)

# comparing that the mean and median are relatively similar, so that a boxplot would be a good representation of comparing the average housing expenditure by region
# in each region, the mean and median have a negligible difference
for (i in levels(regions)) {
  region_mean <- round(mean(expenditure$Y[regions == i]), 2)
  region_median <- round(median(expenditure$Y[regions == i]), 2)
  
  print(paste(i, "mean expenditure =", region_mean))
  print(paste(i, "median expenditure =", region_median))
  cat("\n")
}

# use boxplot to compare data distributions across multiple groups and spot outliers
# scale_shape_manual() with the value 18 represents a diamond shape in R (this is to differentiate the mean and median line on the boxplot graph)
plot_3 <- ggplot(expenditure, aes(x = regions, y = Y, color = factor(regions))) +
  geom_boxplot() +
  stat_summary(
    aes(shape = "Mean"),
    fun = mean,
    geom = "point",
    size = 3,
    alpha = 0.45
  ) +
  labs(
    title = "Housing Expenditure by Region",
    x = "Region",
    y = "Per Capita Expenditure",
    color = "Region",
    shape = ""
  ) +
  scale_shape_manual(
    values = c("Mean" = 18)
  )

ggsave("plot_3.pdf", plot = plot_3, width = 9, height = 7)

# On average, the West region has the highest per capita expenditure on housing assistance, with a mean of $88.31 and $87.
# As shown on the boxplot, both the mean and the median surpass those of the other three regions.

# c. 

# use scatterplot to most accurately represent the relationship between housing expenditure and income, since both are also continous variables
# the line of best fit represents the magnitude and direction of the correlation between the two variables
plot_4 <- ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    color = "blue",
    se = FALSE
  ) +
  labs(
    title = "The Relationship between Housing Expenditure and Income by Region",
    subtitle = paste("r =", round(cor(expenditure$X1, expenditure$Y), 3)),
    x = "Per Capita Income",
    y = "Per Capita Expenditure"
  ) 

ggsave("plot_4.pdf", plot = plot_4, width = 9, height = 7)

# The scatterplot shows the per capita personal income versus the per capita expenditure on shelters/housing assistance in the respective states.
# We observe a moderate positive linear relationship between per capital income and per capita expenditure, also represented by the correlation coefficient of 0.532.
# There are several outliers that stand out, since they noticeably deviate from the overall linear trend.
# For example, there is one observation around an income of 2,100 with expenditure near 42, and another around an income of 2,750 with expenditure near 73. 
# There are also a few unusually high expenditure observations around incomes of 2,550 to 2,700, with expenditures above 120.
# In general, states with higher per capita income tend to have higher per capita housing expenditure.

# use shape and color to group the observations by each region with its respective shape and color
# use factor(regions) to separate observations based on the region 
# regions is the variable created in part b of Question 2: Political Economy to explicitly label each region number with its respective names
plot_5 <- ggplot(
  expenditure,
  aes(
    x = X1,
    y = Y,
    color = factor(regions),
    shape = factor(regions)
  )
) +
  geom_point() +
  labs(
    title = "The Relationship between Housing Expenditure and Income by Region",
    x = "Per Capita Income",
    y = "Per Capita Expenditure",
    color = "Region",
    shape = "Region"
  )

ggsave("plot_5.pdf", plot = plot_5, width = 9, height = 7)

# calculating the correlations between per capital income and per capita expenditure for each region to illustrate that the correlation of r=0.532 of all the observations do not translate to each respective region
# some regions have much stronger positive linear correlation between the two variables than other regions 
for (i in levels(regions)) {
  region_correlation <- round(cor(expenditure$X1[regions == i], expenditure$Y[regions == i]), 3)
  
  print(paste(i, "correlation =", region_correlation))
  cat("\n")
}

# We observe some clustering of observations based on the specific region. 
# For example, in the South, individuals tend to have both lower income and lower expenditure. 
# In contrast, individuals in the North East, North Central, and West exhibit higher levels of both income and expenditure. 
# When the observations are separated by region, we observe less of a linear association between the data points in the North Central and West. 
# This also demonstrated by the respective correlation coefficients of r=0.184 for North Central and r=0.305 for West. 
# In contrast, the data points in the South have a moderate positive linear correlation at r=0.556 and the data points in the North East have a strong positive linear correlation at r=0.802.

