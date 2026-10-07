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

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# 1. 

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
# explanatory: students (numeric)
# response: IQ scores (numeric)

# visualizing the distribution

pdf("plot_1.pdf")

hist(y,
     breaks = 8,
     probability = TRUE,
     main = "IQ Scores of Students",
     xlab = "Scores")

lines(density(y), col = "blue")

dev.off()

# 2. 

# question:is the average IQ score in the counselor's school from the sample higher than the average IQ score (100) among all the schools in the country?
# hypotheses: 
# H0: average IQ score of the students is 100                   (mu = 100)
# HA: average IQ score of the students is greater than 100      (mu > 100)

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

# loading library
library(ggplot2)

# 1. 

vars <- c("Y", "X1", "X2", "X3")

labels <- c(
  "Expenditure",
  "Income",
  "Financially Insecure",
  "Urban"
)

pdf("plot_2.pdf")

par(mfrow = c(2, 3))

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

# 2. 

plot_3 <- ggplot(expenditure, aes(x = factor(Region), y = Y, color = factor(Region))) +
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

ggsave("plot_3.pdf", plot = plot_3, width = 7, height = 5)

# 3. 

plot_4 <- ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  labs(
    title = "Housing Expenditure vs Income",
    x = "Per Capita Income",
    y = "Per Capita Expenditure"
  )

ggsave("plot_4.pdf", plot = plot_4, width = 7, height = 5)

plot_5 <- ggplot(
  expenditure,
  aes(
    x = X1,
    y = Y,
    color = factor(Region),
    shape = factor(Region)
  )
) +
  geom_point() +
  labs(
    title = "Housing Expenditure vs Income by Region",
    x = "Per Capita Income",
    y = "Per Capita Expenditure",
    color = "Region",
    shape = "Region"
  )

ggsave("plot_5.pdf", plot = plot_5, width = 7, height = 5)



