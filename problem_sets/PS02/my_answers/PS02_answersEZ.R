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

lapply(c("ggplot2", "stargazer"),  pkgTest)

# set working directory to current parent folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# creating the table data frame and labelling the columns and rows
political_science_tb <- data.frame(matrix(nrow=2, ncol=3))
rownames(political_science_tb) <- c("Upper Class", "Lower Class")
colnames(political_science_tb) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")

# entering the values into the table
political_science_tb[1, 1] <- 14
political_science_tb[1, 2] <- 6
political_science_tb[1, 3] <- 7
political_science_tb[2, 1] <- 7
political_science_tb[2, 2] <- 7
political_science_tb[2, 3] <- 1

# importing dplyr package to use mutate() function to find the row and column totals
library(dplyr)

political_science_tb <- political_science_tb %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  bind_rows(summarise(., across(where(is.numeric), sum)))

rownames(political_science_tb)[3] <- "Total"

political_science_tb

# a) 

# stating the null and alternate hypothesis
h0 <- "Social class and police extortion are statistically independent."
h1 <- "Social class and police extortion are statistically dependent"

# creating expected value function to loop through the data table
chi_exp <- function(row_total, grand_total, col_total){
  exp <- (row_total/grand_total)*col_total
  return(exp)
}

# calculating the grand total of the table
grand_total <- political_science_tb[nrow(political_science_tb), ncol(political_science_tb)]

# creating for loop to iterate through data table and apply the chi_exp() function from above
exp_v <- c()

for(i in 1:ncol(political_science_tb)){
  for(j in 1:nrow(political_science_tb)){
    row_total <- political_science_tb[j, "Total"]
    col_total <- political_science_tb[nrow(political_science_tb), i]
    exp_temp <- chi_exp(row_total, grand_total, col_total)
    exp_v <- c(exp_v, exp_temp)
  }
}

# putting the expected values into table format
exp_m <- matrix(exp_v, nrow=3)
exp_m <- round(exp_m, 3)
rownames(exp_m) <- c("Upper Class", "Lower Class", "Total")
colnames(exp_m) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning", "Total")
exp_m

# creating variable for observed values and expected values
f_obs <- political_science_tb
f_exp <- exp_m

f_obs
f_exp

# creating function to calculate chi_square when the observed values and expected values are given
chi_square <- function(f_obs, f_exp){
  n_rows <- nrow(f_obs) - 1
  n_cols <- ncol(f_obs) - 1
  
  chi_square <- 0
  
  for(i in 1: n_rows){
    for(j in 1:n_cols){
      obs <- f_obs[i, j]
      exp <- f_exp[i, j]
      chi_square <- chi_square + (((obs-exp)^2)/exp)
    }
    
  }
  
  return(chi_square)
  
}
# applying the chi_square() function from above
chi_square <- chi_square(f_obs, f_exp)
chi_square

# checking output with R's built-in chi-square function: chisq.test()
check_chi <- chisq.test(f_obs[1:2, 1:3])
check_chi

# b)

# checking assumption: frequency >= 5 for all cells
n_rows <- nrow(f_exp) - 1
n_cols <- ncol(f_exp) - 1

freq_check <- c()
count <- 0

# looping through the expected table to count how many cells are below 5
for(i in 1: n_rows){
  for(j in 1:n_cols){
      freq_check <- c(freq_check, f_exp[i, j] >= 5)
      if(f_exp[i, j] < 5){
        count <- count+1
      }
        
  }
}

#  outputting whether assumptions are met and if not, how many cells do not meet this requirement
if(all(freq_check)){
  print("All expected frequencies are at least 5, so the chi-square assumption is met.")
} else {
  print("At least one cell has an expected frequency of less than 5, so assumption for the chi-square test is not met.")
  print(count)
}

# calculating degrees of freedom
df <- (n_rows-1)*(n_cols-1)

# calculating p-value using R's built-in function pchisq()
p_val <- pchisq(chi_square, df, lower.tail = FALSE)

alpha <- 0.1

# comparing the alpha value with the p-value to come to a conclusion for the significance test
max(p_val, alpha)

if (p_val < alpha){
  print("Reject H0")
} else {
  "Fail to reject H0"
}

# hypothesis test conclusion
conclusion <- "We observe that the p-value is greater than the significance level of alpha at 0.1. 
              Hence, we do not have sufficient evidence to reject the null, so we can say that social class and police extortion are statistically independent.
              However, because two expected counts were below 5, the assumption was not met and the result should be interpreted cautiously."

# c)

# creating function to calculate the standardised residuals when given the observed values, expected values, and totals
stand_residuals <- function(f_obs, f_exp, row_total, grand_total, col_total){
  stand_residuals <- (f_obs-f_exp)/sqrt(f_exp*(1-(row_total/grand_total))*(1-(col_total/grand_total)))
  return(stand_residuals)
}

# using for loop to loop through observed values and expected values, so that the standardised residual can be calculated for each cell
n_rows <- nrow(f_obs) - 1
n_cols <- ncol(f_obs) - 1
grand_total <- f_obs[nrow(f_obs), ncol(f_obs)]

res_v <- c()

for(i in 1: n_rows){
  for(j in 1:n_cols){
    row_total <- f_obs[i, "Total"]
    col_total <- f_obs[nrow(f_obs), j]
    obs <- f_obs[i, j]
    exp <- f_exp[i, j]
    
    res_temp <- stand_residuals(obs, exp, row_total, grand_total, col_total)
    res_v <- c(res_v, res_temp)
  }
}

# formatting the standardised residuals into a table within their respective cells
res_m <- matrix(res_v, nrow=n_rows, ncol=n_cols, byrow=TRUE)
rownames(res_m) <- c("Upper Class", "Lower Class")
colnames(res_m) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")
res_m


# d)

# interpretating the standardised residuals
interpretation <- "The standardised residuals illustrate the measure of strength of the difference between observed and expected values. 
                  Because it is a standardised measure to easily interpret the chi-squared value from different models, 
                  we can base our interpretation off of the normal distribution. According to the empirical rule, 
                  approximately 95% of the data should fall between -2 and +2 standard deviations of the mean. 
                  Any standardised residuals from our chi-squared test that fall outside of these bounds may indicate the existence of outliers. 
                  Using the cutoff |z| > 2, no cell shows a significant departure from independence, which is consistent with the non-significant chi-square test.
                  They have the highest standardised residuals of |+/-1.642| and |+/-1.523|, respectively, in comparison to the standardised residual for the Non Stopped category at |+/-0.322|.
                  This pattern weakly suggests that upper-class respondents were asked for bribes less often than expected. 
                  Stopped/warned upper-class respondents were more often than expect. 
                  However, it is important to note that none of these deviations is large enough to conclude that social class and police extortion are dependent.
                  The small expected counts mean that this pattern should be interpreted cautiously."
    


#####################
# Problem 2
#####################

# reading the data into a table
gender_policies <- read.table("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header = TRUE, sep = ",")

# exploring the data 
head(gender_policies)
str(gender_policies)
dim(gender_policies)

# a) 

# stating null and alternate hypothesis
h0 <- "There is no linear relationship between the reservation policy and the number of new/repaired drinking water facilities in the villages."
h1 <- "There is a linear relationship between the reservation policy and the number of new/repaired drinking water facilities in the villages."

# b)

# calculating the needed statistics to run a bivariate regression by hand (length, mean of x, mean of y)
n <- length(1:nrow(gender_policies))
x <- gender_policies$reserved
y <- gender_policies$water

mean_x <- mean(x)
mean_y <- mean(y)

# function to calculate the estimated slope 
slope_est <- function(x,y, mean_x, mean_y, n) {
  num <- 0
  den <- 0
  for(i in 1:n){
    num <- num + (x[i] - mean_x) * (y[i] - mean_y)
    den <- den + (x[i] - mean_x)^2
  }
  return(num / den)
}

# applying the slope_est() function from above
slope_est <- slope_est(x, y, mean_x, mean_y, n)
slope_est

# calculating the estimated y-intercept
b_est <- mean_y-slope_est*mean_x
y_pred <- b_est+slope_est*x

# calculating the sum of squared errors of the slope 
sse_est <- sum((y-y_pred)^2)

# calculating the residual standard error: typical distance of 1 data point from the regression line, in the same units as y 
# also known as root mean square error 
se_est <- sqrt(sse_est/(n-2))

# calculating the standard error of the slope: how much the estimated slope would bounce around from sample to sample, so it tells how precise the slope is
slope_se <- se_est/sqrt(sum((x-mean_x)^2))

# calculating t-statistic
t_stat <- slope_est/slope_se
t_stat

# calculating p-value
p_value <- 2 * pt(abs(t_stat), df = n-2, lower.tail = FALSE)
p_value

# checking calculations with R's built-in function lm()
model <- lm(gender_policies$water ~ gender_policies$reserved)
summary(model)

# c) 

# comparing the alpha level and p-value to make a conclusion about the hypothesis test 
# making an assumption that the alpha = 0.05
alpha <- 0.05
max(p_value, alpha)

if (p_value < alpha){
  print("Reject H0")
} else {
  "Fail to reject H0"
}

# drawing conclusions about the significance test 
conclusion <- "We observe that the p-value is less than the significance level of alpha at 0.05. 
              Hence, we do have sufficient evidence to rejct the null. We can say that there is a linear relationship between the reservation policy and the number of new/repaired drinking water facilities in the villages.
              On average, GPs that are reserved to women have 9.252 more new/repaired drinking water facilities in the villages.
              However, it is important to note that the p-value we observe from the slope of 0.0197 is not statistically significant if the alpha threshold is changed to 0.01.
              My assumption of a threshold level at 0.05, I am accepting a 5% risk of a false positive, that is rejecting a true null hypothesis.
              We also observe from our linear regression model that the y-intercept also has a p-value almost at 0, which is less than the alpha level of 0.05.
              This indicates that the average number of water facilities in non-reserved villages is significantly different from 0.
              From a real-world standpoint, this is not necessarily abnormal because the non-reserved villages probably do get some new/repaired water facilities, 
              so it would be unlikely to assume that the control group should average exactly zero facilities."

              
              
              
              