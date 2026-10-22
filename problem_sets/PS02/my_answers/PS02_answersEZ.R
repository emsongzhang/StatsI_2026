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

political_science_tb <- data.frame(matrix(nrow=2, ncol=3))
rownames(political_science_tb) <- c("Upper Class", "Lower Class")
colnames(political_science_tb) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")

political_science_tb[1, 1] <- 14
political_science_tb[1, 2] <- 6
political_science_tb[1, 3] <- 7
political_science_tb[2, 1] <- 7
political_science_tb[2, 2] <- 7
political_science_tb[2, 3] <- 1

library(dplyr)

political_science_tb <- political_science_tb %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  bind_rows(summarise(., across(where(is.numeric), sum)))

rownames(political_science_tb)[3] <- "Total"

political_science_tb

# # install.packages("stargazer")
# library(stargazer)
# 
# output_stargazer <- function(outputFile, ...) {
#   output <- capture.output(stargazer(...))
#   cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
# }
# 
# output_stargazer("political_science_output.tex", political_science_tb, summary = FALSE, rownames = TRUE)

# a) 

h0 <- "Social class and police extortion are statistically independent."
h1 <- "Social class and police extortion are statistically dependent"

chi_exp <- function(row_total, grand_total, col_total){
  exp <- (row_total/grand_total)*col_total
  return(exp)
}

grand_total <- political_science_tb[nrow(political_science_tb), ncol(political_science_tb)]

exp_v <- c()

for(i in 1:ncol(political_science_tb)){
  for(j in 1:nrow(political_science_tb)){
    row_total <- political_science_tb[j, "Total"]
    col_total <- political_science_tb[nrow(political_science_tb), i]
    exp_temp <- chi_exp(row_total, grand_total, col_total)
    exp_v <- c(exp_v, exp_temp)
  }
}

exp_m <- matrix(exp_v, nrow=3)
exp_m <- round(exp_m, 3)
rownames(exp_m) <- c("Upper Class", "Lower Class", "Total")
colnames(exp_m) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning", "Total")
exp_m

f_obs <- political_science_tb
f_exp <- exp_m

f_obs
f_exp

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

chi_square <- chi_square(f_obs, f_exp)
chi_square

check_chi <- chisq.test(f_obs)

# b)

# checking assumption: frequency >= 5 for all cells

n_rows <- nrow(f_exp) - 1
n_cols <- ncol(f_exp) - 1

freq_check <- c()
count <- 0

for(i in 1: n_rows){
  for(j in 1:n_cols){
      freq_check <- c(freq_check, f_exp[i, j] >= 5)
      if(f_exp[i, j] < 5){
        count <- count+1
      }
        
  }
}

if(all(freq_check)){
  print("All expected frequencies are at least 5, so the chi-square assumption is met.")
} else {
  print("At least one cell has an expected frequency of less than 5, so assumption for the chi-square test is not met.")
  print(count)
}

df <- (n_rows-1)*(n_cols-1)

p_val <- pchisq(chi_square, df, lower.tail = FALSE)

alpha <- 0.1

max(p_val, alpha)

conclusion <- "We observe that the p-value is greater than the significance level of alpha at 0.1. 
              Hence, we do not have sufficient evidence to rejct the null, so we can say that social class and police extortion are statistically independent.
              However, because two expected counts were below 5, the assumption was not met and the result should be interpreted cautiously."

# c)

stand_residuals <- function(f_obs, f_exp, row_total, grand_total, col_total){
  stand_residuals <- (f_obs-f_exp)/sqrt(f_exp*(1-(row_total/grand_total))*(1-(col_total/grand_total)))
  return(stand_residuals)
}

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

res_m <- matrix(res_v, nrow=n_rows, ncol=n_cols, byrow=TRUE)
rownames(res_m) <- c("Upper Class", "Lower Class")
colnames(res_m) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")
res_m


# d)

interpretation <- "The standardised residuals illustrate the measure of strength of the difference between observed and expected values. 
                  Because it is a standardised measure to easily interpret the chi-squared value from different models, 
                  we can base our interpretation off of the normal distribution. According to the empirical rule, 
                  approximately 95% of the data should fall between -2 and +2 standard deviations of the mean. 
                  Any standardised residuals from our chi-squared test that fall outside of these bounds may indicate the existence of outliers. 
                  Using the cutoff |z| > 2, no cell shows a significant departure from independence, which is consistent with the non-significant chi-square test.
                  They have the highest standardised residuals of |+/-1.642| and |+/-1.523|, respectively, in comparison to the standardised residual for the Non Stopped category at |+/-0.322|.
                  This pattern weakly suggests that upper-class respondents were asked for bribes less often than expected. 
                  Stopped/warned upper-class respondents were more often than expect. 
                  However, it is important to note that none of these deviations is large enought to conclude that social class and police extortion are dependent.
                  The small expected counts mean that this pattern should be interpreted cautiously."
    


#####################
# Problem 2
#####################

gender_policies <- read.table("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header = TRUE, sep = ",")

head(gender_policies)
str(gender_policies)
dim(gender_policies)

# a) 

h0 <- "There is no linear relationship between the reservation policy and the number of new/repaired drinking water facilities in the villages."
h1 <- "There is a linear relationship between the reservation policy and the number of new/repaired drinking water facilities in the villages."

# b)

n <- length(1:nrow(gender_policies))
x <- gender_policies$reserved
y <- gender_policies$water

mean_x <- mean(x)
mean_y <- mean(y)

slope_est <- function(x,y, mean_x, mean_y, n) {
  num <- 0
  den <- 0
  for(i in 1:n){
    num <- num + (x[i] - mean_x) * (y[i] - mean_y)
    den <- den + (x[i] - mean_x)^2
  }
  return(num / den)
}

slope_est <- slope_est(x, y, mean_x, mean_y, n)
slope_est

b_est <- mean_y-slope_est*mean_x
y_pred <- b_est+slope_est*x

sse_est <- sum((y-y_pred)^2)
se_est <- sqrt(sse_est/(n-2))
slope_se <- se_est/sqrt(sum((x-mean_x)^2))

t_stat <- slope_est/slope_se
t_stat

p_value <- 2 * pt(abs(t_stat), df = n-2, lower.tail = FALSE)
p_value

model <- lm(gender_policies$water ~ gender_policies$reserved)
summary(model)

# c) 

alpha <- 0.05
max(p_value, alpha)

conclusion <- "We observe that the p-value is less than the significance level of alpha at 0.05. 
              Hence, we do have sufficient evidence to rejct the null. We can say that there is a linear relationship between the reservation policy and the number of new/repaired drinking water facilities in the villages.
              On average, GPs that are reserved to women have 9.252 more new/repaired drinking water facilities in the villages.
              However, it is important to note that the p-value we observe from the slope of 0.0197 is not statistically significant if the alpha threshold is changed to 0.01.
              My assumption of a threshold level at 0.05, I am accepting a % risk of a false positive, that is rejecting a true null hypothesis.
              We also observe from our linear regression model that the y-intercept also has a p-value almost at 0, which is less than the alpha level of 0.05.
              This indicates that the average number of water facilities in non-reserved villages is significantly different from 0.
              From a real-world standpoint, this is not necessarily abnormal because the non-reserved villages probably do get some new/repaired water facilities, 
              so it would be unlikey to assume that the control group should average exactly zero facilities.

              