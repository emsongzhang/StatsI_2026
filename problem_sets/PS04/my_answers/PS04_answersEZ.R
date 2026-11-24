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
lapply(c("car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# utilizing stargazer package and library to format output
library(stargazer)

output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(..., table.placement = "H"))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=FALSE)
}

# Question 1

# installing prestige dataset in the car library 
install.packages(car)
library(car)
data(Prestige)
help(Prestige)

# a) 

# creating new column called "professional" that encodes the column "type"
# professionals are coded as 1
# blue and white collar workers are coded as 0
Prestige["professional"] <- ifelse(Prestige["type"] == "prof", 1, 0)

# b) 

# using lm function to run a linear model with prestige as an outcome
# income, professional, and the interaction of the two are the predictors
model <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(model)

# displaying summary of model usin stargazer for formatting
output_stargazer("q1_regression.tex", model,
                 title = "Regression of prestige on income and profession",
                 label = "tab:q1")

# c) 

# defining intercept and each slope value to write out the prediction equation
intercept <- coef(model)[1]
slope_income <- coef(model)[2]
slope_professional <- coef(model)[3]
slope_interaction <- coef(model)[4]
equation <- paste("prestige =", intercept, "+", slope_income, "* income", "+", slope_professional, "* professional", "+", slope_interaction, "* (income*professional)")

# d)

# if professional = 0: prestige = 21.142 + 0.003 * income 
# if professional = 1: prestige = 58.923 + 0.001*income, which is simplified from prestige = 21.142 + 0.003 * income + 37.781*1 + (-0.002)*income*1

# interpreting the slope coefficient of income
interpretation_income <- "Provided that professional = 0 (those who work blue or white collared jobs), a one unit increase in income is, on average, associated with 0.003 units increase in prestige.
                          Provided that professional = 1 (those who work professional jobs), a one unit increase in income is, on average, associated with 0.001 units increase in prestige.
                          The coefficient of the interaction term (income:profession) shows the difference in slope between the two lines from the equations above (0.001 - 0.003 = -0.002).
                          The average effect on prestige of a one-unit increase income does depend on the type of profession.
                          A real-world explanation of the model coefficients represent that prestige generally tends to be higher for individuals with a higher income;
                          however, prestige increases at a much lower rate with the increase in income for individuals who work professional jobs rather than blue or white collared jobs. 
                          Adding everything together, the positive association between prestige and income is dependent on the type of profession a given individual has, whether that be professional versus blue and white collared jobs."

# e) 

# prestige = 21.142 + 37.781* professional 

# interpreting the slope coefficient of professional
interpretation_professional <- "Holding income constant, on average, those who have professional jobs have 37.781 units of prestige more than those who work blue or white collared jobs."

# f)

# calculating the prestige when income is 0 and when income is 100
# this is to determine the marginal effect of an increase in income of $1000
prestige_income0 <- unname(intercept + slope_income*0 + slope_professional*1 + slope_interaction*0*1)
prestige_income1000 <- unname(intercept + slope_income*1000 + slope_professional*1 + slope_interaction*1000*1)

# marginal effect of income
prestige_income_effect <- prestige_income1000 - prestige_income0

# g) 

# calculating the prestige when the profession is professional and when the profession is blue or white collared jobs
# this is to determine the marginal effect of changing profession when income is $6000
prestige_prof <- unname(intercept + slope_income*6000 + slope_professional*1 + slope_interaction*6000*1)
prestige_unprof <- unname(intercept + slope_income*6000 + slope_professional*0 + slope_interaction*6000*0)

# marginal effect of type of profession
prestige_prof_effect <- prestige_prof - prestige_unprof

# Question 2

# a) 

alpha = 0.05

# testing precinct assigned lawn signs

# assumptions 
# 1) linearity between having yard signs in precinct and the proportion of vote that went to McAuliff's opponent Ken Cuccinelli
# 2) errors are independent, constant, and normal

# hypothesis
h0_assigned <- "There is no linear relationship between having yard signs in a precinct and the proportion of vote that went to McAuliff's opponent Ken Cuccinelli, 
                after controlling for the effects of being next to precincts with these yard signs in the model."
ha_assigned <- "There is a linear relationship between having yard signs in a precinct and the proportion of vote that went to McAuliff's opponent Ken Cuccinelli, 
                after controlling for the effects of being next to precincts with these yard signs in the model."

# test statistic
t_stat_assigned <- 0.042/0.016
df_assigned <- 131-2-1

# p-value
p_val_assigned <- 2*pt(abs(t_stat_assigned), df_assigned, lower.tail = F)

# conclusion 
conclusion_assigned <- "There is a linear relationship between the proportion of vote that went to McAuliff's opponent Ken Cuccinelli and having yard signs in a precinct."   

# b) 

# testing precinct adjacent to lawn signs

# assumptions 
# 1) linearity between being adjacent to precincts with yard signs and the proportion of vote that went to McAuliff's opponent Ken Cuccinelli
# 2) errors are independent, constant, and normal

# hypothesis
h0_adjacent <- "There is no linear relationship between being next to precincts with yard signs and the proportion of vote that went to McAuliff's opponent Ken Cuccinelli, 
                after controlling for the effects of having these yard signs in the precincts in the model."
ha_adjacent <- "There is a linear relationship between being next to precincts with yard signs and the proportion of vote that went to McAuliff's opponent Ken Cuccinelli, 
                after controlling for the effects of having these yard signs in the precincts in the model."

# test statistic
t_stat_adjacent <- 0.042/0.013
df_adjacent <- 131-2-1

# p-value
p_val_adjacent <- 2*pt(abs(t_stat_adjacent), df_adjacent, lower.tail = F)

# conclusion
conclusion_adjacent <- "There is a linear relationship between the proportion of vote that went to McAuliff's opponent Ken Cuccinelli and being next to precincts with yard signs." 

# c)

# y-intercept and standard error of y-intercept
constant <- 0.302
constant_se <- 0.011

interpretation_constant <- "The intercept represents the predicted average base proportion of vote that went to McAuliff's opponent Ken Cuccinelli (which is 0.302) when there are no yard signs in the precincts.
                            We also observe a relatively low standard error of 0.011, which indicates that the estimated baseline value of vote shares when there are no yard signs is precise 
                            and would vary only slightly across different random samples from the same population."

# d)

# r-squared value to represent the fit of the model 
r_sq <- 0.094
n <- 131

interpretation_fit <- "We observe an R-squared value of 0.094, which means that taking into account of precincts with yard signs and precincts adjacent to yard signs,
                      both factors only explain 9.4% of the variation in the vote share that went to McAuliff's opponent Ken Cuccinelli. 
                      This tells us that whether or not a precinct has yard signs does not make a strong predictor to explain changes in voteshares.
                      There are many other variables that have not been taken into account that may be confounders or would better explain the changes in voteshares. 
                      For example, income could have a large impact in determining the type of people that live within each precinct. That could influence political opinion as well. "

