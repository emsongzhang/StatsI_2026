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

# Question 1

# installing prestige dataset in the car library 
install.packages(car)
library(car)
data(Prestige)
help(Prestige)

# a) 

Prestige["professional"] <- ifelse(Prestige["type"] == "prof", 1, 0)

# b) 

model <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(model)

# c) 

intercept <- round(coef(model)[1], 3)
slope_income <- round(coef(model)[2], 3)
slope_professional <- round(coef(model)[3], 3)
slope_interaction <- round(coef(model)[4], 3)
equation <- paste("prestige =", intercept, "+", slope_income, "* income", "+", slope_professional, "* professional", "+", slope_interaction, "* (income*professional)")

# d)

# if professional = 0: prestige = 21.142 + 0.003 * income 
# if professional = 1: prestige = 58.923 + 0.001*income, which is simplified from prestige = 21.142 + 0.003 * income + 37.781*1 + (-0.002)*income*1

interpretation_income <- "Provided that professional = 0 (those who work blue or white collared jobs), a one unit increase in income is, on average, associated with 0.003 units increase in prestige.
                          Provided that professional = 1 (those who work professional jobs), a one unit increase in income is, on average, associated with 0.001 units increase in prestige.
                          The coefficient of the interaction term (income:profession) shows the difference in slope between the two lines (0.001 - 0.003 = -0.002).
                          The average effect on prestige of a one-unit increase income does depend on the type of profession.
                          A real-world explanation of the model coefficients represent that prestige generally tends to be higher for individuals with a higher income;
                          however, prestige increases at a much higher rate with the increase in income for individuals who work professional jobs rather than blue or white collared jobs. 
                          Adding everything together, the positive association between prestige and income is dependent on the type of profession a given individual has, whether that be professional versus blue and white collared jobs."

# e) 

# prestige = 21.142 + 37.781* professional 

interpretation_professional <- "Holding income constant, on average, those who have professional jobs have 37.781 units of prestige more than those who work blue or white collared jobs."

# f)

prestige_income_effect <- unname(intercept + slope_income*1000 + slope_professional*1 + slope_interaction*1000*1)

# g) 

prof <- unname(intercept + slope_income*6000 + slope_professional*1 + slope_interaction*6000*1)
unprof <- unname(intercept + slope_income*6000 + slope_professional*0 + slope_interaction*6000*0)

prestige_prof_effect <- prof - unprof

# Question 2

# a) 

alpha = 0.05

# testing precinct assigned lawn signs

# assumptions 

# hypothesis
h0_assigned <- "There is no linear relationship between having yard signs in a precinct and the proportion of vote that went to McAuliff's opponent Ken Cuccinelli, 
                after controlling for the effects of being next to precincts with these yard signs in the model."
ha_assigned <- "There is a linear relationship between having yard signs in a precinct and the proportion of vote that went to McAuliff's opponent Ken Cuccinelli, 
                after controlling for the effects of being next to precincts with these yard signs in the model."

# test statistic
t_stat_assigned <- 0.042/0.016
df_assigned <- 30-2-1

# p-value
p_val_assigned <- 2*pt(abs(t_stat_assigned), df_assigned, lower.tail = F)

# conclusion 
conclusion_assigned <- "There is a linear relationship between the proportion of vote that went to McAuliff's opponent Ken Cuccinelli and having yard signs in a precinct."   

# b) 

# testing precinct adjacent to lawn signs

# assumptions 

# hypothesis
h0_adjacent <- "There is no linear relationship between being next to precincts with yard signs and the proportion of vote that went to McAuliff's opponent Ken Cuccinelli, 
                after controlling for the effects of having these yard signs in the precincts in the model."
ha_adjacent <- "There is a linear relationship between being next to precincts with yard signs and the proportion of vote that went to McAuliff's opponent Ken Cuccinelli, 
                after controlling for the effects of having these yard signs in the precincts in the model."

# test statistic
t_stat_adjacent <- 0.042/0.013
df_adjacent <- 76-2-1

# p-value
p_val_adjacent <- 2*pt(abs(t_stat_adjacent), df, lower.tail = F)

# conclusion
conclusion_adjacent <- "There is a linear relationship between the proportion of vote that went to McAuliff's opponent Ken Cuccinelli and being next to precincts with yard signs." 

# c)

constant <- 0.302
constant_se <- 0.011

# d)

r_sq <- 0.094
n <- 131


