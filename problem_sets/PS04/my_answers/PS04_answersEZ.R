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
equation <- paste("prestige =", intercept, "+", slope_income, "* income", "+", slope_professional, "* professional", slope_interaction, "* interaction")

# d)

# e) 

# f)

# g) 



# Question 2
