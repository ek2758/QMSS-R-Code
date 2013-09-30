# ---------------------------------------------------------
# Title: Multinomial logit
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.8.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4014 Lecture 4
# ---------------------------------------------------------
library(nnet)
library(plyr)

load("gssCS.RData")

# -----------------------------------------------------------
# Multinomial logit predicting childcare spending preferences
# -----------------------------------------------------------
child.mult <- multinom(childcare ~ childs + age + married + lnrealinc + as.numeric(polviews),
                       data = gss.cs)
summary(child.mult)

# Extract the relative risk ratios
exp(coef(child.mult))

# ----------------------------------------------------------
# Predict biggest concern as result impact of global warming
# ----------------------------------------------------------
# Summarize variables
summary(subset(gss.cs, select=c("caremost","sex","educ","age","polviews","fund")))

# Multinomial logit
global.mult <- multinom(caremost ~ as.numeric(sex) + educ + age + as.numeric(polviews) + as.numeric(fund), data = gss.cs)
# No worries about the warning -- run the following and you'll see what it means
table(gss.cs$polviews)

summary(global.mult)
exp(coef(global.mult))

# Generate predicted probabilities for each level of variable: caremost
head(fitted(global.mult)) # View first few predicted probabilities

# Create dataset to get predicted probabilities
# For females, keep variables at mean and just change according to polviews
new.care <- data.frame(sex = "female", educ = mean(gss.cs$educ, na.rm=T), age = mean(gss.cs$age, na.rm=T), 
                       polviews = c("extremely liberal","liberal","slightly liberal","moderate",
                                    "slightly conservative","conservative","extrmly conservative"), 
                       fund = mean(as.numeric(gss.cs$fund), na.rm=T))

predict(global.mult, newdata = new.care, "probs")