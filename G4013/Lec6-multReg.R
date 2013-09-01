# ---------------------------------------------------------
# Title: Multiple Regression
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4013 Lecture 6
# ---------------------------------------------------------

# Let's try the GSS 1972-2012 cross-sectional cumulative data file
# http://publicdata.norc.org/GSS/DOCUMENTS/OTHR/GSS_stata.zip
library(foreign)
getwd()
gss.cs <- read.dta("GSS7212_R2.DTA") # 350MB -- this will take a minute to convert

# Check out some variables for a multiple regression
summary(gss.cs$paattend)
summary(gss.cs$educ)

# ---------------------------------------------------------------------------------
# Predicting educational attainment with frequency of father's religious attendance
# ---------------------------------------------------------------------------------
educDadR.lm <- lm(educ ~ as.numeric(paattend), 
                    data = subset(gss.cs, !is.na(paeduc)), # You can subset a dataset in a regression call
                    year==1985) # You can run the regression for an additional subset
summary(educDadR.lm)

# Account for father's educational attainment
educDadRE.lm <- update(educDadR.lm, ~ . + paeduc)
# "update" is a super-nifty command to update a previously saved regressional model.
# Here, we're just adding an additional variable, so it's more readable to simply update the previous model

summary(educDadRE.lm)

# -------------------------------------
# Predicting smarts from married status
# -------------------------------------
# Recode marital variable
summary(gss.cs$marital)
gss.cs$married <- ifelse(gss.cs$marital=="married", 1, 
                         ifelse(gss.cs$marital %in% c("widowed","divorced","separated","never married"), 0, NA))
table(gss.cs$marital, gss.cs$married) # sanity check

wordMarry.lm <- lm(wordsum ~ married, data = gss.cs)
summary(wordMarry.lm)

# ------------------------------------------------
# Predicting smarts from married status and degree
# ------------------------------------------------
wordMarDeg.lm <- update(wordMarry.lm, ~ . + as.numeric(degree))
summary(wordMarDeg.lm)

# Add educational attainment
wordMarEduc.lm <- lm(wordsum ~ as.numeric(educ) + as.numeric(speduc), data = gss.cs)
summary(wordMarEduc.lm)

# --------------------------------
# Predicting occupational prestige
# --------------------------------
prestg.lm <- lm(prestg80 ~ papres80, data = gss.cs)
summary(prestg.lm)

# Add education
prestgE.lm <- update(prestg.lm, ~ . + educ)
summary(prestgE.lm)

# ----------------------
# Predicting family size
# ----------------------
fam.lm <- lm(childs ~ sibs, data = gss.cs)
summary(fam.lm)

# Add age
famAge.lm <- update(fam.lm, ~ . + age)
summary(famAge.lm)

# Get standardized regression coefficients of previous model
install.packages("QuantPsyc")
library(QuantPsyc)
?lm.beta
lm.beta(famAge.lm)

# ---------------------------------------
# Predicting family size from age and sex
# ---------------------------------------
table(gss.cs$sex)

# Create male variable
gss.cs$male <- ifelse(gss.cs$sex=="male", 1, 0)

famAgeM.lm <- update(famAge.lm, ~ . + male)
summary(famAgeM.lm)
lm.beta(famAgeM.lm)

# ------------------
# On dummy variables
# ------------------
class(gss.cs$region)
# R has a useful variable type called a "factor."
# Akin to labels in STATA, these data types can be seamlessly converted from numeric to character. 
# Factors also take ordered levels, which come in handy for ordinal outcome variables and plots.
levels(gss.cs$region) # this is the "order" of this factor
table(as.numeric(gss.cs$region))
table(gss.cs$region)

# --------------------------------
# Predicting education from region
# --------------------------------
educReg.lm <- lm(educ ~ region, data = gss.cs)
summary(educReg.lm)
# Notice the simple inclusion of the region variable runs the regression with region as a character variable
# whereas, in STATA, "i." must be pre-pended to create a dummy variable. 
# I appreciate the R default. I hope you do too. 

levels(gss.cs$region) # New England is the base category

# Changing the levels
educReg.lm <- lm(educ ~ relevel(region, ref=9), data = gss.cs)
summary(educReg.lm)
# Many functions in R can be used in the manner used in the above regression.
# E.g, Relevel can be used on the fly in a regression,
# but it can also be used to change the levels of a factor in a data set. 
# In other words, you can be "creative" and it will work in R.

# The following site is a useful resource describing factor variables in R:
# http://www.stat.berkeley.edu/classes/s133/factors.html

# Save the workspace
save.image("gssCS.RData")