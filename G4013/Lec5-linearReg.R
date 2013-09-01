# ---------------------------------------------------------
# Title: Linear Regression
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4013 Lecture 5
# ---------------------------------------------------------

# Load the previously saved workspace with GSS 2010 file
load("gss2010.RData")

# ---------------------------------------------------
# Predicting job prestige with educational attainment
# ---------------------------------------------------
prestg.lm <- lm(prestg80 ~ educ, data = gss2010)
# You can assign the model results to an object

# Print the regression results
summary(prestg.lm)

# T-test for correlation
cor.test(gss2010$educ, gss2010$prestg80, na.rm = TRUE) # default confidence level is 95%

# -------------------------------
# Predicting spanking with degree
# -------------------------------
# Linear regression predicting spanking preferences from degree attainment
lm(spanking ~ degree, data = gss2010)
# You should get an error here because the degree variable is a nominal variable, not numeric

# You can dynamically change the variables within the regression to numeric
spank.lm <- lm(as.numeric(spanking) ~ as.numeric(degree), data = gss2010)
summary(spank.lm)

# --------------------------------------
# Predicting TV-watching hours with race
# --------------------------------------
gss2010$white <- ifelse(gss2010$race == "white", 1, 0)

tv.lm <- lm(tvhours ~ white, data = gss2010)
summary(tv.lm)

# Save the workspace
save.image("gss2010.RData")