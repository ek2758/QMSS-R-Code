# ---------------------------------------------------------
# Title: Statistical Inference
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4013 Lecture 4B
# ---------------------------------------------------------

# Load the previously saved workspace with GSS 2010 file
load("gss2010.RData")

# -------------------------
# Looking at internet hours
# -------------------------

# Create a new variable
gss2010$totalhr <- gss2010$wwwhr + gss2010$emailhr
summary(gss2010$totalhr) # Look at the new variable

# Look at the 95% confidence interval
t.test(gss2010$totalhr)

# Look at the 99% confidence interval
t.test(gss2010$totalhr, conf.level = 0.99)

# -------------------
# Politicians example
# -------------------
table(gss2010$polgreed)
gss2010$newpolgreed <- recode(gss2010$polgreed, 
                              "agree" <- c("strongly agree","agree"),
                              "don't agree" <- c("neither agree nor disagree","disagree","strongly disagree"))
table(gss2010$newpolgreed)
class(gss2010$newpolgreed) # R stores this variable as a factor with an order designated by its levels
levels(gss2010$newpolgreed) # Check the levels and the order of the factor
# This order is important to remember when you're plotting, running an ordinal regression, etc.
# "agree" is in the first position and will be assigned numeric label 1
# "don't agree is in the second position and will be assigned numeric label 2

# Find the confindence interval of the new variable
t.test(gss2010$newpolgreed) # This will give you an error; b/c it's a nominal variable
# Turn it into its numeric label
t.test(as.numeric(gss2010$newpolgreed))

# ------------------------------
# Working hours and race example
# ------------------------------
gss2010$white <- ifelse(gss2010$race == "white", 1, 0)

# T-test of significance of two groups and working hours
t.test(hrs1 ~ white, data = gss2010) # difference in means for both groups not statistically significant

# Women and men self-described as kind people -------------------------------
# Proportions are are tested in the same way as above

# Save the workspace
save.image("gss2010.RData")