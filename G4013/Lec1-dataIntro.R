# ---------------------------------------------------------
# Title: Exploring the GSS
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4013 Lecture 2
# ---------------------------------------------------------

# Navigate to your project folder - I call this one G4013
getwd()
# Check out the Files tab and verify the contenst include the file "gss2010.RData"

# Load the previously saved workspace with GSS 2010 file
load("gss2010.RData")

# ----------------
# Inspect the data
# ----------------
summary(gss2010$health) # gss2010 is the table; health is the column/variable
# The nifty thing about RStudio is that there exists an auto-completion feature.
# Type "gss" and [tab] -- you should now have "gss2010"
# Add a "$" after "gss2010" and [tab] -- you should have a list of available variables. Nifty, eh?

table(gss2010$health) # Similar command, but automatically leaves out the NAs

length(gss2010$health) # Number of non-null observations, but non-null doesn't account for ""
is.na(gss2010$health) # Prints binary T/F whether variable is null; T=1, F=0
sum(is.na(gss2010$health)) # Summing this counts the number of null observations
sum(!is.na(gss2010$health)) # Summing this counts the number of non-null observations

class(gss2010$health) # Type of variable

table(as.numeric(gss2010$health)) # Tabulation by numeric labels
table(gss2010$health) # Tabulation by character labels
# Unless I have an intimidating amount of re-coding on my hands, I don't typically employ numeric labels.
# I prefer to code directly with the text labels; they're explicitly meaningful

# ------------------
# Describe your data
# ------------------
table(gss2010$happy) # Tabluate 

sum(!is.na(gss2010$sibs)) # Non-null observations
mean(gss2010$sibs, na.rm = TRUE) # Mean, not counting null values
sd(gss2010$sibs, na.rm = TRUE) # Standard deviation, not counting null values
min(gss2010$sibs, na.rm = TRUE) # Minimum, not counting null values
max(gss2010$sibs, na.rm = TRUE) # Maximum, not counting null values

summary(gss2010$sibs) # Some descriptive statistics of sibs variable

# -----------------
# Analyze your data
# -----------------
# Unless you upgrade your R version, you only need to install packages once.
install.packages("plyr") # One of my favorite packages for summarizing data
# Packages are like modules in STATA. Think of the "findit" feature. 

# Load the package.
library(plyr) # This needs to be done for each R session.
# Nifty package for splitting and aggregating data.

# Some descriptive statistics for siblings variable
summary(gss2010$sibs)
quantile(gss2010$sibs, na.rm = TRUE) 
# You can pull out single statistics from these commands by using the index number.
quantile(gss2010$sibs, na.rm = TRUE)[1]
quantile(gss2010$sibs, na.rm = TRUE)[4]

# Mean siblings per region
ddply(gss2010, .(region), summarise, 
      meanSibs = mean(sibs, na.rm = TRUE))
# Dig into the functions by this package. I use this package all. the. time.

# Additional statistics on siblings per region
ddply(gss2010, .(region), summarise, 
      sdSibs = sd(sibs, na.rm = TRUE),
      quartile25 = quantile(sibs, na.rm = TRUE)[2],
      quartile75 = quantile(sibs, na.rm = TRUE)[4],
      iqrSibs = quantile(sibs, na.rm = TRUE)[4]-quantile(sibs, na.rm=TRUE)[2])

# Cross-tabulation of counts contained in two nominal/ordinal variables, two ways:
table(gss2010$happy, gss2010$health)
ddply(gss2010, .(happy, health), summarise, N = length(id))

# Save the table
save.image("gss2010.RData")