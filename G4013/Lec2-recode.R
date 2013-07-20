# ---------------------------------------------------------
# Title: Customizing the GSS
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4013 Lecture 3B
# ---------------------------------------------------------

# Load the previously saved workspace with GSS 2010 file
load("gss2010.RData")

# There are several ways to recode in R. Here are a few:
summary(gss2010$age) # It's always a good idea to check the distribution before coding a variable

# 1. ifelse <- this is nice to use for coding two categories
gss2010$newage <- ifelse(gss2010$age <= 45, "young", "old") # You'll see that this drops the NAs
# Another cool RStudio tidbit:
# Place your cursor within the parentheses and press [tab]
# You'll see the ordered arguments for each function
ifelse()

# 2. memisc package
install.packages("memisc")
library(memisc)
gss2010$newage2 <- recode(gss2010$age, 
                          "young" <- range(min, 45),
                          "old" <- range(46, max))
# I find the recode package to be the most flexible and easy to use.
# It includes a nice feature called "otherwise" -- check it out in the help files
?recode

# Conditional sum
subset(gss2010, age < 50, select="age") # First, subset the table
sum(subset(gss2010, age < 50, select="age")) # Then sum the whole thing

sum(gss2010$age < 50, na.rm = TRUE) # This is NOT the same thing -- it counts each instance of the first argument
sum(gss2010$age == 50, na.rm = TRUE)

# Similar to STATA, "&" stands in for "and" and "|" stands in for "or"
gss2010$age <= 18|gss2010$age == 30 # Prints out T/F for each row in table
which(gss2010$age <= 18 | gss2010$age == 30) # Adding in which prints the index number
gss2010$age[which(gss2010$age <= 18 | gss2010$age == 30)] # Bracketing the index variables and pre-pending a variable prints out the variable
table(gss2010$age[which(gss2010$age <= 18 | gss2010$age == 30)]) # Tabulate the list
# Cool, amirite?

# Save the table
save.image("gss2010.RData")