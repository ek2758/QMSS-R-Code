# ---------------------------------------------------------
# Title: Interaction Variables
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4013 Lecture 8
# ---------------------------------------------------------

# Load the saved workspace with cross-sectional GSS file
load("gssCS.RData")

# It's a good idea to list required packages at the beginning of your code.
# When you or some one else comes along and drops somewhere in the middle of your code,
# they don't have to figure out which packages are missing from errors that would have been otherwise prevented!
library(ggplot2)

# ----------------------------------------
# Predict income from education and gender
# ----------------------------------------
gss.cs$female <- ifelse(gss.cs$sex == "female", 1, 0)

incm.lm <- lm(realrinc ~ educ + female, data = gss.cs)
summary(incm.lm)

# Plot income against educational attainment
ggplot(gss.cs, aes(x = educ, y = realrinc, group = sex, colour = sex)) + geom_point(shape = 1) + 
    geom_smooth(method = "lm")

# Income prediction for men
incmM.lm <- lm(realrinc ~ educ, data = gss.cs, female == 0)
summary(incmM.lm)

# Income prediction for women
incmF.lm <- lm(realrinc ~ educ, data = gss.cs, female == 1)
summary(incmF.lm)

# -----------------
# Interaction model
# -----------------
incm2.lm <- update(incm.lm, ~ . + educ:female)
summary(incm2.lm)

# Another way to write the interaction model
lm(realrinc ~ educ*female, data = gss.cs)
# Personally, I think it is a better idea to explicitly call the individual variables.
# I.e., "educ + female + educ:female" rather than "educ*female"
# It's immediately clear about the substance of the model, but it is less typing ...
# and we live in a free country.

# ---------------------------------------------------------
# Predicting educational attainment with number of siblings
# ---------------------------------------------------------
gss.cs$maBA <- ifelse(gss.cs$madeg %in% c("bachelor","graduate"), 1, 0)

educSibs.lm <- lm(educ ~ sibs + maBA, data = gss.cs)
summary(educSibs.lm)

# Educational attainment prediction for those with mothers w/o Bachelor's
educSibs0.lm <- lm(educ ~ sibs, data = gss.cs, maBA == 0)
summary(educSibs0.lm)

# Educational attainment prediction for those with mothers w/ Bachelor's
educSibs1.lm <- lm(educ ~ sibs, data = gss.cs, maBA == 1)
summary(educSibs1.lm)

# -----------------
# Interaction model
# -----------------
educSibs2.lm <- update(educSibs.lm, ~ . + sibs:maBA)
summary(educSibs2.lm)

# Plot educational attainment against siblings
ggplot(gss.cs, aes(x = sibs, y = as.numeric(educ), group = as.factor(maBA), colour = as.factor(maBA))) + 
    geom_point(shape = 1) + geom_smooth(method = "lm")

# Add more variables
gss.cs$twobio <- ifelse(gss.cs$family16 == "mother & father", 1, 0)

educSibs3.lm <- update(educSibs2.lm, ~ . + age + twobio)
summary(educSibs3.lm)

# --------------------------------------------------
# Predicting religious fundamentalism from education
# --------------------------------------------------
gss.cs <- within(gss.cs, {
    fundamentalist <- ifelse(fund == "fundamentalist", 1, 0)
    evolution <- ifelse(evolved == "true", 1, 0)
})

fund.lm <- lm(evolution ~ fundamentalist + educ, data = gss.cs)
summary(fund.lm)

# Interaction model
fund2.lm <- update(fund.lm, ~ . + educ:fundamentalist)
summary(fund2.lm)

# ------------------------------
# Centering the data at the mean
# ------------------------------
gss.cs$c.educ <- gss.cs$educ - mean(gss.cs$educ, na.rm = TRUE)
summary(gss.cs$c.educ)

fundc.lm <- lm(evolution ~ fundamentalist + c.educ + c.educ:fundamentalist, data = gss.cs)
summary(fundc.lm)

# --------------
# wordsum, again
# --------------
gss.cs$marital1 <- ifelse(gss.cs$marital == "married", 1, 0)

word.lm <- lm(wordsum ~ marital1 + educ, data = gss.cs)
summary(word.lm)

word2.lm <- update(word.lm, ~ . + marital1:educ)
summary(word2.lm)

# --------------------
# wordsum with spouses
# --------------------
wordSp.lm <- lm(wordsum ~ educ + speduc, data = gss.cs)
summary(wordSp.lm)

wordSp2.lm <- update(wordSp.lm, ~ . + speduc:educ)
summary(wordSp2.lm)

# Save the workspace
save.image("gssCS.RData")