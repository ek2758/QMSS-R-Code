# ---------------------------------------------------------
# Title: Continuous Interaction Variables
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4013 Lecture 9
# ---------------------------------------------------------

# Load the saved workspace with cross-sectional GSS file
load("gssCS.RData")

# Review regression results of wordsum prediction
summary(wordSp.lm)

# Review regression results of wordsum prediction with interaction variable
summary(wordSp2.lm)

# -----------------
# Predicting health
# -----------------
# Reverse order of health variable
levels(gss.cs$health)
gss.cs$rhealth <- factor(gss.cs$health, levels = c("poor","fair","good","excellent"))

health.lm <- lm(as.numeric(rhealth) ~ educ + as.numeric(attend) + age, data = gss.cs)
summary(health.lm)

health2.lm <- update(health.lm, ~ . + educ:as.numeric(attend))
summary(health2.lm)

# ----------------
# Predicting anger
# ----------------
angry.lm <- lm(angry ~ sei, data = gss.cs)
summary(angry.lm)

angry2.lm <- update(angry.lm, ~ . + I(sei^2)) # "I" insulates a mathematic function 
summary(angry2.lm)

# Angry scatterplot :)
library(ggplot2)
ggplot(gss.cs, aes(x = sei, y = angry)) + 
    geom_point(shape = 1) + # scatterplot with open circle; default is solid circle
    geom_smooth(method = "lm", formula = y ~ poly(x, 2)) # fitted quadratic line

# Omitted variable test of angry.lm regression model
library(lmtest)
resettest(angry.lm)

# -------------------------------
# Predicting religious attendance
# -------------------------------
attend.lm <- lm(as.numeric(attend) ~ as.numeric(partyid), data = subset(gss.cs, as.numeric(partyid) <= 7))
summary(attend.lm)

# Graph mean religious attendance for each partyid
require(plyr)
attendMean <- ddply(gss.cs, .(partyid), mean.attend = mean(as.numeric(attend), na.rm = TRUE))
ggplot(subset(attendMean, as.numeric(partyid) <= 7), aes(x = partyid, y = mean.attend)) + 
    geom_bar(stat = "identity")

# Save the workspace
save.image("gssCS.RData")