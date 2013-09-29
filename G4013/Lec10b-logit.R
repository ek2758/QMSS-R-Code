# ---------------------------------------------------------
# Title: Linear probability model and logits
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4013 Lecture 10
# ---------------------------------------------------------
# It's a good idea to load packages up front to enable you to run code from anywhere w/o errors
# You may need to install some of these packages if you haven't done so already.
library(memisc)
library(ggplot2)
library(lmtest)
library(foreach)
library(plyr)

# Load the saved workspace with cross-sectional GSS file
load("gssCS.RData")

# Tabulate GSS question: Muslims - positive contribution to country?
summary(gss.cs$contmslm)

# Tabulate religion
summary(gss.cs$relig)

gss.cs <- within(gss.cs, {
    muslim <- recode(contmslm, 
                     "not most" <- c("important contribution","some contribution","little positive contribution"), 
                     "most" <- "most important contribution")
    relig_muslim <- recode(relig,
                           "not muslim" <- c("protestant","catholic","jewish","none","other","buddhism",
                                             "hinduism","other easter","orthodox-christian","christian",
                                             "native american","inter-nondenominational"),
                           "muslim" <- "moslem/islam")
})

# -------------------------
# Linear probability models
# -------------------------
muslim.lm <- lm(as.numeric(muslim) ~ as.numeric(relig_muslim), data = gss.cs)
summary(muslim.lm)

# With polviews
muslim2.lm <- update(muslim.lm, ~ . + as.numeric(polviews))
summary(muslim2.lm)

# Don't have data for 13-17 year-olds and romantic relationships
# Will continute with Muslim example

# -----------------
# Cross-tabulations
# -----------------
table(gss.cs$muslim, gss.cs$relig_muslim)

table(gss.cs$muslim, gss.cs$polviews)

# Normality of errors assumption is violated
qplot(muslim2.lm$residuals, geom='blank') + geom_histogram(aes(y = ..density..)) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(muslim2.lm$residuals, na.rm = TRUE), 
                              sd = sd(muslim2.lm$residuals, na.rm = TRUE)), 
                  color = "red")

# Fitted values can be outside the range of 1-2
# The range is 1-2 b/c it's how we recoded the response variable
table(as.numeric(gss.cs$muslim))

range(muslim2.lm$fitted) # We're actually fine here

# Homoskedasticity if violated
bptest(muslim2.lm)

# Variance by category
muslim.res <- data.frame(cbind(muslim2.lm$model), muslim2.lm$residuals)

var(muslim2.lm$residuals)
tapply(muslim.res$muslim2.lm.residuals, muslim.res$as.numeric.relig_muslim., var)
tapply(muslim.res$muslim2.lm.residuals, muslim.res$as.numeric.polviews., var)

# ----------------
# Logit regression
# ----------------

# Start with a proportion
prop.table(table(gss.cs$muslim))

# Predicting Muslims - most positive contribution to country
muslim.glm <- glm(muslim ~ as.numeric(relig_muslim) + as.numeric(polviews), data = gss.cs,
                  family = binomial(link = logit))
summary(muslim.glm)

# Odds ratios of logit model
exp(coef(muslim.glm))
# Being a Muslim increases one's odds of selecting Muslims as 
# having made most positive contribution to country
# by a factor of 7.33, net of political views

# With each step towards being more conservative, one's odds of selecting Muslims as
# having made most positive contribution to country
# decreases by (0.80 - 1) 20%, net of religion

# --------------------
# Daughters on partyid
# --------------------

# Make a function to recode variables easily
dummy <- function(x) {
    recode(as.numeric(x),
           0 <- 2,
           1 <- 3)
}

gss.cs <- within(gss.cs, {
    nkdsex1 <- dummy(kdsex1)
    nkdsex2 <- dummy(kdsex2)
    nkdsex3 <- dummy(kdsex3)
    nkdsex4 <- dummy(kdsex4)
    nkdsex5 <- dummy(kdsex5)
    nkdsex6 <- dummy(kdsex6)
    nkdsex7 <- dummy(kdsex7)
    nkdsex8 <- dummy(kdsex8)
    nkdsex9 <- dummy(kdsex9)
})

# Calculate mean of all nkdsex variables for each row
prgirls <- foreach(i=1:nrow(gss.cs)) %do% 
    mean(c(gss.cs$nkdsex1[i], gss.cs$nkdsex2[i], gss.cs$nkdsex3[i], 
           gss.cs$nkdsex4[i], gss.cs$nkdsex5[i], gss.cs$nkdsex6[i], 
           gss.cs$nkdsex7[i], gss.cs$nkdsex8[i], gss.cs$nkdsex9[i]), na.rm = TRUE)
prgirls <- ldply(prgirls) # Convert list to dataframe
gss.cs <- cbind(gss.cs, prgirls)
colnames(gss.cs)[ncol(gss.cs)] <- "prgirls"

summary(prgirls) # Summary statistics
sum(!is.na(prgirls)) # Number of non-nulls

# Recode to create republican dummy variable
gss.cs$rep <- ifelse(gss.cs$partyid %in% c("ind,near rep","not str republican","strong republican"),1,0)

# Linear probability model
girls.lm <- lm(rep ~ prgirls, data = gss.cs)
summary(girls.lm)

# Add controls
girls2.lm <- update(girls.lm, ~ . + childs + marital + race + fund + realinc + sex)
summary(girls2.lm)

# Save the workspace
save.image("gssCS.RData")