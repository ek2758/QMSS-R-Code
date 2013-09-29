# ---------------------------------------------------------
# Title: Predicted probabilities and probits
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4013 Lecture 11
# ---------------------------------------------------------

# Load the saved workspace with cross-sectional GSS file
load("gssCS.RData")

# Let's use the Muslim example
summary(muslim.glm)
table(gss.cs$relig_muslim)

# Hat-tip to the UCLA ATS: http://www.ats.ucla.edu/stat/r/dae/logit.htm

# Let's see how answers change for Muslims as polviews change
muslim.newdata <- with(gss.cs, data.frame(relig_muslim = 1, polviews = 2:8))
muslim.pred <- predict(muslim.glm, newdata = muslim.newdata, type = "response", se = TRUE)

muslim.newdata <- within(muslim.newdata, {
    fit <- muslim.pred$fit
    se.fit <- muslim.pred$se.fit
    pred.prob <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})

# Let's see how answers change for non-Muslims as polviews change
nonmuslim.newdata <- with(gss.cs, data.frame(relig_muslim = 0, polviews = 2:8))
nonmuslim.pred <- predict(muslim.glm, newdata = nonmuslim.newdata, type = "response", se = TRUE)

nonmuslim.newdata <- within(nonmuslim.newdata, {
    fit <- nonmuslim.pred$fit
    se.fit <- nonmuslim.pred$se.fit
    pred.prob <- plogis(fit)
    LL <- plogis(fit - (1.96 * se.fit))
    UL <- plogis(fit + (1.96 * se.fit))
})

# -----------------
# Probit regression
# -----------------
# Predicting Muslims - most positive contribution to country
muslim.glmp <- glm(muslim ~ as.numeric(relig_muslim) + as.numeric(polviews), data = gss.cs,
                  family = binomial(link = probit))
summary(muslim.glmp)

# Compare these summary statistics with the logit
summary(muslim.glm)

# Save the workspace
save.image("gssCS.RData")