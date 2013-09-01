# ---------------------------------------------------------
# Title: Multiple Regression 2
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4013 Lecture 7
# ---------------------------------------------------------

# Load the saved workspace with cross-sectional GSS file
load("gssCS.RData")

# You can save table commands to objects to save as part of a workspace
home <- table(gss.cs$dwelown)
home

# ---------------------------------
# Predict vocab from home-ownership
# ---------------------------------
gss.cs$home1 <- ifelse(gss.cs$dwelown=="own or is buying", 1, 0)

wordHome.lm <- lm(wordsum ~ home1, data = gss.cs, !is.na(degree))
summary(wordHome.lm)

# Add degree
wordHomeD.lm <- update(wordHome.lm, ~ . + as.numeric(degree))
summary(wordHomeD.lm)

# ------------------------
# Categorical aggregations
# ------------------------
# Mean wordsum scores by home-ownership and degree
wordMean <- tapply(gss.cs$wordsum, list(gss.cs$home1, gss.cs$degree), mean, na.rm = TRUE)
wordMean

# The data needs to be re-structured; simple transpose is required
wordMean <- t(wordMean)

# Convert object to dataframe b/c plotting only takes dataframes as arguments
wordMean <- as.data.frame(wordMean)

# Change column names b/c not great to begin them with #s
colnames(wordMean) <- c("non.homeowner","homeowner")

# Add row with overall wordsum means per home-owner status
wordMean <- rbind(wordMean, 
                  tapply(gss.cs$wordsum, gss.cs$home1, mean, na.rm = TRUE))

# Change rowname of overall mean to "overall"
rownames(wordMean)[9] <- "overall"

# Now we can calculate the differences between non-homeowners and homeowners
wordMean$diff <- wordMean$homeowner - wordMean$non.homeowner

# Create a column with the rownames
# This makes it easier to call in the plot
wordMean$degree <- rownames(wordMean)

# Remove the non-response degrees
wordMean <- subset(wordMean, !(degree %in% c("iap","dk","na")))

# Change the character degree variable to an ordered factor
class(wordMean$degree)
wordMean$degree <- factor(wordMean$degree, levels = c("overall",
                                                      "lt high school",
                                                      "high school",
                                                      "junior college",
                                                      "bachelor",
                                                      "graduate"),
                          ordered = TRUE)

# ----------
# Bar graphs
# ----------
install.packages("ggplot2") # A graphing package for nicely formatted plots
library(ggplot2)
ggplot(wordMean, aes(x = degree, y = diff, fill = degree)) + geom_bar(stat="identity")

# ---------------------------------
# Visualize 3-dimension scatterplot
# ---------------------------------
prestg.lm <- lm(prestg80 ~ educ + male, data = gss.cs)

# Sample 5 random pairs of educ & male for prestg80 predictions using model
s <- sample(nrow(gss.cs), 5)
prestg80.new <- gss.cs[s,]

# Predict prestg80 values of sample data
prestg80.new$prestg.pred <- predict(prestg.lm, newdata = prestg80.new)

install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(prestg80.new$educ, prestg80.new$prestg.pred, prestg80.new$male)
# 3-d plots are not usually useful -- I haven't used this command until now.

# -------------------------------
# How variables are held constant
# -------------------------------
# Need correlations between variables of interest
prestg <- subset(gss.cs, select = c(as.numeric(prestg80), as.numeric(educ), as.numeric(male)))
prestg <- na.omit(prestg) # pairwise deletion
cor(prestg)

# Need means and standard deviations
sapply(prestg, mean, na.rm = TRUE)
sapply(prestg, sd, na.rm = TRUE)

summary(prestg.lm)

# -------------------------------------------------------------
# For the sake of demonstration: purge education of "male-ness"
# -------------------------------------------------------------
educ.lm <- lm(educ ~ male, data = prestg)
summary(educ.lm)

# Extract residuals from model
prestg$educ.res <- residuals(educ.lm)

# educ.res is educ purged of maleness
head(prestg)

# educ.res and male are essentially not correlated
cor(prestg[,2:4])

# Now, estimate effect of education (purged of maleness) on prestige
educRes.lm <- lm(prestg80 ~ educ.res, data = prestg)
summary(educRes.lm) # 2.38 coefficient for educ.res

summary(prestg.lm) # 2.38 coefficient for educ

# Cool! Bias!

# -------------------------------------------------------------
# For the sake of demonstration: purge "male-ness" of education
# -------------------------------------------------------------
male.lm <- lm(male ~ educ, data = prestg)
summary(male.lm)

# Extract residuals from model
prestg$male.res <- residuals(male.lm)

# male.res is male purged of education
head(prestg)

# male-ness and educ are essentially not correlated
cor(prestg[,c(2:3,5)])

# Now, estimate effect of male-ness (purged of education) on prestige
maleRes.lm <- lm(prestg80 ~ male.res, data = prestg)
summary(maleRes.lm) # 0.37 coefficient for educ.res

summary(prestg.lm) # 0.37 coefficient for male

# Cool! Bias again!

# -----------------------------
# Do trust levels vary by race?
# -----------------------------
# Here, I employ the function, "within" to add multiple variables.
# You may run into a function called "attach" to enables one to "open up" a data frame to work on/from it.
# The problem with "attach" is that you have to "detach."
# Sure, it may be less typing (e.g., no need to do the $variable thing each time),
# but code is easier to follow and less likely to screw up.
# Besides, if you're using RStudio, just press the tab key after typing "gss.cs$"
# You will have a list of available variables in front of you. Niiice.
gss.cs <- within(gss.cs, {
    newtrust <- factor(trust, levels = c("cannot trust","depends","can trust"), ordered = TRUE)
    black <- ifelse(race == "black", 1, 0)
    lnrealinc <- log(realinc)
})

trust.lm <- lm(as.numeric(newtrust) ~ black, data = gss.cs, !is.na(educ) & !is.na(lnrealinc) & !is.na(region))
summary(trust.lm)

# Global F-test
anova(trust.lm)

# Add in more predictors
trust2.lm <- lm(as.numeric(newtrust) ~ black + as.numeric(educ) + lnrealinc + region, data = gss.cs)
summary(trust2.lm)

# Did the addition of region variable add sig info to the model?
trust3.lm <- update(trust2.lm, ~ . - region)

# Partial F-test
anova(trust3.lm, trust2.lm) # Keep region

# ------------
# Collinearity
# ------------
lm(tvhours ~ age + age, data = gss.cs) # 2nd age variable is just dropped

# ------------------
# Heteroskedasticity
# ------------------
tv.lm <- lm(tvhours ~ as.numeric(degree), data = gss.cs)
summary(tv.lm)

install.packages("lmtest")
library(lmtest)
bptest(tv.lm) # Reject! We have heteroskedasticity

# Organize data to draw boxplots
tv <- subset(gss.cs, !is.na(degree) & !is.na(tvhours), select = c("tvhours","degree"))
tv <- cbind(tv, residuals(tv.lm))
colnames(tv)[3] <- "tvhours.res"

# Plot distributions of residuals for each degree category
ggplot(tv, aes(x = degree, y = tvhours.res)) + geom_boxplot()

# ----------------------
# Robust standard errors
# ----------------------
install.packages("sandwich")
library(sandwich)
tv.lmNW <- coeftest(tv.lm, vcov=NeweyWest(tv.lm, prewhite=FALSE))
tv.lmNW

# Histogram of the residuals
tv.res <- as.data.frame(residuals(tv.lm))
colnames(tv.res) <- "residuals"

# Density curve with overlaid normal curve in red
ggplot(tv.res, aes(x = residuals)) + geom_density(binwidth = .5, alpha = .5) + 
    stat_function(fun = dnorm, colour = "red")

# Test residuals for normality
?shapiro.test # only takes numeric vectors between 3 and 5,000 observations
??nortest::sf.test # only takes numeric vectors between 5 and 5,000 observations

install.packages("e1071")
library(e1071)
kurtosis(tv.res$residuals) # woah, highly significant. Definitely not normally distributed

# --------------------------------
# Observe influential observations
# --------------------------------
# Draw series of plots to gauge model fit
par(mfrow = c(2,2))
plot(tv.lm) # lower right-corner plot provides leverage plot
par(mfrow = c(1,1)) # reset plot window to default

# Draw influence plot to observe influential observations
install.packages("car")
library(car)
influencePlot(tv.lm)

# ---------------------
# Run robust regression
# ---------------------
install.packages("MASS")
library(MASS)
tv.rlm <- rlm(tvhours ~ as.numeric(degree), data = gss.cs)
summary(tv.rlm)

# -----------------------
# Run quantile regression
# -----------------------
install.packages("quantreg")
library(quantreg)
tv.rq <- rq(tvhours ~ as.numeric(degree), data = gss.cs)
summary(tv.rq)

# Save the workspace
save.image("gssCS.RData")