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

# -------------------------------------------------
# Continuous by continuous interactions, visualized
# -------------------------------------------------
speduc.mean <- mean(gss.cs$speduc, na.rm = TRUE) # Mean
speduc.sd <- sd(gss.cs$speduc, na.rm = TRUE) # SD

# Re-center variables holding constant speduc at both high and low levels 
gss.cs$speduc.high <- gss.cs$speduc - (speduc.mean + speduc.sd) 
gss.cs$speduc.low <- gss.cs$speduc - (speduc.mean - speduc.sd)

# Predict wordsum with educ keeping speduc constant at high value
wordSp.high.lm <- lm(wordsum ~ educ + speduc.high + speduc.high:educ, data = gss.cs)
summary(wordSp.high.lm)

# Predict wordsum with educ keeping speduc constant at low value
wordSp.low.lm <- lm(wordsum ~ educ + speduc.low + speduc.low:educ, data = gss.cs)
summary(wordSp.low.lm)

# Store intercept and slope values for easy plotting
wordSp.coefs <- data.frame(i = c(coef(wordSp.low.lm)[1], coef(wordSp.high.lm)[1]),
                           s = c(coef(wordSp.low.lm)[2], coef(wordSp.high.lm)[2]))

# Plot results -- it takes a while...
library(ggplot2)
theme_set(theme_bw(base_family="Helvetica")) # Changes graph settings. I like this simple version.

p <- ggplot(data = gss.cs, aes(x = educ, y = wordsum)) + geom_point(shape = 1)
p + geom_abline(data = wordSp.coefs, aes(intercept = i, slope = s, color = factor(i))) +
    annotate("text", label = "speduc @ +1sd", x = 10, y = 7.75) +
    annotate("text", label = "speduc @ -1sd", x = 10, y = 3.75)
# The resources in the ggplot2 are package rich.
# Check them out here: http://docs.ggplot2.org/current/

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
ggplot(gss.cs, aes(x = sei, y = angry)) + 
    geom_point(shape = 1) + # scatterplot with open circle; default is solid circle
    geom_smooth(method = "lm", formula = y ~ poly(x, 2)) # fitted quadratic line

# Omitted variable test of angry.lm regression model
library(lmtest)
resettest(angry.lm, power = 2:4)

# -------------------------------
# Predicting religious attendance
# -------------------------------
attend.lm <- lm(as.numeric(attend) ~ as.numeric(partyid), data = subset(gss.cs, as.numeric(partyid) <= 7))
summary(attend.lm)

# Graph mean religious attendance for each partyid
attendMean <- tapply(as.numeric(gss.cs$attend), as.numeric(gss.cs$partyid), mean, na.rm = TRUE)
attendMean <- data.frame(cbind(levels(gss.cs$partyid)[1:8], attendMean))
head(attendMean)
colnames(attendMean) <- c("partyid","mean.attend")
attendMean$partyid <- factor(attendMean$partyid, levels = levels(gss.cs$partyid)[1:8])
attendMean$mean.attend <- as.numeric(as.character(attendMean$mean.attend))

ggplot(subset(attendMean, as.numeric(partyid)<=7), aes(x = partyid, y = mean.attend, fill = partyid)) + 
    geom_bar(stat = "identity") + ylim(0, 6)

# Omitted variable test of attend.lm regression model
resettest(attend.lm, power = 2:4)

# Predict attendance with higher power partyid
attend2.lm <- update(attend.lm, ~ . + I(as.numeric(partyid)^2)) # "I" insulates a mathematic function 
summary(attend2.lm)

# Attend scatterplot
ggplot(gss.cs, aes(x = as.numeric(partyid), y = as.numeric(attend))) + 
    geom_point(shape = 1) + # scatterplot with open circle; default is solid circle
    geom_smooth(method = "lm", formula = y ~ poly(x, 2)) # fitted quadratic line

# -------------------
# Log transformations
# -------------------
# Apologies ahead of time -- these graphs take a while. ggplot is pretty slow. 

# Histogram of realrinc
ggplot(gss.cs, aes(x = realrinc)) + geom_histogram(aes(y = ..density..)) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(gss.cs$realrinc, na.rm = TRUE), 
                              sd = sd(gss.cs$realrinc, na.rm = TRUE)), 
                  color = "red")

# Histogram of log realrinc
ggplot(gss.cs, aes(x = log(realrinc))) + geom_histogram(aes(y = ..density..)) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(log(gss.cs$realrinc), na.rm = TRUE), 
                              sd = sd(log(gss.cs$realrinc), na.rm = TRUE)), 
                  color = "red")

# -------------
# Log-log model
# -------------
gss.cs <- within(gss.cs, {
    ln.realrinc <- log(realrinc)
    ln.hrs1 <- log(hrs1)
})
summary(gss.cs$ln.hrs1) # log of 0 is not defined -- we're going to drop these from the regression. No choice.

incmHrs.lm <- lm(ln.realrinc ~ ln.hrs1 + sex, data = subset(gss.cs, hrs1 > 0))
summary(incmHrs.lm)

# ---------------
# Level-log model
# ---------------
summary(gss.cs$wrkstat)

library(memisc) # Package for recoding variables
gss.cs <- within(gss.cs, {
    ln.tvhours <- log(tvhours)
    new.wrkstat <- recode(wrkstat,
                          "working" <- c("working fulltime","working parttime"),
                          "not working" <- c("temp not working","unempl, laid off",
                                             "reitred","school","keeping house","other"))
})

# Check this out
class(gss.cs$new.wrkstat) # wrkstat was automatically made into a factor variable
summary(gss.cs$new.wrkstat) # with numeric labels assigned in the order in which they were re-coded. Nice. STATA doesn't do that.

wordTV.lm <- lm(wordsum ~ ln.tvhours + as.numeric(new.wrkstat), data = subset(gss.cs, tvhours > 0))
summary(wordTV.lm)

# --------------------
# Making pretty tables
# --------------------
# IMHO, LaTeX is a winner when it comes to writing technical papers or producing regression output.
# It's a language all it's own, so it's another learning curve to climb.
# But you won't regret it.
# The following are incomplete instructions, which behooves you to provide me with some feedback. Thanks.

# LaTeX install instructions for both Mac and Windows
# http://guides.macrumors.com/Installing_LaTeX_on_a_Mac
# http://miktex.org/download 

# Verify that the LaTeX installation is also in your PATH with the following instructions
# http://www.tech-recipes.com/rx/2621/os_x_change_path_environment_variable/
# http://www.computerhope.com/issues/ch000549.htm

# And install the knitr package.
install.packages("knitr")
library(knitr)

# And install the texreg package.
install.packages("texreg")
library(texreg)

texreg(list(wordSp2.lm, wordTV.lm),
       caption = "WordSum Score Regressions", dcolumn = FALSE,
       custom.coef.names = c("(Intercept)", "Education","Spouse's education","Education * Spouse's education",
                             "Log, tv hours","Work status"),
       custom.model.names = c("Interaction","Log"))

# Start a Sweave document and paste the output between "begin/end document"

# Save the workspace
save.image("gssCS.RData")