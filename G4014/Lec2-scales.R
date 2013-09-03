# ---------------------------------------------------------
# Title: Review and scales
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4014 Lecture 2
# ---------------------------------------------------------

library(foreign)
library(stringr)
library(ltm)
library(QuantPsyc)
library(HH)

# Let's start with the cross-sectional GSS data file
# http://publicdata.norc.org/GSS/DOCUMENTS/OTHR/GSS_stata.zip
gss.cs <- read.dta("GSS7212_R2.DTA") 

# Save variable labels of gss dataset
gss.labels <- data.frame(cbind(var.name = names(gss.cs), attr(gss.cs,"var.labels")))
head(gss.labels)
colnames(gss.labels)[2] <- "var.label"

# There appears some trailing spaces. Let's get ride of those
?str_trim # This is from the stringr package. Get used to this package. "Real" data is unclean. Unclean! o_0
gss.labels$var.name <- str_trim(gss.labels$var.name)
gss.labels$var.label <- str_trim(gss.labels$var.label)

# Reverse some variables
gss.cs$nattend <- factor(gss.cs$attend, levels=rev(levels(gss.cs$attend)))
gss.cs$nrelneg <- factor(gss.cs$relneg, levels=rev(levels(gss.cs$relneg)))

# Print labels of variables of interest
subset(gss.labels, var.name %in% c("reborn","relexp","attend","fund","reliten","relneg","relpersn","pray","punsin"))

# -----------------------------------
# Religious views and political views
# -----------------------------------

# Make a correlation matrix
# First, separate variables of interest into a separate dataframe
relig.num <- subset(gss.cs, select = c("reborn","relexp","nattend","fund","reliten","nrelneg","relpersn","pray","punsin"))
relig.num <- within(relig.num, {
     reborn <- as.numeric(reborn)
     relexp <- as.numeric(relexp)
     nattend <- as.numeric(nattend)
     fund <- as.numeric(fund)
     reliten <- as.numeric(reliten)
     nrelneg <- as.numeric(nrelneg)
     relpersn <- as.numeric(relpersn)
     pray <- as.numeric(pray)
     punsin <- as.numeric(punsin)
})

# Correlation matrix of the religion variables
cor(relig.num, use="complete.obs") # listwise deletion

# Cronbach's alpha of religion variables
?cronbach.alpha # see the default settings
cronbach.alpha(relig.num, na.rm=T) # unstandardized

cronbach.alpha(relig.num, standardized=T, na.rm=T) # standardized

# Regression with all religion variables
polview.lm <- lm(as.numeric(polviews) ~ as.numeric(reborn) + as.numeric(relexp) + 
                     as.numeric(nattend) + as.numeric(fund) + as.numeric(reliten) + 
                     as.numeric(nrelneg) + as.numeric(relpersn) + as.numeric(pray) + as.numeric(punsin),
   data = gss.cs)
summary(polview.lm)

# Beta coefficients of regression variables
lm.beta(polview.lm)

# Variance inflation factors for regression
vif(polview.lm)

# Scale function standardizes variables
relig.num <- cbind(relig.num, (scale(relig.num))) # Attach it to existing dataframe

# Calculate mean of standardized variables, which is our scale
relig.num$alpha <- apply(relig.num[,10:18], 1, mean, na.rm = TRUE) # mean of variables, by row
relig.num$alpha <- -(relig.num$alpha) # reverse

# Polviews prediction using new religion scale
polview2.lm <- lm(as.numeric(gss.cs$polviews) ~ relig.num$alpha)
summary(polview2.lm)
lm.beta(polview2.lm)

# ---------------------
# SES, Cronbach's alpha
# ---------------------
# Unstandardized
cronbach.alpha(cbind(gss.cs$educ, gss.cs$realinc, gss.cs$prestg80), na.rm = T)

# Standardized
cronbach.alpha(cbind(gss.cs$educ, gss.cs$realinc, gss.cs$prestg80), standardized=T, na.rm=T)

# -----------------------------
# Principal components analysis
# -----------------------------

# Social conservative variables?
gss.cs$npornlaw <- factor(gss.cs$pornlaw, levels=rev(levels(gss.cs$pornlaw)))

table(gss.cs$npornlaw)
table(gss.cs$attend)
table(gss.cs$grass)

# Neo-conservative variables?
gss.cs$ntaxrich <- factor(gss.cs$taxrich, levels=rev(levels(gss.cs$taxrich)))
gss.cs$nconarmy <- factor(gss.cs$conarmy, levels=rev(levels(gss.cs$conarmy)))

table(gss.cs$ntaxrich)
table(gss.cs$nconarmy)

# Principal components analysis of variables of interest
con.pca <- prcomp(~ as.numeric(npornlaw) + as.numeric(attend) + as.numeric(grass) + as.numeric(ntaxrich)
                  + as.numeric(nconarmy), data = gss.cs, na.action = na.omit, scale = T)
summary(con.pca)

# Factor loadings from PCA
con.pca$rotation # eigenvectors

