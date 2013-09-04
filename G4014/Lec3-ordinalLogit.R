# ---------------------------------------------------------
# Title: Ordinal logit
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4014 Lecture 3
# ---------------------------------------------------------

load("gssCS.RData")

# ---------------------------------------------------------
# Predict support for more government spending on childcare
# ---------------------------------------------------------

# Reverse natchld variable
gss.cs$childcare <- factor(gss.cs$natchld, levels=rev(levels(gss.cs$natchld)))

table(gss.cs$childcare)

# Generate log of income variable
gss.cs$lnrealinc <- log(gss.cs$realinc)

# Make dummy marital status variable
gss.cs$married <- ifelse(gss.cs$marital=="married",1,0)

# Ordinal logit
child.polr <- polr(childcare ~ childs + age + married + lnrealinc + as.numeric(polviews), 
                   data = gss.cs, Hess=T)
summary(child.polr)

# Odds ratios
exp(coef(child.polr))

# Collapse childcare categories
summary(gss.cs$childcare)
gss.cs$childcare.rightLess <- recode(gss.cs$childcare,
                                     1 <- "too much",
                                     0 <- c("about right","too little"))
table(gss.cs$childcare.rightLess)

# Logit to predict log-odds of being in higher category
child.glm <- glm(childcare.rightLess ~ childs + age + married + lnrealinc + as.numeric(polviews),
                 data = gss.cs, family = binomial(link = logit))
summary(child.glm)