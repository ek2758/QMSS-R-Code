# ---------------------------------------------------------
# Title: Panel data
# R version:  3.0.2 (2013-04-03) -- "Frisbee Sailing"
# R Studio version:  0.97.551
# OS: Mac OS X 10.8.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: G4014 Lecture 5
# ---------------------------------------------------------

library(foreign)
library(MASS)
library(VGAM)
library(plm)

# The panel data set will be provided: gss_panel06w123_r1-long.dta
# which I renamed to something more simple
gss.panel <- read.dta("gss_panel.dta")

# Make panel variable a factor so it is not treated as a numeric variable
gss.panel$panelwave <- factor(gss.panel$panelwave)

# Are divorced people more likely to think divorce s/b made easier/difficult?
table(gss.panel$divlaw)

# Unlike STATA, R automatically assigns numeric labels to each text label according to your ordering
gss.panel$divorce.easier <- factor(gss.panel$divlaw, levels = c("more difficult","stay same","easier"), 
                                   ordered = T)
table(as.numeric(gss.panel$divorce.easier))

# Create divorced indicator
table(gss.panel$marital)
gss.panel$divorced <- ifelse(gss.panel$marital %in% c("divorced","separated"), 1, 0)

# --------------------------------------------------------------------------------
# Linear regression predicting penchant towards ease of divorce by divorced status
# --------------------------------------------------------------------------------
div.lm <- lm(as.numeric(divorce.easier) ~ divorced, data = gss.panel)
summary(div.lm)

# Create log of income variable
gss.panel$lnrealinc <- log(gss.panel$realinc)

# Add controls to divorce regression
div2.lm <- update(div.lm, ~ . + lnrealinc + educ + as.factor(race) + as.factor(sex) + age)
summary(div2.lm)

# ----------------------------------------------------------------------------
# Ordinal logit predicting penchant towards ease of divorce by divorced status
# ----------------------------------------------------------------------------
div.polr <- polr(divorce.easier ~ divorced, data = gss.panel, Hess=T)
summary(div.polr)

# There's a second function to run an ordinal logit in the VGAM package
div.vglm <- vglm(divorce.easier ~ divorced, data = gss.panel, family = propodds)
summary(div.vglm) # Same coefficients as those of polr function

# But what if you don't assume proportional odds?
div2.vglm <- vglm(divorce.easier ~ divorced, data = gss.panel, family = cumulative(reverse=T))
summary(div2.vglm)

# Test whether the non-proportional odds model is better than proportional odds model
pchisq(deviance(div.vglm) - deviance(div2.vglm),
       df = df.residual(div.vglm) - df.residual(div2.vglm), lower.tail = F)
# Nope

# --------------------------------------------
# Function to create clustered standard errors
# --------------------------------------------
# Lifted from my co-worker, Kevin's blog: http://thetarzan.wordpress.com/2011/06/11/clustered-standard-errors-in-r/
cl <- function(dat,fm, cluster){
    require(sandwich, quietly = TRUE)
    require(lmtest, quietly = TRUE)
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fm$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
    coeftest(fm, vcovCL) }

cl(gss.panel.div, div.lm, gss.panel$idnum)
# Error here because the # observations employed in div.lm != # observations of idnum
sum(!is.na(gss.panel$divorce.easier)) # 3009 obs
sum(!is.na(gss.panel$divorced)) # 6000 obs

# Create another dataset
gss.panel.sub <- subset(gss.panel, !is.na(divorce.easier), 
                        select = c("divorce.easier","divorced","idnum","panelwave"))

div3.lm <- lm(as.numeric(divorce.easier) ~ divorced, data = gss.panel.sub)
cl(gss.panel.sub, div3.lm, gss.panel.sub$idnum)

# -----------------------
# First-differenced model
# -----------------------
div.fd <- plm(as.numeric(divorce.easier) ~ divorced, index = c("idnum","panelwave"), 
              data = gss.panel, model = "fd")
summary(div.fd)
coeftest(div.fd)
coeftest(div.fd, vcov = function(x) vcovHC(x, cluster="group", type="HC1"))

# Save workspace
save.image("gssPanel.RData")
