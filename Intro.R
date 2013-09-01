# ---------------------------------------------------------
# Title: An Introduction to R
# R version:  3.0.0 (2013-04-03) -- "Masked Marvel"
# R Studio version:  0.97.336
# OS: Mac OS X 10.7.5
# Author: Eurry Kim <ek2758@columbia.edu>
# Maintainer: Eurry Kim <ek2758@columbia.edu>
# Description: R Evangelism
# ---------------------------------------------------------

# Why R?
# Can you say open-source? This means that there is an incredibly vibrant social community behind it.
# Why is this good? Think: wisdom of crowds.
# Oh, and it also happens to be a highly marketable skill that will help get you a job. True story.
# Notice how I didn't write "help YOU get a job"; instead, "help get YOU a job."
# But, of course, it always helps if you know someone. :P

# And this guy:
# http://ethanfosse.blogspot.com/2012/05/im-converted-to-r-full-time.html

# Before we get started, I have one piece of advice in two words: search engine.
# You will be Googling/Binging/Duck-duck-going for how to accomplish the many things you will need to do in R. 
# You'll be amazed by the number of people who have previously asked your same question.
# Most of you will find R's help guides unhelpful. I did. It's mostly techie language.
# So get ready to run lots of search queries. For hours. Literally. Learn to love the search.
# You'll likely meet StackOverflow.com -- one of my favored sites for R help.
# Remember: search first, ask professor/TA/whomever second, pull hair out third (JK, kinda)
# If you're into the quantified self movement, turn on your Google search tracking. Results might be interesting.

# These are helpful! Please review these.
# http://cran.r-project.org/doc/contrib/Torfs+Brauer-Short-R-Intro.pdf
# http://www.rstudio.com/ide/docs/

# Some R resources I heart:
# http://www.ats.ucla.edu/stat/r/
# http://flowingdata.com/2012/06/04/resources-for-getting-started-with-r/
# http://www.r-bloggers.com/
# http://blog.revolutionanalytics.com/
# http://www.meetup.com/nyhackr/

# I personally own these:
# O'Reilly R Cookbook: http://shop.oreilly.com/product/9780596809164.do
# O'Reilly R in a Nutsehll: http://shop.oreilly.com/product/0636920022008.do
# O'Reilly Machine Learning for Hackers: http://shop.oreilly.com/product/0636920018483.do
# Professor Gelman's book on multilevel models: http://www.stat.columbia.edu/~gelman/arm/
    
# 1. Download R.
# http://www.rstudio.com/ide/download/desktop
# I use RStudio, a GUI for R. You need to download both to use RStudio.
# Take a gander around the RStudio website -- it contains helpful information on getting started.

# 2. Create a project in RStudio.
# A project acts as a repository for organizing your R code, files, and data.
# I typically make a new one for every class (i.e., G4013, G4014) and I save files within it as, e.g., HW2.
# Nice thing about this is that it helps organize code (ahem -- replication!)
# A project is assigned to a folder by you. This is your "working directory."
# Click on the "Files" tab in the lower-right window. You'll see the contents of your working directory.

getwd() # This prints your current working directory. 
# To run with a mouse-click, click "Run" in code window.
# To run on a Mac, press [command][return]
# To run on a PC, press [control][return]

# You may also set your own working directory. You can figure this one out.

# 3. Play with data. 
# R is great because it is largely data-type agnostic. It likes STATA files, SAS files, CSV files, etc.
# Meet R packages. A la the open source community, R users have written and submitted code to accomplish myriad tasks not already available in base R.

# You only have to do this once unless you upgrade your R version.
install.packages("foreign") # Installs "foreign" package

# Alternatively, you can use the RStudio GUI to install packages. This is what I do.
# Click on "Packages" tab in the lower-right window.
# Click "Install Packages."
# Search for "foreign" -- RStudio will often auto-complete these for you -- nice b/c everything is case-senstive.
# Click "Install" and your Packages list will refresh to include the newly installed package. 

# Load the "foreign" package for use in your current session. 
# You need to load packages in each R session; that is, each time R is fired up.
library(foreign)
require(foreign) # Another method of loading a package into a current session. 

# You can read about the differences between the two by prepending "?" 
?library
?require
# See what I mean about the help pages? Ugh. You'll get quickly acclimated. No worries. 

# Navigate to the foreign package in the Packages tab. Click on the "foreign" link.
# Listed are the functions available in the foreign package.
# read.dta is the function for reading in STATA files.
?read.dta
# Notice the arguments and the default settings in the "Usage" section. 
# You must specify otherwise in the function if you don't want to work with the default settings.

# Save the following zip-file and open in your working directory folder
# The file is called "GSS 2010 merged with all cases and variables (Release 2, April 2012)"
# http://publicdata.norc.org:41000/gss/documents//OTHR/gss2010merged_stata.zip
# The file should be called "gss2010merged_r2b.dta"

# The following commands accomplish the same task: that is, convert STATA file into R file called gss2010
gss2010 <- read.dta(file = "gss2010merged_r2b.dta") # Takes a minute...
gss2010 <- read.dta("gss2010merged_r2b.dta")

# Don't worry about the warning messages for now -- one of many seemingly inscrutable R warning messages you'll see. 
# They're just saying that some values were not assigned labels in STATA.

# The nice thing about R is that when using functions, the arguments don't have to be referenced explicitly.
# That is, 'file="gss2010merged_r2b.dta"' is not necessary; simply "gss2010merged_r2b.dta" is sufficient.
# As long as the order is followed for the arguments in the function, explicit arguments aren't required.
# But perhaps you're wanting readable code -- then explicit arguments are advisable.

# Run this to see all 5548 variables available in the file
colnames(gss2010)

# See variable types of a few variables
class(gss2010$health) # Factor, i.e., categorical variable
class(gss2010$year) # Integer

# You can also reference variable types by their index numbers
class(gss2010[, 73]) # the health variable
class(gss2010[, 1]) # the year variable

# See first 6 lines of the health variable
head(gss2010$health)

# See last 6 lines of the health variable
tail(gss2010$health)

# See 7th line of health variable
gss2010$health[7]

# See 7th line
gss2010[7, ]

# See 7th column
gss2010[ ,7]

# Save this as an R Workspace in your working directory
# This saves all objects in the Workspace!
save.image("gss2010.RData")
# You can also do this by clicking the disk button in the "Workspace" window. 

# Save R code in your working directory
# I like to save my code in a separate folder
# Click the disk icon in the code window to save as specified file name in your working directory.
# I have a sub-folder called "R Code"

# More fun to come!
