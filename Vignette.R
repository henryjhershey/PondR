#this script will walk you through how to use and interpret the 
#functions in the PondManagement Package

#First, make up some data to show off the functions

BGtl <- rnorm(100, mean= 150, sd = 10)
LMBtl <- rnorm(100, mean= 290, sd = 10)
REtl <- rnorm(100, mean= 180, sd = 10)

BGwt <- rnorm(100, mean= 70, sd = 10)
LMBwt <- rnorm(100, mean= 300, sd = 10)
REwt <- rnorm(100, mean= 120, sd = 10)

sp=c(rep.int("BG",100),rep.int("LMB",100),rep.int("RE",100))
     
dat = data.frame(sp, tl=c(BGtl,LMBtl,REtl), wt=c(BGwt,LMBwt,REwt), stringsAsFactors = F)
head(dat)
#we now have a dataframe with 300 fish, 100 of each species.


#okay, let's tell R where to find the functions that we want to use on these data
#make sure the PondManagement.R script is saved in your working directory
source("PondManagement.R")


##################################################################################################################
#There are four functions in the package, but really there are two, and their complementary
#plotting functions

#one is for PSDs, and one is for relative weights
#let's take a look at the PSD function

#first, calculate the PSDs (quality) for each species. you can change the type of PSD you want by
#changing the type argument to quality, prefered, memorable, or trophy. In this case, none
#of our fish are prefered length so we'll just stick to quality.

PSD(dat, type="quality")

#looks like the bass are pretty short. Let's see what happens if we change the mean length
#to 350

LMBtl <- rnorm(100, mean= 350, sd = 10)
dat = data.frame(sp, tl=c(BGtl,LMBtl,REtl), wt=c(BGwt,LMBwt,REwt), stringsAsFactors = F)
PSD(dat)

#looks like the bass are no longer short. they are all over quality length.
#what happens if we make them really short?

LMBtl <- rnorm(100, mean= 175, sd = 10)
dat = data.frame(sp, tl=c(BGtl,LMBtl,REtl), wt=c(BGwt,LMBwt,REwt), stringsAsFactors = F)
PSD(dat)

#as you can see, the PSD() function throws an error if you try to calculate PSDs for fish that 
#are smaller than stock length since PSD= (#fish > qual length/ #fish > stock length)
#If the denominator is zero, R says the answer is undefined and calls it NA

#let's make our bass normal length and see if our pond is balanced
LMBtl <- rnorm(100, mean= 300, sd = 10)
dat = data.frame(sp, tl=c(BGtl,LMBtl,REtl), wt=c(BGwt,LMBwt,REwt), stringsAsFactors = F)

#the PSD function prints a dataframe called PSD, but if you want to save it you have to 
#assign it a name
PSD(dat, type="quality")

#the plot.PSD function creates a chart that shows how "balanced" the pond is in terms of PSD
#for bluegill and bass

plot.PSD(dat)

#looks like the pond is pretty balanced! woohoo!

#Are the fish healthy?
#let's calculate some relative weights

dat <- add.Wr(dat)

#we can plot the relative weight against total length to see how healthy the fish are. 
#the plot.wr function makes three plots

par(mfrow=c(2,2))
plot.wr(dat)

#this data is obviously not perfectly simulated (I just assigned each fish a random weight
#from a normal distribution around a mean instead of calculating the weight based on a 
#growth equation). So... the variation in relative weight is not exactly representative
#of a real population, BUT the code works, so try it on your own data.

#make a table of the average weights for each species
write.table(tapply(dat$Wr, dat$sp, mean), file="AveWr.docx")

#make a table of the PSDs of each length for each species
write.table(rbind(PSD(dat, type="quality")[1,],PSD(dat, type="prefered")[1,], PSD(dat, type="memorable")[1,], PSD(dat, type="trophy")[1,]), file="PSDs3")

#you can make these into tables in microsoft word by clicking insert> table> convert text to table
#then you have to make sure that the separator is a space, and it will automatically
#put all the values into table cells.

