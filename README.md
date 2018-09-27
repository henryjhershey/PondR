# PondR
An R package for the analysis of data commonly gathered for management of small impoundments.

#Pond Management Package 1.0

##Author: Henry Hershey
##henryjhershey@gmail.com

###IMPORTANT NOTES ABOUT THIS PACKAGE
###This package is designed to do calculations, create graphs, and summary tables that would
#be useful to a pond manager. It includes analyses for largemouth bass, bluegill, 
#and redear sunfish, three sportfish species commonly found in small impoundments.
#Future versions of this package may include more species and more analyses.

###The dataset that you pass to these functions must have these column titles
#     COLUMN TITLES:

#     sp    tl    wt


###All units must be in metric.

###The species column must use standard species codes, but can vary a little bit.
#the package recognizes the following species codes:

#Largemouth Bass species codes:
#LMB lmb 

#Bluegill species codes:
#BLG blg BG bg blue

#Redear Sunfish species codes
#RES res RE re resu
