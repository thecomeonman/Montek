# rm(list = ls())

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(scales)
library(data.table)
library(rjson)

theme_set(theme_bw(12))
options(shiny.maxRequestSize = 500*1024^2)
lArchitectureParms = list()

# Should point to where you've downloaded the repository
# eg. if it's "C:/Documents/Rhyhorn" then this should be "C:/Documents"
# This is the path used to read other assets like the file needed for 
# dtAllowedOperations
lArchitectureParms$cRootDirectory = Sys.getenv('AtherGitRepo')
lArchitectureParms$cRootDirectory = ifelse(
   lArchitectureParms$cRootDirectory == '',
   '/mnt/disks/vi-data/AtherGit',
   lArchitectureParms$cRootDirectory
)

# should point to some location where you want to save the scenarios on your VM
# this is used in the Cloud saving and loading box. If you're running this only
# locally then you can ignore this.
lArchitectureParms$cAtherDataLocation = Sys.getenv('AtherDataLocation')
lArchitectureParms$cAtherDataLocation = ifelse(
   lArchitectureParms$cAtherDataLocation == '',
   '/mnt/disks/vi-data/Data',
   lArchitectureParms$cAtherDataLocation
)

# The number of variables that a user is allowed to add
# You can increase this, if you wish. This is the number of placeholders for
# variables that get created so a smaller number is more efficient.
iRandomlyLargeNumberForVariables = 299

# The number of empirical distributions that a user is allowed to add
# You can increase this, if you wish. This is the number of placeholders for
# empirical distributions that get created so a smaller number is more 
# efficient.
iRandomlyLargeNumberForEmpirical = 99

# The operations that a user is allowed. It has four columns: 
# Operator, OperatorString, Usage, and Description. Except for OperatorString, 
# the other three are self explanatory and also get used in the Help button 
# entry. OperatorString is the regex pattern that should be searched for in 
# an output equation to parse it and calculate it correctly.
dtAllowedOperations = fread(
   paste0(
      lArchitectureParms$cRootDirectory,
      '/Rhyhorn/Metadata/Operations.csv'
   ),
   sep = ';'
)
# sorting it in order of longer operators to shorter operators to
# avoid mix ups with the string parsing
# Don't edit this.
dtAllowedOperations = dtAllowedOperations[rev(order(nchar(Operator)))]

# This is the file to add custom functions that the user is allowed
# Update operations.csv accordingly
source(
   paste0(
      lArchitectureParms$cRootDirectory,
      '/Rhyhorn/Metadata/Functions.R'
   )
)

# My expectation of a string that will never get encountered in a variable
# name that is set. If you want to use this character then change this
# string to something else which you are unlikely to enounter in you
# variable names.
cTempVariableString = 'Д'

# stores the values of the variables for debugging, downloading, etc
cResultsStorageLocation = paste0(
   tempdir(), '/MCSimStorage/' 
)

cEvaluationOrderLocation = '/tmp/qweqwe.csv' # tempfile()
