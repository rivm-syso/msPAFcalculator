#global.R
rm(list=ls())


#load in the Gross data
#needed for shinyportaal.rivm.nl
#gross_folder <- "/shiny-data/"
#N.B. reset for local dev to:
gross_folder <- "data/"

#what are the gross files to load in
SSDplusList <- readRDS(paste0(gross_folder,"SSDplusList.RDS"))
for (name in names(SSDplusList)) {
  #get the object name, based on the name
  assign(name, SSDplusList[[name]], envir = .GlobalEnv)
}
gross_choices <- ls(pattern = "^Gross")

#get the git head
if (!is.null(attributes(SSDplusList)$githead)) {
  git_head <- attributes(SSDplusList)$githead
  if( grepl(" (HEAD) ",git_head, fixed = TRUE)){
    git_head <- unlist(strsplit(git_head," (HEAD) ",fixed = TRUE))[2]
  }
} else {
  git_head <- NULL
}


#load in all other data
load("data/Modifyers.rda")
load("data/ModifierDefaults.rda")
load("data/UnitConversions.rda")
Textdata <- read.csv2("data/Textfile.csv")
load("data/OtherChar.rda") #translate InchiKEY, AquoCode or alternative CAS to CAS
source("R/HU_Calc2.R")
source("R/HU2msPAFs.R")
source("R/ValidCAS.R")
source("R/leesIMformat.R")
sep <- ";"
dec <- ","