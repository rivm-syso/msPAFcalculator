#global.R
rm(list=ls())

# Load config.yml
# i: https://rshinydocs.rivm.nl/config.html
# With this, we load in the directory
if(!requireNamespace("config")) { install.packages("config") }
if (file.exists("config.yml")) {
  conf <- config::get()
  gross_folder <- conf$data_directory
} else {
  # If you do not have the config-file, you can set the directory with this
  gross_folder <- "data"
  #gross_folder <- "/shiny-data/" #needed for shinyportaal.rivm.nl
  #N.B. reset for local dev to:
  #gross_folder <- "data"
}


#load in the Gross data


#what are the gross files to load in
SSDplusList <- readRDS(paste0(gross_folder,"/SSDplusList.RDS"))
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