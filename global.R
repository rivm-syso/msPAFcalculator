#global.R
rm(list=ls())


#load in the Gross data
#needed for shinyportaal.rivm.nl
##load("/shiny-data/Gross.rda")
#gross_folder <- "/shiny-data/"
#N.B. reset for local dev to:
##load("data/Gross.rda")
gross_folder <- "data/"

#what are the gross files to load in
gross_files <- list.files(gross_folder, pattern = "^Gross.*\\.rda$", full.names = TRUE)
#loop over the files
for (file in gross_files) {
  #get the object name, based on the file name
  obj_name <- tools::file_path_sans_ext(basename(file))
  #load temporarily in a different envirnment, otherwise it will overwrite 'Gross'
  e <- new.env()
  load(file, envir = e)
  #assign to the global anvirnment with the orrect name
  assign(obj_name, e[[ls(e)]])
}
gross_choices <- ls(pattern = "^Gross")

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