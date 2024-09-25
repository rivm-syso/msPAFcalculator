#global.R
#needed for shinyportaal.rivm.nl
load("/shiny-data/Gross.rda")
#N.B. reset for local dev to:
#load("data/Gross.rda")
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