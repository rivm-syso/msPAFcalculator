rm(list=ls())
# Configuration Setup Explanation:
# We use the 'config' package to manage environment-specific settings 
# This allows us to keep code portable and avoid hard-coding environment-dependent values.
# For more RIVM-information, please see https://rshinydocs.rivm.nl/config.html
# 
# config.yml structure:
#   default:
#     DATA_DIRECTORY: /shiny-data/
#   develop:
#     inherits: default
#     DATA_DIRECTORY: ./data
#
# An example can be found under config_example.yml
# If you want to use it, re-name it to config.yml for the best experience
#
# Load config.yml
# With this, we load in the directory
if(!requireNamespace("config")) { install.packages("config") }
if (file.exists("config.yml")) {
  conf <- config::get()
  gross_folder <- conf$data_directory
} else {
  # If you do not have the config file set-up yet, you can use the config_example.yml
  # For example: copy the config_example.yml to config.yml
  # file.copy("config_example.yml", "config.yml", overwrite = FALSE)
  #
  # Or, by default, use the data folder
  # If you do not have the config-file, you can set the directory with this
  # For local developmeny, the data folder is most useful.
  gross_folder <- "./data"
  # For deployment within RIVM, you can use the folder called shiny-data
  #gross_folder <- "/shiny-data/" 
}


#load in the Gross data

#what are the gross files to load in
SSDplusList <- readRDS(paste0(gross_folder,"/SSDplusList.RDS"))
#for (name in names(SSDplusList)) {
#  #get the object name, based on the name
#  assign(name, SSDplusList[[name]], envir = .GlobalEnv)
#}
#Temporarily use  msPAF::Gross2025
Gross2025 <-  msPAF::Gross2025
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

#get the git head for the app itself
app_git_head <- tryCatch({
  system("git rev-parse --short HEAD", intern = TRUE)
}, error = function(e) {
  NULL
})


#load in all other data
#load("data/Gross.rda")
load("data/Modifyers.rda")
load("data/ModifierDefaults.rda")
load("data/UnitConversions.rda")
Textdata <- read.csv2("data/Textfile.csv")
load("data/OtherChar.rda") #translate InchiKEY, AquoCode or alternative CAS to CAS
#source("R/HU_Calc2.R")
#source("R/HU2msPAFs.R")
#source("R/ValidCAS.R")
#source("R/leesIMformat.R")
sep <- ";"
dec <- ","
#install.packages("devtools")
#devtools::install_github("rivm-syso/msPAF")
#devtools::install_github("rivm-syso/msPAFcalculator")
library(msPAF)
library(dplyr)
library(shiny)
library(openxlsx)
library(DT)