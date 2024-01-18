Rootfolder <- "../../.."
#Rootfolder <- "."
# set to Rootfolder <- "." to test this helper
Rfiles <- list.files(paste(Rootfolder, "R/", sep = "/"), pattern = "\\.R$")
sourced <- lapply(Rfiles, function(x) {
  Rfilename <- paste(Rootfolder, "R/", x, sep = "/")
  if (exists("verbose") && verbose) cat(Rfilename, "\n")
  source(Rfilename)
})

Dfiles <- list.files(paste(Rootfolder, "data", sep = "/"), pattern = "\\.rda$")
Rded <- lapply(Dfiles, function(x) {
  Dfilename <- paste(Rootfolder, "data", x, sep = "/")
  if (exists("verbose") && verbose) cat(Dfilename, "\n")
  load(Dfilename, envir = globalenv())
})


#### Test files hebben 308 rows en eindigen als alles correct gaat met 44 rows valide data
# - 1 row verdwijnt vanwege units
# - 253 verdwijnen vanwege < of > teken
# - 10 rijen verdwijnen vanwege ?? nf?


testFile <- paste(Rootfolder, "tests/testthat/testdata/SampleDataMet.csv", sep = "/")
testFileInt <- paste(Rootfolder, "tests/testthat/testdata/SampleDataMetInt.csv", sep = "/")
# pvb For testing of incorrerct units: remove later
testFileUnits <- paste(Rootfolder, "tests/testthat/testdata/SampleDataMet_units.csv", sep= "/")
# For testing double values
testFileDoubles <- paste(Rootfolder, "tests/testthat/testdata/SampleDataMet_doubles.csv", sep= "/")

testData <- leesIMformat(testFile, National = "Nederlands")
testDataInt <- leesIMformat(testFileInt, National = "anders")
testDataIntError <- leesIMformat(testFileInt, National = "Nederlands")
testDataNLError <- leesIMformat(testFile, National = "anders")

# PVB: For testing of incorrect units -- remove later
#pvb: mogelijk units naar lower case converteren zodat bijv. ug/L wel meegenomen wordt. Hier test voor schrijven
testDataNLUnits <- leesIMformat(testFileUnits, National = "Nederlands")
#testing doubles
testDataNLDoubles <- leesIMformat(testFileDoubles, National = "Nederlands")
