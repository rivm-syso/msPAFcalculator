test_that(desc = "leesIMformat exists", code = {
  expect_that(leesIMformat, is_a("function"))
})
# in helper-function:
# testFile <- paste(Rootfolder, "tests/testthat/testdata/SampleDataMet.csv", sep = "/")
# testData <- leesIMformat(testFile)
test_that(desc = "leesIMformat returns", code = {
  expect_equal(names(testData), c("inputData","DataSamples","inputwarnings"))
})
test_that(desc = "nr rows", code = {
  #inpDF <- read.csv2(testFile)
  rowsinpDF <- 44
  expect_equal(nrow(testData$inputData), rowsinpDF)
})
test_that(desc = "nr rows int", code = {
  #inpDF <- read.csv2(testFileInt)
  rowsinpDF <- 44
  expect_equal(nrow(testData$inputData), rowsinpDF)
})

test_that(desc = "unknown units skipped", code = {
  inpfl <- paste(Rootfolder, "tests/testthat/testdata/fileRonMetng.csv", sep = "/")
  redData <- leesIMformat(inpfl, National = "Nederlands")
  ngLines <- which(redData$inputData$Eenheid.code == "ng/l")
  inpData <- read.csv2(inpfl)
  inpLines <- which(inpData$Eenheid.code == "ng/l")
  quo <- inpData$MeasuredValue[inpLines] / redData$inputData$MeasuredValue[ngLines]
  expect_equal(mean(quo), 1000, tolerance=1e-9)
})

#### Fouten bij importeren van verkeerde nationaliteit: Voor nu geen rijen, aanpassen wanneer foutmelding worden toegevoegd

#Testen dat Internationaal bestand bij NL versie tot 0 rijen leidt (import gaat niet door)
test_that(desc = "No import for wrong combination Nationality and file", code = {
  rowsinpDF <-0
  expect_equal(nrow(testDataIntError$inputData), rowsinpDF)
})

#Testen dat NL bestand bij 'anders' nationaliteit input tot 0 rijen leidt (import gaat niet door)
test_that(desc = "No import for wrong combination Nationality and file", code = {
  rowsinpDF <-0
  expect_equal(nrow(testDataNLError$inputData), rowsinpDF)
})


#### Aantal verwijderde rijen d.m.v. "<" of ">" teken
# Mooier is om op daadwerkelijk aantal te testen in de string en niet de gehele string
test_that(desc = "row removal for '<' or '>' sign", code = {
  rowsRemoved <- "253 number of rows removed with Limietsymbool < or >"
  df <- as.data.frame(testDataInt$inputwarnings)
  expect_equal(df[2,2],rowsRemoved)
})

### Testen dat rijen verwijderd worden bij Units die niet overeenkomen
test_that(desc = "row removal for incorrect units", code = {
  unitsExpected <- "' %','ug/L','<a href=\"www.rivm.nl\">asdf</a>','<script>alert(0)</script>',' '  unknown concentration unit(s); row(s) deleted"
  df <- as.data.frame(testDataNLUnits$inputwarnings)
  expect_equal(df[2,2],unitsExpected) # komt error message overeen
  expect_equal(nrow(testDataNLUnits$inputData), 0) # er zouden nul rijen uit moeten komen met de huidige manier van omgaan met Units
})
