
test_that(desc = "HU2msPAFs exists", code = {
  expect_that(HU_Calc2, is_a("function"))
})
test_that(desc = "HU2msPAFs returns list of which $acute has 1 row", code = {
  #select only Fe which has "nf" AND ""
  OnlyFe <- testData$inputData[testData$inputData$ChemCode == "Fe",]
  ResultHU <- HU_Calc2(OnlyFe)
  Result <- HU2msPAFs(ResultHU)
  expect_equal(nrow(Result$acute), 1)
  # #also for int?
  # testDataInt <- leesIMformat(testFileInt, National = "anders")
  # Result <- HU_Calc2(testDataInt$inputData, EnvData = testDataInt$DataSamples)
  # expect_equal(length(Result),3)
})
test_that(desc = "HU2msPAFs returns one msPAF per sample, for $Acute.All == 0.05567201 ", code = {
  testDataInt <- leesIMformat(testFileInt, National = "anders")
  ResultHU <- HU_Calc2(testDataInt$inputData, EnvData = testDataInt$DataSamples)
  Result <- HU2msPAFs(ResultHU)
  expect_equal(nrow(Result$acute), 1) #1 time-place combination, rows
  expect_equal(Result$acute$Acute.All, 0.05567201)
})
