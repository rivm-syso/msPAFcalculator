
test_that(desc = "HU_Calc2 exists", code = {
  expect_that(HU_Calc2, is_a("function"))
})
test_that(desc = "HU_Calc2 priority to nf", code = {
  #select only Fe which has "nf" AND ""
  OnlyFe <- testData$inputData[testData$inputData$ChemCode == "Fe",]
  Result <- HU_Calc2(OnlyFe)
  expect_equal(nrow(Result), 1)
})
test_that(desc = "HU_Calc2 one HU per sample / chemical", code = {
  Result <- HU_Calc2(testData$inputData[1:4,], EnvData = testData$DataSamples)
  expect_equal(nrow(Result), 3) #4 rows, of which 2 are Fe
})
