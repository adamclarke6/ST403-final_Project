library(ST403FinalProject)

test_that("tests the DistFunction",{
  expect_error(convertDist(100,"cc","ft"))
  expect_no_error(convertDist(212,"meter","yards"))
  expect_no_error(convertDist(82,"miles","mm"))
})
