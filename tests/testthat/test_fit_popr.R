library(ST403FinalProject)

test_that("fit_popr produces valid data", {
  expect_output(str(fit_popr("Africa", "2022", "1970")), "List of 5")
  expect_error(fit_popr("xxx", "2022", "1970"))
  expect_error(fit_popr("World", "3000", "1970"))
})
