context("infrastructure")
# 
# td <- tempdir()
# unlink(file.path(td, "R"))

test_that("check ttwp formula", {
  expect_that(ttwp("a","b","c"), throws_error())  
  expect_that(ttwp(mtwci = NA,mtwcii = NA,nomtwp = NA),is_equivalent_to(NA))
  
  
})
