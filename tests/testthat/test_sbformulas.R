context("infrastructure")
# 
# td <- tempdir()
# unlink(file.path(td, "R"))

test_that("check ttwp formula", {
  expect_that(ttwp("a","b","c"), throws_error())  
  expect_that(ttwp(mtwci = NA,mtwcii = NA,nomtwp = NA),is_equivalent_to(NA))

})

test_that("check cip_number_check",{
  a <- sbformula::cip_number_check("CIP123456.123")
  b <- sbformula::cip_number_check("CIP123456")
  
  expect_that((a$cipnumber_ok),is_equivalent_to("CIP123456.123"))
  expect_that((b$cipnumber_ok),is_equivalent_to("CIP123456"))
  expect_that(str(a),prints_text("List of 2"))
  expect_that(sbformula::cip_number(),throws_error())
})

