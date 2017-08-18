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

test_that("Check PWL_SPT and PWL_RSPT",{
  FTWRSPT <- rep(NA, 10)
  ITW <- 1:10
  answer <- rep(NA, 10)
  fml_anwer <- sbformula::pct_if(inital_value = ITW, final_value = -FTWRSPT)
  testthat::expect_that(fml_anwer, is_equivalent_to(NA))

})

# test_that("Check biochemical content",{
#   
#   bch <- 1:100
#   dm <- 1:100
#   biochem_cont(biochem = bch, dm = dm)
#   
# })
#   
# test_that("Check NA values for dry matter",{
#   
#   bch <- 1:100
#   dm <- rep(NA, 100)
#   biochem_cont(biochem = bch, dm = dm)
#   
# })


test_that("Check AVDM",{
 
   set.seed(1234)
   dm1 <- rnorm(n = 6,mean = 20,sd = 5)
   dm1[c(1,5)] <- NA
   
   dm2 <- rnorm(n = 6,mean = 20,sd = 5)
   dm2[c(2,4)] <- NA
   
   avdm <- round(c( 17.126300, 21.387146, 21.299973 , 8.271511, 17.614037, 18.769174),digits = 2)
   
   avdm_direct <- sbformula::avdm(dm1 = dm1, dm2 = dm2)
   avdm_direct <- round(avdm_direct,2)
   
   expect_equal(avdm, avdm_direct)
  
})



test_that("Check AVDM with NA",{
  
  #set.seed(1234)
  dm1 <- rep(NA,5)
  dm2 <- rep(NA,5)
  avdm <- c(NaN, NaN, NaN, NaN, NaN)
  avdm_direct <- sbformula::avdm(dm1 = dm1, dm2 = dm2)
  expect_equal(avdm, avdm_direct)
  
})


test_that("Check AVDM with NULL",{
  
  #set.seed(1234)
  dm1 <- NULL
  dm2 <- NULL
  avdm <- NULL
  avdm_direct <- sbformula::avdm(dm1 = dm1, dm2 = dm2)
  expect_equal(avdm, avdm_direct)
  #testthat::expect_that(avdm_result, is_equivalent_to(NA))
  
})



test_that("Check NA values for biochemichal content",{
  
  bch <- rep(NA, 100)
  dm <- 1:100
  biochem_cont(biochem = bch, dm = dm)
  
  expect_that(length(biochem_cont(biochem = bch, dm = dm)), equals(100))
  
  
})