library(testthat)
context("test-lib")
library(shinyvsn)
library(dplyr)

test_that("unit.regression", {

  df = shinyvsn::S100.df
  load("refdata_200916.RData")
  r0 = df %>% do(vsn0(.)) %>% do(vsnh(.))
  expect_equal(r0, result0)

  r0.na = df %>% do(vsn0(., normalization = FALSE)) %>% do(vsnh(.))
  expect_equal(r0.na, result0.na)

  dfr = df %>% mutate(RefFactor = factor(Sample.batch))
  rr = dfr %>% do(vsnr(.)) %>% do(vsnh(.))
  expect_equal(rr, resultr)

  expect_error(dfr %>% do(vsnr(., normalization = FALSE)) %>% do(vsnh(.)), "subscript out of bounds")

})


