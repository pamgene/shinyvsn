#refdata
# 200916
rm(list= ls())
library(shinyvsn)
library(testthat)
library(dplyr)
expect_equal(as.character(packageVersion("shinyvsn")), "1.6") # ref set to package version 1.6

df = shinyvsn::S100.df

result0 = df %>% do(vsn0(.)) %>% do(vsnh(.))
result0.na =df %>% do(vsn0(., normalization = FALSE)) %>% do(vsnh(.))

dfr = df %>% mutate(RefFactor = factor(Sample.batch))
resultr = dfr %>% do(vsnr(.)) %>% do(vsnh(.))
expect_error({resultr.na = dfr %>% do(vsnr(., normalization = FALSE)) %>% do(vsnh(.))}, "subscript out of bounds")

save(file = "./tests/testthat/refdata_200916.RData", result0, result0.na, resultr)
