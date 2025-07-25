library(nlmeU)
data(armd.wide)
dtf <- armd.wide[ , c("visual12", "visual24", "visual52")]
res <-  missPat(dtf, symbols = c("?","+"))
print(head(res))

library(testthat)
# expect_that(res[1], equals(c("1" = "+??")))