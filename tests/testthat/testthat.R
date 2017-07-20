library(testthat)
library(farsfunctionsJT)

#test_check("farsfunctionsJT")

# test to garantee that the produced filename is of class character
test_that(desc="My test", expect_is(make_filename(2015),class="character"))
