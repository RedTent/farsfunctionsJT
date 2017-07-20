library(testthat)
library(farsfunctionsJT)

test_check("farsfunctionsJT")
test_that(desc="My test", expect_is(make_filename(2015),class="character"))
