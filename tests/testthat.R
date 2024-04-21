library(testthat)
context("R test")

add_numbers <- function(a,b){
  a+b
}

test_that("General test add_numbers adds two numbers correctly", {
  result <- add_numbers(2, 3)
  expect_equal(result, 5)
})


context("FARS library small test")
library(fars)
test_check("fars")

test_that( "string test ", {
      tmp <- make_filename(2014)
      expect_that(tmp, is_a("character"))
})

test_that( "generate name check ", {
      tmp <- make_filename(2024)
      expect_equal(tmp, "accident_2024.csv.bz2" )
})

