context("test-nationwide_apprehensions")

setwd(here::here("data/clean"))
load("nationwide_total_apprehensions_1925_2018.rda")
test_data <- nationwide_total_apprehensions_1925_2018

test_that("Years are right", {
  expect_equal(test_data$fiscal_year, 2018:1925)
})


test_that("Apprehensions are right", {
  expect_equal(head(test_data$total_apprehensions), c(404142,
                                                      310531,
                                                      415816,
                                                      337117,
                                                      486651,
                                                      420789))
  expect_equal(tail(test_data$total_apprehensions), c(20880,
                                                      32711,
                                                      23566,
                                                      16393,
                                                      12735,
                                                      22199))


  expect_equal(test_data$total_apprehensions[test_data$fiscal_year %in% 1930:1935],
               rev(c(20880,
                 22276,
                 22735,
                 20949,
                 10319,
                 11016)))
  expect_equal(test_data$total_apprehensions[test_data$fiscal_year %in% 1940:1945],
               rev(c(10492,
                 11294,
                 11784,
                 11175,
                 31175,
                 69164)))
  expect_equal(test_data$total_apprehensions[test_data$fiscal_year %in% 1960:1970],
               rev(c(28966,
                 29384,
                 29897,
                 38861,
                 42879,
                 52422,
                 79610,
                 94778,
                 123519,
                 172391,
                 231116)))
  expect_equal(test_data$total_apprehensions[test_data$fiscal_year %in% 1980:1985],
               rev(c(759420,
                 825290,
                 819919,
                 1105670,
                 1138566,
                 1262435)))
  expect_equal(test_data$total_apprehensions[test_data$fiscal_year %in% 2000:2005],
               rev(c(1676438,
                 1266214,
                 955310,
                 931557,
                 1160395,
                 1189075)))
})
