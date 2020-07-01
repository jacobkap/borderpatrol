context("test-southwest_border_deaths")

setwd(here::here("data/clean"))
load("southwest_border_deaths_1998_2019.rda")
test_data <- southwest_border_deaths_1998_2019

test_that("Equal number of sectors and years works", {
  expect_equal(as.numeric(table(table(test_data$fiscal_year))), 22)
  expect_equal(as.numeric(table(table(test_data$sector))), 10)
})


test_that("Correct years and sectors", {
  expect_equal(unique(test_data$fiscal_year), 2019:1998)
  expect_equal(unique(test_data$sector), southwest_sectors)
})



test_that("Correct deaths", {
  expect_equal(head(test_data$deaths[test_data$sector == "big bend"]),
               c(3, 10, 3, 2, 4, 5))
  expect_equal(tail(test_data$deaths[test_data$sector == "big bend"]),
               c(0, 4, 3, 3, 0, 3))
  expect_equal(test_data$deaths[test_data$sector == "big bend" &
                                  test_data$fiscal_year %in% 2010:2005],
               c(0, 3, 3, 0, 4, 4))


  expect_equal(head(test_data$deaths[test_data$sector == "el centro"]),
               c(17, 17, 2, 9, 4, 6))
  expect_equal(tail(test_data$deaths[test_data$sector == "el centro"]),
               c(61, 64, 96, 72, 56, 90))
  expect_equal(test_data$deaths[test_data$sector == "el centro" &
                                  test_data$fiscal_year %in% 2010:2005],
               c(14, 27, 20, 12, 21, 30))


  expect_equal(head(test_data$deaths[test_data$sector == "rio grande valley"]),
               c(69, 96, 104, 132, 97, 116))
  expect_equal(tail(test_data$deaths[test_data$sector == "rio grande valley"]),
               c(39, 30, 37, 40, 36, 26))
  expect_equal(test_data$deaths[test_data$sector == "rio grande valley" &
                                  test_data$fiscal_year %in% 2010:2005],
               c(29, 68, 92, 61, 81, 55))




  expect_equal(head(test_data$deaths[test_data$sector == "tucson"]),
               c(61, 58, 73, 84, 63, 107))
  expect_equal(tail(test_data$deaths[test_data$sector == "tucson"]),
               c(137, 134, 80, 74, 29, 11))
  expect_equal(test_data$deaths[test_data$sector == "tucson" &
                                  test_data$fiscal_year %in% 2010:2005],
               c(251, 212, 171, 202, 169, 219))



  expect_equal(head(test_data$deaths[test_data$sector == "southwest border"]),
               c(300, 283, 298, 329, 251, 315))
  expect_equal(tail(test_data$deaths[test_data$sector == "southwest border"]),
               c(338, 320, 340, 380, 249, 263))
  expect_equal(test_data$deaths[test_data$sector == "southwest border" &
                                  test_data$fiscal_year %in% 2010:2005],
               c(365, 420, 385, 398, 454, 492))
})
