context("test-southwest_border_apprehensions")

setwd(here::here("data/clean"))
load("southwest_border_apprehensions_1960_2018.rda")
test_data <- southwest_border_apprehensions_1960_2018

test_that("Equal number of sectors and years works", {
  expect_equal(as.numeric(table(table(test_data$fiscal_year))), 59)
  expect_equal(as.numeric(table(table(test_data$sector))), 10)
})


test_that("Correct years and sectors", {
  expect_equal(unique(test_data$fiscal_year), 2018:1960)
  expect_equal(unique(test_data$sector), southwest_sectors)
})



test_that("Correct apprehensions", {
  expect_equal(head(test_data$total_apprehensions[test_data$sector == "big bend"]),
               c(8045, 6002, 6366, 5031, 4096, 3684))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "big bend"]),
               c(3973, 3146, 2026, 1431, 954, 732))
  expect_equal(test_data$total_apprehensions[test_data$sector == "big bend" &
                                             test_data$fiscal_year %in% 2006:2011],
               rev(c(7520, 5536, 5391, 6360, 5288, 4036)))
  expect_equal(test_data$total_apprehensions[test_data$sector == "big bend" &
                                               test_data$fiscal_year %in% 1990:1995],
               rev(c(7180, 8764, 13819, 15486, 13494, 11552)))



  expect_equal(head(test_data$total_apprehensions[test_data$sector == "el centro"]),
               c(29230, 18633, 19448, 12820, 14511, 16306))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "el centro"]),
               c(5344, 2640, 1690, 1426, 1878, 1839))
  expect_equal(test_data$total_apprehensions[test_data$sector == "el centro" &
                                               test_data$fiscal_year %in% 2006:2011],
               rev(c(61465, 55883, 40961, 33521, 32562, 30191)))
  expect_equal(test_data$total_apprehensions[test_data$sector == "el centro" &
                                               test_data$fiscal_year %in% 1990:1995],
               rev(c(28708, 30450, 29852, 30058, 27654, 37317)))



  expect_equal(head(test_data$total_apprehensions[test_data$sector == "rio grande valley"]),
               c(162262, 137562, 186830, 147257, 256393, 154453))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "rio grande valley"]),
               c(8057, 9173, 9992, 5569, 6713, 5515))
  expect_equal(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                               test_data$fiscal_year %in% 2006:2011],
               rev(c(110528, 73430, 75473, 60989, 59766, 59243)))
  expect_equal(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                               test_data$fiscal_year %in% 1990:1995],
               rev(c(97018, 87319, 85889, 109048, 124251, 169101)))




  expect_equal(head(test_data$total_apprehensions[test_data$sector == "tucson"]),
               c(52172, 38657, 64891, 63397, 87915, 120939))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "tucson"]),
               c(1480, 1200, 1466, 1247, 1178, 1255))
  expect_equal(test_data$total_apprehensions[test_data$sector == "tucson" &
                                               test_data$fiscal_year %in% 2006:2011],
               rev(c(392074, 378239, 317696, 241673, 212202, 123285)))
  expect_equal(test_data$total_apprehensions[test_data$sector == "tucson" &
                                               test_data$fiscal_year %in% 1990:1995],
               rev(c(53061, 59728, 71036, 92639, 139473, 227529)))



  expect_equal(head(test_data$total_apprehensions[test_data$sector == "southwest border"]),
               c(396579, 303916, 408870, 331333, 479371, 414397))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "southwest border"]),
               c(40020, 32519, 29644, 21103, 21745, 21022))
  expect_equal(test_data$total_apprehensions[test_data$sector == "southwest border" &
                                               test_data$fiscal_year %in% 2006:2011],
               rev(c(1071972, 858638, 705005, 540865, 447731, 327577)))
  expect_equal(test_data$total_apprehensions[test_data$sector == "southwest border" &
                                               test_data$fiscal_year %in% 1990:1995],
               rev(c(1049321, 1077876, 1145574, 1212886, 979101, 1271390)))
})
