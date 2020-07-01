context("test-apprehensions_seizures_stats")

setwd(here::here("data/clean"))
load("apprehensions_seizures_stats_2011_2019.rda")
test_data <- apprehensions_seizures_stats_2011_2019

test_that("Equal number of sectors and years works", {
  expect_length(unique(test_data$fiscal_year), 9)
  expect_length(unique(test_data$sector), 4)
})


test_that("Correct years and sectors", {
  expect_equal(unique(test_data$fiscal_year), 2019:2011)
  expect_equal(unique(test_data$sector), c("coastal border",
                                           "northern border",
                                           "southwest border",
                                           "nationwide total"))
})


test_that("cocaine is correct", {
  expect_equal(test_data$cocaine_pounds[test_data$sector == "coastal border"],
               c(6843, 1701, 3169, 1288, 6884, 20, 709, 5962, 989))
  expect_equal(test_data$cocaine_seizures[test_data$sector == "coastal border"],
               c(72, 58, 33, 31, 18, 8, 29, 24, 19))


  expect_equal(test_data$cocaine_pounds[test_data$sector == "southwest border"],
               c(4826, 4838, 6174, 4183, 4294, 4443, 3910, 5992, 8763))
  expect_equal(test_data$cocaine_seizures[test_data$sector == "southwest border"],
               c(374, 453, 463, 431, 421, 360, 351, 457, 500))


  expect_equal(test_data$cocaine_pounds[test_data$sector == "nationwide total"],
               c(11682, 6550, 9346, 5473, 11220, 4554, 4696, 12161, 9963))
  expect_equal(test_data$cocaine_seizures[test_data$sector == "nationwide total"],
               c(480, 559, 542, 480, 472, 403, 414, 524, 569))


})


test_that("marijuana is correct", {
  expect_equal(test_data$marijuana_pounds[test_data$sector == "coastal border"],
               c(859, 731, 2277, 1232, 1154, 1232, 1262, 661, 2001))
  expect_equal(test_data$marijuana_seizures[test_data$sector == "coastal border"],
               c(107, 99, 113, 90, 39, 24, 63, 40, 133))


  expect_equal(test_data$marijuana_pounds[test_data$sector == "southwest border"],
               c(262903, 458834, 857888, 1292105, 1536499, 1920411, 2428419, 2297662, 2518211))
  expect_equal(test_data$marijuana_seizures[test_data$sector == "southwest border"],
               c(5176, 7505, 9371, 11526, 12338, 13406, 15088, 14025, 15252))


  expect_equal(test_data$marijuana_pounds[test_data$sector == "nationwide total"],
               c(266882, 461030, 861231, 1294052, 1538307, 1922545, 2430123, 2299864, 2529685))
  expect_equal(test_data$marijuana_seizures[test_data$sector == "nationwide total"],
               c(5710, 8041, 9739, 11830, 12535, 13611, 15356, 14396, 15889))


})



test_that("heroin is correct", {
  expect_equal(test_data$heroin_ounces[test_data$sector == "coastal border"],
               c(292, 4, 0, 87, 0, 0, 259, 441, 158))
  expect_equal(test_data$heroin_seizures[test_data$sector == "coastal border"],
               c(7, 3, 0, 1, 0, 0, 1, 6, 2))


  expect_equal(test_data$heroin_ounces[test_data$sector == "southwest border"],
               c(12336, 11302, 15182, 8961, 8237, 9205, 8937, 6383, 6191))
  expect_equal(test_data$heroin_seizures[test_data$sector == "southwest border"],
               c(216, 204, 219, 165, 142, 145, 171, 124, 85))


  expect_equal(test_data$heroin_ounces[test_data$sector == "nationwide total"],
               c(12927, 11314, 15244, 9062, 8282, 9691, 9212, 6873, 6394))
  expect_equal(test_data$heroin_seizures[test_data$sector == "nationwide total"],
               c(246, 238, 248, 188, 179, 182, 192, 153, 108))


})



test_that("firearms is correct", {

  expect_equal(test_data$firearms[test_data$sector == "coastal border"],
               c(14, 17, 9, 10, 8, 9, 19, 28, 3))
  expect_equal(test_data$firearms[test_data$sector == "southwest border"],
               c(299, 314, 369, 346, 316, 475, 524, 577, 523))
  expect_equal(test_data$firearms[test_data$sector == "nationwide total"],
               c(338, 404, 423, 406, 375, 517, 582, 671, 619))

})



test_that("currency is correct", {

  expect_equal(test_data$currency_in_dollars[test_data$sector == "coastal border"],
               c(570145, 131976, 325129, 72351, 2626, 33045, 308361, 280002, 41742))
  expect_equal(test_data$currency_in_dollars[test_data$sector == "southwest border"],
               c(5862399, 6665864, 5169593, 7652390, 4401155, 7351640, 4750598, 5535732, 9849274))
  expect_equal(test_data$currency_in_dollars[test_data$sector == "nationwide total"],
               c(6805696, 7176142, 5869004, 7924537, 4741288, 7991385, 6003336, 7575360, 14791541))
})



test_that("apprehensions is correct", {

  expect_equal(test_data$apprehensions[test_data$sector == "coastal border"],
               c(3585, 3247, 3588, 4663, 668, 3942, 3162, 3685, 6552))
  expect_equal(test_data$apprehensions[test_data$sector == "southwest border"],
               c(851508, 396579, 303916, 408870, 186017, 479371, 414397, 356873, 327577))
  expect_equal(test_data$apprehensions[test_data$sector == "nationwide total"],
               c(859501, 404142, 310531, 415816, 188122, 486651, 420789, 364768, 340252))

})

