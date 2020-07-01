context("test-other_than_mexico_apprehensions")

setwd(here::here("data/clean"))
load("other_than_mexico_apprehensions_2000_2019.rda")
test_data <- other_than_mexico_apprehensions_2000_2019

test_that("Equal number of sectors and years works", {
  expect_equal(as.numeric(table(table(test_data$fiscal_year))), 20)
  expect_equal(as.numeric(table(table(test_data$sector))), 25)
})


test_that("Correct years and sectors", {
  expect_equal(unique(test_data$fiscal_year), 2019:2000)
  expect_true(all(unique(test_data$sector) %in% c(all_borders,
                                              "livermore")))
})



test_that("Correct apprehensions", {
  expect_equal(head(test_data$total_apprehensions[test_data$sector == "big bend"]),
               c(9637, 8045, 6002, 6366, 5031, 4096))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "big bend"]),
               c(10536, 10530, 10319, 11392, 12087, 13689))
  expect_equal(head(test_data$mexican_apprehensions[test_data$sector == "big bend"]),
               c(3431, 3411, 2656, 2728, 2177, 3164))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "big bend"]),
               c(9568, 9736, 9477, 10321, 11107, 12851))
  expect_equal(head(test_data$other_than_mexican_apprehensions[test_data$sector == "big bend"]),
               c(6206, 4634, 3346, 3638, 2854, 932))
  expect_equal(tail(test_data$other_than_mexican_apprehensions[test_data$sector == "big bend"]),
               c(968, 794, 842, 1071, 980, 838))


  expect_true(is.na(unique(head(test_data$total_apprehensions[test_data$sector == "livermore"]))))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "livermore"]),
               c(114, 1850, 3565, 4371, 5211, 6205))
  expect_true(is.na(unique(head(test_data$mexican_apprehensions[test_data$sector == "livermore"]))))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "livermore"]),
               c(106, 1753, 3382, 4186, 5007, 6034))
  expect_true(is.na(unique(head(test_data$other_than_mexican_apprehensions[test_data$sector == "livermore"]))))
  expect_equal(tail(test_data$other_than_mexican_apprehensions[test_data$sector == "livermore"]),
               c(8, 97, 183, 185, 204, 171))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "rio grande valley"]),
               c(339135, 162262, 137562, 186830, 147257, 256393))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "rio grande valley"]),
               c(134186, 92947, 77749, 89927, 107844, 133243))
  expect_equal(head(test_data$mexican_apprehensions[test_data$sector == "rio grande valley"]),
               c(29840, 31453, 29653, 46334, 48173, 63468))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "rio grande valley"]),
               c(54332, 66521, 62830, 78588, 95286, 122501))
  expect_equal(head(test_data$other_than_mexican_apprehensions[test_data$sector == "rio grande valley"]),
               c(309295, 130809, 107909, 140496, 99084, 192925))
  expect_equal(tail(test_data$other_than_mexican_apprehensions[test_data$sector == "rio grande valley"]),
               c(79854, 26426, 14919, 11339, 12557, 10742))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "northern border"]),
               c(4408, 4316, 3027, 2283, 2626, 3338))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "northern border"]),
               c(7343, 9959, 10157, 10487, 12338, 12108))
  expect_equal(head(test_data$mexican_apprehensions[test_data$sector == "northern border"]),
               c(2101, 2245, 1489, 1169, 1437, 1665))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "northern border"]),
               c(4081, 5495, 5947, 6095, 7444, 7362))
  expect_equal(head(test_data$other_than_mexican_apprehensions[test_data$sector == "northern border"]),
               c(2307, 2071, 1538, 1114, 1189, 1673))
  expect_equal(tail(test_data$other_than_mexican_apprehensions[test_data$sector == "northern border"]),
               c(3262, 4464, 4210, 4392, 4894, 4746))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "blaine"]),
               c(524, 359, 288, 271, 282, 272))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "blaine"]),
               c(1001, 1354, 1380, 1732, 2089, 2581))
  expect_equal(head(test_data$mexican_apprehensions[test_data$sector == "blaine"]),
               c(144, 70, 51, 40, 67, 58))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "blaine"]),
               c(506, 759, 754, 1059, 1214, 1560))
  expect_equal(head(test_data$other_than_mexican_apprehensions[test_data$sector == "blaine"]),
               c(380, 289, 237, 231, 215, 214))
  expect_equal(tail(test_data$other_than_mexican_apprehensions[test_data$sector == "blaine"]),
               c(495, 595, 626, 673, 875, 1021))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "swanton"]),
               c(1056, 736, 449, 291, 341, 506))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "swanton"]),
               c(1935, 2701, 1955, 1736, 2463, 1957))
  expect_equal(head(test_data$mexican_apprehensions[test_data$sector == "swanton"]),
               c(336, 173, 90, 23, 43, 75))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "swanton"]),
               c(391, 457, 250, 168, 148, 102))
  expect_equal(head(test_data$other_than_mexican_apprehensions[test_data$sector == "swanton"]),
               c(720, 563, 359, 268, 298, 431))
  expect_equal(tail(test_data$other_than_mexican_apprehensions[test_data$sector == "swanton"]),
               c(1544, 2244, 1705, 1568, 2315, 1855))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "nationwide total"]),
               c(859501, 404142, 310531, 415816, 337117, 486651))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "nationwide total"]),
               c(1189075, 1160395, 931557, 955310, 1266214, 1676438))
  expect_equal(head(test_data$mexican_apprehensions[test_data$sector == "nationwide total"]),
               c(169536, 155452, 130454, 192969, 188122, 229178))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "nationwide total"]),
               c(1023905, 1085006, 882012, 917993, 1224046, 1636883))
  expect_equal(head(test_data$other_than_mexican_apprehensions[test_data$sector == "nationwide total"]),
               c(689965, 248690, 180077, 222847, 148995, 257473))
  expect_equal(tail(test_data$other_than_mexican_apprehensions[test_data$sector == "nationwide total"]),
               c(165170, 75389, 49545, 37317, 42167, 39555))


})
