context("test-other_than_mexico_apprehensions")

setwd(here::here("data/clean"))
load("other_than_mexico_apprehensions_2000_2018.rda")
test_data <- other_than_mexico_apprehensions_2000_2018

test_that("Equal number of sectors and years works", {
  expect_equal(as.numeric(table(table(test_data$fiscal_year))), 19)
  expect_equal(as.numeric(table(table(test_data$sector))), 25)
})


test_that("Correct years and sectors", {
  expect_equal(unique(test_data$fiscal_year), 2018:2000)
  expect_true(all(unique(test_data$sector) %in% c(all_borders,
                                              "livermore")))
})



test_that("Correct apprehensions", {
  expect_equal(head(test_data$total_apprehensions[test_data$sector == "big bend"]),
               c(8045, 6002, 6366, 5031, 4096, 3684))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "big bend"]),
               c(10536, 10530, 10319, 11392, 12087, 13689))
  expect_equal(head(test_data$mexican_apprehensions[test_data$sector == "big bend"]),
               c(3411, 2656, 2728, 2177, 3164, 3174))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "big bend"]),
               c(9568, 9736, 9477, 10321, 11107, 12851))
  expect_equal(head(test_data$other_than_mexican_apprehensions[test_data$sector == "big bend"]),
               c(4634, 6002, 3638, 2854, 932, 510))
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
               c(162262, 137562, 186830, 147257, 256393, 154453))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "rio grande valley"]),
               c(134186, 92947, 77749, 89927, 107844, 133243))
  expect_equal(head(test_data$mexican_apprehensions[test_data$sector == "rio grande valley"]),
               c(31453, 29653, 46334, 48173, 63468, 57624))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "rio grande valley"]),
               c(54332, 66521, 62830, 78588, 95287, 122501))
  expect_equal(head(test_data$other_than_mexican_apprehensions[test_data$sector == "rio grande valley"]),
               c(130809, 137562, 140496, 99084, 192925, 96829))
  expect_equal(tail(test_data$other_than_mexican_apprehensions[test_data$sector == "rio grande valley"]),
               c(79854, 26426, 14919, 11339, 12557, 10742))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "northern border"]),
               c(4316, 3027, 2283, 2626, 3338, 3230))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "northern border"]),
               c(7343, 9959, 10157, 10487, 12338, 12108))
  expect_equal(head(test_data$mexican_apprehensions[test_data$sector == "northern border"]),
               c(2245, 1489, 1169, 1437, 1665, 1672))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "northern border"]),
               c(4081, 5495, 5947, 6095, 7444, 7362))
  expect_equal(head(test_data$other_than_mexican_apprehensions[test_data$sector == "northern border"]),
               c(2071, 1538, 1114, 1189, 1673, 1558))
  expect_equal(tail(test_data$other_than_mexican_apprehensions[test_data$sector == "northern border"]),
               c(3262, 4464, 4210, 4392, 4894, 4746))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "blaine"]),
               c(359, 288, 271, 282, 272, 360))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "blaine"]),
               c(1001, 1354, 1380, 1732, 2089, 2581))
  expect_equal(head(test_data$mexican_apprehensions[test_data$sector == "blaine"]),
               c(70, 51, 40, 67, 58, 106))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "blaine"]),
               c(506, 759, 754, 1059, 1214, 1560))
  expect_equal(head(test_data$other_than_mexican_apprehensions[test_data$sector == "blaine"]),
               c(289, 237, 231, 215, 214, 254))
  expect_equal(tail(test_data$other_than_mexican_apprehensions[test_data$sector == "blaine"]),
               c(495, 595, 626, 673, 875, 1021))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "swanton"]),
               c(736, 449, 291, 341, 506, 531))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "swanton"]),
               c(1935, 2701, 1955, 1736, 2463, 1957))
  expect_equal(head(test_data$mexican_apprehensions[test_data$sector == "swanton"]),
               c(173, 90, 23, 43, 75, 80))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "swanton"]),
               c(391, 457, 250, 168, 148, 102))
  expect_equal(head(test_data$other_than_mexican_apprehensions[test_data$sector == "swanton"]),
               c(563, 359, 268, 298, 431, 451))
  expect_equal(tail(test_data$other_than_mexican_apprehensions[test_data$sector == "swanton"]),
               c(1544, 2244, 1705, 1568, 2315, 1855))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "nationwide total"]),
               c(404142, 310531, 415816, 337117, 486651, 420789))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "nationwide total"]),
               c(1189075, 1160395, 931557, 955310, 1266214, 1676438))
  expect_equal(head(test_data$mexican_apprehensions[test_data$sector == "nationwide total"]),
               c(155452, 130454, 192969, 188122, 229178, 267734))
  expect_equal(tail(test_data$mexican_apprehensions[test_data$sector == "nationwide total"]),
               c(1023905, 1085006, 882012, 917993, 1224047, 1636883))
  expect_equal(head(test_data$other_than_mexican_apprehensions[test_data$sector == "nationwide total"]),
               c(248690, 180077, 222847, 148995, 257473, 153055))
  expect_equal(tail(test_data$other_than_mexican_apprehensions[test_data$sector == "nationwide total"]),
               c(165170, 75389, 49545, 37317, 42167, 39555))


})
