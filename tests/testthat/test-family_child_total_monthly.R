context("test-family_child_total_monthly")

setwd(here::here("data/clean"))
load("family_child_total_monthly_2000_2019.rda")
test_data <- family_child_total_monthly_2000_2019



test_that("Correct years and sectors", {
  expect_equal(unique(test_data$fiscal_year), 2019:2000)
  expect_true(all(unique(test_data$sector) %in% c(all_borders,
                                              "livermore")))
})


test_that("Correct apprehensions", {
  expect_equal(head(test_data$total_apprehensions[test_data$sector == "big bend" &
                                                    test_data$month == "october"]),
  c(555, 819, 697, 735, 302, 316))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "big bend" &
                                                    test_data$month == "october"]),
               c(844, 707, 754, 913, 844, 891))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "big bend" &
                                                     test_data$month == "october"]),
               c(17, 109, 138, 240, 30, 4))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "big bend" &
                                                                 test_data$month == "october"]),
               c(37, 126, 86, 185, 13, 18))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "big bend" &
                                                    test_data$month == "august"]),
               c(922, 585, 563, 326, 600, 302))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "big bend" &
                                                    test_data$month == "august"]),
               c(777, 930, 867, 940, 906, 998))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "big bend" &
                                                     test_data$month == "august"]),
               c(464, 65, 67, 97, 192, 25))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "big bend" &
                                                                 test_data$month == "august"]),
               c(81, 70, 83, 27, 136, 23))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "big bend" &
                                                    test_data$month == "yearly total"]),
               c(9637, 8045, 6002, 6366, 5031, 4096))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "big bend" &
                                                    test_data$month == "yearly total"]),
               c(10536, 10530, 10319, 11392, 12087, 13689))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "big bend" &
                                                     test_data$month == "yearly total"]),
               c(2931, 741, 941, 1051, 807, 176))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "big bend" &
                                                                 test_data$month == "yearly total"]),
               c(779, 989, 811, 951, 839, 256))


  # ------------------
  expect_equal(head(test_data$total_apprehensions[test_data$sector == "coastal border" &
                                                    test_data$month == "october"]),
               c(296, 249, 331, 366, 260, 322))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "coastal border" &
                                                    test_data$month == "october"]),
               c(920, 1230, 1717, 1183, 1584, 1897))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "coastal border" &
                                                     test_data$month == "october"]),
               c(9, 0, 2, 14, 9, 4))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "coastal border" &
                                                                 test_data$month == "october"]),
               c(12, 3, 1, 2, 2, 7))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "coastal border" &
                                                    test_data$month == "august"]),
               c(390, 328, 285, 389, 387, 362))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "coastal border" &
                                                    test_data$month == "august"]),
               c(928, 817, 1116, 1667, 1393, 1736))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "coastal border" &
                                                     test_data$month == "august"]),
               c(4, 14, 0, 7, 33, 10))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "coastal border" &
                                                                 test_data$month == "august"]),
               c(5, 11, 6, 3, 0, 7))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "coastal border" &
                                                    test_data$month == "yearly total"]),
               c(3585, 3247, 3588, 4663, 3158, 3942))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "coastal border" &
                                                    test_data$month == "yearly total"]),
               c(10336, 11154, 16335, 15014, 18158, 20651))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "coastal border" &
                                                     test_data$month == "yearly total"]),
               c(39, 48, 49, 116, 131, 125))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "coastal border" &
                                                                 test_data$month == "yearly total"]),
               c(67, 56, 65, 48, 51, 65))



  # ----------------

  expect_equal(head(test_data$total_apprehensions[test_data$sector == "miami" &
                                                    test_data$month == "october"]),
               c(201, 196, 178, 271, 90, 131))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "miami" &
                                                    test_data$month == "october"]),
  c(541, 437, 686, 391, 338, 401))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "miami" &
                                                     test_data$month == "october"]),
               c(9, 0, 0, 8, 6, 4))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "miami" &
                                                                 test_data$month == "october"]),
               c(4, 2, 1, 1, 1, 3))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "miami" &
                                                    test_data$month == "august"]),
               c(179, 173, 196, 274, 346, 217))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "miami" &
                                                    test_data$month == "august"]),
               c(586, 494, 481, 564, 532, 543))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "miami" &
                                                     test_data$month == "august"]),
               c(2, 10, 0, 3, 30, 8))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "miami" &
                                                                 test_data$month == "august"]),
               c(1, 8, 6, 1, 0, 7))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "miami" &
                                                    test_data$month == "yearly total"]),
               c(1891, 2169, 2280, 3205, 1752, 2034))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "miami" &
                                                    test_data$month == "yearly total"]),
               c(7245, 4602, 5931, 5143, 5962, 6237))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "miami" &
                                                     test_data$month == "yearly total"]),
               c(17, 43, 32, 78, 98, 87))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "miami" &
                                                                 test_data$month == "yearly total"]),
               c(23, 27, 42, 20, 20, 28))



  # -------------
  expect_equal(head(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                                    test_data$month == "october"]),
               c(20755, 9722, 22642, 15036, 12031, 15192))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                                    test_data$month == "october"]),
               c(7813, 5414, 6024, 4784, 6634, 8416))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "rio grande valley" &
                                                     test_data$month == "october"]),
               c(11525, 2923, 8718, 4172, 1556, 1472))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "rio grande valley" &
                                                                 test_data$month == "october"]),
               c(2306, 1406, 4082, 3011, 1547, 2652))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                                    test_data$month == "august"]),
               c(22355, 16744, 8650, 19155, 14750, 17273))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                                    test_data$month == "august"]),
               c(12713, 8542, 7737, 8762, 9426, 9073))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "rio grande valley" &
                                                     test_data$month == "august"]),
               c(13533, 7093, 2815, 6341, 3577, 2467))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "rio grande valley" &
                                                                 test_data$month == "august"]),
               c(1971, 2237, 1294, 3768, 2895, 2173))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                                    test_data$month == "yearly total"]),
               c(339135, 162262, 137562, 186830, 147257, 256393))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                                    test_data$month == "yearly total"]),
               c(134186, 92947, 77749, 89927, 107844, 133243))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "rio grande valley" &
                                                     test_data$month == "yearly total"]),
               c(211631, 63278, 49896, 52006, 27409, 52326))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "rio grande valley" &
                                                                 test_data$month == "yearly total"]),
               c(34523, 23757, 23708, 36714, 23864, 49959))

})
