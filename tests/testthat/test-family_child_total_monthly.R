context("test-family_child_total_monthly")

setwd(here::here("data/clean"))
load("family_child_total_monthly_2000_2018.rda")
test_data <- family_child_total_monthly_2000_2018



test_that("Correct years and sectors", {
  expect_equal(unique(test_data$fiscal_year), 2018:2000)
  expect_true(all(unique(test_data$sector) %in% c(all_borders,
                                              "livermore")))
})


test_that("Correct apprehensions", {
  expect_equal(head(test_data$total_apprehensions[test_data$sector == "big bend" &
                                                    test_data$month == "october"]),
  c(819, 697, 735, 302, 316, 356))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "big bend" &
                                                    test_data$month == "october"]),
               c(844, 707, 754, 913, 844, 891))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "big bend" &
                                                     test_data$month == "october"]),
               c(109, 138, 240, 30, 4, 16))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "big bend" &
                                                                 test_data$month == "october"]),
               c(126, 86, 185, 13, 18, 15))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "big bend" &
                                                    test_data$month == "august"]),
               c(585, 563, 326, 600, 302, 218))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "big bend" &
                                                    test_data$month == "august"]),
               c(777, 930, 867, 940, 906, 998))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "big bend" &
                                                     test_data$month == "august"]),
               c(65, 67, 97, 192, 25, 7))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "big bend" &
                                                                 test_data$month == "august"]),
               c(70, 83, 27, 136, 23, 6))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "big bend" &
                                                    test_data$month == "yearly total"]),
               c(8045, 6002, 6366, 5031, 4096, 3684))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "big bend" &
                                                    test_data$month == "yearly total"]),
               c(10536, 10530, 10319, 11392, 12087, 13689))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "big bend" &
                                                     test_data$month == "yearly total"]),
               c(741, 941, 1051, 807, 176, 102))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "big bend" &
                                                                 test_data$month == "yearly total"]),
               c(989, 811, 951, 839, 256, 125))


  # ------------------
  expect_equal(head(test_data$total_apprehensions[test_data$sector == "coastal border" &
                                                    test_data$month == "october"]),
               c(249, 331, 366, 260, 322, 321))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "coastal border" &
                                                    test_data$month == "october"]),
               c(920, 1230, 1717, 1183, 1584, 1897))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "coastal border" &
                                                     test_data$month == "october"]),
               c(0, 2, 14, 9, 4, 21))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "coastal border" &
                                                                 test_data$month == "october"]),
               c(3, 1, 2, 2, 7, 9))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "coastal border" &
                                                    test_data$month == "august"]),
               c(328, 285, 389, 387, 362, 382))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "coastal border" &
                                                    test_data$month == "august"]),
               c(928, 817, 1116, 1667, 1393, 1736))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "coastal border" &
                                                     test_data$month == "august"]),
               c(14, 0, 7, 33, 10, 5))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "coastal border" &
                                                                 test_data$month == "august"]),
               c(11, 6, 3, 0, 7, 11))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "coastal border" &
                                                    test_data$month == "yearly total"]),
               c(3247, 3588, 4663, 3158, 3942, 3162))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "coastal border" &
                                                    test_data$month == "yearly total"]),
               c(10336, 11154, 16335, 15014, 18158, 20651))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "coastal border" &
                                                     test_data$month == "yearly total"]),
               c(48, 49, 116, 131, 125, 67))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "coastal border" &
                                                                 test_data$month == "yearly total"]),
               c(56, 65, 48, 51, 65, 44))



  # ----------------

  expect_equal(head(test_data$total_apprehensions[test_data$sector == "miami" &
                                                    test_data$month == "october"]),
               c(196, 178, 271, 90, 131, 248))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "miami" &
                                                    test_data$month == "october"]),
  c(541, 437, 686, 391, 338, 401))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "miami" &
                                                     test_data$month == "october"]),
               c(0, 0, 8, 6, 4, 12))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "miami" &
                                                                 test_data$month == "october"]),
               c(2, 1, 1, 1, 3, 7))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "miami" &
                                                    test_data$month == "august"]),
               c(173, 196, 274, 346, 217, 171))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "miami" &
                                                    test_data$month == "august"]),
               c(586, 494, 481, 564, 532, 543))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "miami" &
                                                     test_data$month == "august"]),
               c(10, 0, 3, 30, 8, 2))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "miami" &
                                                                 test_data$month == "august"]),
               c(8, 6, 1, 0, 7, 9))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "miami" &
                                                    test_data$month == "yearly total"]),
               c(2169, 2280, 3205, 1752, 2034, 1738))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "miami" &
                                                    test_data$month == "yearly total"]),
               c(7245, 4602, 5931, 5143, 5962, 6237))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "miami" &
                                                     test_data$month == "yearly total"]),
               c(43, 32, 78, 98, 87, 43))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "miami" &
                                                                 test_data$month == "yearly total"]),
               c(27, 42, 20, 20, 28, 28))



  # -------------
  expect_equal(head(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                                    test_data$month == "october"]),
               c(9722, 22642, 15036, 12031, 15192, 8869))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                                    test_data$month == "october"]),
               c(7813, 5414, 6024, 4784, 6634, 8416))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "rio grande valley" &
                                                     test_data$month == "october"]),
               c(2923, 8718, 4172, 1556, 1472, 266))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "rio grande valley" &
                                                                 test_data$month == "october"]),
               c(1406, 4082, 3011, 1547, 2652, 1067))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                                    test_data$month == "august"]),
               c(16744, 8650, 19155, 14750, 17273, 16253))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                                    test_data$month == "august"]),
               c(12713, 8542, 7737, 8762, 9426, 9073))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "rio grande valley" &
                                                     test_data$month == "august"]),
               c(7093, 2815, 6341, 3577, 2467, 1240))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "rio grande valley" &
                                                                 test_data$month == "august"]),
               c(2237, 1294, 3768, 2895, 2173, 2427))


  expect_equal(head(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                                    test_data$month == "yearly total"]),
               c(162262, 137562, 186830, 147257, 256393, 154453))
  expect_equal(tail(test_data$total_apprehensions[test_data$sector == "rio grande valley" &
                                                    test_data$month == "yearly total"]),
               c(134186, 92947, 77749, 89927, 107844, 133243))
  expect_equal(head(test_data$family_apprehensions[test_data$sector == "rio grande valley" &
                                                     test_data$month == "yearly total"]),
               c(63278, 49896, 52006, 27409, 52326, 7265))
  expect_equal(head(test_data$unaccompanied_child_apprehension[test_data$sector == "rio grande valley" &
                                                                 test_data$month == "yearly total"]),
               c(23757, 23708, 36714, 23864, 49959, 21553))

})
