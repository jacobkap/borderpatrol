context("test-sector_profile")

setwd(here::here("data/clean"))
load("sector_profile_2011_2019.rda")
test_data <- sector_profile_2011_2019

test_that("Equal number of sectors and years works", {
  expect_length(unique(test_data$fiscal_year), 9)
  expect_length(unique(test_data$sector), 24)
})


test_that("Correct years and sectors", {
  expect_equal(unique(test_data$fiscal_year), 2019:2011)
  expect_true(all(unique(test_data$sector) %in% all_borders))
})

test_that("number of agents is correct", {


  expect_equal(test_data$number_of_agents[test_data$sector == "miami"],
               c(127, 111, 111, 105, 96, 94, 92, 92, 94))
  expect_equal(test_data$number_of_agents[test_data$sector == "blaine"],
               c(292, 310, 296, 295, 297, 314, 315, 323, 331))
  expect_equal(test_data$number_of_agents[test_data$sector == "big bend"],
               c(535, 499, 500, 511, 549, 588, 623, 693, 667))
  expect_equal(test_data$number_of_agents[test_data$sector == "yuma"],
               c(809, 844, 859, 829, 804, 859, 911, 954, 969))
  expect_equal(test_data$number_of_agents[test_data$sector == "nationwide total"],
               c(19648, 19555, 19437, 19828, 20273, 20863, 21391, 21165, 21444))

})



test_that("cocaine is correct", {


  expect_equal(test_data$cocaine_pounds[test_data$sector == "miami"],
               c(650, 92, 231, 344, 316, 10, 131, 68.49, 10.6))
  expect_equal(test_data$cocaine_pounds[test_data$sector == "blaine"],
               c(0, 0, 0, 0, 0, 0, 76, 125.39, 93))
  expect_equal(test_data$cocaine_pounds[test_data$sector == "big bend"],
               c(53, 65, 45, 16, 95, 27, 10, 58.46, 262.80))
  expect_equal(test_data$cocaine_pounds[test_data$sector == "yuma"],
               c(0, 78, 261, 271, 3, 120, 140, 437.76, 257.91))
  expect_equal(test_data$cocaine_pounds[test_data$sector == "nationwide total"],
               c(11684, 6550, 9346, 5473, 11220, 4554, 4696, 12160.67, 9963.38))

})

test_that("accepted prosecutions is correct", {


  expect_equal(test_data$accepted_prosecutions[test_data$sector == "miami"],
               c(241, 231, 292, 263, 155, 168, 228, 160, 108))
  expect_equal(test_data$accepted_prosecutions[test_data$sector == "blaine"],
               c(25, 13, 9, 17, 13, 19, 19, 28, 47))
  expect_equal(test_data$accepted_prosecutions[test_data$sector == "big bend"],
               c(4538, 5128, 2847, 2392, 1451, 1637, 1381, 1776, 950))
  expect_equal(test_data$accepted_prosecutions[test_data$sector == "yuma"],
               c(3867, 4005, 2367, 2341, 2071, 3986, 4856, 3954, 3163))
  expect_equal(test_data$accepted_prosecutions[test_data$sector == "nationwide total"],
               c(122325, 110340, 60498, 74908, 78843, 92434, 103772, 93023, 75846))

})


test_that("assaults is correct", {


  expect_equal(test_data$assaults[test_data$sector == "miami"],
               c(0, 1, 1, 3, 0, 1, 0, 0, 0))
  expect_equal(test_data$assaults[test_data$sector == "blaine"],
               c(2, 0, 0, 1, 4, 0, 1, 3, 0))
  expect_equal(test_data$assaults[test_data$sector == "big bend"],
               c(3, 11, 11, 9, 5, 2, 5, 1, 3))
  expect_equal(test_data$assaults[test_data$sector == "yuma"],
               c(27, 33, 33, 7, 8, 6, 16, 17, 31))
  expect_equal(test_data$assaults[test_data$sector == "nationwide total"],
               c(725, 786, 786, 454, 378, 373, 468, 555, 675))

})


test_that("death is correct", {


 # expect_true(is.na(unique(test_data$deaths[test_data$sector == "miami"])))
  #expect_true(is.na(unique(test_data$deaths[test_data$sector == "blaine"])))
  expect_equal(test_data$deaths[test_data$sector == "big bend"],
               c(3, 10, 1, 2, 4, 5, 3, 1, 2))
  expect_equal(test_data$deaths[test_data$sector == "yuma"],
               c(7, 1, 2, 7, 5, 3, 6, 9, 1))
  expect_equal(test_data$deaths[test_data$sector == "nationwide total"],
               c(300, 283, 294, 322, 240, 307, 445, 463, 357))

})




test_that("accompanied juveniles is correct", {


  expect_equal(test_data$accompanied_juveniles[test_data$sector == "miami"],
               c(10, 22, 19, 38, 61, 63, 24, 32, 34))
  expect_equal(test_data$accompanied_juveniles[test_data$sector == "blaine"],
               c(48, 35, 29, 15, 19, 9, 9, 15, 16))
  expect_equal(test_data$accompanied_juveniles[test_data$sector == "big bend"],
               c(1529, 399, 506, 578, 482, 111, 66, 44, 63))
  expect_equal(test_data$accompanied_juveniles[test_data$sector == "yuma"],
               c(27178, 7730, 3241, 3357, 999, 434, 125, 122, 98))
  expect_equal(test_data$accompanied_juveniles[test_data$sector == "nationwide total"],
               c(245572, 57353, 41223, 42507, 22132, 38982, 8564, 6548, 7022))

})


test_that("total adults is correct", {


  expect_equal(test_data$total_adults[test_data$sector == "miami"],
               c(1858, 2120, 2219, 3147, 1671, 1943, 1686, 2459, 4340))
  expect_equal(test_data$total_adults[test_data$sector == "blaine"],
               c(470, 318, 252, 253, 261, 260, 343, 510, 567))
  expect_equal(test_data$total_adults[test_data$sector == "big bend"],
               c(7329, 2377, 4685, 4837, 3710, 3729, 3493, 3752, 3784))
  expect_equal(test_data$total_adults[test_data$sector == "yuma"],
               c(33802, 18578, 6739, 7547, 5053, 5117, 5734, 6098, 5513))
  expect_equal(test_data$total_adults[test_data$sector == "nationwide total"],
               c(537793, 296644, 227762, 313552, 274950, 379038, 373392, 333739, 317163))

})

test_that("female apprehensions is correct", {


  expect_equal(test_data$female_apprehensions[test_data$sector == "miami"],
               c(220, 239, 219, 353, 261, 365, 236, 284, 479))
  expect_equal(test_data$female_apprehensions[test_data$sector == "blaine"],
               c(162, 114, 97, 90, 71, 57, 74, 95, 82))
  expect_equal(test_data$female_apprehensions[test_data$sector == "big bend"],
               c(2171, 906, 985, 1155, 1001, 448, 339, 270, 317))
  expect_equal(test_data$female_apprehensions[test_data$sector == "yuma"],
               c(27271, 9227, 4328, 4904, 1784, 872, 503, 669, 534))
  expect_equal(test_data$female_apprehensions[test_data$sector == "nationwide total"],
               c(299725, 97035, 81686, 101418, 72293, 120629, 69562, 52613, 44104))

})
