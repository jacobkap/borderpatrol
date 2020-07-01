context("test-border_patrol_staffing")

setwd(here::here("data/clean"))
load("border_patrol_staffing_1992_2019.rda")
test_data <- border_patrol_staffing_1992_2019

test_that("Equal number of sectors and years works", {
  expect_equal(as.numeric(table(table(test_data$fiscal_year))), 28)
  expect_equal(as.numeric(table(table(test_data$sector))), 25)
})


test_that("Correct years and sectors", {
  expect_equal(unique(test_data$fiscal_year), 2019:1992)
  expect_true(all(unique(test_data$sector) %in% c(all_borders,
                                           "livermore")))
})


test_that("Correct number of agents", {

  expect_equal(head(test_data$number_of_agents[test_data$sector == "miami"]),
               c(127, 125, 111, 105, 105, 95))
  expect_equal(tail(test_data$number_of_agents[test_data$sector == "miami"]),
               c(48, 52, 51, 55, 58, 58))
  expect_equal(test_data$number_of_agents[test_data$sector == "miami" &
                                            test_data$fiscal_year %in% 2006:2011],
               c(94, 96, 91, 86, 81, 81))
  expect_equal(test_data$number_of_agents[test_data$sector == "miami" &
                                            test_data$fiscal_year %in% 2000:2005],
               c(78, 80, 65, 61, 58, 56))


  expect_equal(head(test_data$number_of_agents[test_data$sector == "detroit"]),
               c(404, 416, 408, 411, 412, 405))
  expect_equal(tail(test_data$number_of_agents[test_data$sector == "detroit"]),
               c(27, 28, 29, 31, 31, 29))
  expect_equal(test_data$number_of_agents[test_data$sector == "detroit" &
                                            test_data$fiscal_year %in% 2006:2011],
               c(409, 430, 318, 185, 157, 117))
  expect_equal(test_data$number_of_agents[test_data$sector == "detroit" &
                                            test_data$fiscal_year %in% 2000:2005],
               c(130, 128, 80, 64, 38, 35))


  expect_equal(head(test_data$number_of_agents[test_data$sector == "spokane"]),
               c(253, 236, 230, 234, 229, 244))
  expect_equal(tail(test_data$number_of_agents[test_data$sector == "spokane"]),
               c(32, 32, 32, 36, 36, 35))
  expect_equal(test_data$number_of_agents[test_data$sector == "spokane" &
                                            test_data$fiscal_year %in% 2006:2011],
               c(257, 255, 232, 186, 132, 119))
  expect_equal(test_data$number_of_agents[test_data$sector == "spokane" &
                                            test_data$fiscal_year %in% 2000:2005],
               c(120, 120, 78, 63, 39, 34))



  expect_equal(head(test_data$number_of_agents[test_data$sector == "big bend"]),
               c(535, 499, 500, 511, 546, 585))
  expect_equal(tail(test_data$number_of_agents[test_data$sector == "big bend"]),
               c(135, 134, 133, 128, 134, 108))
  expect_equal(test_data$number_of_agents[test_data$sector == "big bend" &
                                            test_data$fiscal_year %in% 2006:2011],
               c(667, 672, 682, 421, 336, 246))
  expect_equal(test_data$number_of_agents[test_data$sector == "big bend" &
                                            test_data$fiscal_year %in% 2000:2005],
               c(220, 239, 230, 253, 194, 196))



  expect_equal(head(test_data$number_of_agents[test_data$sector == "el centro"]),
               c(863, 844, 870, 927, 997, 1071))
  expect_equal(tail(test_data$number_of_agents[test_data$sector == "el centro"]),
               c(249, 189, 190, 194, 198, 217))
  expect_equal(test_data$number_of_agents[test_data$sector == "el centro" &
                                            test_data$fiscal_year %in% 2006:2011],
               c(1164, 1181, 1187, 1080, 894, 713))
  expect_equal(test_data$number_of_agents[test_data$sector == "el centro" &
                                            test_data$fiscal_year %in% 2000:2005],

               c(684, 734, 755, 683, 589, 502))



  expect_equal(head(test_data$number_of_agents[test_data$sector == "rio grande valley"]),
               c(3105, 3096, 3130, 3135, 3042, 3059))
  expect_equal(tail(test_data$number_of_agents[test_data$sector == "rio grande valley"]),
               c(760, 507, 474, 392, 393, 418))
  expect_equal(test_data$number_of_agents[test_data$sector == "rio grande valley" &
                                            test_data$fiscal_year %in% 2006:2011],
               c(2504, 2441, 2422, 2063, 1822, 1431))
  expect_equal(test_data$number_of_agents[test_data$sector == "rio grande valley" &
                                            test_data$fiscal_year %in% 2000:2005],

               c(1380, 1439, 1524, 1484, 1451, 1368))




  expect_equal(head(test_data$number_of_agents[test_data$sector == "tucson"]),
               c(3695, 3681, 3691, 3834, 3962, 4042))
  expect_equal(tail(test_data$number_of_agents[test_data$sector == "tucson"]),
               c(875, 702, 407, 282, 287, 300))
  expect_equal(test_data$number_of_agents[test_data$sector == "tucson" &
                                            test_data$fiscal_year %in% 2006:2011],
               c(4239, 3353, 3318, 3049, 2806, 2595))
  expect_equal(test_data$number_of_agents[test_data$sector == "tucson" &
                                            test_data$fiscal_year %in% 2000:2005],
               c(2324, 2104, 1838, 1626, 1686, 1548))



  expect_equal(head(test_data$number_of_agents[test_data$sector == "coastal border"]),
               c(255, 248, 212, 211, 213, 217))
  expect_equal(tail(test_data$number_of_agents[test_data$sector == "coastal border"]),
               c(146, 155, 153, 170, 183, 187))
  expect_equal(test_data$number_of_agents[test_data$sector == "coastal border" &
                                            test_data$fiscal_year %in% 2006:2011],
               c(232, 246, 223, 209, 172, 153))
  expect_equal(test_data$number_of_agents[test_data$sector == "coastal border" &
                                            test_data$fiscal_year %in% 2000:2005],
               c(160, 160, 152, 143, 148, 150))


  expect_equal(head(test_data$number_of_agents[test_data$sector == "nationwide total"]),
               c(19648, 19555, 19437, 19828, 20183, 20824))
  expect_equal(tail(test_data$number_of_agents[test_data$sector == "nationwide total"]),
               c(6895, 5942, 4945, 4287, 4028, 4139))
  expect_equal(test_data$number_of_agents[test_data$sector == "nationwide total" &
                                            test_data$fiscal_year %in% 2006:2011],
               c(21444, 20558, 20119, 17499, 14923, 12349))
  expect_equal(test_data$number_of_agents[test_data$sector == "nationwide total" &
                                            test_data$fiscal_year %in% 2000:2005],
               c(11264, 10819, 10717, 10045, 9821, 9212))
})

