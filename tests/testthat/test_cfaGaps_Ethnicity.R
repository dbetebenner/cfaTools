context("Achievement Gaps Test by Ethnicity")

test_that("cfaGaps correctly runs basic gap analyses by ethnicity", {
  cfaGaps_Percentiles <- c("0th", "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th",
    "10th", "11th", "12th", "13th", "14th", "15th", "16th", "17th",
    "18th", "19th", "20th", "21st", "22nd", "23rd", "24th", "25th",
    "26th", "27th", "28th", "29th", "30th", "31st", "32nd", "33rd",
    "34th", "35th", "36th", "37th", "38th", "39th", "40th")
  using_cfaGaps <- cfaGaps

  expect_equal(
    cfaGaps_Percentiles, using_cfaGaps
  )
})
