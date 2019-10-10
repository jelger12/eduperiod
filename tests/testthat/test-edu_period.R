test_that("date is transformed to period", {
  expect_equal(edu_period(lubridate::dmy("01-01-2011")), 3)
})
