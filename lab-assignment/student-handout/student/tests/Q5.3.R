test = list(
  name = "Q5.3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      failure_message = "tidy_SLR should be a data frame",
      code = {
        testthat::expect_true("data.frame" %in% class(tidy_SLR))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      failure_message = "tidy_SLR does not have the right estimates",
      code = {
        testthat::expect_equal(digest(round(sum(tidy_SLR$estimate), 2)), "a9f0cb4905810fd503591e0deb301798")
      }
    )
  )
)