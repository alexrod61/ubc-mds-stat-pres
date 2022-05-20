test = list(
  name = "Q5.2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "the correct fitting function is not being used",
      code = {
        testthat::expect_true("lm" %in% class(facebook_SLR))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "check the formula and data arguments in the fitting function",
      code = {
        testthat::expect_equal(digest(round(sum(facebook_SLR$coefficients), 2)), "a9f0cb4905810fd503591e0deb301798")
      }
    )
  )
)