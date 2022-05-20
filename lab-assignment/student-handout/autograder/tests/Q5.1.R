test = list(
  name = "Q5.1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      failure_message = "beta_0_hat should be vector and numeric",
      code = {
        testthat::expect_true("numeric" %in% class(beta_0_hat))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      failure_message = "beta_1_hat should be vector and numeric",
      code = {
        testthat::expect_true("numeric" %in% class(beta_1_hat))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "beta_0_hat computation is wrong",
      code = {
        testthat::expect_equal(digest(round(beta_0_hat, 2)), "e1901f8dd167541f390c7ba7101a44cd")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "beta_1_hat computation is wrong",
      code = {
        testthat::expect_equal(digest(round(beta_1_hat, 2)), "597c0fe04eaee0d9994580e6c92628f6")
      }
    )
  )
)