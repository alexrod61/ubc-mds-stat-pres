test = list(
  name = "Q5.4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      failure_message = "SSR_facebook should be vector and numeric",
      code = {
        testthat::expect_true("numeric" %in% class(SSR_facebook))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "SSR_facebook computation is wrong",
      code = {
        testthat::expect_equal(digest(round(SSR_facebook, 2)), "447623f72057f1e4ee31f2acb46a5c3a")
      }
    )
  )
)