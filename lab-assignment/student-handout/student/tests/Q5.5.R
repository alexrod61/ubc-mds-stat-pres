test = list(
  name = "Q5.5",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "geom_smooth is missing",
      code = {
        testthat::expect_true("GeomSmooth" %in% class(facebook_plot$layers[[2]]$geom))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      failure_message = "incorrect method in geom_smooth",
      code = {
        properties <- facebook_plot$layers[[2]]
        method <- formals(properties[["stat_params"]][["method"]])[["method"]]
        testthat::expect_equal(digest(tolower(method)), "0ebfb0ddc1a5ced965136ef1538883c6")
      }
    )
  )
)