test = list(
  name = "Q3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      failure_message = "the plot should be assigned to an object called facebook_plot",
      code = {
        testthat::expect_true(exists("facebook_plot"))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      failure_message = "the variable used for the x-axis is incorrect",
      code = {
        properties <- facebook_plot$mapping
        testthat::expect_true("share_percentage" == rlang::get_expr(properties$x))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      failure_message = "the variable used for the y-axis is incorrect",
      code = {
        properties <- facebook_plot$mapping
        testthat::expect_true("total_engagement_percentage" == rlang::get_expr(properties$y))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      failure_message = "the plot type is incorrect",
      code = {
        testthat::expect_true("GeomPoint" %in% class(facebook_plot$layers[[1]]$geom))
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      failure_message = "you should use a human-readable name for the x-axis label",
      code = {
        testthat::expect_false((facebook_plot$labels$x) == "share_percentage")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      failure_message = "you should use a human-readable name for the y-axis label",
      code = {
        testthat::expect_false((facebook_plot$labels$y) == "total_engagement_percentage")
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      failure_message = "your plot should have a title",
      code = {
        testthat::expect_false(is.null(facebook_plot$labels$title))
      }
    )
  )
)