initial_scatterplot <- function(data)
{
  data %>%
    ggplot(aes(danceability, valence)) +
    geom_point(color = "dimgray") +
    labs(x = "Danceability Score", y = "Valence Score") +
    ggtitle("Spotify Sample Scatterplot") +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      axis.text = element_text(size = 22),
      axis.title = element_text(size = 24)
    ) +
    lims(x = c(0, 100), y = c(0, 100))
}

intro_estimation_scatterplot <- function(data)
{
  data %>%
    ggplot(aes(danceability, valence)) +
    geom_point(color = "dimgray") +
    labs(x = "Danceability Score", y = "Valence Score") +
    ggtitle("Spotify Sample Scatterplot") +
    theme(
      plot.title = element_text(size = 32, face = "bold"),
      axis.text = element_text(size = 22),
      axis.title = element_text(size = 24)
    ) +
    lims(x = c(0, 100), y = c(0, 100)) +
    geom_abline(intercept = 15, slope = 1, col = "pink") +
    geom_abline(intercept = 50, slope = 0.6, col = "orange") +
    geom_abline(intercept = 10, slope = 0.3, col = "red") +
    geom_abline(intercept = -10, slope = 0.9, col = "purple") +
    geom_abline(intercept = 40, slope = 0.1, col = "darkgreen")
}

OLS_scatterplot <- function(data) {
  OLS_plot <- data %>%
    ggplot(aes(danceability, valence)) +
    geom_point(color = "dimgray") +
    labs(x = "Danceability Score", y = "Valence Score") +
    theme(
      plot.title = element_text(size = 22, face = "bold"),
      axis.text = element_text(size = 22),
      axis.title = element_text(size = 22)
    ) +
    lims(x = c(0, 100), y = c(0, 100)) +
    stat_smooth(method = "lm", fullrange = TRUE, se = FALSE, formula = y ~ x)

  OLS_model <- lm(valence ~ danceability, data = data)
  data$pred <- predict(OLS_model, newdata = data)
  SSR <- round(sum((data$valence - data$pred)^2), 1)

  for (i in 1:nrow(data))
  {
    if (data$pred[i] < data$valence[i]) {
      OLS_plot <- OLS_plot +
        geom_segment(
          x = data$danceability[i], y = data$pred[i],
          xend = data$danceability[i], yend = data$valence[i],
          linetype = "dotted", color = "dimgray", size = 0.1
        )
    } else {
      OLS_plot <- OLS_plot +
        geom_segment(
          x = data$danceability[i], y = data$valence[i],
          xend = data$danceability[i], yend = data$pred[i],
          linetype = "dotted", color = "dimgray", size = 0.1
        )
    }
  }
  OLS_plot <- OLS_plot +
    ggtitle(paste("OLS Fit with SSR =", SSR))
  OLS_plot
}

non_OLS_scatterplot <- function(data, beta_0, beta_1, colour) {
  non_OLS_plot <- data %>%
    ggplot(aes(danceability, valence)) +
    geom_point(color = "dimgray") +
    labs(x = "Danceability Score", y = "Valence Score") +
    theme(
      plot.title = element_text(size = 22, face = "bold"),
      axis.text = element_text(size = 22),
      axis.title = element_text(size = 22)
    ) +
    lims(x = c(0, 100), y = c(0, 100)) +
    geom_abline(intercept = beta_0, slope = beta_1, col = colour)
    

  data$pred <- beta_0 + beta_1 * data$danceability
  SSR <- round(sum((data$valence - data$pred)^2), 2)
  
  for (i in 1:nrow(data))
  {
    if (data$pred[i] < data$valence[i]) {
      non_OLS_plot <- non_OLS_plot +
        geom_segment(
          x = data$danceability[i], y = data$pred[i],
          xend = data$danceability[i], yend = data$valence[i],
          linetype = "dotted", color = "dimgray", size = 0.1
        )
    } else {
      non_OLS_plot <- non_OLS_plot +
        geom_segment(
          x = data$danceability[i], y = data$valence[i],
          xend = data$danceability[i], yend = data$pred[i],
          linetype = "dotted", color = "dimgray", size = 0.1
        )
    }
  }
  non_OLS_plot <- non_OLS_plot +
    ggtitle(paste("Non-OLS Fit with SSR =", SSR))
  non_OLS_plot
}