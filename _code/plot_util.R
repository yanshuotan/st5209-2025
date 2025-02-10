require(rlang)
require(magrittr)

gg_custom_season <- function(data, y, period, start = 1) {
  # Make a seasonal plot with period specified in integer
  # start argument specifies the row number that will be the first season
  # in the period
  y <- enquo(y)
  data |>
    mutate(Season = (row_number() - start) %% period + start,
           Iteration = as.factor((row_number() - start) %/% period + 1)) |>
    ggplot(aes(x = Season, y = !!y, color = Iteration)) +
    geom_line()
}

sim_ar <- function(ar_model) {
  n <- ar_model |> augment() |> nrow()
  alpha <- ar_model |> tidy() |> pluck("estimate", 1)
  ar_coefs <- ar_model |> tidy() |> pluck("estimate") |> extract(-1)
  p <- length(ar_coefs)
  mu <- alpha / (1 - sum(ar_coefs))
  sigma2 <- ar_model |> pluck(1, 1, "fit", "sigma2")
  xt <- arima.sim(n = n, model = list(order = c(p, 0, 0), ar = ar_coefs), 
                  sd = sqrt(sigma2)) + mu
  xt
}

periodogram <- function(x, max_freq = 0.5) {
  #' Plot a Periodogram for a Time Series
  #'
  #' This function takes a vector representing a time series,
  #' and plots the periodogram of the time series.
  #'
  #' @param x A numeric vector representing the time series.
  #' @param max_freq The max frequency to be plotted
  #'
  #' @return A ggplot object representing the periodogram of the time series.
  #'
  
  # old_warn <- options(warn = -1)  # Disable warnings
  n <- length(x)
  freq <- 0 : (n - 1) / n
  per <- Mod(fft(x - mean(x, na.rm = TRUE))) ^ 2
  tibble(freq = freq, per = per) |>
    ggplot(aes(x = freq, y = per)) +
    geom_segment(aes(xend = freq, yend = 0)) +
    labs(x = "Frequency", y = "Periodogram") +
    theme_minimal() + xlim(0, max_freq)
  # on.exit(options(old_warn))      # Restore warning setting on exit
}