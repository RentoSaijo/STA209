# ----- Setup ----- #

# Load libraries.
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))
suppressMessages(library(fs))
suppressMessages(library(plotly))
suppressMessages(library(htmlwidgets))
suppressMessages(library(astsa))

# Set seed.
set.seed(20060527)

# ----- Helpers ----- #

# Read from Excel.
ts_seasonal <- function(filename, nums = 1:4) {
  data.seasonal <- matrix(0, nrow = 1, ncol = 5)
  for(i in 1:length(nums)){
    dataexcel <- suppressMessages(readxl::read_excel(filename, sheet = nums[i], range = 'D7:H77'))
    dataexcel <- dataexcel[-1,-2]
    dataexcel <- dplyr::filter(dataexcel, !is.na(dataexcel$Avg))
    data <- dplyr::mutate(dataexcel, Month = nums[i])
    colnames(data.seasonal) <- colnames(data)
    data.seasonal <- rbind(data.seasonal, data)
  }
  data.seasonal <- data.seasonal[-1, ]
  data.seasonal <- dplyr::mutate(data.seasonal, Year = readr::parse_number(as.character(Year)))
  data.seasonal <- dplyr::filter(data.seasonal, !is.na(Year))
  data.seasonal <- dplyr::arrange(data.seasonal, Year)
  dplyr::mutate(data.seasonal, Month = factor(Month, levels = 1:4, labels = c('January', 'April', 'July', 'October')))
}

# Make ts objects (freq = 4) for Min/Avg/Max, starting at the earliest year.
make_site_ts <- function(df) {
  df <- dplyr::mutate(df, Year = readr::parse_number(as.character(Year)))
  df <- dplyr::filter(df, !is.na(Year))
  df$Season <- as.numeric(df$Month)
  df <- dplyr::arrange(df, Year, Season)
  start_year <- min(df$Year, na.rm = TRUE)
  ts_min <- stats::ts(df$Min, start = c(start_year, 1), frequency = 4)
  ts_avg <- stats::ts(df$Avg, start = c(start_year, 1), frequency = 4)
  ts_max <- stats::ts(df$Max, start = c(start_year, 1), frequency = 4)
  list(ts_min = ts_min, ts_avg = ts_avg, ts_max = ts_max, start_year = start_year)
}

# Plot min, max, and avg of site statically.
plot_site_static_ts <- function(df, site) {
  ts_list <- make_site_ts(df)
  ylo <- min(ts_list$ts_min, ts_list$ts_avg, ts_list$ts_max, na.rm = TRUE)
  yhi <- max(ts_list$ts_min, ts_list$ts_avg, ts_list$ts_max, na.rm = TRUE)
  par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))
  astsa::tsplot(ts_list$ts_max, main = paste(site, 'Temperature Time Series'), xlab = 'Time (years)', ylab = 'Max (°F)', lwd = 2, ylim = c(ylo, yhi))
  astsa::tsplot(ts_list$ts_avg, main = '', xlab = 'Time (years)', ylab = 'Avg (°F)', lwd = 2, ylim = c(ylo, yhi))
  astsa::tsplot(ts_list$ts_min, main = '', xlab = 'Time (years)', ylab = 'Min (°F)', lwd = 2, ylim = c(ylo, yhi))
  par(mfrow = c(1, 1))
}

# Plot min, max, and avg of site interactively.
plot_site_interactive_ts <- function(df, site) {
  ts_list <- make_site_ts(df)
  t <- as.numeric(stats::time(ts_list$ts_avg))
  dfp <- tibble::tibble(Time = t, Max = as.numeric(ts_list$ts_max), Avg = as.numeric(ts_list$ts_avg), Min = as.numeric(ts_list$ts_min))
  pmax <- plotly::plot_ly(dfp, x = ~Time, y = ~Max, name = 'Max') %>% plotly::add_lines() %>% plotly::layout(yaxis = list(title = 'Max (°F)'))
  pavg <- plotly::plot_ly(dfp, x = ~Time, y = ~Avg, name = 'Avg') %>% plotly::add_lines() %>% plotly::layout(yaxis = list(title = 'Avg (°F)'))
  pmin <- plotly::plot_ly(dfp, x = ~Time, y = ~Min, name = 'Min') %>% plotly::add_lines() %>% plotly::layout(yaxis = list(title = 'Min (°F)'))
  plotly::subplot(pmax, pavg, pmin, nrows = 3, shareX = TRUE, titleY = TRUE) %>% plotly::layout(title = list(text = paste(site, 'Temperature Time Series (Interactive)')))
}

# ----- Problem 1 ----- #

# Load data.
data(soi)

# Make time series plot.
plot(soi, main = 'Southern Oscillation Index over 453 Months', xlab = 'Time', ylab = 'Sourthern Oscillation Index', lwd = 2)

# Make decomposition plots.
plot(stats::decompose(soi, type = 'additive'))
plot(stats::decompose(soi, type = 'multiplicative'))

# Fit models.
t1 <- stats::time(soi)
t2 <- t1^2 / factorial(2)
t3 <- t1^3 / factorial(3)
m1 <- lm(soi ~ t1)
m2 <- lm(soi ~ t1 + t2)
m3 <- lm(soi ~ t1 + t2 + t3)
summary(m1)
summary(m2)
summary(m3)

# Report anova.
anova(m1)
anova(m2)
anova(m3)

# Plot fitted lines.
plot(soi, main = 'Southern Oscillation Index over 453 Months', xlab = 'Time', ylab = 'Sourthern Oscillation Index', lwd = 2)
par(new = TRUE)
plot(m1$fitted, type = 'l', lwd = 6, lty = 2, col = 2, main='', xlab='', ylab='', xaxt='n', yaxt='n')
par(new = TRUE)
plot(m2$fitted, type = 'l', lwd = 6, lty = 2, col = 3, main='', xlab='', ylab='', xaxt='n', yaxt='n')
par(new = TRUE)
plot(m3$fitted, type = 'l', lwd = 6, lty = 2, col = 4, main='', xlab='', ylab='', xaxt='n', yaxt='n')
legend('bottomleft', c('Observed', 'Model 1', 'Model 2', 'Model 3'), lty = c(1, rep(2, 3)), lwd = rep(2, 4), col = 1:4)

# Plot detrended data.
plot(time(soi), m1$residuals, type = 'l', main = 'Detrended Data for Model 1', ylab = 'Residual')
plot(time(soi), m2$residuals, type = 'l', main = 'Detrended Data for Model 2', ylab = 'Residual')
plot(time(soi), m3$residuals, type = 'l', main = 'Detrended Data for Model 3', ylab = 'Residual')

# ----- Problem 2 ----- #

# Load data.
NewHaven <- ts_seasonal('data/Climate_Northeast_NewHavenCT.xlsx')
Warwick <- ts_seasonal('data/Climate_Northeast_WarwickRI.xlsx')
Worcester <- ts_seasonal('data/Climate_Northeast_WorcesterMA.xlsx')

# Make static time series plots.
plot_site_static_ts(NewHaven, 'New Haven, CT')
plot_site_static_ts(Warwick, 'Warwick, RI')
plot_site_static_ts(Worcester, 'Worcester, MA')

# Make interactive plots.
p_NewHaven <- plot_site_interactive_ts(NewHaven, 'New Haven, CT')
p_Warwick <- plot_site_interactive_ts(Warwick, 'Warwick, RI')
p_Worcester <- plot_site_interactive_ts(Worcester, 'Worcester, MA')
dir_create('plots')
htmlwidgets::saveWidget(p_NewHaven, 'plots/NewHaven.html', selfcontained = TRUE)
htmlwidgets::saveWidget(p_Warwick, 'plots/Warwick.html', selfcontained = TRUE)
htmlwidgets::saveWidget(p_Worcester, 'plots/Worcester.html', selfcontained = TRUE)

# https://rentosaijo.github.io/STA209/plots/NewHaven.html
# https://rentosaijo.github.io/STA209/plots/Warwick.html
# https://rentosaijo.github.io/STA209/plots/Worcester.html

# ----- Project ----- #

