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
  data.seasonal <- dplyr::mutate(data.seasonal, Year = as.numeric(Year))
  data.seasonal <- dplyr::arrange(data.seasonal, Year)
  dplyr::mutate(data.seasonal, Month = factor(Month, levels = 1:4, labels = c('January', 'April', 'July', 'October')))
}

# Collapse 4 months into 1 value per year (for each of Min/Avg/Max).
collapse_year <- function(df) {
  df <- dplyr::filter(df, !is.na(Year))
  df <- dplyr::mutate(df, Year = as.numeric(Year))
  df %>% 
    dplyr::group_by(Year) %>% 
    dplyr::summarize(Min = mean(Min, na.rm = TRUE), Avg = mean(Avg, na.rm = TRUE), Max = mean(Max, na.rm = TRUE), .groups = 'drop')
}

# Plot min, max, and avg of site statically.
plot_site_static <- function(df, site) {
  df_year <- collapse_year(df)
  df_long <- df_year %>% tidyr::pivot_longer(cols = c('Min', 'Avg', 'Max'), names_to = 'Type', values_to = 'Temp')
  df_long$Type <- factor(df_long$Type, levels = c('Min', 'Avg', 'Max'), labels = c('Min', 'Avg', 'Max'))
  g <- df_long %>%
    ggplot2::ggplot(ggplot2::aes(Year, Temp, color = Type)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::theme_classic() +
    ggplot2::labs(x = 'Time (year)', y = 'Temperature (°F)') +
    ggplot2::ggtitle(paste(site, 'Temperature over Time (Min/Avg/Max)'))
  print(g)
}

# Plot min, max, and avg of site interactively.
plot_site_interactive <- function(df, site) {
  df_year <- collapse_year(df)
  df_long <- df_year %>% tidyr::pivot_longer(cols = c('Min', 'Avg', 'Max'), names_to = 'Type', values_to = 'Temp')
  df_long$Type <- factor(df_long$Type, levels = c('Min', 'Avg', 'Max'), labels = c('Min', 'Avg', 'Max'))
  p <- plotly::plot_ly(df_long, x = ~Year, y = ~Temp, color = ~Type) %>%
    plotly::add_lines() %>%
    plotly::layout(title = list(text = paste(site, 'Temperature over Time (Min/Avg/Max)')), yaxis = list(title = 'Temperature (°F)'))
  p
}

# ----- Problem 1 ----- #

# Load data.
data(soi)

# Make time series plot.
plot(soi, main = 'Southern Oscillation Index over 453 Months', xlab = 'Time', ylab = 'Sourthern Oscillation Index', lwd = 2)

# Make decomposition plot.
plot(stats::decompose(soi, type = 'additive'))

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
NewHaven  <- ts_seasonal('data/Climate_Northeast_NewHavenCT.xlsx')
Warwick   <- ts_seasonal('data/Climate_Northeast_WarwickRI.xlsx')
Worcester <- ts_seasonal('data/Climate_Northeast_WorcesterMA.xlsx')

# Make static time series plots.
plot_site_static(NewHaven, 'New Haven, CT')
plot_site_static(Warwick, 'Warwick, RI')
plot_site_static(Worcester, 'Worcester, MA')

# Make interactive plots.
p_NewHaven <- plot_site_interactive(NewHaven, 'New Haven, CT')
p_Warwick <- plot_site_interactive(Warwick, 'Warwick, RI')
p_Worcester <- plot_site_interactive(Worcester, 'Worcester, MA')
htmlwidgets::saveWidget(p_NewHaven, 'plots/NewHaven_interactive.html', selfcontained = TRUE)
htmlwidgets::saveWidget(p_Warwick, 'plots/Warwick_interactive.html', selfcontained = TRUE)
htmlwidgets::saveWidget(p_Worcester, 'plots/Worcester_interactive.html', selfcontained = TRUE)
