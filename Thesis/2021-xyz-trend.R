require('pacman')
pacman::p_load(dplyr,gvlma, MASS, brant,EnvStats, performance, ggplot2, forecast,funtimes,tseries)


"
The code below generates the tests for a linear trend in synchrony across the matches for each team in the group stage for year 2021 as follows:

NOTE : the no_trendtest function was modified to return the estimated coefficient for the linear model fit to each bootstrapped sample
for the purposes of generating a confidence internval. This modification is loaded in the new_boot.R file

The no_trendtest function [Vyacheslav et al., 2022] was used to test for a linear trend (increasing or decreasing) 
in S ̅ for a given team as they progressed through the group stage. First an autoregressive model is estimated for each
timeseries and used to generate 1000 bootstrapped resamples. Then, the no_trendtest function [Vyacheslav et al., 2022] fits a 
simple linear regression to each bootstrapped sample and generates a distribution of t-statistics. Finally, the t-statistic 
generated for the simple linear regression of the original time series is tested against this distribution for significance. 
We derived a confidence internal for the estimated slope using a bootstrap method. 
For each axis, the p-values were corrected for false discovery rate [Benjamini & Hochberg, 1995]. 
"

# import modified no_trend test
source("new_boot.R")

# Import data
x_2021_times <- read.csv('x_2021_time_df.csv')
y_2021_times <- read.csv('y_2021_time_df.csv')
z_2021_times <- read.csv('z_2021_time_df.csv')

# set seed for reproduction
set.seed(123)

#test for linear trend X-axis
x_2021 <- c()
for (team in unique(x_2021_times$team)) {
  df <- x_2021_times[x_2021_times$team == team,]
  df <- df[order(df$startTime),]
  obj <- new_notrend(ts(df$meanRhoForMatch),ar.method = "burg")
  ci <- round(quantile(obj$slopes, c(0.025,0.975)), 2)
  x_2021 <- c(x_2021, c(team, obj$p.value, obj$statistic, ci[1], ci[2]))
}
x_2021_df <- data.frame(matrix(x_2021, ncol = 5, byrow = TRUE))
colnames(x_2021_df) <- c("Team", "p-value", "T-statistic", "CI_Lower", "CI_Upper")
x_2021_df$BH_adj_pvalue <- p.adjust(x_2021_df$"p-value", method = "BH")
x_2021_df$combined_column <- paste("(", x_2021_df$CI_Lower, ",", x_2021_df$CI_Upper, ")", sep="")

#test for linear trend Y-axis
y_2021 <- c()
for (team in unique(y_2021_times$team)) {
  df <- y_2021_times[y_2021_times$team == team,]
  df <- df[order(df$startTime),]
  obj <- new_notrend(ts(df$meanRhoForMatch),ar.method = "burg")
  ci <- round(quantile(obj$slopes, c(0.025,0.975)), 2)
  y_2021 <- c(y_2021, c(team, obj$p.value, obj$statistic, ci[1], ci[2]))
}
y_2021_df <- data.frame(matrix(y_2021, ncol = 5, byrow = TRUE))
colnames(y_2021_df) <- c("Team", "p-value", "T-statistic", "CI_Lower", "CI_Upper")
y_2021_df$BH_adj_pvalue <- p.adjust(y_2021_df$"p-value", method = "BH")
y_2021_df$combined_column <- paste("(", y_2021_df$CI_Lower, ",", y_2021_df$CI_Upper, ")", sep="")


#test for linear trend Z-axis
z_2021 <- c()
for (team in unique(z_2021_times$team)) {
  df <- z_2021_times[z_2021_times$team == team,]
  df <- df[order(df$startTime),]
  obj <- new_notrend(ts(df$meanRhoForMatch),ar.method = "burg")
  ci <- round(quantile(obj$slopes, c(0.025,0.975)), 2)
  z_2021 <- c(z_2021, c(team, obj$p.value, obj$statistic, ci[1], ci[2]))
}
z_2021_df <- data.frame(matrix(z_2021, ncol = 5, byrow = TRUE))
colnames(z_2021_df) <- c("Team", "p-value", "T-statistic", "CI_Lower", "CI_Upper")
z_2021_df$BH_adj_pvalue <- p.adjust(z_2021_df$"p-value", method = "BH")
z_2021_df$combined_column <- paste("(", z_2021_df$CI_Lower, ",", z_2021_df$CI_Upper, ")", sep="")
