require('pacman')
pacman::p_load(dplyr,gvlma, MASS, brant,EnvStats, performance, ggplot2, forecast,funtimes,tseries)
"
These lines of code build a simple linear regression to test the relationship between points earned in the group stage and synchrony
"

#import data
x_2021_final <- read.csv("x_2021_final.csv")
y_2021_final <- read.csv('y_2021_final.csv')
z_2021_final <- read.csv('z_2021_final.csv')

#model 
x_2021_points_model <- lm(points ~ meanRho, data = x_2021_final)
y_2021_points_model <- lm(points ~ meanRho, data = y_2021_final)
z_2021_points_model <- lm(points ~ meanRho, data = z_2021_final)

#check model for each model
check_model(x_2021_points_model)
check_model(y_2021_points_model)
check_model(z_2021_points_model)

# models with influential points omitted
# omit Thunder awaken
x_omitted_2021_points_model <- lm(points~meanRho, data = x_2021_final[-13,])
# omit beastcoast
z_omitted_2021_points_model <- lm(points ~ meanRho, data = z_2021_final[-12,])
