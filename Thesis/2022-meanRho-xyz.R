require('pacman')
pacman::p_load(dplyr,gvlma, MASS, brant,EnvStats, performance, ggplot2, forecast,funtimes,tseries)

"
These lines of code build a simple linear regression to test the relationship between points earned in the group stage and synchrony
"

x_2022_final <- read.csv("x_2022_final.csv")
y_2022_final <- read.csv("y_2022_final.csv")
z_2022_final <- read.csv("z_2022_final.csv")


#model
x_2022_points_model <- lm(points ~ meanRho, data = x_2022_final)
y_2022_points_model <- lm(points ~ meanRho, data = y_2022_final)
z_2022_points_model <- lm(points ~ meanRho, data = z_2022_final)

#check model for each model
check_model(x_2022_points_model)
check_model(y_2022_points_model)
check_model(z_2022_points_model)
