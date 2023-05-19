require('pacman')
pacman::p_load(dplyr,gvlma, MASS, brant,EnvStats, performance, ggplot2, forecast,funtimes,,car)

"
These lines of code are used to investigate the relationship of (S̅_tota ) with the final ranking at the conclusion of the tournamnent.
"

#import data
x_2022_final <- read.csv("x_2022_final.csv")
y_2022_final <- read.csv("y_2022_final.csv")
z_2022_final <- read.csv("z_2022_final.csv")

# corr test
cor.test(x_2022_final$meanRho, x_2022_final$finalResults, method = "kendall", exact = FALSE)
# corr test
cor.test(y_2022_final$meanRho, y_2022_final$finalResults, method = "kendall", exact = FALSE)
# corr test
cor.test(z_2022_final$meanRho, z_2022_final$finalResults, method = "kendall", exact = FALSE)

# cast final rank as a factor
x_2022_final$finalResults <- factor(x_2022_final$finalResults)
y_2022_final$finalResults <- factor(y_2022_final$finalResults)
z_2022_final$finalResults <- factor(z_2022_final$finalResults)

#model
x_2022_final_model <- polr(finalResults ~ meanRho, data = x_2022_final, method = c("logistic"))
y_2022_final_model <- polr(finalResults ~ meanRho, data = y_2022_final, method = c("logistic"))
z_2022_final_model <- polr(finalResults ~ meanRho, data = z_2022_final, method = c("logistic"))

#check model
poTest(x_2022_final_model)
poTest(y_2022_final_model)
poTest(z_2022_final_model)

#summary tables
x_2022_summary_table <- coef(summary(x_2022_final_model))
pval <- pnorm(abs(x_2022_summary_table[, "t value"]),lower.tail = FALSE)* 2
x_2022_summary_table <- cbind(x_2022_summary_table, "p value" = round(pval,3))

y_2022_summary_table <- coef(summary(y_2022_final_model))
pval <- pnorm(abs(y_2022_summary_table[, "t value"]),lower.tail = FALSE)* 2
y_2022_summary_table <- cbind(y_2022_summary_table, "p value" = round(pval,3))

z_2022_summary_table <- coef(summary(z_2022_final_model))
pval <- pnorm(abs(z_2022_summary_table[, "t value"]),lower.tail = FALSE)* 2
z_2022_summary_table <- cbind(z_2022_summary_table, "p value" = round(pval,3))


