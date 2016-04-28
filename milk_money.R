library(ggplot2)
library(scales)
library(lubridate)
library(plyr)
master_df <- read.csv("milk_and_money.csv")

#data cleaning
master_df$Month <- paste("01",master_df$Month,sep = "-")
master_df$Month <- as.Date(master_df$Month, "%d-%B-%y")

# ==============================
# ==== Question 1 ==============
# ==============================

# graphs defaults
title_theme <- element_text(face = "bold", color = "red")
axis_theme <- element_text(face = "italic", color = "black")

# Time Series
p <- ggplot(master_df, aes(Month, Mailbox)) + geom_line(colour = "blue")
p <- p + scale_x_date(minor_breaks = "1 month",labels = date_format("%b/%Y")) + theme_bw()
p <- p + labs(title = "Mailbox Price", x = "", y = "Price [$]") 
p + theme(title = title_theme , axis.title = axis_theme)


# Butter x Mailbox
df <- master_df[,c(2,5)]
lm_model <- lm(df)
b0 <- unname(lm_model$coefficients[1])
b1 <- unname(lm_model$coefficients[2])
p <- ggplot(df, aes(x = Butter, y = Mailbox))
p <- p + geom_point() + geom_abline(intercept = b0, slope = b1, colour = "blue") + theme_bw()
p <- p + labs(title = "Price Relation", x = "Butter [$]", y = "Mailbox [$]") 
p + theme(title = title_theme , axis.title = axis_theme)

#residuals
master_df$lm_residuals <- summary(lm_model)$residuals
master_df$fitted <- fitted(lm_model)

# Plotting against the months
p <- ggplot(master_df, aes(x = fitted, y = lm_residuals))
p <- p + geom_point() + theme_bw()
p <- p + labs(title = "Residuals for Class III", x = "Fitted values", y = "Residuals [$]") 
p + theme(title = title_theme , axis.title = axis_theme)

# Plotting against the months
p <- ggplot(master_df, aes(x = Month, y = lm_residuals))
p <- p + geom_point() + theme_bw()
p <- p + labs(title = "Residuals by Month", x = "", y = "Residuals [$]") 
p + theme(title = title_theme , axis.title = axis_theme)

# NFDM x Mailbox
df <- master_df[,c(2,6)]
lm_model <- lm(df)
b0 <- unname(lm_model$coefficients[1])
b1 <- unname(lm_model$coefficients[2])
p <- ggplot(df, aes(x = NFDM, y = Mailbox))
p <- p + geom_point() + geom_abline(intercept = b0, slope = b1, colour = "blue") + theme_bw()
p <- p + labs(title = "Price Relation", x = "Not Fat Dry Milk [$]", y = "Mailbox [$]") 
p + theme(title = title_theme , axis.title = axis_theme)

# Class 3 x Mailbox
df <- master_df[,c(2,4)]
lm_model <- lm(df)
b0 <- unname(lm_model$coefficients[1])
b1 <- unname(lm_model$coefficients[2])
p <- ggplot(df, aes(x = Class.III, y = Mailbox))
p + geom_point() + geom_abline(intercept = b0, slope = b1, colour = "blue") + theme_bw()
p <- p + geom_point() + geom_abline(intercept = b0, slope = b1, colour = "blue") + theme_bw()
p <- p + labs(title = "Price Relation", x = "Class III [$]", y = "Mailbox [$]") 
p + theme(title = title_theme , axis.title = axis_theme)

#residuals
master_df$lm_residuals <- summary(lm_model)$residuals
master_df$fitted <- fitted(lm_model)

# Residuals x Fitted
p <- ggplot(master_df, aes(x = fitted, y = lm_residuals))
p <- p + geom_point() + theme_bw()
p <- p + labs(title = "Residuals for Class III", x = "Fitted values", y = "Residuals [$]") 
p + theme(title = title_theme , axis.title = axis_theme)

# Residuals x Months
p <- ggplot(master_df, aes(x = Month, y = lm_residuals))
p <- p + geom_point() + theme_bw()
p <- p + labs(title = "Residuals by Month", x = "", y = "Residuals [$]") 
p + theme(title = title_theme , axis.title = axis_theme)

#------ Class 4 x Mailbox -----------
df <- master_df[,c(2,3)]
lm_model <- lm(df)
b0 <- unname(lm_model$coefficients[1])
b1 <- unname(lm_model$coefficients[2])
p <- ggplot(df, aes(x = Class.IV, y = Mailbox))
p + geom_point() + geom_abline(intercept = b0, slope = b1, colour = "blue") + theme_bw()
p <- p + geom_point() + geom_abline(intercept = b0, slope = b1, colour = "blue") + theme_bw()
p <- p + labs(title = "Price Relation", x = "Class IV [$]", y = "Mailbox [$]") 
p + theme(title = title_theme , axis.title = axis_theme)


#-- checking for seasonality
df <- master_df[6:nrow(master_df),]
avg <- mean(df$Mailbox)
df$Month <- month(df$Month)
df <- aggregate(df$Mailbox,by= list(df$Month), mean)
df$months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
df$months <- factor(df$months, levels = months)
names(df) <- c("month_num","price_avg","months")
df$price_avg <- df$price_avg - avg

p <- ggplot(df, aes(x = months, y = price_avg))
p <- p + geom_bar(stat = "identity", fill="darkblue") + theme_bw()
p <- p + labs(title = "Price Variation around the Mean", x = "Months", y = "Dollars [$]") 
p + theme(title = title_theme , axis.title = axis_theme)


