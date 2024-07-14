# Load the required libraries
install.packages("pacman")
install.packages("factoextra")
install.packages("fastDummies")
install.packages("strucchange")
install.packages("tseries")
install.packages("dummy")
install.packages("vars")

pacman::p_load(dplyr, factoextra, dummy, fastDummies, ggplot2, plotly, tidyr, lubridate, forecast, ggfortify, lmtest, urca, reshape2)

library('tseries')
library('strucchange')
library('urca')
library('forecast')
library('dplyr')
library('factoextra')
library('dummy')
library('fastDummies')
library('ggplot2')
library('plotly')
library('cluster')
library("pacman")
library('tidyr')
library('lmtest')
library('broom')
library('reshape2')
library('vars')

# Load the data
df1 <- read.csv("sample1_sales_timeseries.csv")
df2 <- read.csv("sample2_sales_timeseries.csv")
df3 <- read.csv("sample3_sales_timeseries.csv")
df4 <- read.csv("sample4_sales_timeseries.csv")
df5 <- read.csv("sample5_sales_timeseries.csv")

combined_df <- rbind(df1, df2, df3, df4, df5)
combined_df$date <- as.Date(combined_df$date, format="%Y-%m-%d")

# Group by date and family and summarize sales
summarized_df <- combined_df %>%
  group_by(date, family) %>%
  summarize(total_sales = sum(sales, na.rm = TRUE), .groups = 'drop')

# Pivot the data to have separate columns for each family
wide_df <- summarized_df %>%
  pivot_wider(names_from = family, values_from = total_sales)

# Sort the dataframe by date
sorted_wide_df <- wide_df %>%
  arrange(date)

# Fill missing values with zero
sorted_wide_df[is.na(sorted_wide_df)] <- 0

# Calculate means and standard deviations from original data
means <- colMeans(sorted_wide_df[-1])
sds <- apply(sorted_wide_df[-1], 2, sd)

# Print means and standard deviations for verification
print("Means:")
print(means)
print("Standard Deviations:")
print(sds)

# Normalize the data using Z-score Normalization
normalized_data <- as.data.frame(scale(sorted_wide_df[-1]))
normalized_data$date <- sorted_wide_df$date

# Check for stationarity using the Augmented Dickey-Fuller test
adf_results <- lapply(normalized_data[-ncol(normalized_data)], adf.test)
print(adf_results)

# Perform VAR modeling on differenced data
time_series_matrix <- as.matrix(normalized_data[-ncol(normalized_data)])
var_model <- VAR(time_series_matrix, p = 1, type = "const")
summary(var_model)

# Forecasting
forecast_horizon <- 15
var_forecast <- predict(var_model, n.ahead = forecast_horizon)
forecast_values <- data.frame(date = seq(max(normalized_data$date) + 1, by = "day", length.out = forecast_horizon))

for (i in 1:length(var_forecast$fcst)) {
  forecast_name <- names(var_forecast$fcst)[i]
  forecast_values[[forecast_name]] <- var_forecast$fcst[[i]][, "fcst"]
}

colnames(forecast_values) <- c("date", "FROZEN FOODS", "LIQUOR,WINE,BEER", "MEATS", "POULTRY", "PREPARED FOODS")

# Denormalize the forecasted values
denormalized_forecast <- forecast_values
for (col in names(forecast_values)[-1]) {
  denormalized_forecast[[col]] <- forecast_values[[col]] * sds[col] + means[col]
}

# Print denormalized forecast values for verification
print("Denormalized Forecast Values:")
print(denormalized_forecast)

# Save the denormalized forecast values to a CSV file
write.csv(denormalized_forecast, "denormalized_forecast_values.csv", row.names = FALSE)

# Plotting
par(mfrow = c(2, 2))

# Plot the forecasted values for each product family
for (col in names(sorted_wide_df)[-1]) {
  plot(sorted_wide_df$date, sorted_wide_df[[col]], type = "l", xlab = "Date", ylab = "Sales",
       main = paste("Forecasted Sales for", col))
  lines(denormalized_forecast$date, denormalized_forecast[[col]], col = "red", lty = 2)
  legend("topright", legend = c("Actual", "Forecasted"), col = c("black", "red"), lty = 1:2)
}

# AIC evaluation
aic_value <- AIC(var_model)
print(paste("AIC Value:", aic_value))

# Perform serial correlation test (Ljung-Box test)
serial_test_results <- serial.test(var_model, lags.pt = 12, type = "PT.asymptotic")
print(serial_test_results)

# Check residuals
residuals <- residuals(var_model)

# Residuals mean
residuals_mean <- colMeans(residuals)
print("Residuals Mean:")
print(residuals_mean)

# Residuals distribution and correlation
par(mfrow = c(2, 2))
for (i in 1:ncol(residuals)) {
  hist(residuals[, i], main = paste("Residuals Distribution for", colnames(residuals)[i]), xlab = "Residuals")
}

# Save the residuals for further analysis if needed
write.csv(residuals, "var_model_residuals.csv", row.names = FALSE)
