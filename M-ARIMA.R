install.packages("pacman")
install.packages("factoextra")
install.packages("fastDummies")
install.packages("strucchange")
install.packages("tseries")
library('tseries')
pacman::p_load(
  tidyverse,
  forecast
)
pacman::p_load(dplyr, factoextra, dummy, fastDummies, ggplot2, plotly, tidyr, lubridate, forecast, ggfortify)
install.packages("dummy")
library('strucchange')
library('urca')

library('forecast')
library('dplyr')
library('factoextra')
library('dummy')
library('fastDummies')
library('ggplot2')
library('plotly')
library('factoextra')
library('cluster')
library("pacman")
library('tidyr')

list.files()
df1<- read.csv("sample1_sales_timeseries.csv")
df2<- read.csv("sample2_sales_timeseries.csv")
df3<- read.csv("sample3_sales_timeseries.csv")
df4<- read.csv("sample4_sales_timeseries.csv")
df5<- read.csv("sample5_sales_timeseries.csv")

combined_df <- rbind(df1, df2,df3,df4,df5)  # Add more dataframes if needed
combined_df$date <- as.Date(combined_df$date, format="%Y-%m-%d") 
# Group by the date and family columns and summarize the sales
summarized_df <- combined_df %>%
  group_by(date, family) %>%  # Replace "family" with the actual name of the family column
  summarize(total_sales = sum(sales, na.rm = TRUE))  # Replace "sales" with the actual name of the sales column

# Sort the dataframe by date
sorted_df <- summarized_df %>%
  arrange(date)

# View the resulting dataframe
print(sorted_df)

# Group by the date and family columns and summarize the sales
summarized_df <- combined_df %>%
  group_by(date, family) %>%
  summarize(total_sales = sum(sales, na.rm = TRUE), .groups = 'drop')  # Replace "sales" with the actual name of the sales column

# Pivot the data to have separate columns for each family
wide_df <- summarized_df %>%
  pivot_wider(names_from = family, values_from = total_sales)

# Sort the dataframe by date
sorted_wide_df <- wide_df %>%
  arrange(date)

# View the resulting dataframe
print(sorted_wide_df)

# Check for missing values
print(sum(is.na(sorted_wide_df)))

# Fill missing values with zero (or use other imputation methods as needed)
sorted_wide_df[is.na(sorted_wide_df)] <- 0

# Confirm no missing values
print(sum(is.na(sorted_wide_df)))
View(sorted_wide_df)

# EDA: Summary statistics
summary(sorted_wide_df)
# EDA: Plot sales over time for each family
melted_df <- sorted_wide_df %>%
  pivot_longer(cols = -date, names_to = "family", values_to = "total_sales")

ggplot(melted_df, aes(x = date, y = total_sales, color = family)) +
  geom_line() +
  labs(title = "Sales Over Time by Product Family", x = "Date", y = "Total Sales") +
  theme_minimal() +
  theme(legend.position = "bottom")
# EDA: Boxplot to check for outliers
ggplot(melted_df, aes(x = family, y = total_sales, color = family)) +
  geom_boxplot() +
  labs(title = "Sales Distribution by Product Family", x = "Family", y = "Total Sales") +
  theme_minimal()





# Function to summarize decomposition components
summarize_decomposition <- function(decomposition, family_name) {
  trend <- decomposition$trend
  seasonal <- decomposition$seasonal
  random <- decomposition$random
  
  # Plot decompositions with titles
  plot(decomposition)
  title(main = paste("Decomposition of", family_name))
  
  list(
    Trend = trend,
    Seasonal = seasonal,
    Random = random
  )
}

# Decompose each product family's time series
decompositions <- lapply(processed_sales_data[-1], function(ts_data) {
  ts_object <- ts(ts_data, frequency = 7)  # Adjust frequency as needed
  decompose(ts_object)
})

# Name the decompositions list elements
names(decompositions) <- names(processed_sales_data)[-1]

# Apply summary function and interpret components
decomposition_summaries <- lapply(names(decompositions), function(family) {
  summarize_decomposition(decompositions[[family]], family)
})

# Name the decomposition summaries
names(decomposition_summaries) <- names(decompositions)






# Function to detect structural breaks in each product family's time series
detect_breaks <- function(time_series) {
  ts_data <- ts(time_series, frequency = 365)
  breakpoints(ts_data ~ 1)
}

# Detect structural breaks for each product family
breakpoints_list <- lapply(sorted_wide_df[-1], detect_breaks)
names(breakpoints_list) <- names(sorted_wide_df)[-1]


# Function to create a plot for breakpoints
plot_breakpoints <- function(bp, family_name) {
  plot(bp, main = paste("Breakpoints in", family_name, "Sales"))
}

# Plot breakpoints for each product family
par(mfrow = c(2, 3))  # Adjust the layout to fit multiple plots
for (family in names(breakpoints_list)) {
  plot_breakpoints(breakpoints_list[[family]], family)
}


# Calculate Z-score Normalization parameters
z_score_normalization <- function(data) {
  mean_val <- mean(data, na.rm = TRUE)
  sd_val <- sd(data, na.rm = TRUE)
  scaled_data <- (data - mean_val) / sd_val
  list(scaled_data = scaled_data, mean_val = mean_val, sd_val = sd_val)
}

# Normalize the data using Z-score Normalization
normalized_data_z_score <- lapply(sorted_wide_df[-1], function(column) {
  z_score_normalization(column)$scaled_data
})
processed_sales_data <- as.data.frame(normalized_data_z_score)
processed_sales_data$date <- sorted_wide_df$date
processed_sales_data <- data.frame(date = sorted_wide_df$date, normalized_data_z_score)

print(processed_sales_data)




# Save the preprocessed data to a CSV file
write.csv(processed_sales_data, "processed_sales_data.csv", row.names = FALSE)

# Print a message to indicate the file has been saved
cat("The processed data has been saved to 'processed_sales_data'\n")

# Load the processed sales data
processed_sales_data <- read.csv("processed_sales_data.csv")

# Convert 'date' column to Date format
processed_sales_data$date <- as.Date(processed_sales_data$date)

# View the structure of the data
str(processed_sales_data)

# Function to detect structural breaks in each product family's time series
detect_breaks <- function(time_series) {
  ts_data <- ts(time_series, frequency = 365)
  breakpoints(ts_data ~ 1)
}

# Detect structural breaks for each product family
breakpoints_list <- lapply(processed_sales_data[-1], detect_breaks)
names(breakpoints_list) <- names(processed_sales_data)[-1]

# Plot breakpoints function
plot_breakpoints <- function(breakpoints, family_name) {
  plot(breakpoints, main = "")
  lines(fitted(breakpoints), col = "red")
  title(main = paste("Breakpoints in", family_name, "Sales"), line = -1, cex.main = 1.2)
}

# Summarize and plot breakpoints after normalization
par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))  # Adjust layout for multiple plots
for (i in seq_along(breakpoints_list)) {
  family <- names(breakpoints_list)[i]
  print(summary(breakpoints_list[[family]]))
  plot_breakpoints(breakpoints_list[[family]], family)
}

install.packages("tseries")
library(tseries)


# Apply ADF test to each product family
adf_test_results <- lapply(processed_sales_data[-1], function(ts) {
  adf.test(ts)
})



# Print results
for (i in seq_along(adf_test_results)) {
  family <- names(adf_test_results)[i]
  cat("\n", family, "\n")
  print(adf_test_results[[i]])
}

# Function to difference a time series and perform the ADF test
make_stationary <- function(ts_data) {
  differenced_data <- diff(ts_data)
  adf_test_result <- adf.test(differenced_data)
  list(differenced_data = differenced_data, adf_test_result = adf_test_result)
}

# Apply differencing to non-stationary series
meats_stationary <- make_stationary(processed_sales_data$MEATS)
poultry_stationary <- make_stationary(processed_sales_data$POULTRY)
prepared_foods_stationary <- make_stationary(processed_sales_data$PREPARED.FOODS)

# Print the ADF test results after differencing
cat("\nMEATS after differencing\n")
print(meats_stationary$adf_test_result)

cat("\nPOULTRY after differencing\n")
print(poultry_stationary$adf_test_result)

cat("\nPREPARED FOODS after differencing\n")
print(prepared_foods_stationary$adf_test_result)


# Function to plot ACF and PACF with adjusted layout
plot_acf_pacf <- function(ts_data, title_suffix) {
  par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))  # Adjust margins for better layout
  acf(ts_data, main = "", xlab = "Lag", ylab = "Correlation")
  mtext(side = 3, line = 0.5, paste("ACF for", title_suffix), cex = 1.2)
  
  pacf(ts_data, main = "", xlab = "Lag", ylab = "Correlation")
  mtext(side = 3, line = 0.5, paste("PACF for", title_suffix), cex = 1.2)
  
  par(mfrow = c(1, 1))  # Reset layout to default
}

# Plot ACF and PACF for differenced MEATS
plot_acf_pacf(meats_stationary$differenced_data, "MEATS (Differenced)")

# Plot ACF and PACF for differenced POULTRY
plot_acf_pacf(poultry_stationary$differenced_data, "POULTRY (Differenced)")

# Plot ACF and PACF for differenced PREPARED FOODS
plot_acf_pacf(prepared_foods_stationary$differenced_data, "PREPARED FOODS (Differenced)")

# Function to plot ACF and PACF with adjusted layout and titles
plot_acf_pacf <- function(ts_data, title_suffix) {
  par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))  # Adjust margins for better layout
  acf(ts_data, main = "", xlab = "Lag", ylab = "Correlation")
  mtext(side = 3, line = 0.5, paste("ACF for", title_suffix), cex = 1.2)
  
  pacf(ts_data, main = "", xlab = "Lag", ylab = "Correlation")
  mtext(side = 3, line = 0.5, paste("PACF for", title_suffix), cex = 1.2)
  
  par(mfrow = c(1, 1))  # Reset layout to default
}

# Plot ACF and PACF for FROZEN FOODS
plot_acf_pacf(processed_sales_data$FROZEN.FOODS, "FROZEN FOODS (Original)")

# Plot ACF and PACF for LIQUOR, WINE, BEER
plot_acf_pacf(processed_sales_data$LIQUOR.WINE.BEER, "LIQUOR, WINE, BEER (Original)")

# Load the processed sales data
processed_sales_data <- read.csv("processed_sales_data.csv")




# Convert 'date' column to Date format
processed_sales_data$date <- as.Date(processed_sales_data$date)
processed_sales_data <- processed_sales_data %>%
  arrange(date)
print(head(processed_sales_data))
# Check the last date in your dataset
last_date <- max(processed_sales_data$date)
print(last_date)  

# Create a time series object for each product family
ts_poultry <- ts(processed_sales_data$POULTRY, frequency = 7)
ts_meats <- ts(processed_sales_data$MEATS, frequency = 7)
ts_liquor <- ts(processed_sales_data$LIQUOR.WINE.BEER, frequency = 7)
ts_prepared_foods <- ts(processed_sales_data$PREPARED.FOODS, frequency = 7)
ts_frozen_foods <- ts(processed_sales_data$FROZEN.FOODS, frequency = 7)

trn <- processed_sales_data$POULTRY



# Multivariate ARIMA ----

# Load required libraries
install.packages(c("pacman", "factoextra", "fastDummies", "strucchange", "tseries", "dummy", "forecast"))
library(pacman)

pacman::p_load(
  tidyverse,
  forecast,
  dplyr,
  factoextra,
  dummy,
  fastDummies,
  ggplot2,
  plotly,
  tidyr,
  lubridate,
  ggfortify,
  strucchange,
  urca
)


# Load the processed sales data
processed_sales_data <- read.csv("processed_sales_data.csv")

# Convert 'date' column to Date format
processed_sales_data$date <- as.Date(processed_sales_data$date)

# Prepare the data matrix
sales_data <- processed_sales_data %>% select(-date)  # Exclude date column

# Define function to fit ARIMA model and forecast
forecast_arima <- function(data, steps = 15) {
  forecast_list <- vector("list", length(data))
  
  for (i in seq_along(data)) {
    ts_data <- ts(data[[i]], frequency = 7)  # Assuming weekly data
    fit <- tryCatch(auto.arima(ts_data), error = function(e) NULL)
    
    if (is.null(fit)) {
      # Fallback to a simpler ARIMA model if auto.arima fails
      fit <- Arima(ts_data, order = c(1, 1, 1))
    }
    
    forecast_list[[i]] <- forecast(fit, h = steps)$mean
  }
  
  forecasted_values <- do.call(cbind, forecast_list)
  colnames(forecasted_values) <- colnames(data)
  return(forecasted_values)
}

# Perform the forecast
forecasted_sales <- forecast_arima(sales_data, steps = 15)

# Specify the forecast dates from 16/08/2017 to 30/08/2017
forecast_dates <- seq(as.Date("2017-08-16"), by = "day", length.out = 15)
forecasted_df <- data.frame(date = forecast_dates, forecasted_sales)

# View the forecasted data
View(forecasted_df)

# Convert the forecasted sales data to a long format for printing
forecasted_sales_df <- forecasted_df %>%
  pivot_longer(cols = -date, names_to = "family", values_to = "sales")

# Print the forecasted sales values in a tidy format
print("Forecasted Sales Values:")
print(forecasted_sales_df)

# Plot forecasted sales for each family using ggplot2
ggplot() +
  geom_line(data = forecasted_sales_df, aes(x = date, y = sales, color = family), linetype = "dashed") +
  facet_wrap(~family, scales = "free_y") +
  labs(title = "Forecasted Sales for Each Family", x = "Date", y = "Sales") +
  scale_color_manual(values = c("Forecasted" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())

#PREDICTION

# Install required libraries
install.packages(c("pacman", "vars"))

# Load required libraries
library(pacman)
pacman::p_load(
  tidyverse,
  forecast,
  dplyr,
  factoextra,
  dummy,
  fastDummies,
  ggplot2,
  plotly,
  tidyr,
  lubridate,
  ggfortify,
  strucchange,
  urca,
  vars
)

# Load the processed sales data
processed_sales_data <- read.csv("processed_sales_data.csv")

# Convert 'date' column to Date format
processed_sales_data$date <- as.Date(processed_sales_data$date)

# Prepare the data matrix
sales_data <- processed_sales_data %>% select(-date)  # Exclude date column
sales_data <- dplyr::select(processed_sales_data, -date)
# Fit the VAR model
var_model <- VAR(sales_data, p = 1, type = "const")

# Check the summary of the fitted model
summary(var_model)
# Evaluate the model using AIC
aic_value <- AIC(var_model)
print(paste("AIC Value: ", aic_value))

# Perform serial test on residuals
serial_test_results <- serial.test(var_model, lags.pt = 12, type = "PT.asymptotic")
print(serial_test_results)

# Extract residuals
residuals <- residuals(var_model)

# Check residuals mean
residual_means <- colMeans(residuals)
print(residual_means)

# Check residuals distribution
par(mfrow = c(2, 3))
for (i in 1:ncol(residuals)) {
  hist(residuals[, i], main = colnames(residuals)[i], xlab = "Residuals", col = "lightblue")
}

# Check residuals correlation
par(mfrow = c(2, 3))
for (i in 1:ncol(residuals)) {
  acf(residuals[, i], main = paste("ACF of", colnames(residuals)[i]), lag.max = 20)
}