# Time Series Forecasting of Sales Data using R

## Overview
This project involves time series forecasting of sales data for different product families using R. The goal is to predict future sales based on historical data, identify trends, seasonal patterns, and anomalies.

## Data
The dataset includes sales data for product families such as Frozen Foods, Liquor, Wine, Beer, Meats, Poultry, and Prepared Foods. Data preprocessing steps include normalization, handling missing values, and differencing to achieve stationarity.

### Data Files
- `data/denormalized_forecast_values.csv`: Forecasted sales values.
- `data/denormalized_sales_data.csv`: Denormalized sales data.
- `data/processed_sales_data.csv`: Preprocessed sales data.
- `data/sample1_sales_timeseries.csv`: Sample time series data for sales.
- `data/sample2_sales_timeseries.csv`: Sample time series data for sales.
- `data/sample3_sales_timeseries.csv`: Sample time series data for sales.
- `data/sample4_sales_timeseries.csv`: Sample time series data for sales.
- `data/sample5_sales_timeseries.csv`: Sample time series data for sales.
- `data/var_model_residuals.csv`: Residuals from the VAR model.

## Methodology
1. **Data Preprocessing**:
   - Normalization
   - Handling Missing Values
   - Differencing
   - Smoothing and Decomposition

2. **Exploratory Data Analysis (EDA)**:
   - Visualization of sales trends over time
   - Analysis of seasonal patterns and anomalies

3. **Modeling**:
   - Augmented Dickey-Fuller (ADF) Test for stationarity
   - ARIMA and VAR models for forecasting
   - Evaluation using AIC and residual analysis

## How to Run the Project
### Prerequisites
- R and RStudio installed on your system.
- Required R packages installed. You can install necessary packages by running:
  ```R
  install.packages(c("forecast", "tseries", "ggplot2", "dplyr", "readr"))
