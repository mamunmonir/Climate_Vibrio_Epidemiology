# Load necessary libraries
install.packages(c("ggplot2", "forecast", "tseries", "zoo"))
library(ggplot2)
library(forecast)
library(tseries)
library(zoo)
setwd("/Users/mdmamunmonir/MAC_DRIVE/DOC/MS_hospital_data")
# Create example monthly data
set.seed(123)

months <- seq(as.Date("2000/1/1"), as.Date("2020/12/1"), by = "month")
cases <- round(100 + 20 * sin(2 * pi * (1:252) / 12) + rnorm(252, 0, 10))  # Adding seasonality and some noise
cholera_data <- data.frame(Date = months, Cases = cases)

# Save as CSV for consistency with previous steps
#write.csv(cholera_data, "example_case_data.csv", row.names = FALSE)

# Load the data (simulating real-world scenario)
cholera_data <- read.csv("example_case_data.csv")
head(cholera_data)

# Convert data to time series
start_year <- 2000
start_month <- 1
cholera_ts <- ts(cholera_data$Cases, start = c(start_year, start_month), frequency = 12)

# Plot the time series
plot(cholera_ts, main = "Monthly Cholera Cases", xlab = "Year", ylab = "Cases")

# Decompose the time series
decomposed_ts <- decompose(cholera_ts)
plot(decomposed_ts)

# Stationarity test
adf.test(cholera_ts)

# Difference the data if not stationary
diff_cholera_ts <- diff(cholera_ts)
plot(diff_cholera_ts, main = "Differenced Monthly Cholera Cases", xlab = "Year", ylab = "Differenced Cases")
adf.test(diff_cholera_ts)

# Fit a seasonal ARIMA model
fit <- auto.arima(cholera_ts, seasonal = TRUE)
summary(fit)

# Forecast future values
forecasted_values <- forecast(fit, h = 24) # Forecast for the next 24 months (2 years)
plot(forecasted_values, main = "Forecasted Monthly Cholera Cases")

# Manually check residuals

# 1. Plot the residuals
residuals <- residuals(fit)
plot(residuals, main = "Residuals of the ARIMA Model", ylab = "Residuals", xlab = "Time")

# 2. Perform Ljung-Box test
Box.test(residuals, lag = 20, type = "Ljung-Box")

# 3. Plot ACF of residuals
acf(residuals, main = "ACF of Residuals")

# Optionally, perform additional diagnostic plots
tsdiag(fit)
