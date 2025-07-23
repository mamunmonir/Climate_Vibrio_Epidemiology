# Install these packages if you haven't already
# install.packages("EpiEstim")
# install.packages("dplyr")
# install.packages("ggplot2")

library(EpiEstim)
library(dplyr)
library(ggplot2)


cholera<-read.csv("cholera_cases_daily.csv", header=TRUE)

# Load cholera cases data
cholera_data <- read.csv("cholera_cases_daily.csv", header=TRUE)
cholera_data$Date <- as.Date(cholera_data$Date, format="%Y-%m-%d")
cholera_data$Year <- as.numeric(format(cholera_data$Date, "%Y"))

# Set up the serial interval for cholera (mean and SD based on literature)
serial_interval <- c(15, 7)  # Mean = 5 days, SD = 2 days


cholera_data$Cases[is.na(cholera_data$Cases)] <- 0
# Estimate Rt
Rt_estimates <- estimate_R(
  incid = cholera_data$Cases,
  method = "parametric_si",
  config = make_config(list(mean_si = serial_interval[1], std_si = serial_interval[2]))
)

cholera_data$Rt <- NA  # Initialize Rt column

# Calculate the number of leading NA values to account for the estimation process
na_leading <- length(cholera_data$Cases) - nrow(Rt_estimates$R)

# Assign Rt values based on the estimation
cholera_data$Rt[(na_leading + 1):length(cholera_data$Rt)] <- Rt_estimates$R[, "Mean(R)"]  # or use "Quantile.0.5(R)" for median

ggplot(cholera_data, aes(x = Date)) +
  geom_line(aes(y = Rt, color = "Estimated R_t"), size = 1) +
  geom_point(aes(y = Rt), color = "blue") +
  labs(title = "Effective Reproduction Number (R_t) for Cholera",
       x = "Date",
       y = "R_t") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = "blue") +
  scale_x_date(date_breaks = "1 month",    # Set major ticks to every month
               date_labels = "%Y-%m") +    # Format labels to show Year-Month
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
