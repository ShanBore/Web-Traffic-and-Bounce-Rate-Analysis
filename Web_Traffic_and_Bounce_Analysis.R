# Install and load necessary libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("lubridate")

library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

# Load the dataset
data <- read_csv("web_traffic_data.csv")

# Preview data
head(data)


# Check for missing values
sum(is.na(data))

# Get an overview of the dataset
glimpse(data)

# Convert 'Date' to Date-Time format
data$Date <- ymd_hms(data$Date)

# Check summary
summary(data)

library(dplyr)

# Summarize bounce rate by traffic source
traffic_summary <- data %>%
  group_by(Traffic_Source) %>%
  summarise(
    Avg_Bounce_Rate = mean(Bounce_Rate, na.rm = TRUE),
    Avg_Session_Duration = mean(Avg_Session_Duration, na.rm = TRUE),
    Total_Sessions = sum(Sessions)
  )

# View the summary
print(traffic_summary)

# Bar chart of average bounce rate by traffic source
ggplot(traffic_summary, aes(x = Traffic_Source, y = Avg_Bounce_Rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Bounce Rate by Traffic Source", 
       x = "Traffic Source", 
       y = "Bounce Rate (%)") +
  theme_minimal()

# Bar chart of average session duration by traffic source
ggplot(traffic_summary, aes(x = Traffic_Source, y = Avg_Session_Duration)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Average Session Duration by Traffic Source", 
       x = "Traffic Source", 
       y = "Session Duration (seconds)") +
  theme_minimal()

# Summarize bounce rate by device category and traffic source
device_traffic_summary <- data %>%
  group_by(Device_Category, Traffic_Source) %>%
  summarise(
    Avg_Bounce_Rate = mean(Bounce_Rate),
    Avg_Session_Duration = mean(Avg_Session_Duration)
  )

# View the summary
print(device_traffic_summary)

# Visualize bounce rate across devices
ggplot(device_traffic_summary, aes(x = Device_Category, y = Avg_Bounce_Rate, fill = Traffic_Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bounce Rate by Device Category and Traffic Source", 
       x = "Device Category", 
       y = "Bounce Rate (%)") +
  theme_minimal()

# Perform ANOVA to compare bounce rates across traffic sources
anova_result <- aov(Bounce_Rate ~ Traffic_Source, data = data)
summary(anova_result)

# Perform t-test to compare bounce rate between Mobile and Desktop
t_test_result <- t.test(Bounce_Rate ~ Device_Category, 
                        data = filter(data, Device_Category %in% c('Mobile', 'Desktop')))
print(t_test_result)

