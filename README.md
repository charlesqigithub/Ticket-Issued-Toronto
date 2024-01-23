# Ticket-Issued-Toronto
library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("path/to/your/Tickets_Issued_(ASR-ENF-TBL-002).csv")

# 1. Trend Analysis
trend_analysis <- data %>%
  group_by(OFFENCE_YEAR) %>%
  summarise(Total_Tickets = sum(TICKET_COUNT))

ggplot(trend_analysis, aes(x = OFFENCE_YEAR, y = Total_Tickets)) +
  geom_line() +
  labs(title = "Ticket Counts by Year", x = "Year", y = "Total Tickets")

# 2. Division Analysis
division_analysis <- data %>%
  group_by(DIVISION) %>%
  summarise(Total_Tickets = sum(TICKET_COUNT)) %>%
  arrange(desc(Total_Tickets))

ggplot(division_analysis, aes(x = reorder(DIVISION, Total_Tickets), y = Total_Tickets)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Ticket Counts by Police Division", x = "Police Division", y = "Total Tickets")

# 3. Offence Category Analysis
category_analysis <- data %>%
  group_by(OFFENCE_CATEGORY) %>%
  summarise(Total_Tickets = sum(TICKET_COUNT)) %>%
  arrange(desc(Total_Tickets))

ggplot(category_analysis, aes(x = reorder(OFFENCE_CATEGORY, Total_Tickets), y = Total_Tickets)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Ticket Counts by Offence Category", x = "Offence Category", y = "Total Tickets")

# 4. Age Group Analysis
age_group_analysis <- data %>%
  group_by(AGE_GROUP) %>%
  summarise(Total_Tickets = sum(TICKET_COUNT)) %>%
  arrange(desc(Total_Tickets))

ggplot(age_group_analysis, aes(x = reorder(AGE_GROUP, Total_Tickets), y = Total_Tickets)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Ticket Counts by Age Group", x = "Age Group", y = "Total Tickets")

# 5. Neighborhood Analysis
neighborhood_analysis <- data %>%
  group_by(NEIGHBOURHOOD_158) %>%
  summarise(Total_Tickets = sum(TICKET_COUNT)) %>%
  top_n(10, Total_Tickets)

ggplot(neighborhood_analysis, aes(x = reorder(NEIGHBOURHOOD_158, Total_Tickets), y = Total_Tickets)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Neighborhoods by Ticket Counts", x = "Neighborhood", y = "Total Tickets")
