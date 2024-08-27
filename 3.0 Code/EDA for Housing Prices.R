# Load Required Libraries
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(stringr)

# Define File Path
housing_file_path <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/1.0 Housing Prices/cleaned-housing-prices-2020-2023.csv"

# Read the Cleaned Data
housing_data <- read_csv(housing_file_path)


# Average House Price From (2020 - 2023) – (Line Chart) – For both counties
avg_price_yearly <- housing_data %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year, County) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE)) %>%
  mutate(Year = ifelse(is.na(Year), "2023", Year))



ggplot(avg_price_yearly, aes(x = Year, y = Avg_Price, color = County, group = County)) +
  geom_line() +
  geom_point() +
  labs(title = "Average House Price From (2020 - 2023) – (Line Chart) – For Both Counties", x = "Year", y = "Average Price") +
  theme_minimal()



# Filtering the data for the Year 2022
data_2022 <- housing_data %>%
  filter(format(Date, "%Y") == "2022")


# Average Price by Town/City and County
avg_price_town_2022 <- data_2022 %>%
  mutate(`Town/City` = str_to_title(`Town/City`)) %>%  # Capitalize the start of each word
  group_by(County, `Town/City`) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE)) %>%
  ungroup()

# Back-to-Back Bar Chart Split by County
ggplot(avg_price_town_2022, aes(x = `Town/City`, y = Avg_Price, fill = County)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Average House Price in Year 2022 by Town/City (Back-to-Back Bar Chart)",
       x = "Town/City",
       y = "Average Price",
       fill = "County") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  facet_wrap(~ County, scales = "free_y") +
  scale_y_continuous(labels = label_number(scale = 0.001, suffix = "K", big.mark = ","))


# Average House Price in Year 2022 (Boxplot) – For Both Counties
ggplot(data_2022, aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_number(scale = 0.001, suffix = "K", big.mark = ",")) +
  labs(title = "Average House Price in Year 2022 (Boxplot) – For Both Counties", 
       x = "County", 
       y = "Price") +
  theme_minimal()

# Average House Price in Year 2022 (Bar Chart) – For Both Counties
ggplot(avg_price_town_2022, aes(x = County, y = Avg_Price, fill = County)) +
  geom_bar(stat = "identity") +  labs(title = "Average House Price in Year 2022 by County",
       x = "County",
       y = "Average Price") + scale_y_continuous(labels = label_number(scale = 0.001, suffix = "K", big.mark = ",")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5))



# Histogram of Frequency of House Prices in 2022
ggplot(data_2022, aes(x = Price, fill = County)) +
  geom_histogram(binwidth = 10000, alpha = 0.7, position = "identity") +
  facet_wrap(~ County, scales = "free_x") +
  scale_x_continuous(labels = label_number(scale = 0.001, suffix = "K", big.mark = ",")) +
  labs(title = "Histogram of Frequency of House Prices in 2022",
       x = "Price",
       y = "Frequency",
       fill = "County") +
  theme_minimal()
