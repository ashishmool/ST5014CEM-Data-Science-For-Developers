# Load Required Libraries
library(dplyr)
library(readr)
library(scales)

# Define File Path for the Cleaned Broadband Speed Data
broadband_file_path <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/3.0 Broadband Speed/cleaned-combined-broadband-speed.csv"

# Read the Cleaned Broadband Speed Data
broadband_data <- read_csv(broadband_file_path)



# Calculate Average Download Speed by Town/City and County
avg_speed_town <- broadband_data %>%
  rename(County = ladnm) %>%
  group_by(County, `Town/City`) %>%
  summarise(Avg_Download_Speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE)) %>%
  ungroup()

# Create Broadband Speed Score: Higher Speed corresponds to a Higher Score
# We will normalize the speeds directly to create the score.
avg_speed_town <- avg_speed_town %>%
  mutate(BroadbandScore = rescale(Avg_Download_Speed, to = c(0, 10)))

# Custom function to capitalize each word in a string
capitalize_words <- function(x) {
  sapply(strsplit(x, " "), function(y) paste(toupper(substring(y, 1, 1)), tolower(substring(y, 2)), sep = "", collapse = " "))
}

# Ensure 'Town/City' column has each word capitalized
avg_speed_town <- avg_speed_town %>%
  mutate(`Town/City` = capitalize_words(`Town/City`)) %>%
  # Remove duplicate entries based on 'Town/City'
  distinct(`Town/City`, .keep_all = TRUE)


# Select only the relevant columns for the final dataset
final_broadband_score <- avg_speed_town %>%
  select(`Town/City`, County, Avg_Download_Speed, BroadbandScore)

# Display the top 10 towns/cities with the highest Broadband Scores
top_10_broadband_score <- final_broadband_score %>%
  arrange(desc(BroadbandScore)) %>%
  head(10)

# Print the top 10 towns/cities
print(top_10_broadband_score)

# View the top 10 broadband scores in a table
View(top_10_broadband_score)

# Save the combined scores to a CSV file
write_csv(final_broadband_score, "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/BroadbandScores.csv")
