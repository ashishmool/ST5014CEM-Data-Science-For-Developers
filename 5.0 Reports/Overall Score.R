# Load necessary libraries
library(dplyr)
library(readr)

# Read the CSV files
housing_data <- read_csv("D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/HousingPriceScores.csv")
crime_rate_town <- read_csv("D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/CrimeScores.csv")
broadband_data <- read_csv("D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/BroadbandScores.csv")
cleaned_school_data <- read_csv("D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/EducationScores.csv")


# Check the structure of the updated data frames
str(housing_data)
str(crime_rate_town)
str(broadband_data)
str(cleaned_school_data)

View(housing_data)
View(crime_rate_town)
View(broadband_data)
View(cleaned_school_data)


# Combine all scores into a single dataset
combined_scores <- housing_data %>%
  inner_join(crime_rate_town, by = c("Town/City", "County")) %>%
  inner_join(broadband_data, by = c("Town/City", "County")) %>%
  inner_join(cleaned_school_data, by = c("Town/City", "County")) %>%
  mutate(OverallScore = (HouseScore + CrimeRateScore + BroadbandScore+EducationScore) / 4) %>%
  select(`Town/City`, County, HouseScore, CrimeRateScore, BroadbandScore, EducationScore, OverallScore) %>%
  arrange(desc(OverallScore))

# Display the top 10 towns/cities with the highest Overall Scores
top_10_overall_score <- combined_scores %>%
  head(10)

# Print the top 10 towns/cities
print(top_10_overall_score)

# View the top 10 overall scores in a table
View(top_10_overall_score)
