# Load Required Libraries
library(dplyr)
library(readr)
library(ggplot2)

# Defining Folder Paths for Cleaned and Uncleaned Dataset
folder_path_uncleaned <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data/1.0 Housing Prices/"
folder_path_cleaned <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/1.0 Housing Prices/"
postcode_folder_path <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data"
bristol_folder_path <- "D:/ST5014CEM-Data-Science-For-Developers/1.0 Obtain Data"

# Defining File Names for Cleaned and Uncleaned Dataset
file_name_uncleaned <- "housing-prices-2020-2023.csv"
file_name_cleaned <- "cleaned-housing-prices-2020-2023.csv"
postcode_file_name <- "filtered_postcode_to_lsoa_data.csv"
lsoa_lookup_file_name <- "LSOA_lookup.csv"

# Reading the Uncleaned Data
uncleaned_data <- read_csv(file.path(folder_path_uncleaned, file_name_uncleaned))

# Showing the Structure of the Uncleaned Data before Cleaning
str(uncleaned_data)

View(uncleaned_data)

# Removing 'Not Required' 1-5 Columns
cleaned_data <- uncleaned_data %>%
  select(-Category, -`Not Required 1`, -`Not Required 2`, -`Not Required 3`, -`Not Required 4`, -`Not Required 5`)

# Checking Missing Values in each Column
missing_values <- sapply(cleaned_data, function(x) sum(is.na(x)))
print(missing_values)

# Removing Rows with missing Price, Postcode, County, Town/City
cleaned_data <- cleaned_data %>%
  filter(!is.na(Price) & !is.na(County) & !is.na(`Town/City`) & !is.na(`Postcode`))

# Ensuring Data Consistency by converting Price to numeric and Date to Date
cleaned_data <- cleaned_data %>%
  mutate(Price = as.numeric(Price), Date = as.Date(Date, format = "%Y-%m-%d"))

# Checking for Blank Values and Replacing with 'NA'
cleaned_data[cleaned_data == ""] <- NA

# Rechecking Missing Values
missing_values_after_cleaning <- sapply(cleaned_data, function(x) sum(is.na(x)))
print(missing_values_after_cleaning)

# Filtering Bristol or Cornwall in the County Column
filtered_data <- cleaned_data %>%
  filter(grepl("Bristol", County, ignore.case = TRUE) | grepl("Cornwall", County, ignore.case = TRUE))

# View filtered data
View(filtered_data)

# Box-plotting Prices in Bristol and Cornwall
ggplot(filtered_data, aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +
  labs(title = "Comparison of Housing Prices: Bristol vs Cornwall", x = "County", y = "Price")

# Identifying and Removing Outliers using IQR method
Q1 <- quantile(cleaned_data$Price, 0.25, na.rm = TRUE)
Q3 <- quantile(cleaned_data$Price, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
outlier_threshold <- 1.5 * IQR

# Filter out outliers
cleaned_data <- cleaned_data %>%
  filter(Price >= (Q1 - outlier_threshold) & Price <= (Q3 + outlier_threshold))

# Filtering the data to keep only rows from Bristol or Cornwall
cleaned_data_bc <- cleaned_data %>%
  filter(grepl("Bristol", County, ignore.case = TRUE) | grepl("Cornwall", County, ignore.case = TRUE))

# Remove columns 'PAON' and 'SAON'
cleaned_data_bc <- cleaned_data_bc %>%
  select(-PAON, -SAON)

# Replacing 'CITY OF BRISTOL' with 'Bristol' and 'CORNWALL' with 'Cornwall'
cleaned_data_bc <- cleaned_data_bc %>%
  mutate(County = recode(County, "CITY OF BRISTOL" = "Bristol", "CORNWALL" = "Cornwall"))

# Check the first few rows of the data to inspect values
head(cleaned_data_bc)

# Box-plotting Prices in Bristol and Cornwall
ggplot(cleaned_data_bc, aes(x = County, y = Price, fill = County)) +
  geom_boxplot() +
  labs(title = "Comparison of Housing Prices: Bristol vs Cornwall", x = "County", y = "Price")

# Showing the 10 Highest Priced Town/City in Bristol and Cornwall
cat("10 Highest Priced Town/City in Bristol and Cornwall:\n")
print(filtered_data %>% 
        group_by(`Town/City`) %>% 
        summarise(Avg_Price = mean(Price, na.rm = TRUE)) %>% 
        arrange(desc(Avg_Price)) %>% 
        head(10))

# Showing the 10 Lowest Priced Town/City in Bristol and Cornwall
cat("10 Lowest Priced Town/City in Bristol and Cornwall:\n")
print(cleaned_data_bc %>% 
        group_by(`Town/City`) %>% 
        summarise(Avg_Price = mean(Price, na.rm = TRUE)) %>% 
        arrange(Avg_Price) %>% 
        head(10))

# Remove the row with 'London' and Avg_price = 750 from cleaned_data_bc
cleaned_data_bc <- cleaned_data_bc %>%
  filter(!(`Town/City` == "LONDON" & Price == 750))

# Showing the 10 Lowest Priced Town/City in Bristol and Cornwall
cat("10 Lowest Priced Town/City in Bristol and Cornwall:\n")
print(cleaned_data_bc %>% 
        group_by(`Town/City`) %>% 
        summarise(Avg_Price = mean(Price, na.rm = TRUE)) %>% 
        arrange(Avg_Price) %>% 
        head(10))

# Checking Density Distribution for Bristol and Cornwall
ggplot(cleaned_data_bc, aes(x = Price, fill = County)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ County) +
  labs(title = "Density Distribution: Bristol vs Cornwall", x = "Price", y = "Density")

# Reading the Postcode to LSOA Data
postcode_to_lsoa_data <- read_csv(file.path(postcode_folder_path, postcode_file_name))

# Left Join with cleaned_data_bc on 'Postcode' columns to retain all rows from cleaned_data_bc
merged_data <- left_join(cleaned_data_bc, postcode_to_lsoa_data, by = c("Postcode" = "pcds"))

# Rename 'lsoa11cd' to 'LSOA' and remove 'ladnm' column
merged_data <- merged_data %>%
  rename(LSOA = lsoa11cd) %>%
  select(-ladnm)

# Read the LSOA Lookup Data
lsoa_lookup_data <- read_csv(file.path(bristol_folder_path, lsoa_lookup_file_name))

# Check if the data is loaded
if (is.null(lsoa_lookup_data)) {
  stop("Failed to read the LSOA lookup data.")
}

# Select only the relevant columns from lsoa_lookup_data
lsoa_lookup_data <- lsoa_lookup_data %>%
  select(`LSOA code (2011)`, `2017 Ward name bestfit based on Population Weighted Centroid`)

# Join with merged_data on LSOA Code
final_data <- left_join(merged_data, lsoa_lookup_data, by = c("LSOA" = "LSOA code (2011)"))

# Rename 'LSOA11 local name' to 'Town Name'
final_data <- final_data %>%
  rename(`Town Name` = `2017 Ward name bestfit based on Population Weighted Centroid`)

# Replace `Town/City` in merged_data with `Town Name` from final_data
merged_data <- merged_data %>%
  left_join(final_data %>% select(Postcode, `Town Name`), by = "Postcode") %>%
  mutate(`Town/City` = coalesce(`Town Name`, `Town/City`)) %>%
  select(-`Town Name`)

# View the final data to confirm
View(merged_data)

# Saving the Merged Data to the Cleaned Dataset File Path
output_file_path_cleaned <- file.path(folder_path_cleaned, file_name_cleaned)
write_csv(merged_data, output_file_path_cleaned, col_names = TRUE)

# Confirming File Saved Message
cat("Merged data for Bristol and Cornwall saved to:", output_file_path_cleaned, "\n")