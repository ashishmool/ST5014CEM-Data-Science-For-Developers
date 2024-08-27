# Load Required Libraries
library(dplyr)
library(ggplot2)
library(readr)
library (stringr)

# Define File Paths for Cleaned Dataset and Postcode to LSOA Data
folder_path_cleaned <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data/3.0 Broadband Speed/"
file_name_cleaned <- "cleaned-combined-broadband-speed.csv"
postcode_folder_path <- "D:/ST5014CEM-Data-Science-For-Developers/2.0 Clean Data"

# Read the Cleaned Broadband Speed Data
cleaned_broadband_data <- read_csv(file.path(folder_path_cleaned, file_name_cleaned))

cleaned_broadband_data <- cleaned_broadband_data %>%
  mutate(`Town/City` = str_to_title(`Town/City`))

# Average Download Speeds in Both Counties (Boxplot)
ggplot(cleaned_broadband_data, aes(x = ladnm, y = `Average download speed (Mbit/s)`, fill = ladnm)) +
  geom_boxplot() +
  labs(title = "Average Download Speeds in Bristol and Cornwall (Boxplot)",
       x = "County", 
       y = "Average Download Speed (Mbit/s)") +
  theme_minimal()

# Average Download Speeds in Both Counties (Bar Chart)
avg_download_speeds <- cleaned_broadband_data %>%
  group_by(ladnm) %>%
  summarise(Avg_Download_Speed = mean(`Average download speed (Mbit/s)`, na.rm = TRUE))

ggplot(avg_download_speeds, aes(x = ladnm, y = Avg_Download_Speed, fill = ladnm)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Download Speeds in Bristol and Cornwall (Bar Chart)",
       x = "County", y = "Average Download Speed (Mbit/s)", fill = "County") +
  theme_minimal()

# Maximum Download Speeds in Both Counties (Bar Chart)
max_download_speeds <- cleaned_broadband_data %>%
  group_by(ladnm) %>%
  summarise(Max_Download_Speed = max(`Maximum download speed (Mbit/s)`, na.rm = TRUE))

ggplot(max_download_speeds, aes(x = ladnm, y = Max_Download_Speed, fill = ladnm)) +
  geom_bar(stat = "identity") +
  labs(title = "Maximum Download Speeds in Bristol and Cornwall (Bar Chart)",
       x = "County", y = "Maximum Download Speed (Mbit/s)", fill="County") +
  theme_minimal()

# Summarize the User Count by Town/City and County
user_count_by_town <- cleaned_broadband_data %>%
  group_by(ladnm, `Town/City`) %>%
  summarise(User_Count = n())

# Plot: Broadband Users Count by Town/City and County
ggplot(user_count_by_town, aes(x = `Town/City`, y = User_Count, fill = ladnm)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Broadband Users Count by Town/City and County",
       x = "Town/City", y = "User Count", fill = "County") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ ladnm, scales = "free_x", ncol = 1)

# Scatter Plot: Average Data Usage vs. Maximum Download Speed (Mbit/s)
ggplot(cleaned_broadband_data, aes(x = `Average data usage (GB)`, y = `Maximum download speed (Mbit/s)`, color = ladnm)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Average Data Usage vs. Maximum Download Speed",
       x = "Average Data Usage (GB)", y = "Maximum Download Speed (Mbit/s)", color = "County") +
  theme_minimal()

# Remove the outlier in Cornwall with Average Data Usage near 40000
cleaned_broadband_data_filtered <- cleaned_broadband_data %>%
  filter(!(ladnm == "Cornwall" & `Average data usage (GB)` > 2000))%>%
  filter(!(ladnm == "Bristol" & `Average data usage (GB)` > 2000))

# Re-Plot: Average Data Usage vs. Maximum Download Speed (Mbit/s)
ggplot(cleaned_broadband_data_filtered, aes(x = `Average data usage (GB)`, y = `Maximum download speed (Mbit/s)`, color = ladnm)) +
  geom_point(alpha = 0.7, size = 3) + coord_flip() +
  labs(title = "Average Data Usage vs. Maximum Download Speed (After Removing Outlier)",
       x = "Average Data Usage (GB)", y = "Maximum Download Speed (Mbit/s)", color = "County") +
  theme_minimal()
