# Install Packages
#install.packages("googledrive")

# Load Libraries
library(tidyverse)
library(janitor)
library(googledrive)

# Connect to Google Drive.  Place the email address you use to reach the shared drive. 
email = "blakerma@ncsu.edu"
drive_auth(email)

# Imports dataset to R environment; use clean_names to coerce col_names for R
Employer_Data <- read_csv("Employer_Dentistry_Survey.csv", skip = 2) %>%
  clean_names()

# Add grouping identifier now so if we combine data later, we have our groups delineated. 
Employer_Data <- Employer_Data %>%
  mutate(
    group = "Employer", #Categorizes all responses as Employer
    respondent_id = row_number() #Maintains respondent ID
  )



# 2. Identify multi-response columns
multi_response_cols <- Employer_Data %>% 
  select(matches("^qid(4|5|7|10|11|12|13|14|16|17|18|19|20|21|22|24|26|27|28|29|30|33|34|353|36|37)_\\d+$")) %>%
  names()


# 3. Pivot to long format
Employer_Long <- Employer_Data %>%
  pivot_longer(
    cols = all_of(multi_response_cols),
    names_to = "question_item",
    values_to = "response"
  ) %>%
  filter(!is.na(response) & response != "")


Employer_Long



