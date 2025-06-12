#Install Packages
#install.packages("tidyverse")
#install.packages("janitor")
#install.packages("gtools")
#install.packages("naniar")

#Load Libraries
library(tidyverse)
library(janitor)
library(gtools)
library(naniar)

#############################################
## Connect, Import, group and primary key. ##
#############################################

# Read data into R environment. 
Employer_Data <- read_csv("Employer_Dentistry_Survey.csv") %>% 
  clean_names()
Employer_Data_Clean <- Employer_Data

# Add grouping identifier now so if we combine data later, we have our groups delineated. 
Employer_Data_Clean <- Employer_Data_Clean %>%
  mutate(
    group = "Employer", #Categorizes all responses as Employer
    respondent_id = row_number() #Maintains respondent ID
  )

############################
#####  DATA WRANGLING  #####
############################

# LIKERT SCALE
#Question 4   (Binarize)
#Question 17  (Binarize)

# SELECT ALL THAT APPLY
#Question 16  (Widen)

# TEXT
#Question 09
#Question 22 (Related:  Question 23)
#Question 24

# Adjust headers for readability and add grouping identifier.
# Remove redundancy in column names, replace "quid" with "Q", all "_text" with "T". 
colnames(Employer_Data_Clean) <- gsub("_text$", "_T", gsub("q", "Q", colnames
(Employer_Data_Clean)))

# Format the column headers so that it is sort-able both by question and sub question. 
pad_question_ids <- function(col_names) {
  col_names %>%
    str_replace_all("Q(\\d{1})(?!\\d)", "Q0\\1") %>%
    str_replace_all("Q(\\d{2})_(\\d{1})(?!\\d)", "Q\\1_0\\2")
}
# Apply to your data frame column names
colnames(Employer_Data_Clean) <- pad_question_ids(colnames(Employer_Data_Clean))



##### FILTER DATASET #####

# Remove uncertain/problematic columns while awaiting clarification
Employer_Data_Clean <- Employer_Data_Clean %>%
  select(
    ip_address, progress, duration_in_seconds,
    finished, recorded_date, response_id, group, respondent_id,
    starts_with("Q04"),
    starts_with("Q09"),
    starts_with("Q16"),
    starts_with("Q17"),
    starts_with("Q22"),
    starts_with("Q24")
  )

# Remove specified record_id rows (corresponds to rows where multiple answers were given but not appropriate to the question)
Employer_Data_Clean <- Employer_Data_Clean %>%
  slice(-1, -2)


##### Making corrections to question formatting #####

# Question #04
# Renames the text response to index 13 and places it in its order. 
Employer_Data_Clean <- Employer_Data_Clean %>%
  rename(Q04_13 = Q04_12_T) %>%
  relocate(Q04_13, .after = Q04_12)

# Question #09 does not need correcting. 

# Question #16
# Add column for unanswered response #9 to account for no responses.
Employer_Data_Clean <- Employer_Data_Clean %>%
  mutate(Q16_09 = 0)

# Change column name from _10_T to _11 for readability. 
Employer_Data_Clean <- Employer_Data_Clean %>%
  rename(Q16_11 = Q16_10_T)

# Process Q16: Split responses and pivot to wide format
Employer_Data_Clean <- Employer_Data_Clean %>%
  # Create a temporary respondent ID if not already done
  mutate(respondent_id = row_number()) %>%
  # Separate comma-separated values into long format
  separate_rows(Q16, sep = ",") %>%
  # Trim whitespace if any
  mutate(Q16 = str_trim(Q16)) %>%
  # Filter out any non-numeric or NA entries just in case
  filter(!is.na(Q16) & str_detect(Q16, "^\\d+$")) %>%
  # Pad values with leading zero for consistent column naming (01, 02, ..., 11)
  mutate(Q16 = str_pad(Q16, width = 2, pad = "0"),
         value = 1) %>%
  # Pivot wider to get binary indicator columns
  pivot_wider(
    names_from = Q16,
    names_prefix = "Q16_",
    values_from = value,
    values_fill = 0
  )

dim(Employer_Data_Clean)

# Question #17
# Contains NA values but I was opposed to removing them due to the sampling concerns 
# the fact that the rest of the rows are useable. 

# Question #22
# Contains NA values but I was opposed to removing them due to the sampling concerns 
# the fact that the rest of the rows are useable. 

# Question #20
# Contains NA values but I was opposed to removing them due to the sampling concerns 
# the fact that the rest of the rows are useable. 


# Sort Everything:
# Define metadata columns to preserve in order
meta_cols <- c("ip_address", "progress", "duration_in_seconds",
               "finished", "recorded_date", "response_id", "group", "respondent_id")

# Sort the rest alphabetically
Employer_Data_Clean <- Employer_Data_Clean %>%
  select(all_of(meta_cols), sort(setdiff(names(.), meta_cols)))

summary(Employer_Data_Clean)


#Visualize heatmap of missing values. 
Employer_Data_Clean %>%
  select(-ip_address, -recorded_date, -response_id) %>%  # Optional: remove metadata
  vis_miss(sort_miss = TRUE) +
  labs(title = "Heatmap of Missing Values in Employer Survey")


Employer_Data_Clean %>% 
