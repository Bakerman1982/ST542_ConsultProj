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
Educator_Data <- read_csv("PCVE_Dentistry_Survey.csv") %>% 
  clean_names()
Educator_Data_Clean <- Educator_Data

# Add grouping identifier now so if we combine data later, we have our groups delineated. 
Educator_Data_Clean <- Educator_Data_Clean %>%
  mutate(
    group = "Educator", #Categorizes all responses as Employer
    respondent_id = row_number() #Maintains respondent ID
  )



Educator_Data_Clean %>%
  select(starts_with("Q04"))

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
colnames(Educator_Data_Clean) <- gsub("_text$", "_T", gsub("q", "Q", colnames
                                                           (Educator_Data_Clean)))

# Format the column headers so that it is sort-able both by question and sub question. 
pad_question_ids <- function(col_names) {
  col_names %>%
    str_replace_all("Q(\\d{1})(?!\\d)", "Q0\\1") %>%
    str_replace_all("Q(\\d{2})_(\\d{1})(?!\\d)", "Q\\1_0\\2")
}
# Apply to your data frame column names
colnames(Educator_Data_Clean) <- pad_question_ids(colnames(Educator_Data_Clean))



##### FILTER DATASET #####

# Remove uncertain/problematic columns while awaiting clarification
Educator_Data_Clean <- Educator_Data_Clean %>%
  select(
    ip_address, progress, duration_in_seconds,
    finished, recorded_date, response_id, group, respondent_id,
    starts_with("Q04"), #Keep
    starts_with("Q07"), #Was 9 in EMP, Its 7 in EDU
    starts_with("Q12"), #Was 16 in EMP, Its 12 in EDU
    starts_with("Q13"), #Was 17 in EMP, Its 13 in EDU
    starts_with("Q18"), #Was 22 in EMP, Its 18 in EDU
    starts_with("Q24")  #Was 24 in EMP, Its 19 in EDU
  )

colnames(Educator_Data_Clean)


# Remove specified record_id rows (corresponds to rows where multiple answers were given but not appropriate to the question)
Educator_Data_Clean <- Educator_Data_Clean %>%
  slice(-1, -2)

##### Making corrections to question formatting #####
Educator_Data_Clean %>% select(starts_with("Q12")) %>% print(n=length(Educator_Data_Clean))

# Question #04
# Renames the text response to index 13 and places it in its order. 
Educator_Data_Clean <- Educator_Data_Clean %>%
  rename(Q04_13 = Q04_12_T) %>%
  relocate(Q04_13, .after = Q04_12)

# Question #07 does not need correcting. 

# Question #12
# Change column name from _10_T to _11 for readability. 
Educator_Data_Clean <- Educator_Data_Clean %>%
  rename(Q12_11 = Q12_10_T)

# Process Q16: Split responses and pivot to wide format
Educator_Data_Clean <- Educator_Data_Clean %>%
  # Create a temporary respondent ID if not already done
  mutate(respondent_id = row_number()) %>%
  # Separate comma-separated values into long format
  separate_rows(Q12, sep = ",") %>%
  # Trim whitespace if any
  mutate(Q12 = str_trim(Q12)) %>%
  # Filter out any non-numeric or NA entries just in case
  filter(!is.na(Q12) & str_detect(Q12, "^\\d+$")) %>%
  # Pad values with leading zero for consistent column naming (01, 02, ..., 11)
  mutate(Q12 = str_pad(Q12, width = 2, pad = "0"),
         value = 1) %>%
  # Pivot wider to get binary indicator columns
  pivot_wider(
    names_from = Q12,
    names_prefix = "Q12_",
    values_from = value,
    values_fill = 0
  )

# Question #13
# Contains NA values but I was opposed to removing them due to the sampling concerns 
# the fact that the rest of the rows are useable. 

# Question #18
# Contains NA values but I was opposed to removing them due to the sampling concerns 
# the fact that the rest of the rows are useable. 

# Question #24
# Contains NA values but I was opposed to removing them due to the sampling concerns 
# the fact that the rest of the rows are useable. 


# Sort Everything:
# Define metadata columns to preserve in order
meta_cols <- c("ip_address", "progress", "duration_in_seconds",
               "finished", "recorded_date", "response_id", "group", "respondent_id")

# Sort the rest alphabetically
Educator_Data_Clean <- Educator_Data_Clean %>%
  select(all_of(meta_cols), sort(setdiff(names(.), meta_cols)))

colnames(Educator_Data_Clean)

summary(Educator_Data_Clean)



Educator_Data_Clean %>%
  select(-ip_address, -recorded_date, -response_id) %>%  # Optional: remove metadata
  vis_miss(sort_miss = TRUE) +
  labs(title = "Heatmap of Missing Values in Employer Survey")



