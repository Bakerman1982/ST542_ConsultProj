# Install Packages
#install.packages("googledrive")

# Load Libraries
library(tidyverse)
library(janitor)
library(googledrive)


# Connect and Import
# Connect to Google Drive.  Place the email address you use to reach the shared drive. 
email = "blakerma@ncsu.edu"
drive_auth(email)


# Imports dataset to R environment; use clean_names to coerce col_names for R
Employer_Data <- read_csv("Employer_Dentistry_Survey.csv", skip = 2) %>%
  clean_names()


# Adjust headers for readability and add grouping identifier.
# Remove redundancy in column names, replace "quid" with "Q", all "_text" with "T". 
colnames(Employer_Data) <- gsub("_text$", "_T", gsub("^import_id_", "", gsub("qid", "Q", colnames(Employer_Data))))


# Add grouping identifier now so if we combine data later, we have our groups delineated. 
Employer_Data <- Employer_Data %>%
  mutate(
    group = "Employer", #Categorizes all responses as Employer
    respondent_id = row_number() #Maintains respondent ID
  )



############################
#####  DATA WRANGLING  #####
############################

# Convert column responses that are comma-separated into wide-format. 
# Convert all question responses to binaries. 

# These are the columns that have comma-separated values that need splitting. 
# Q3, Q10_3, Q11, Q16, Q17_2, Q_18, Q20, Q25, Q49, Q31, Q39, Q51, Q40
Q3, What does a COHAT include at your practice/organization?  Select all that apply. - Selected Choice

Q10_4, Which of the following types of instructors found in a typical DVM program do you think should be training DVM students in dentistry?  Select one response for each category of educator listed below. - Registered Veterinary Technicians

Q11, Which of the following instructors trained the early career veterinarians hired into your practice in dentistry during their DVM program?  Select all that apply. - Selected Choice

Q16, Which of the following skills do you think were taught as part of the pre-clinical dentistry curriculum in the DVM program attended by the early career veterinarians at your practice?  Select all that apply. - Selected Choice

Q17_2, Which of the following skills do you think that DVM students should be taught during their pre-clinical dentistry courses.  Select one response for skill listed below. - Radiographic positioning

Q_18, What format of pre-clinical instruction in dentistry do you believe that the early career veterinarians hired into your practice completed as part of their DVM training?  Select all that apply. - Selected Choice

Q20, Which of the following format of instruction in dentistry do you believe that the early career veterinarians hired into your practice completed during their clinical training?  Select all that apply. - Selected Choice

Q25, Which of the following skills do you think that early career veterinarians hired into your practice observed during their clinical training?  Select all that apply. - Selected Choice

Q49, Which of the following skills do you think that early career veterinarians hired into your practice practiced during their clinical training?  Select all that apply. - Other: Please describe - Text

Q31, Did the early career veterinarians hired into your practice earned their degrees from a DVM program with a teaching hospital?
  
Q39, During the clinical training of your DVM program, which of the following skills did you observe?  Select all that apply. - Selected Choice

Q51, During the clinical training of your DVM program, which of the following skills did you practice?  Select all that apply. - Selected Choice

Q40, Which of the following skills did you learn after graduation from your DVM program?  Select all that apply. - Selected Choice





df <- Employer_Data %>%
  mutate(Q3_list = strsplit(as.character(Q3), ","))  # Convert string to list

# 2. Create binary indicator columns for Q3_1 to Q3_14
for (i in 1:14) {
  df[[paste0("Q3", i)]] <- sapply(df$Q3_list, function(x) as.integer(i %in% as.integer(x)))
}

# 3. Optional: Remove the original list column
df <- df %>% select(-Q3_list)


colnames(Employer_Data)











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



