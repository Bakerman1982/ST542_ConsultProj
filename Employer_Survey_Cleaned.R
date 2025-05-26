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

# LANGUAGE CONVERSIONS
# There are some differences in terminologies within the survey and the output. 

#SURVEY:  where it says "practice/learn"
#OUTPUT:  the output uses "observe(d)"

#SURVEY:  where it says "DVM students"
#OUTPUT:  the output uses "early career veterinarians"

#SURVEY:  where it says "dental procedures"
#OUTPUT:  the output uses the term "COHAT" (short for `Comprehensive Oral Health Assessment and Treatment`)


# Convert column responses that are comma-separated into wide-format. 
# Convert all question responses to binaries. 

# These are the columns that have comma-separated values that need splitting. 
# Q3, Q10_4, Q11, Q16, Q17_2, Q_18, Q20, Q25, Q49, Q31, Q39, Q51, Q40

#Q3 -> Q3 (Formatting Issue), What does a COHAT include at your practice/organization?  Select all that apply. - Selected Choice

#Q10_4, Which of the following types of instructors found in a typical DVM program do you think should be training DVM students in dentistry?  Select one response for each category of educator listed below. - Registered Veterinary Technicians 
##############NOTE:  Participant answered "agree" and "disagree"; not a formatting issue. 

#Q11 -> Q11 (Formatting Issue), Which of the following instructors trained the early career veterinarians hired into your practice in dentistry during their DVM program?  Select all that apply. - Selected Choice

#Q16 -> Q16 (Formatting Issue), Which of the following skills do you think were taught as part of the pre-clinical dentistry curriculum in the DVM program attended by the early career veterinarians at your practice?  Select all that apply. - Selected Choice

#Q17_2, Which of the following skills do you think that DVM students should be taught during their pre-clinical dentistry courses.  Select one response for skill listed below. - Radiographic positioning
##############NOTE:  Participant answered "agree" and "disagree"; not a formatting issue. 

#Q_18 -> Q18 (Formatting Issue), What format of pre-clinical instruction in dentistry do you believe that the early career veterinarians hired into your practice completed as part of their DVM training?  Select all that apply. - Selected Choice

#Q20, Which of the following format of instruction in dentistry do you believe that the early career veterinarians hired into your practice completed during their clinical training?  Select all that apply. - Selected Choice

#Q25 -> (I do not know which question this is related to) (Formatting Issue), Which of the following skills do you think that early career veterinarians hired into your practice observed during their clinical training?  Select all that apply. - Selected Choice
##############NOTE:  Requires clarification from client on question mapping

#Q49 -> (I do not know which question this is related to) (Formatting Issue), Which of the following skills do you think that early career veterinarians hired into your practice practiced during their clinical training?  Select all that apply. - Other: Please describe - Text
##############NOTE:  Requires clarification from client on question mapping

#Q31 -> Q31, Did the early career veterinarians hired into your practice earned their degrees from a DVM program with a teaching hospital?
##############NOTE:  Requires clarification from client on what an answer of 1,2 means. 
  
#Q39 -> Q39 (Formatting issue), During the clinical training of your DVM program, which of the following skills did you observe?  Select all that apply. - Selected Choice
#Q51 -> Q39 (Formatting issue), During the clinical training of your DVM program, which of the following skills did you practice?  Select all that apply. - Selected Choice
##############NOTE:  For Q39 and Q51, we need to reconcile the differences between these two questions.  I believe them to be one in the same but we need clarification. 

#Q40 -> Q40 (Formatting issue), Which of the following skills did you learn after graduation from your DVM program?  Select all that apply. - Selected Choice

# Remove uncertain/problematic columns while awaiting clarification
Employer_Data_Clean <- Employer_Data %>%
  select(
    -starts_with("Q10"),
    -starts_with("Q17"),
    -starts_with("Q25"),
    -starts_with("Q49"),
    -starts_with("Q31"),
    -starts_with("Q39"),
    -starts_with("Q51")
  )

# Remove specified record_id rows (corresponds to rows where multiple answers were given but not appropriate to the question)
Employer_Data_Clean <- Employer_Data_Clean %>%
  filter(!(record_id %in% c("R_3NQGIXluF0NVk8B", "R_1JyDJnIPpkeoXOi")))

# Dimensionality checkpoint.  Making sure removals and filters have been applied. 
dim(Employer_Data)
dim(Employer_Data_Clean)

colnames(Employer_Data_Clean)
view(Employer_Data_Clean)
view(Employer_Data)

unique(Employer_Data_Clean$Q3)


