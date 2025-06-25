######################################
##           Load Library           ##
######################################

#Complimentary "Install Packages" code
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

#Read data into R environment. 
Employer_Data <- read_csv("Employer_Dentistry_Survey.csv") %>% clean_names()
#Name new object to work with leaving original intact/untouched
Employer_Data_Clean <- Employer_Data

#Add grouping identifier now so if we combine data later, we have our groups delineated. 
Employer_Data_Clean <- Employer_Data_Clean %>%
  mutate(
    group = "Employer", #Categorizes all responses as Employer
    respondent_id = row_number() #Maintains respondent ID
  )


############################
#####  DATA WRANGLING  #####
############################

#Adjust headers for readability and add grouping identifier.
#1.) Remove redundancy in column names, 
#2.) Replace "quid" with "Q", and
#3.) All "_text" with "T".
colnames(Employer_Data_Clean) <- gsub("_text$", "_T", gsub("q", "Q", colnames(Employer_Data_Clean)))
#4.) Format the column headers so that it is sort-able both by question and sub question. So where 
#questions are formatted with header Q10_2, the header will now be Q10_02.  This preserves ordering
#and makes indexing easier. 
pad_question_ids <- function(col_names) {
  col_names %>%
    str_replace_all("Q(\\d{1})(?!\\d)", "Q0\\1") %>%
    str_replace_all("Q(\\d{2})_(\\d{1})(?!\\d)", "Q\\1_0\\2")}

#Apply above formatting changes to data frame column names
colnames(Employer_Data_Clean) <- pad_question_ids(colnames(Employer_Data_Clean))

#Removes Qualtric metadata from the first two rows
Employer_Data_Clean <- Employer_Data_Clean %>% slice(-1, -2)



### QUESTION-BY-QUESTION Cleaning

#------------
#Question #04
#------------
#Renames the text response to index 13 and places it in its order. 
Employer_Data_Clean <- Employer_Data_Clean %>%
  rename(Q04_13 = Q04_12_T)

#------------
#Question #09 does not need correcting. 
#------------

#------------
#Question #16
#------------
#Renames the text response to index 11 and places it in its order. 
Employer_Data_Clean <- Employer_Data_Clean %>%
  rename(Q16_11 = Q16_10_T) 

#Process Q12: Split responses and pivot to wide format
Employer_Data_Clean <- Employer_Data_Clean %>%
  mutate(respondent_id = row_number()) %>%
  separate_rows(Q16, sep = ",") %>%
  mutate(Q16 = str_trim(Q16)) %>%
  #filter(!is.na(Q16)) %>%  # 🔧 prevent Q16_NA column in pivot_wider
  mutate(Q16 = str_pad(Q16, width = 2, pad = "0"),
         value = 1) %>%
  pivot_wider(
    names_from = Q16,
    names_prefix = "Q16_",
    values_from = value,
    values_fill = 0
  ) %>%
  right_join(select(Employer_Data_Clean, respondent_id), by = "respondent_id")  # 🔁 bring back NA responders


if (!"Q16_09" %in% colnames(Employer_Data_Clean)) {
  Employer_Data_Clean$Q16_09 <- 0
}


#------------
#Question #17
#------------
#Renames the text response to index 14 and places it in its order.
Employer_Data_Clean <- Employer_Data_Clean %>%
  rename(Q17_15 = Q17_14_T)

#------------
#Question #20
#------------
# Create binary indicator columns from Q16.  Recall that Q16 was a multiple responsive
# "Select all that Apply" question where participants could choose however many they 
# wanted.  In the Qualtrics output, these responses were comma-separated.  This query
# breaks each reponse out over a column and assigns values of "0" or "1" depending on 
# whether they choose that particular response. The same issue is address in Question #20. 
Q20_binary <- Employer_Data_Clean %>%
  select(respondent_id, Q20) %>%
  separate_rows(Q20, sep = ",") %>%
  mutate(Q20 = str_trim(Q20)) %>%
  #filter(!is.na(Q20)) %>%   # 👈 Prevents Q20_NA from being created
  mutate(Q20 = str_pad(Q20, width = 2, pad = "0"),
         value = 1) %>%
  pivot_wider(
    names_from = Q20,
    names_prefix = "Q20_",
    values_from = value,
    values_fill = 0
  ) %>%
  right_join(select(Employer_Data_Clean, respondent_id), by = "respondent_id")

# Remove the old Q16 columns and join the binary ones on respondent_id
Employer_Data_Clean <- Employer_Data_Clean %>%
  select(-Q20) %>%
  left_join(Q20_binary, by = "respondent_id")

# Renames the text response to index 07 and places it in its order.
Employer_Data_Clean <- Employer_Data_Clean %>%
  rename(Q20_07 = Q20_06_T) %>%
  relocate(Q20_07, .after = Q20_06)

# Add Q16_05 as a column of zeros if it doesn't exist (it does not)
if (!"Q20_05" %in% colnames(Employer_Data_Clean)) {
  Employer_Data_Clean$Q20_05 <- 0
}

#------------
#Question #21
#------------
#Renames the text response to index 07 and places it in its order.
Employer_Data_Clean <- Employer_Data_Clean %>%
  rename(Q21_07 = Q21_06_T) 

#------------
#Question #22 does not need correcting. 
#------------

#------------
#Question #24 does not need correcting. 
#------------

#------------
#Question #25
#------------
# Create binary indicator columns from Q25.  Recall that Q25 was a multiple responsive
# "Select all that Apply" question where participants could choose however many they 
# wanted.  In the Qualtrics output, these responses were comma-separated.  This query
# breaks each reponse out over a column and assigns values of "0" or "1" depending on 
# whether they choose that particular response. The same issue is address in Question #16. 
Q25_binary <- Employer_Data_Clean %>%
  select(respondent_id, Q25) %>%
  separate_rows(Q25, sep = ",") %>%
  mutate(Q25 = str_trim(Q25)) %>%
  #filter(!is.na(Q25)) %>%   # 👈 Prevents Q25_NA
  mutate(Q25 = str_pad(Q25, width = 2, pad = "0"),
         value = 1) %>%
  pivot_wider(
    names_from = Q25,
    names_prefix = "Q25_",
    values_from = value,
    values_fill = 0
  ) %>%
  right_join(select(Employer_Data_Clean, respondent_id), by = "respondent_id")


# Remove the old Q16 columns and join the binary ones in
Employer_Data_Clean <- Employer_Data_Clean %>%
  select(-Q25) %>%
  left_join(Q25_binary, by = "respondent_id")

# Add Q25_13 as a column of zeros if it doesn't exist (it does not)
if (!"Q25_13" %in% colnames(Employer_Data_Clean)) {
  Employer_Data_Clean$Q25_13 <- 0
}

# Rename and relocate the "Other" text entry for Q16
Employer_Data_Clean <- Employer_Data_Clean %>%
  rename(Q25_14 = Q25_13_T) %>%
  relocate(Q25_14, .after = Q25_13)

#------------
#Question #26
#------------
#Renames the text response to index 14 and places it in its order. 
Employer_Data_Clean <- Employer_Data_Clean %>%
  rename(Q26_14 = Q26_13_T) 


##########################
##### FILTER DATASET #####
##########################
#"Q20_NA"
#"Q25_NA"


#Remove irrelevant/problematic columns to the research/stats questions
Employer_Data_Clean <- Employer_Data_Clean %>%
  mutate(progress = as.numeric(progress)) %>%
  filter(Q02 == '1', progress >= 50) %>%
  select(
    ip_address, progress, duration_in_seconds,
    finished, recorded_date, response_id, group, respondent_id,
    starts_with("Q04"),
    starts_with("Q09"),
    starts_with("Q16"),
    starts_with("Q17"),
    starts_with("Q20"),
    starts_with("Q21"),
    starts_with("Q22"),
    starts_with("Q24"),
    starts_with("Q25"),
    starts_with("Q26"),
    -Q16_NA,  # 👈 drop this
    -Q20_NA,  # 👈 drop this
    -Q25_NA
  )

#Identify metadata columns to keep at the front
meta_cols <- c("ip_address", "progress", "duration_in_seconds",
               "finished", "recorded_date", "response_id", "group", "respondent_id")

#Identify and sort Q columns using mixedsort
q_cols <- setdiff(colnames(Employer_Data_Clean), meta_cols)
q_cols_sorted <- mixedsort(q_cols)

#Reorder the data frame
Employer_Data_Clean <- Employer_Data_Clean[, c(meta_cols, q_cols_sorted)]

#Print the entire dataframe
Employer_Data_Clean %>% print(n = 43)
colnames(Employer_Data_Clean)


count(Employer_Data_Clean)