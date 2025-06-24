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
Educator_Data <- read_csv("PCVE_Dentistry_Survey.csv") %>% clean_names()
#Name new object to work with leaving original intact/untouched
Educator_Data_Clean <- Educator_Data

#Add identifier affirming educator responses. 
Educator_Data_Clean <- Educator_Data_Clean %>%
  mutate(
    group = "Educator",
    respondent_id = row_number()
  )

############################
#####  DATA WRANGLING  #####
############################

#Adjust headers for readability and add grouping identifier.
  #1.) Remove redundancy in column names, 
  #2.) Replace "quid" with "Q", and
  #3.) All "_text" with "T".
colnames(Educator_Data_Clean) <- gsub("_text$", "_T", gsub("q", "Q", colnames(Educator_Data_Clean)))
  #4.) Format the column headers so that it is sort-able both by question and sub question. So where 
    #questions are formatted with header Q10_2, the header will now be Q10_02.  This preserves ordering
    #and makes indexing easier. 
pad_question_ids <- function(col_names) {
  col_names %>%
    str_replace_all("Q(\\d{1})(?!\\d)", "Q0\\1") %>%
    str_replace_all("Q(\\d{2})_(\\d{1})(?!\\d)", "Q\\1_0\\2")}

#Apply above formatting changes to data frame column names
colnames(Educator_Data_Clean) <- pad_question_ids(colnames(Educator_Data_Clean))

#Removes Qualtric metadata from the first two rows
Educator_Data_Clean <- Educator_Data_Clean %>% slice(-1, -2)


### QUESTION-BY-QUESTION Cleaning

#------------
#Question #04
#------------
#Renames the text response to index 13 and places it in its order. 
Educator_Data_Clean <- Educator_Data_Clean %>%
  rename(Q04_13 = Q04_12_T)

#------------
#Question #07 does not need correcting. 
#------------

#------------
#Question #12
#------------
  #Renames the text response to index 11 and places it in its order. 
Educator_Data_Clean <- Educator_Data_Clean %>%
  rename(Q12_11 = Q12_10_T) 

  #Process Q12: Split responses and pivot to wide format
Educator_Data_Clean <- Educator_Data_Clean %>%
  # Create a temporary respondent ID if not already done
  mutate(respondent_id = row_number()) %>%
  # Separate comma-separated values into long format
  separate_rows(Q12, sep = ",") %>%
  # Trim whitespace if any
  mutate(Q12 = str_trim(Q12)) %>%
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

#------------
#Question #13
#------------
#Renames the text response to index 14 and places it in its order.
Educator_Data_Clean <- Educator_Data_Clean %>%
  rename(Q13_14 = Q13_13_T)

#------------
#Question #16
#------------
# Create binary indicator columns from Q16.  Recall that Q16 was a multiple responsive
# "Select all that Apply" question where participants could choose however many they 
# wanted.  In the Qualtrics output, these responses were comma-separated.  This query
# breaks each reponse out over a column and assigns values of "0" or "1" depending on 
# whether they choose that particular response. The same issue is address in Question #20. 
Q16_binary <- Educator_Data_Clean %>%
  select(respondent_id, Q16) %>%
  separate_rows(Q16, sep = ",") %>%
  mutate(Q16 = str_trim(Q16)) %>%
  mutate(Q16 = str_pad(Q16, width = 2, pad = "0"),
         value = 1) %>%
  pivot_wider(
    names_from = Q16,
    names_prefix = "Q16_",
    values_from = value,
    values_fill = 0
  )

  # Remove the old Q16 columns and join the binary ones on respondent_id
Educator_Data_Clean <- Educator_Data_Clean %>%
  select(-Q16) %>%
  left_join(Q16_binary, by = "respondent_id")

  # Renames the text response to index 07 and places it in its order.
Educator_Data_Clean <- Educator_Data_Clean %>%
  rename(Q16_07 = Q16_06_T) %>%
  relocate(Q16_07, .after = Q16_06)

  # Add Q16_05 as a column of zeros if it doesn't exist (it does not)
if (!"Q16_05" %in% colnames(Educator_Data_Clean)) {
  Educator_Data_Clean$Q16_05 <- 0
}

#------------
#Question #17
#------------
#Renames the text response to index 07 and places it in its order.
Educator_Data_Clean <- Educator_Data_Clean %>%
  rename(Q17_07 = Q17_06_T) 

#------------
#Question #18 does not need correcting. 
#------------

#------------
#Question #19 does not need correcting. 
#------------

#------------
#Question #20
#------------
# Create binary indicator columns from Q16.  Recall that Q16 was a multiple responsive
# "Select all that Apply" question where participants could choose however many they 
# wanted.  In the Qualtrics output, these responses were comma-separated.  This query
# breaks each reponse out over a column and assigns values of "0" or "1" depending on 
# whether they choose that particular response. The same issue is address in Question #16. 
Q20_binary <- Educator_Data_Clean %>%
  select(respondent_id, Q20) %>%
  separate_rows(Q20, sep = ",") %>%
  mutate(Q20 = str_trim(Q20)) %>%
  mutate(Q20 = str_pad(Q20, width = 2, pad = "0"),
         value = 1) %>%
  pivot_wider(
    names_from = Q20,
    names_prefix = "Q20_",
    values_from = value,
    values_fill = 0
  )

# Remove the old Q16 columns and join the binary ones in
Educator_Data_Clean <- Educator_Data_Clean %>%
  select(-Q20) %>%
  left_join(Q20_binary, by = "respondent_id")

# Rename and relocate the "Other" text entry for Q16
Educator_Data_Clean <- Educator_Data_Clean %>%
  rename(Q20_14 = Q20_13_T) %>%
  relocate(Q20_14, .after = Q20_13)

#------------
#Question #21
#------------
#Renames the text response to index 14 and places it in its order. 
Educator_Data_Clean <- Educator_Data_Clean %>%
  rename(Q21_14 = Q21_13_T) 



##########################
##### FILTER DATASET #####
##########################

#Remove irrelevant/problematic columns to the research/stats questions
Educator_Data_Clean <- Educator_Data_Clean %>%
  filter(Q44 != 2, progress < 50) %>%
  select(
    ip_address, progress, duration_in_seconds,
    finished, recorded_date, response_id, group, respondent_id,
    starts_with("Q04"),
    starts_with("Q07"),
    starts_with("Q12"),
    starts_with("Q13"),
    starts_with("Q16"),
    starts_with("Q17"),
    starts_with("Q18"),
    starts_with("Q19"),
    starts_with("Q20"),
    starts_with("Q21")
  )

#Identify metadata columns to keep at the front
meta_cols <- c("ip_address", "progress", "duration_in_seconds",
               "finished", "recorded_date", "response_id", "group", "respondent_id")

#Identify and sort Q columns using mixedsort
q_cols <- setdiff(colnames(Educator_Data_Clean), meta_cols)
q_cols_sorted <- mixedsort(q_cols)

#Reorder the data frame
Educator_Data_Clean <- Educator_Data_Clean[, c(meta_cols, q_cols_sorted)]
Educator_Data_Clean
colnames(Educator_Data_Clean)

View(Educator_Data_Clean)






print(Educator_Data_Clean, n = nrow(Educator_Data_Clean))


colnames(Educator_Data)["q44"]

dim(Educator_Data[Educator_Data$q44 == 2, ])


 

















###########################################
##### RESIDUAL CODE I MIGHT USE LATER #####
###########################################


#summary(Educator_Data_Clean)

#Educator_Data_Clean %>%
#  select(-ip_address, -recorded_date, -response_id) %>%  # Optional: remove metadata
#  vis_miss(sort_miss = TRUE) +
#  labs(title = "Heatmap of Missing Values in Employer Survey")

#Educator_Data_Clean %>% select(starts_with("Q04")) %>% print(n=length(Educator_Data_Clean))
# Multiple Likert = 2 [5,25]; NA = 1, [7] ;

#Educator_Data_Clean %>% select(starts_with("Q07")) %>% print(n=length(Educator_Data_Clean))
# NA = 7 [1,7,12,13,23,27,29]

#Educator_Data_Clean %>% select(starts_with("Q12")) %>% print(n=length(Educator_Data_Clean))

#Educator_Data_Clean %>% select(starts_with("Q13")) %>% print(n=length(Educator_Data_Clean))
# Multiple Likert 2, [20,27]; NA = 13, [1,4,6,12,14,15,16,17,19,20,22,25,27]

#Educator_Data_Clean %>% select(starts_with("Q16")) %>% print(n=length(Educator_Data_Clean))

#Educator_Data_Clean %>% select(starts_with("Q17")) %>% print(n=length(Educator_Data_Clean))
# Multiple Likert 9, [8, 9, 12, 16, 24, 25, 26, 28, 29]

#Educator_Data_Clean %>% select(starts_with("Q18")) %>% print(n=length(Educator_Data_Clean))
# Multiple non-Likert omissions 12, [4, 5, 7, 12, 15, 16, 17, 18, 21, 23, 27, 28]

#Educator_Data_Clean %>% select(starts_with("Q19")) %>% print(n=length(Educator_Data_Clean))
# Multiple non-Likert omissions 13, [3, 5, 7, 15, 16, 17, 21, 23, 26, 27, 28, 29]

#Educator_Data_Clean %>% select(starts_with("Q20")) %>% print(n=length(Educator_Data_Clean))
# Omission 1 [17]

#Educator_Data_Clean %>% select(starts_with("Q21")) %>% print(n=length(Educator_Data_Clean))
# Multiple Likert 1 [4]; omission 4 [17, 26, 28, 30]



