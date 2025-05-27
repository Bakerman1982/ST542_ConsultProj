# Install Packages
#install.packages("tidyverse")
#install.packages("janitor")

# Load Libraries
library(tidyverse)
library(janitor)


#############################################
## Connect, Import, group and primary key. ##
#############################################

# Read data into R environment. 
Employer_Data <- read_csv("Employer_Dentistry_Survey.csv") %>%#, skip = 2) %>% 
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


# Remove uncertain/problematic columns while awaiting clarification
Employer_Data_Clean <- Employer_Data %>%
  select(
    -starts_with("Q10"),
    -starts_with("Q17"),
    -starts_with("Q25"),
    -starts_with("Q49"),
    -starts_with("Q31"),
    -starts_with("Q39"),
    -starts_with("Q51"),
    -starts_with("Q43"),
    -starts_with("Q44"),
    -starts_with("Q46"),
    -starts_with("Q47"),
    -starts_with("Q48"),
    -starts_with("Q50"),
    -starts_with("Q52"),
    -starts_with("Q5")
    )


# Remove specified record_id rows (corresponds to rows where multiple answers were given but not appropriate to the question)
Employer_Data_Clean <- Employer_Data_Clean %>%
filter(!(response_id %in% c("R_3NQGIXluF0NVk8B", "R_1JyDJnIPpkeoXOi")))

# Dimensionality checkpoint.  Making sure removals and filters have been applied. 
dim(Employer_Data)
dim(Employer_Data_Clean)

# Format the column headers so that it is sort-able both by question and sub question. 
pad_question_ids <- function(col_names) {
  col_names %>%
    # Step 1: Pad main Q numbers (e.g., Q3 → Q03, Q9 → Q09)
    str_replace_all("Q(\\d{1})(?!\\d)", "Q0\\1") %>%
    # Step 2: Pad sub-question numbers (e.g., Q03_3 → Q03_03, Q12_4 → Q12_04)
    str_replace_all("Q(\\d{2})_(\\d{1})(?!\\d)", "Q\\1_0\\2")
}

# Apply to your data frame column names
colnames(Employer_Data_Clean) <- pad_question_ids(colnames(Employer_Data_Clean))
colnames(Employer_Data_Clean)




Employer_Data_Clean %>% select("q3")


###### Spread columns that can go to wide ######



#####  Restructure Questions:  #####
# Question 03
q03_choices <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")
# Loop over choices and create binary columns named Q11_1, Q11_2, ..., Q11_6
for (choice in q03_choices) {
  Employer_Data_Clean[[paste0("Q03_", choice)]] <- ifelse(
    grepl(paste0("\\b", choice, "\\b"), Employer_Data_Clean$q11),
    1, 0
  )
}
# Question 11
q11_choices <- c("1", "2", "3", "4", "5", "6")
for (choice in q11_choices) {
  Employer_Data_Clean[[paste0("Q11_", choice)]] <- ifelse(
    grepl(paste0("\\b", choice, "\\b"), Employer_Data_Clean$q11), 1, 0)
}




# Question 13 -- Rename columns
Employer_Data_Clean <- Employer_Data_Clean %>%
  rename(
    q13_01 = q13_1,
    q13_02 = q13_06_T
  )




# Drop the original Q11 column
Employer_Data_Clean$q11 <- NULL
Employer_Data_Clean$q11 <- NULL




# Optional: View all column names that start with Q, now properly sorted
colnames(Employer_Data_Clean)[startsWith(colnames(Employer_Data_Clean), "q")] %>%
  sort()

Employer_Data_Clean %>% select("Q02", "Q02_13_T") %>% print(n = 31)



# Rearrange the table so that it makes sense and lines up with the survey document. 
# Identify the non-Q columns to keep at the beginning and end
non_q_start <- Employer_Data_Clean[1:17]                       # First 17 columns
non_q_end   <- Employer_Data_Clean[, (ncol(Employer_Data_Clean)-1):ncol(Employer_Data_Clean)]  # Last 2 columns

# Extract and sort the "Q"-starting columns
q_cols <- Employer_Data_Clean %>%
  select(starts_with("Q")) %>%
  select(sort(names(.)))  # Sort by name

# Combine into final sorted data frame
Employer_Data_Clean_Sorted <- bind_cols(non_q_start, q_cols, non_q_end)

# Optional: View the new column order
colnames(Employer_Data_Clean_Sorted)

