
```{r question_4prep, include=FALSE, echo=FALSE, warning=FALSE, message=FALSE}
library(dplyr)
library(tidyr)
library(purrr)
library(effectsize)
library(gt)

# 1. Define variables
educator_vars <- paste0("Q19_0", 1:4)
employer_vars <- paste0("Q24_0", 1:4)
setting_labels <- c("Primary_Care", "Dentistry_Rotation", "Shelter_Medicine", "Lab_Animal")

# 2. Select and rename columns
educator_proc <- Educator_Data_Clean %>%
  select(all_of(educator_vars)) %>%
  mutate(group = "Educator")
employer_proc <- Employer_Data_Clean %>%
  select(all_of(employer_vars)) %>%
  mutate(group = "Employer")

colnames(educator_proc)[1:4] <- setting_labels
colnames(employer_proc)[1:4] <- setting_labels

# 3. Combine and reshape
combined_proc <- bind_rows(educator_proc, employer_proc) %>%
  pivot_longer(
    cols = all_of(setting_labels),
    names_to = "Setting",
    values_to = "Procedure_Count"
  ) %>%
  filter(!is.na(Procedure_Count)) %>%
  mutate(Procedure_Count = as.numeric(Procedure_Count))

# 4. Calculate Mann-Whitney and effect size
mw_results <- combined_proc %>%
  group_by(Setting) %>%
  summarise(
    p_value = wilcox.test(Procedure_Count ~ group)$p.value,
    r_rb = effectsize::rank_biserial(Procedure_Count ~ group)$r_rank_biserial,
    median_educator = median(Procedure_Count[group == "Educator"]),
    median_employer = median(Procedure_Count[group == "Employer"]),
    n_educator = sum(group == "Educator"),
    n_employer = sum(group == "Employer"),
    .groups = "drop"
  ) %>%
  mutate(
    stars = cut(p_value,
                breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                labels = c("***", "**", "*", ".", "")),
    P_Value = ifelse(is.na(p_value), "N/A",
                     ifelse(stars != "", paste0(sprintf("%.3f", p_value), " ", stars),
                            sprintf("%.3f", p_value))),
    r_rb_fmt = sprintf("%.2f", r_rb),
    Interpretation = case_when(
      is.na(r_rb) ~ "N/A (Insufficient Data)",
      abs(r_rb) >= 0.50 ~ paste0("Large ", ifelse(r_rb > 0, "positive", "negative"), " effect"),
      abs(r_rb) >= 0.30 ~ paste0("Medium ", ifelse(r_rb > 0, "positive", "negative"), " effect"),
      abs(r_rb) >= 0.10 ~ paste0("Small ", ifelse(r_rb > 0, "positive", "negative"), " effect"),
      TRUE ~ "Negligible effect"
    )
  ) %>%
  arrange(p_value) %>%
  select(
    Setting,
    median_educator,
    median_employer,
    n_educator,
    n_employer,
    P_Value,
    r_rb_fmt,
    Interpretation
  )
```

```{r question_4, echo=FALSE, warning=FALSE}
mw_results %>%
  gt() %>%
  tab_header(
    title = md("**Mann-Whitney U Test: Expected Number of Dental Procedures**"),
    subtitle = "Comparison of Educator and Employer Expectations by Clinical Setting"
  ) %>%
  cols_label(
    Setting = "Setting",
    median_educator = "Med (Edu)",
    median_employer = "Med (Emp)",
    n_educator = "N (Edu)",
    n_employer = "N (Emp)",
    P_Value = "P-Value",
    r_rb_fmt = "r_rb",
    Interpretation = "Effect Size"
  ) %>%
  cols_align(align = "center") %>%
  fmt_number(columns = vars(median_educator, median_employer), decimals = 1) %>%
  fmt_number(columns = vars(P_Value), decimals = 3) %>%
  fmt_number(columns = vars(r_rb_fmt), decimals = 2) %>%
  tab_options(
    data_row.padding = px(1),
    table.font.size = px(10),
    heading.title.font.size = px(14),
    heading.subtitle.font.size = px(11),
    column_labels.font.size = px(11),
    source_notes.font.size = px(9)
  ) %>%
  opt_align_table_header("center")
```

