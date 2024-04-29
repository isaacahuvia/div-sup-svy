library(here)
library(tidyverse)
library(qualtRics)
library(lubridate)

raw_data <- read_survey(
  here("CAAPS DS Survey_4.26.24.csv")
)

clean_data <- raw_data %>%
  select(
    
    # Metadata
    response_id = ResponseId,
    response_date = RecordedDate,
    duration = `Duration (in seconds)`,
    
    # Demographics
    age = Age,
    gender = `Gender Identity`,
    race = Race,
    ethnicity = Ethnicity,
    
    # Career Information
    discipline = `Researcher Type`,
    degree = `Degree Type`,
    research_time = `Research Time_1`,
    research_funding = `Research Funding_1`,
    research_years = `Years of Research`,
    
    # Div Sup
    div_sup_history = `Div Sup History`,
    div_sup_barriers = `Why not?`
    
  ) %>%
  mutate(
    
    # Metadata
    response_date = response_date %>%
        gsub("\\s.*$", "", .) %>%
        mdy(),
    
    # Demographics
    age = as.numeric(age), # Fix up
    age_binned = cut(age, c(20, 30, 40, 50, 60, 70, 80)),
    gender = case_when(
      gender %in% c("Male", "Female") ~ gender,
      gender %in% c("Non-binary / third gender", "Other (please specify") ~ "TGD",
      T ~ NA_character_
    ) %>%
      factor() %>%
      relevel(ref = "Female"),
    race_ethnicity = case_when(
      ethnicity == "Yes" ~ "Hispanic",
      race == "White" ~ "White non-Hispanic",
      race == "Asian" ~ "Asian non-Hispanic",
      race == "Black or African American" ~ "Black non-Hispanic",
      # No other categories with n > 10
      !is.na(race) ~ "Other or Multiple",
      T ~ NA_character_
    ) %>%
      factor() %>%
      relevel(ref = "White non-Hispanic"),
    
    # Career Information
    discipline = discipline %>%
      factor() %>%
      relevel(ref = "Biology"),
    
    div_sup_applied = case_when(
      div_sup_history %in% c(
        "Supported an application that was awarded",
        "Supported an application that was NOT awarded"
      ) ~ T,
      !is.na(div_sup_history) ~ F,
      is.na(div_sup_history) ~ NA
    ),
    
    div_sup_granted = case_when(
      div_sup_history == "Supported an application that was awarded" ~ T,
      div_sup_history == "Supported an application that was NOT awarded" ~ F,
      T ~ NA
    )
      
  )

saveRDS(clean_data, here("Clean Data.rds"))