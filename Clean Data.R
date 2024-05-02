library(here)
library(tidyverse)
library(qualtRics)
library(lubridate)

raw_data <- read_survey(
  here("CAAPS DS Survey_4.26.24.csv")
)

barrier_data <- raw_data %>%
  select(
    response_id = ResponseId,
    barriers = `Why not?`,
    top_barrier = `top barrier`
  ) %>%
  separate_longer_delim(
    barriers,
    ","
  ) %>%
  mutate(
    across(
      c(barriers, top_barrier),
      ~ recode(
        ., 
        "I did not have a candidate/mentee that qualified" = "No Qualifying Mentees",
        "I didn't have the necessary time and/or energy" = "Not Enough Time/Energy",
        "I didn't know about Diversity Supplements at all" = "Not Familiar",
        "I didn't know enough about the process" = "Unclear Re: Process",
        "I didn't know that I was eligible to support a Diversity Supplement application" = "Unclear Re: Eligibility",
        "I didn't see it as being worth my time and effort" = "Not Worth the Time/Effort",
        "I never wanted to apply for a Diversity Supplement" = "Didn't Want To",
        "My department/institution does not offer any incentives for receiving a Diversity Supplement" = "No Incentives",
        "Other people in my position/department don't apply for Diversity Supplements" = "Others Don't"
      )
    )
  )

clean_data <- raw_data %>%
  mutate(

    # Metadata
    response_id = ResponseId,
    response_date = RecordedDate %>%
        gsub("\\s.*$", "", .) %>%
        mdy(),
    duration = `Duration (in seconds)`,
    
    # Demographics
    age = as.numeric(Age), # Fix up
    gender = case_when(
      `Gender Identity` %in% c("Male", "Female") ~ `Gender Identity`,
      `Gender Identity` %in% c("Non-binary / third gender", "Other (please specify") ~ "TGD",
      T ~ NA_character_
    ) %>%
      factor() %>%
      relevel(ref = "Female"),
    race_ethnicity = case_when(
      Ethnicity == "Yes" ~ "Hispanic",
      Race == "White" ~ "White non-Hispanic",
      Race == "Asian" ~ "Asian non-Hispanic",
      Race == "Black or African American" ~ "Black non-Hispanic",
      # No other categories with n > 10
      !is.na(Race) ~ "Other or Multiple",
      T ~ NA_character_
    ) %>%
      factor() %>%
      relevel(ref = "White non-Hispanic"),
    
    # Career Information
    discipline = `Researcher Type` %>%
      factor() %>%
      relevel(ref = "Biology"),
    institution_type = `Institution Type`,
    job_title = `Position/Role`,
    research_time = `Research Time_1`,
    research_funding = `Research Funding_1`,
    years_experience = as.numeric(`Years of Research`), # Fix up
    mentor_undergrad = grepl("Undergraduate student", `Research mentorship`),
    mentor_postgrad = grepl("Post-Bac", `Research mentorship`),
    mentor_masters = grepl("Master's Student", `Research mentorship`),
    mentor_doctoral = grepl("Doctoral Student", `Research mentorship`),
    mentor_postdoc = grepl("Post-Doc", `Research mentorship`),
    mentor_ecr = grepl("Early Career Researcher", `Research mentorship`),
    
    # DS Uptake
    div_sup_applied = case_when(
      `Div Sup History` %in% c(
        "Supported an application that was awarded",
        "Supported an application that was NOT awarded"
      ) ~ T,
      !is.na(`Div Sup History`) ~ F,
      is.na(`Div Sup History`) ~ NA
    ),
    
    # DS Success
    div_sup_granted = case_when(
      `Div Sup History` == "Supported an application that was awarded" ~ T,
      `Div Sup History` == "Supported an application that was NOT awarded" ~ F,
      T ~ NA
    ),
    
    .keep = "none"
      
  ) %>%
  left_join(
    barrier_data %>%
      distinct(response_id, top_barrier),
    by = "response_id"
  )

saveRDS(clean_data, here("Clean Data.rds"))
saveRDS(barrier_data, here("Barrier Data.rds"))