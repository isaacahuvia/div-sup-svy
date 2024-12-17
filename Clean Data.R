## Load packages
library(here)
library(tidyverse)
library(qualtRics)


## Custom functions
grepl_na <- function(string, x) {
  
  out <- rep(NA, length(x))
  
  nonmissing <- which(!is.na(x))
  
  grepl_output <- grepl(string, x[nonmissing])
  
  out[nonmissing] <- grepl_output
  
  return(out)
  
}

either <- Vectorize(function(x, y) {
  
  if(is.na(x) & is.na(y)) return(NA)
  
  if(is.na(x) & !is.na(y)) return(y)
  
  if(!is.na(x) & is.na(y)) return(x)
  
  if(!is.na(x) & !is.na(y)) return(x | y)
  
})


## Load data
raw_data <- read_survey(
  here("Data", "CAAPS DS Survey_5.12.24.csv")
)


## Clean data
# Load manual lookup for years of experience
years_experience_lookup <- read.csv(here("Data Cleaning Manual Lookups", "years_experience.csv"), stringsAsFactors = F)
n_past_applications_lookup <- read.csv(here("Data Cleaning Manual Lookups", "n_past_applications.csv"), stringsAsFactors = F)
n_past_awards_lookup <- read.csv(here("Data Cleaning Manual Lookups", "n_past_awards.csv"), stringsAsFactors = F)

# Barrier data (long format)
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
        "Other people in my position/department don't apply for Diversity Supplements" = "Others Don't",
        "Other  (please specify)" = "Other"
      )
    )
  )

# Clean participant response dataset
clean_data <- raw_data %>%
  
  # Filter to only participants who reached/responded to div sup status
  drop_na(`Div Sup History`) %>%
  
  # Join in manually-recoded variables
  left_join(years_experience_lookup, by = c("Years of Research" = "years_experience_raw")) %>%
  left_join(n_past_applications_lookup, by = c("number of past apps" = "n_past_applications_raw")) %>%
  left_join(n_past_awards_lookup, by = c("number past awards" = "n_past_awards_raw")) %>%
  
  # Clean variables
  mutate(

    ## Metadata
    # Rename response_id
    response_id = as.character(ResponseId),
    
    # Reformat response date
    response_date = StartDate %>%
        gsub("\\s.*$", "", .) %>%
        mdy(),
    
    # Rename duration
    duration = as.numeric(`Duration (in seconds)`),
    
    
    ## Demographics
    # Make age numeric, which requires some cleaning
    age = case_when(
      Age %in% 25:90 ~ Age,
      grepl("38. ", Age) ~ "38",
      Age == "40 years" ~ "40",
      Age == "51 (soon to be 52)" ~ "51",
      Age == "68.5" ~ "68",
      Age == "75 (Retired)" ~ "75",
      T ~ NA_character_
    ) %>%
      as.numeric(), # Fix up
    
    # Recode gender into three categories, which requires some cleaning
    gender = case_when(
      `Gender Identity` == "Male" ~ "Man",
      `Gender Identity` == "Female" ~ "Woman",
      `Gender Identity` == "Non-binary / third gender" ~ "TGD",
      `Gender Identity_4_TEXT` %in% c("Transgender man", "gender queer", "neither", "womanish") ~ "TGD",
      `Gender Identity_4_TEXT` %in% c("Woman", "woman (those are sexes)") ~ "Woman",
      T ~ NA_character_
    ),
    
    # Add a gender factor variable, with the largest category (Woman) as the reference
    gender_factor = gender %>%
      factor() %>%
      relevel(ref = "Woman"),
    
    # Recode race/ethnicity into one mutually exclusive variable
    race_ethnicity = case_when(
      Ethnicity == "Yes" ~ "Hispanic",
      Race == "American Indian or Alaska Native" ~ "AI/AN non-Hispanic",
      Race == "Asian" ~ "Asian non-Hispanic",
      Race == "Black or African American" ~ "Black non-Hispanic",
      Race == "Native Hawaiian or Other Pacific Islander" ~ "NH/PI non-Hispanic",
      Race == "White" ~ "White non-Hispanic",
      Race == "Other (please specify)" ~ "Other non-Hispanic",
      is.na(Race) | Race == "Prefer not to say" ~ NA_character_,
      T ~ "Multiple non-Hispanic"
    ),
    
    # Add a race/ethnicity factor variable, with the largest category (White) as the reference
    race_ethnicity_recoded = race_ethnicity %>%
      # Collapse small categories which cannot be estimated
      if_else(
        . %in% c(
          "AI/AN non-Hispanic",
          "NH/PI non-Hispanic",
          "Multiple non-Hispanic"
          ),
        "Other non-Hispanic",
        .
      ) %>%
      factor() %>%
      relevel(ref = "White non-Hispanic"),
    
    
    ## Career Information
    # Rename discipline
    discipline = as.character(`Researcher Type`),
    
    # Add a discipline factor variable, with the largest category (Biology) as the reference
    discipline_factor = discipline %>%
      factor() %>%
      relevel(ref = "Biology"),
    
    # Add a binary variable indicating a psychology discipline
    discipline_psych = case_when(
      # If either `Researcher Type` is a psychology field,
      `Researcher Type` %in% c(
        "Behavioral Health",
        "Clinical Psychology",
        "Cognitive Psychology",
        "Social Psychology"
      ) ~ T,
      # or the "other" response is a psychology field, T
      grepl("psychology", `Researcher Type_12_TEXT`, ignore.case = T) ~ T,
      # Otherwise, if another response was given, F
      !is.na(`Researcher Type`) ~ F,
      # Or, if no response was given, NA
      T ~ NA
    ),
    
    # Rename institution_type
    institution_type = as.character(`Institution Type`),
    
    # Rename job_title
    job_title = as.character(`Position/Role`),
    
    # Rename percent of time spent on research
    research_time = as.numeric(`Research Time_1`),
    
    # Rename percent of salary supported by research
    research_funding = as.numeric(`Research Funding_1`),
    
    # years_experience will be merged on, as that had to be recoded manually
    years_experience = years_experience_clean, 
    
    # Mentorship
    mentor_undergrad = grepl_na("Undergraduate student", `Research mentorship`),
    mentor_postbac = grepl_na("Post-Bac", `Research mentorship`),
    mentor_masters = grepl_na("Master's Student", `Research mentorship`),
    mentor_doctoral = grepl_na("Doctoral Student", `Research mentorship`),
    mentor_postdoc = grepl_na("Post-Doc", `Research mentorship`),
    mentor_ecr = grepl_na("Early Career Researcher", `Research mentorship`),
    
    
    ## DS Uptake
    div_sup_applied = case_when(
      `Div Sup History` %in% c(
        "Supported an application that was awarded",
        "Supported an application that was NOT awarded"
      ) ~ T,
      !is.na(`Div Sup History`) ~ F,
      is.na(`Div Sup History`) ~ NA
    ),
    
    # n_past_applications will be merged on, as that had to be recoded manually
    n_past_applications = n_past_applications_clean,
    
    ## DS Success
    div_sup_granted = case_when(
      `Div Sup History` == "Supported an application that was awarded" ~ T,
      `Div Sup History` == "Supported an application that was NOT awarded" ~ F,
      T ~ NA
    ),
    
    # n_past_awards will be merged on, as that had to be recoded manually
    n_past_awards = n_past_awards_clean,
    
    ## Trainees supported on a DS
    supported_undergrad = either(grepl_na("Undergraduate", `DS trainee type`), grepl_na("Undergraduate", Q36)),
    supported_postbac = either(grepl_na("Post-Bac", `DS trainee type`), grepl_na("Post-Bac", Q36)),
    supported_masters = either(grepl_na("Master's Student", `DS trainee type`), grepl_na("Master's Student", Q36)),
    supported_doctoral = either(grepl_na("Doctoral Student", `DS trainee type`), grepl_na("Doctoral Student", Q36)),
    supported_postdoc = either(grepl_na("Post-Doc", `DS trainee type`), grepl_na("Post-Doc", Q36)),
    supported_ecr = either(grepl_na("Early Career Researcher", `DS trainee type`), grepl_na("Early Career Researcher", Q36)),
    
    
    ## Grant characteristics
    # Parent grant type
    parent_grant_type_R01 = grepl_na("R01", `DS parent grant type`),
    parent_grant_type_R34 = grepl_na("R34", `DS parent grant type`),
    parent_grant_type_R6133 = grepl_na("R61/33", `DS parent grant type`),
    parent_grant_type_other = grepl_na("Other (please specify)", `DS parent grant type`),
    
    # Parent grant year
    parent_grant_year_1 = grepl_na("1st", `year of grant for DS`),
    parent_grant_year_2 = grepl_na("2nd", `year of grant for DS`),
    parent_grant_year_3 = grepl_na("3rd", `year of grant for DS`),
    parent_grant_year_4 = grepl_na("4th", `year of grant for DS`),
    parent_grant_year_5 = grepl_na("5th", `year of grant for DS`),
    parent_grant_year_NCE = grepl_na("No Cost Extension Year(s)", `year of grant for DS`),
    
    .keep = "none"
    
  ) %>%
  
  # Join with top barriers from barrier dataset
  left_join(
    barrier_data %>%
      distinct(response_id, top_barrier),
    by = "response_id"
  )

# Remove dataset attributes
attr(clean_data, "column_map") <- NULL


## Save data
saveRDS(clean_data, here("Data", "Clean Data.rds"))
write.csv(clean_data, here("Data", "Clean Data.csv"), row.names = F)
