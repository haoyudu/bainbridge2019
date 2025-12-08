library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)

# load data
json_files <- list.files("~/Desktop/full_sample_raw", pattern = ".*.json$", full.names = TRUE)
raw_data <- map_dfr(json_files, function(file) {
  data <- fromJSON(file, flatten = TRUE)
  return(data)
})

# create trial-level dataset with only needed columns
trial_data <- raw_data %>%
  filter(trial_type == "image-button-response", task == "main_trial") %>%
  select(participant_id, trial_number, drawing_filename, condition, category, 
         memorability, target_image, selected_image, correct, selected_type, 
         rt, is_attention_check, time_elapsed)

# extract survey data for technical difficulties
survey_data <- raw_data %>%
  filter(trial_type %in% c("survey-multi-choice", "survey-text")) %>%
  select(participant_id, trial_type, response) %>%
  mutate(
    technical_difficulties = case_when(
      str_detect(response, '"technical_difficulties"') ~ 
        str_extract(response, '(?<="technical_difficulties":)[^,}]+') %>% str_remove_all('["]'),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(technical_difficulties)) %>%
  distinct(participant_id, .keep_all = TRUE)

# participant-level exclusions
participants_to_exclude <- trial_data %>%
  group_by(participant_id) %>%
  summarise(
    n_trials = n(),
    n_attention_checks = sum(is_attention_check, na.rm = TRUE),
    attention_check_failures = sum(is_attention_check & !correct, na.rm = TRUE),
    session_duration_min = (max(time_elapsed, na.rm = TRUE) - min(time_elapsed, na.rm = TRUE)) / 60000,
    median_rt = median(rt, na.rm = TRUE),
    delayed_recall_accuracy = mean(correct[condition == "delayed_recall"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(survey_data %>% select(participant_id, technical_difficulties), by = "participant_id") %>%
  mutate(
    exclude_attention = attention_check_failures >= 2,
    exclude_incomplete = n_trials < 60,
    exclude_timeout = session_duration_min > 30,
    exclude_fast_rt = median_rt < 1200,
    exclude_low_accuracy = delayed_recall_accuracy < 0.333,
    exclude_technical = technical_difficulties == "Yes",
    exclude_any = exclude_attention | exclude_incomplete | exclude_timeout | 
      exclude_fast_rt | exclude_low_accuracy | exclude_technical
  )

# trial-level exclusions  
trial_data_clean <- trial_data %>%
  filter(!participant_id %in% filter(participants_to_exclude, exclude_any)$participant_id) %>%
  filter(rt >= 1200) %>%
  filter(!is_attention_check)

# check if >20% trials excluded per participant
trial_exclusion_check <- trial_data %>%
  filter(!participant_id %in% filter(participants_to_exclude, exclude_any)$participant_id) %>%
  filter(!is_attention_check) %>%
  group_by(participant_id) %>%
  summarise(
    total_trials = n(),
    valid_trials = sum(rt >= 1200),
    pct_excluded = (total_trials - valid_trials) / total_trials,
    .groups = "drop"
  ) %>%
  filter(pct_excluded > 0.20)

# exclude participants with >20% trial exclusions
final_trial_data <- trial_data_clean %>%
  filter(!participant_id %in% trial_exclusion_check$participant_id) %>%
  select(participant_id, trial_number, drawing_filename, condition, category, 
         memorability, target_image, selected_image, correct, selected_type, rt)

# save participant exclusion summary
write_csv(participants_to_exclude, "../output/participant_exclusions_full_sample.csv")
cat("Participant exclusion data saved to: participant_exclusions.csv\n")

# save clean trial-level data
write_csv(final_trial_data, "../output/clean_trial_data_full_sample.csv")
cat("Clean trial-level data saved to: clean_trial_data.csv\n")

cat("Participants with >20% trials excluded:", nrow(trial_exclusion_check), "\n")
cat("Final sample size:", length(unique(final_trial_data$participant_id)), "participants\n")
cat("Total valid trials:", nrow(final_trial_data), "\n")


# calculate drawing-level accuracy
drawing_accuracy <- final_trial_data %>%
  group_by(drawing_filename, condition, category, memorability) %>%
  summarise(
    n_raters = n(),
    accuracy = case_when(
      condition == "delayed_recall" ~ mean(correct, na.rm = TRUE),
      condition == "category" ~ {
        prop_high <- mean(selected_type == "high", na.rm = TRUE)
        prop_low <- mean(selected_type == "low", na.rm = TRUE)
        (prop_high + prop_low) / 2
      }
    ),
    .groups = "drop"
  ) %>%
  # PILOT B ONLY
  # filter(n_raters >= 1)
  filter(n_raters >= 10)
