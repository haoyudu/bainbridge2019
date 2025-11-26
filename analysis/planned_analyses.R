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

# save drawing-level accuracy data
write_csv(drawing_accuracy, "../output/drawing_accuracy_data.csv")
cat("Drawing-level accuracy data saved to: drawing_accuracy_data.csv\n")

# confirmatory analysis: wilcoxon rank-sum test
delayed_recall_acc <- drawing_accuracy %>% 
  filter(condition == "delayed_recall") %>% 
  pull(accuracy)

category_acc <- drawing_accuracy %>% 
  filter(condition == "category") %>% 
  pull(accuracy)

# primary test
wilcox_result <- wilcox.test(delayed_recall_acc, category_acc, 
                             alternative = "two.sided", exact = FALSE)

# effect size (rank-biserial correlation)
library(effectsize)
rank_biserial_r <- rank_biserial(delayed_recall_acc, category_acc)

# sensitivity analysis: t-test
shapiro_delayed <- shapiro.test(delayed_recall_acc)
shapiro_category <- shapiro.test(category_acc)
levene_result <- car::leveneTest(accuracy ~ condition, 
                                 data = drawing_accuracy %>% filter(condition %in% c("delayed_recall", "category")))

# t-test (welch if variances unequal)
if(levene_result$`Pr(>F)`[1] < 0.05) {
  t_result <- t.test(delayed_recall_acc, category_acc, 
                     alternative = "two.sided", var.equal = FALSE)
} else {
  t_result <- t.test(delayed_recall_acc, category_acc, 
                     alternative = "two.sided", var.equal = TRUE)
}

# cohen's d
cohens_d <- effectsize::cohens_d(delayed_recall_acc, category_acc)

# summary statistics
summary_stats <- drawing_accuracy %>%
  group_by(condition) %>%
  summarise(
    n_drawings = n(),
    mean_accuracy = mean(accuracy),
    sd_accuracy = sd(accuracy),
    median_accuracy = median(accuracy),
    min_accuracy = min(accuracy),
    max_accuracy = max(accuracy),
    .groups = "drop"
  )



print(summary_stats)

cat("\n=== CONFIRMATORY ANALYSIS ===\n")
cat("Wilcoxon rank-sum test:\n")
cat("W =", wilcox_result$statistic, ", p =", wilcox_result$p.value, "\n")
cat("Rank-biserial r =", rank_biserial_r$r_rank_biserial, 
    ", 95% CI [", rank_biserial_r$CI_low, ",", rank_biserial_r$CI_high, "]\n")

cat("\n=== SENSITIVITY ANALYSIS ===\n")
cat("Assumption checks:\n")
cat("Delayed recall normality: W =", shapiro_delayed$statistic, ", p =", shapiro_delayed$p.value, "\n")
cat("Category normality: W =", shapiro_category$statistic, ", p =", shapiro_category$p.value, "\n")
cat("Levene's test: F =", levene_result$`F value`[1], ", p =", levene_result$`Pr(>F)`[1], "\n")

cat("\nt-test:\n")
cat("t =", t_result$statistic, ", df =", t_result$parameter, ", p =", t_result$p.value, "\n")
cat("Cohen's d =", cohens_d$Cohens_d, 
    ", 95% CI [", cohens_d$CI_low, ",", cohens_d$CI_high, "]\n")

library(ggplot2)

# create plot similar to original paper
ggplot(drawing_accuracy, aes(x = condition, y = accuracy)) +
  geom_point(alpha = 0.6, position = position_jitter(width = 0.2)) +
  stat_summary(fun = mean, geom = "crossbar", color = "red", width = 0.3) +
  geom_hline(yintercept = 0.333, linetype = "dashed", color = "gray") +
  labs(title = "Drawing-to-Picture Matching Accuracy", 
       x = "Drawing Condition", y = "Proportion Correct",
       caption = "Dashed line = chance (33.3%)") +
  ylim(0, 1) +
  theme_minimal()

# side-by-side histograms
ggplot(drawing_accuracy, aes(x = accuracy, fill = condition)) +
  geom_histogram(alpha = 0.7, bins = 20, position = "identity") +
  geom_vline(xintercept = 0.333, linetype = "dashed") +
  facet_wrap(~condition) +
  labs(title = "Distribution of Accuracy Scores by Condition",
       x = "Accuracy", y = "Count") +
  theme_minimal()

