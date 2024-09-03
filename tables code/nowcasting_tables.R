# Housekeeping -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(kableExtra)

# load nowcasting results ------------------------------------------------------

# with text-based indicators

load("/Users/lena/Documents/R/master_thesis/nowcasting results/results_p1_text005.Rda")
load("/Users/lena/Documents/R/master_thesis/nowcasting results/results_p2_text005.Rda")
load("/Users/lena/Documents/R/master_thesis/nowcasting results/results_p3_text005.Rda")
load("/Users/lena/Documents/R/master_thesis/nowcasting results/results_p4_text005.Rda")

results_p1_text005 <- results_p1
results_p2_text005 <- results_p2
results_p3_text005 <- results_p3
results_p4_text005 <- results_p4

week_column <- c("week 1", "week 2", "week 3", "week 4", "week 5", "week 6", "week 7", "week 8", "week 9", "week 10", "week 11", "week 12", "week 13")
results_p1_text005$week <- week_column
results_p2_text005$week <- week_column
results_p3_text005$week <- week_column
results_p4_text005$week <- week_column

# baseline

load("/Users/lena/Documents/R/master_thesis/nowcasting results/results_p1_baseline.Rda")
load("/Users/lena/Documents/R/master_thesis/nowcasting results/results_p2_baseline.Rda")
load("/Users/lena/Documents/R/master_thesis/nowcasting results/results_p3_baseline.Rda")
load("/Users/lena/Documents/R/master_thesis/nowcasting results/results_p4_baseline.Rda")

week_column_baseline <- c( "week 4", "week 7", "week 9", "week 10", "week 11","week 13")
results_baseline_p1$week <- week_column_baseline
results_baseline_p2$week <- week_column_baseline
results_baseline_p3$week <- week_column_baseline
results_baseline_p4$week <- week_column_baseline

# tone adjustment

load("/Users/lena/Documents/R/master_thesis/nowcasting results/results_p1_tone.Rda")
load("/Users/lena/Documents/R/master_thesis/nowcasting results/results_p2_tone.Rda")
load("/Users/lena/Documents/R/master_thesis/nowcasting results/results_p3_tone.Rda")
load("/Users/lena/Documents/R/master_thesis/nowcasting results/results_p4_tone.Rda")

results_p1$week <- week_column
results_p2$week <- week_column
results_p3$week <- week_column
results_p4$week <- week_column

# Period 1 : Recession ---------------------------------------------------------

results_p1_both <- results_p1_text %>%
  left_join(results_baseline_p1) %>% 
  rename(text_indicators = results_p1) %>%
  rename(baseline = results_baseline_p1) %>%
  relocate(week, .before = 1) #%>% 
  #pivot_wider(names_from = week, values_from = text_indicators)

results_p1_all <- results_p1_both %>% 
  left_join(results_p1) %>% 
  rename(tone_adjusted = results_p1) %>% 
  relocate(baseline, .after = week) %>% 
  mutate(baseline = round(baseline, 4)) %>% 
  mutate(text_indicators = round(text_indicators, 4)) %>% 
  mutate(tone_adjusted = round(tone_adjusted, 4))

results_p1_all[is.na(results_p1_all)] <- " "

kable(results_p4_all, "latex")

# Period 2 : Cyclical Stability  -----------------------------------------------

results_p2_both <- results_p2_text %>%
  left_join(results_baseline_p2) %>% 
  rename(text_indicators = results_p2) %>%
  rename(baseline = results_baseline_p2) %>%
  relocate(week, .before = 1)

results_p2_all <- results_p2_both %>% 
  left_join(results_p2) %>% 
  rename(tone_adjusted = results_p2) %>% 
  relocate(baseline, .after = week) %>% 
  mutate(baseline = round(baseline, 4)) %>% 
  mutate(text_indicators = round(text_indicators, 4)) %>% 
  mutate(tone_adjusted = round(tone_adjusted, 4))

results_p2_all[is.na(results_p2_all)] <- " "

# Period 3 : COVID pandemic  ---------------------------------------------------

results_p3_both <- results_p3_text %>%
  left_join(results_baseline_p3) %>% 
  rename(text_indicators = results_p3) %>%
  rename(baseline = results_baseline_p3) %>%
  relocate(week, .before = 1)

results_p3_all <- results_p3_both %>% 
  left_join(results_p3) %>% 
  rename(tone_adjusted = results_p3) %>% 
  relocate(baseline, .after = week) %>% 
  mutate(baseline = round(baseline, 4)) %>% 
  mutate(text_indicators = round(text_indicators, 4)) %>% 
  mutate(tone_adjusted = round(tone_adjusted, 4))

results_p3_all[is.na(results_p3_all)] <- " "

# Period 4 : Now ---------------------------------------------------------------

results_p4_both <- results_p4_text %>%
  left_join(results_baseline_p4) %>% 
  rename(text_indicators = results_p4) %>%
  rename(baseline = results_baseline_p4) %>%
  relocate(week, .before = 1)

results_p4_all <- results_p4_both %>% 
  left_join(results_p4) %>% 
  rename(tone_adjusted = results_p4) %>% 
  relocate(baseline, .after = week) %>% 
  mutate(baseline = round(baseline, 4)) %>% 
  mutate(text_indicators = round(text_indicators, 4)) %>% 
  mutate(tone_adjusted = round(tone_adjusted, 4))

results_p4_all[is.na(results_p4_all)] <- " "

