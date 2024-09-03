# Housekeeping -----------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(kableExtra)

# Load preselection results and topic names  -----------------------------------

load("/Users/lena/Documents/R/master_thesis/preselection_results_tone.Rda")
topic_names <- read.csv("/Users/lena/Documents/R/master_thesis/topics/topic_name_mapping.csv") %>% 
  mutate(topic = as.character(topic))

# Preslection Period 1 ---------------------------------------------------------

period1 <- preselection_results$p1 %>% 
  rename(topic = keyword) %>% 
  mutate(topic = as.character(topic))

period1$topic <- gsub("X", "", period1$topic)
period1$tau <- gsub("0.01","1%", period1$tau)
period1$tau <- gsub("0.025","2.5%", period1$tau)
period1$tau <- gsub("0.05","5%", period1$tau)
period1$tau <- gsub("0.1","10%", period1$tau)
period1$tau <- gsub("0.2","20%", period1$tau)
period1$tau <- gsub("0.005","0.05%", period1$tau)

period_1_names <- period1 %>% 
  left_join(topic_names) %>% 
  select(-topic) %>% 
  relocate(name, .before = 1)

# Preslection Period 2 ---------------------------------------------------------

period2 <- preselection_results$p2 %>% 
  rename(topic = keyword) %>% 
  mutate(topic = as.character(topic))

period2$topic <- gsub("X", "", period2$topic)
period2$tau <- gsub("0.05","5%", period2$tau)
period2$tau <- gsub("0.1","10%", period2$tau)
period2$tau <- gsub("0.005","0.5%", period2$tau)
period2$tau <- gsub("0.2","20%", period2$tau)
period2$tau <- gsub("0.005","0.05%", period2$tau)

period_2_names <- period2 %>% 
  left_join(topic_names) %>% 
  select(-topic) %>% 
  relocate(name, .before = 1)

# Preslection Period 3 ---------------------------------------------------------

period3 <- preselection_results$p3 %>% 
  rename(topic = keyword) %>% 
  mutate(topic = as.character(topic))

period3$topic <- gsub("X", "", period3$topic)
period3$tau <- gsub("0.005","0.5%", period3$tau)
period3$tau <- gsub("0.05","5%", period3$tau)
period3$tau <- gsub("0.1","10%", period3$tau)
period3$tau <- gsub("0.2","20%", period3$tau)
period3$tau <- gsub("0.005","0.05%", period3$tau)

period3_names <- period3 %>% 
  left_join(topic_names) %>% 
  select(-topic) %>% 
  relocate(name, .before = 1)


# Preslection Period 4 ---------------------------------------------------------

period4 <- preselection_results$p4 %>% 
  rename(topic = keyword) %>% 
  mutate(topic = as.character(topic))

period4$topic <- gsub("X", "", period4$topic)
period4$tau <- gsub("0.05","5%", period4$tau)
period4$tau <- gsub("0.1","10%", period4$tau)
period4$tau <- gsub("0.005","0.05%", period4$tau)
period4$tau <- gsub("0.01","1%", period4$tau)
period4$tau <- gsub("0.025","2.5%", period4$tau)
period4$tau <- gsub("0.2","20%", period4$tau)

period4_names <- period4 %>% 
  left_join(topic_names) %>% 
  select(-topic) %>% 
  relocate(name, .before = 1)

# Get latex tables -------------------------------------------------------------

kable(period4_names, "latex")
