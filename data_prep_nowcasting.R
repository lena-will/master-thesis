# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(lubridate)

z_scores <- function(x) {
  (x - mean(x)) / sd(x)
}

# load text-based indicators ---------------------------------------------------

attention <- read.csv("/Users/lena/Documents/R/master_thesis/attention_issue_mapping.csv") %>% 
  select(-X)

col_names <- c("Date", rep(1:60, 1), "quarter_avail", "week_avail")
colnames(attention) <- col_names

attention <- attention %>% 
  relocate(quarter_avail, .after = Date) %>% 
  relocate(week_avail, .after = quarter_avail)