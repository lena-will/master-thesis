# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(glmnet)

# load text-based indicators ---------------------------------------------------
attention <- read.csv("/Users/lena/Documents/R/master_thesis/attention_issue_mapping.csv") %>%
  select(-X) %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

col_names <- c("Date", "quarter_avail", "week_avail", rep(1:60, 1))
colnames(attention) <- col_names

attention$month <- as.Date(sub("\\d{2}$", "1", attention$Date))

attention <- attention %>% 
  relocate(month, .before = quarter_avail)

# attention <- attention %>%
#   relocate(quarter_avail, .after = Date) %>%
#   relocate(week_avail, .after = quarter_avail)

# load macro data --------------------------------------------------------------

# esi

esi <- read.csv("/Users/lena/Documents/R/master_thesis/data/esi.csv") %>%
  select(-X) %>%
  mutate(week = case_when(
    month(Date) == 1 |
      month(Date) == 4 |
      month(Date) == 7 |
      month(Date) == 10 ~ 4,
    month(Date) == 2 |
      month(Date) == 5 |
      month(Date) == 8 |
      month(Date) == 11 ~ 9,
    month(Date) == 3 |
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 13))

# cpi

cpi <- read.csv("/Users/lena/Documents/R/master_thesis/data/cpi.csv") %>%
  select(-X) %>%
  mutate(week = case_when(
    month(Date) == 1 |
      month(Date) == 4 |
      month(Date) == 7 |
      month(Date) == 10 ~ 4,
    month(Date) == 2 |
      month(Date) == 5 |
      month(Date) == 8 |
      month(Date) == 11 ~ 9,
    month(Date) == 3 |
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 13))

# vacancies 

vacancies <- read.csv("/Users/lena/Documents/R/master_thesis/data/vacancies.csv") %>%
  select(-X) %>%
  mutate(week = case_when(
    month(Date) == 1 |
      month(Date) == 4 |
      month(Date) == 7 |
      month(Date) == 10 ~ 4,
    month(Date) == 2 |
      month(Date) == 5 |
      month(Date) == 8 |
      month(Date) == 11 ~ 9,
    month(Date) == 3 |
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 13))

# term spread

term_spread <- read.csv("/Users/lena/Documents/R/master_thesis/data/term_spread.csv") %>%
  select(-X) %>%
  mutate(week = case_when(
    month(Date) == 1 |
      month(Date) == 4 |
      month(Date) == 7 |
      month(Date) == 10 ~ 7,
    month(Date) == 2 |
      month(Date) == 5 |
      month(Date) == 8 |
      month(Date) == 11 ~ 11,
    month(Date) == 3 |
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 0))

# ip index 

ip_index <- read.csv("/Users/lena/Documents/R/master_thesis/data/ip_index.csv") %>%
  select(-X) %>%
  mutate(week = case_when(
    month(Date) == 1 |
      month(Date) == 4 |
      month(Date) == 7 |
      month(Date) == 10 ~ 11,
    month(Date) == 2 |
      month(Date) == 5 |
      month(Date) == 8 |
      month(Date) == 11 ~ 0,
    month(Date) == 3 |
      month(Date) == 6 | month(Date) == 9 | month(Date) == 12 ~ 0))

# gdp 

gdp <- read.csv("/Users/lena/Documents/R/master_thesis/data/gdp_weekly.csv") %>% 
  rename(week = quarter_week)

#Period 1: Recession - trainings sample: 2005Q1-2007Q2

min_train <- "2001-10-01"
max_train <- "2007-06-30"
max_test <- "2009-06-30"

y_m1 <- gdp %>% 
  filter(week == 1)
X_m1 <- attention %>% 
  filter(week_avail == 1)


window <- y_m1 %>%
  select(Date) %>%
  filter(Date >= max_train & Date <= max_test)
window <- as.matrix(window)

predictions <- c()
oos_error <- c()
oos_error_all <- NULL
rmsfe <- NULL

alpha_ini <- as.matrix(seq(
  from = 0.1,
  to = 0.9,
  length.out = 9
))
month = 1
#for (ii in 1:nrow(alpha_ini)) {
  #for(month in 1:(nrow(window)-2)){
    y_m1_train <- y_m1 %>% 
      filter(Date >= min_train & Date < window[month]) %>% 
      select(gdp_growth)
    y_m1_train <- as.matrix(y_m1_train)
    X_m1_train <- X_m1 %>% 
      filter(month >= min_train & month < window[month]) %>% # this does not have any observations!!
      select(-c(month, Date, week_avail, quarter_avail))
    
    mean_x <- apply(X_m1_train, 2, mean)
    sd_x <- apply(X_m1_train, 2, sd)
    
    X_m1_train_z <- scale(X_m1_train, center = mean_x, scale = sd_x)
    
    fit_en <-
      cv.glmnet(
        X_m1_train_z,
        y_m1_train,
        alpha = 0.5,
        #alpha = alpha_ini[ii],
        type.measure = "mse",
        nfolds = 10,
        family = "gaussian"
      )
    
    X_m1_test <- X_m1 %>% 
      filter(Date == window[month + 2]) %>% 
      select(-c(Date, week_avail, quarter_avail))
    X_m1_test_z <- scale(X_m1_test, center = mean_x, scale = sd_x)
    
    y_m1_test <- y_m1 %>% 
      filter(Date == window[month + 2]) %>% 
      select(gdp_growth)
    y_m1_test <- as.matrix(y_m1_test)
    
    predict_en <-
      predict(fit_en, s = fit_en$lambda.1se, newx = X_m1_test_z)
    
    predictions[month] <- predict_en
    
    oos_error[month] <- (predict_en - y_m1_test)
    
  #}
  oos_error_prep <- t(oos_error)
  oos_error_all <- oos_error_all %>% 
    rbind(oos_error_prep)
  rmsfe[ii] <- sqrt(mean(oos_error ^ 2))
#}
min_rmsfe <- min(rmsfe)
min_index <- which(rmsfe == min(rmsfe))
oos_error_min <- oos_error_all[min_index[1], ]
results <- list(min_rmsfe, oos_error_all, oos_error_min, min_index)











