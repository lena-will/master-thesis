elasticNet_w2 <- function(gdp,
                          attention,
                          min_train,
                          min_test,
                          max_test) {
  # filter for relevant weeks
  y_m1 <- gdp %>%
    filter(week == 2)
  X_m1 <- attention %>%
    filter(week_avail == 2)
  
  # define nowcasting window
  window <- y_m1 %>%
    select(Date) %>%
    filter(Date >= min_test & Date <= max_test)
  window <- as.matrix(window)
  
  # initiate result vectors
  
  predictions <- c()
  oos_error <- c()
  oos_error_all <- NULL
  rmsfe <- NULL
  
  alpha_ini <- as.matrix(seq(
    from = 0.1,
    to = 0.9,
    length.out = 9
  ))
  
  # nowcasting
  
  for (ii in 1:nrow(alpha_ini)) {
    for (month in 1:(nrow(window)-1)) {
      window_test <- window[month]
      window_test_max <- window[month+1]
      y_m1_train <- y_m1 %>%
        filter(Date >= min_train & Date < window_test) %>%
        select(gdp_growth)
      y_m1_train <- as.matrix(y_m1_train)
      X_m1_train <- X_m1 %>%
        filter(month >= min_train & month < window_test) %>%
        select(-c(month, Date, week_avail, quarter_avail))
      
      mean_x <- apply(X_m1_train, 2, mean)
      sd_x <- apply(X_m1_train, 2, sd)
      
      X_m1_train_z <- scale(X_m1_train, center = mean_x, scale = sd_x)
      
      fit_en <-
        cv.glmnet(
          X_m1_train_z,
          y_m1_train,
          alpha = alpha_ini[ii],
          type.measure = "mse",
          nfolds = 10,
          family = "gaussian"
        )
      
      X_m1_test <- X_m1 %>%
        filter(month >= window_test & month < window_test_max) %>%
        select(-c(month, Date, week_avail, quarter_avail))
      X_m1_test_z <- scale(X_m1_test, center = mean_x, scale = sd_x)
      
      y_m1_test <- y_m1 %>%
        filter(Date >= window_test & Date < window_test_max) %>%
        select(gdp_growth)
      y_m1_test <- as.matrix(y_m1_test)
      
      predict_en <-
        predict(fit_en, s = fit_en$lambda.1se, newx = X_m1_test_z)
      
      predictions[month] <- predict_en
      
      oos_error[month] <- (predict_en - y_m1_test)
      
    }
    oos_error_prep <- t(oos_error)
    oos_error_all <- oos_error_all %>%
      rbind(oos_error_prep)
    rmsfe[ii] <- sqrt(mean(oos_error ^ 2))
  }
  min_rmsfe <- min(rmsfe)
  min_index <- which(rmsfe == min(rmsfe))
  oos_error_min <- oos_error_all[min_index[1], ]
  results <- list(min_rmsfe, oos_error_all, oos_error_min, min_index)
  
  
}