baseline_w13 <- function(gdp, esi_bridge, cpi_bridge, vacancies_bridge, term_spread_bridge, ip_index, min_train, max_train, max_test){
  
  y_m1 <- gdp %>%
    filter(week == 13)
  esi_prep <- esi_bridge %>% 
    filter(week == 13) %>% 
    select(c(Date, ESI_b))
  vacancies_prep <- vacancies_bridge %>% 
    filter(week == 13) %>% 
    select(vacancies_mom_b)
  cpi_prep <- cpi_bridge %>% 
    filter(week == 13) %>% 
    select(cpi_mom_b)
  term_spread_prep <- term_spread_bridge %>% 
    filter(week == 11) %>% 
    select(spread_b)
  ip_index_prep <- ip_index %>% 
    filter(week == 11) %>% 
    select(ip_mom)
  X_m1 <- esi_prep %>% 
    cbind(vacancies_prep) %>% 
    cbind(cpi_prep) %>% 
    cbind(term_spread_prep) %>% 
    cbind(ip_index_prep)
  
  # define nowcasting window
  window <- y_m1 %>%
    select(Date) %>%
    filter(Date >= max_train & Date <= max_test)
  window <- as.matrix(window)
  
  oos_error <- c()
  
  for (month in 1:(nrow(window) - 1)) {
    window_test <- window[month]
    window_test_max <- window[month + 1]
    print(window_test_max)
    y_m1_train <- y_m1 %>%
      filter(Date >= min_train & Date < window_test) %>%
      select(gdp_growth)
    y_m1_train <- as.matrix(y_m1_train)
    X_m1_train <- X_m1 %>%
      filter(Date >= min_train & Date < window_test) %>%
      select(-c(Date))
    
    mean_x <- apply(X_m1_train, 2, mean)
    sd_x <- apply(X_m1_train, 2, sd)
    
    X_m1_train_z <- scale(X_m1_train, center = mean_x, scale = sd_x)
    
    alpha_ini <- as.matrix(seq(
      from = 0.01,
      to = 2,
      length.out = 100
    ))
    n <- nrow(y_m1_train)
    gcv <- c()
    
    for (ii in 1:length(alpha_ini)) {
      ident <- diag(ncol(X_m1_train_z))
      alpha <- alpha_ini[ii] * n
      beta_hat_pls <-
        solve(t(X_m1_train_z) %*% X_m1_train_z + alpha * ident) %*% t(X_m1_train_z) %*%
        y_m1_train
      y_hat_pls <- X_m1_train_z %*% beta_hat_pls
      gcv[ii] <-
        (1 / n) %*% t(y_m1_train - y_hat_pls) %*% (y_m1_train - y_hat_pls) / (1 -
                                                                                sum(diag(
                                                                                  X_m1_train_z %*% solve(t(X_m1_train_z) %*% X_m1_train_z + alpha * ident) %*% t(X_m1_train_z)
                                                                                )) * (1 / n)) ^ 2
    }
    
    gcv_min <- which(gcv == min(gcv))
    alpha_min <- alpha_ini[gcv_min] * n
    beta_hat_opt <-
      solve(t(X_m1_train_z) %*% X_m1_train_z + alpha_min * ident) %*% t(X_m1_train_z) %*%
      y_m1_train
    
    X_m1_test <- X_m1 %>%
      filter(Date >= window_test & Date < window_test_max) %>%
      select(-c(Date))
    X_m1_test_z <- scale(X_m1_test, center = mean_x, scale = sd_x)
    
    y_m1_test <- y_m1 %>%
      filter(Date >= window_test & Date < window_test_max) %>%
      select(gdp_growth)
    y_m1_test <- as.matrix(y_m1_test)
    
    y_pred <- X_m1_test_z %*% beta_hat_opt
    
    oos_error[month] <- (y_pred - y_m1_test)
    
  }
  
  rmsfe <- sqrt(mean(oos_error ^ 2))
}