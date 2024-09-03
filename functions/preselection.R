preselection <- function(gdp,
                         attention_preselection,
                         esi_preselection,
                         cpi_preselection,
                         term_spread_preselection,
                         vacancies_preselection,
                         ip_index_preselection,
                         min_train,
                         max_train) {
  gdp <- gdp %>%
    filter(Quarter >= min_train & Quarter <= max_train) %>%
    select(-Quarter)
  
  attention_preselection <- attention_preselection %>%
    filter(Quarter >= min_train & Quarter <= max_train)
  
  esi_preselection <- esi_preselection %>%
    filter(Quarter >= min_train & Quarter <= max_train)
  
  cpi_preselection <- cpi_preselection %>%
    filter(Quarter >= min_train & Quarter <= max_train)

  vacancies_preselection <- vacancies_preselection %>%
    filter(Quarter >= min_train & Quarter <= max_train)

  term_spread_preselection <- term_spread_preselection %>%
    filter(Quarter >= min_train & Quarter <= max_train)
  
  ip_index_preselection <- ip_index_preselection %>%
    filter(Quarter >= min_train & Quarter <= max_train)
  
  t_stat <- NULL
  
  for (ii in 2:length(attention_preselection)) {
    indicator <- attention_preselection[, c(1, ii)]
    X <- esi_preselection %>%
      left_join(cpi_preselection) %>%
      left_join(term_spread_preselection) %>%
      left_join(ip_index_preselection) %>%
      left_join(vacancies_preselection) %>%
      left_join(indicator) %>%
      select(-Quarter)
    
    mean_x <- apply(X, 2, mean)
    sd_x <- apply(X, 2, sd)
    
    X <- scale(X, center = mean_x, scale = sd_x)
    
    X <- as.data.frame(X) %>%
      mutate(intercept = 1, .before = 1)
    
    X <- as.matrix(X)
    y <- as.matrix(gdp)
    
    beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
    sigma_hat <- t(y - X %*% beta_hat) %*% (y - X %*% beta_hat) / (nrow(X) - ncol(X))
    inv_X <- solve(t(X) %*% X)
    t_stat_tmp <- abs(beta_hat[7, 1] / sqrt(sigma_hat %*% inv_X[7, 7]))
    t_stat_tmp <- as.data.frame(t_stat_tmp)
    t_stat <- bind_rows(t_stat, t_stat_tmp)
  }
  
  t_stat_ordered <- t_stat %>%
    mutate(keyword = colnames(attention_preselection)[2:ncol(attention_preselection)]) %>%
    rename(t_stat = V1) %>%
    arrange(desc(t_stat))
  
  category_choice <- t_stat_ordered %>%
    mutate(
      tau = case_when(
        t_stat < 0.8416 ~ 1.0,
        t_stat >= 0.8416 & t_stat < 1.2816 ~ 0.2,
        t_stat >= 1.2816 & t_stat < 1.6449 ~ 0.1,
        t_stat >= 1.6449 & t_stat < 1.96 ~ 0.05,
        t_stat >= 1.96 & t_stat < 2.3263 ~ 0.025,
        t_stat >= 2.3263 & t_stat < 2.5758 ~ 0.01,
        t_stat >= 2.5758 ~ 0.005
      )
    )
}