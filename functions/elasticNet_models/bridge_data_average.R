bridge_data_average <- function(data, start_col){
  data_bridge <- data %>%
    mutate(across(all_of(c(
      start_col:ncol(data)
    )), ~ ., .names = "{col}_b"))
  
  skip <- ncol(data) - (start_col-1)
  for (j in start_col:ncol(data)) {
    for (ii in 1:nrow(data_bridge)) {
      if (data_bridge$week_avail[ii] == 1) {
        data_bridge[ii, j + skip] = data_bridge[ii, j]
      } else if (data_bridge$week_avail[ii] == 2) {
        data_bridge[ii, j + skip] = (data_bridge[ii, j] + data_bridge[ii - 1, j]) / 2
      } else if (data_bridge$week_avail[ii] == 3) {
        data_bridge[ii, j + skip] = (data_bridge[ii, j] + data_bridge[ii - 1, j] + data_bridge[ii - 2, j]) / 3
      }else if (data_bridge$week_avail[ii] == 4) {
        data_bridge[ii, j + skip] = (data_bridge[ii, j] + data_bridge[ii - 1, j] + data_bridge[ii - 2, j] + data_bridge[ii - 3, j]) / 4
      }else if (data_bridge$week_avail[ii] == 5) {
        data_bridge[ii, j + skip] = (data_bridge[ii, j] + data_bridge[ii - 1, j] + data_bridge[ii - 2, j] + data_bridge[ii - 3, j] + data_bridge[ii - 4, j]) / 5
      }
      else if (data_bridge$week_avail[ii] == 6) {
        data_bridge[ii, j + skip] = (data_bridge[ii, j] + data_bridge[ii - 1, j] + data_bridge[ii - 2, j] + data_bridge[ii - 3, j] + data_bridge[ii - 4, j] + data_bridge[ii - 5, j]) / 6
      }
      else if (data_bridge$week_avail[ii] == 7) {
        data_bridge[ii, j + skip] = (data_bridge[ii, j] + data_bridge[ii - 1, j] + data_bridge[ii - 2, j] + data_bridge[ii - 3, j] + data_bridge[ii - 4, j] + data_bridge[ii - 5, j] + data_bridge[ii - 6, j]) / 7
      }
      else if (data_bridge$week_avail[ii] == 8) {
        data_bridge[ii, j + skip] = (data_bridge[ii, j] + data_bridge[ii - 1, j] + data_bridge[ii - 2, j] + data_bridge[ii - 3, j] + data_bridge[ii - 4, j]+ data_bridge[ii - 5, j] + data_bridge[ii - 6, j] + data_bridge[ii - 7, j]) / 8
      }
      else if (data_bridge$week_avail[ii] == 9) {
        data_bridge[ii, j + skip] = (data_bridge[ii, j] + data_bridge[ii - 1, j] + data_bridge[ii - 2, j] + data_bridge[ii - 3, j] + data_bridge[ii - 4, j] + data_bridge[ii - 5, j]+ data_bridge[ii - 6, j] + data_bridge[ii - 7, j] + data_bridge[ii - 8, j]) / 9
      }
      else if (data_bridge$week_avail[ii] == 10) {
        data_bridge[ii, j + skip] = (data_bridge[ii, j] + data_bridge[ii - 1, j] + data_bridge[ii - 2, j] + data_bridge[ii - 3, j] + data_bridge[ii - 4, j] + data_bridge[ii - 5, j] + data_bridge[ii - 6, j] + data_bridge[ii - 7, j] + data_bridge[ii - 8, j] + data_bridge[ii - 9, j]) / 10
      }
      else if (data_bridge$week_avail[ii] == 11) {
        data_bridge[ii, j + skip] = (data_bridge[ii, j] + data_bridge[ii - 1, j] + data_bridge[ii - 2, j] + data_bridge[ii - 3, j] + data_bridge[ii - 4, j] + data_bridge[ii - 5, j] + data_bridge[ii - 6, j] + data_bridge[ii - 7, j] + data_bridge[ii - 8, j] + data_bridge[ii - 9, j] + data_bridge[ii - 10, j]) / 11
      }
      else if (data_bridge$week_avail[ii] == 12) {
        data_bridge[ii, j + skip] = (data_bridge[ii, j] + data_bridge[ii - 1, j] + data_bridge[ii - 2, j] + data_bridge[ii - 3, j] + data_bridge[ii - 4, j] + data_bridge[ii - 5, j] + data_bridge[ii - 6, j] + data_bridge[ii - 7, j] + data_bridge[ii - 8, j] + data_bridge[ii - 9, j] + data_bridge[ii - 10, j] + data_bridge[ii - 11, j]) / 12
      }
      else{
        data_bridge[ii, j + skip] = (data_bridge[ii, j] + data_bridge[ii - 1, j] + data_bridge[ii - 2, j] + data_bridge[ii - 3, j] + data_bridge[ii - 4, j] + data_bridge[ii - 5, j] + data_bridge[ii - 6, j] + data_bridge[ii - 7, j] + data_bridge[ii - 8, j] + data_bridge[ii - 9, j] + data_bridge[ii - 10, j] + data_bridge[ii - 11, j] + data_bridge[ii - 11, j]) / 13
      }
    }
  }
data_bridge
}