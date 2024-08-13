# housekeeping -----------------------------------------------------------------

library(tidyverse)
library(tidyr)

# load csv with harvard_iv_4 dictionary ----------------------------------------

dict_ini <- read.csv("/Users/lena/Documents/R/master_thesis/havard_HIV_4.csv") %>% 
  select(c(Entry, Positiv, Negativ)) %>% 
  mutate(Entry = tolower(Entry)) %>% 
  mutate(Entry = str_replace_all(Entry, "#", ""))

dict_ini$Entry <- gsub('[[:digit:]]+', '', dict_ini$Entry)

dict <- dict_ini %>% 
  distinct(Entry, .keep_all = TRUE)

positiv <- dict %>% 
  filter(Positiv == "Positiv") %>% 
  select(-c(Negativ, Positiv))

negativ <- dict %>% 
  filter(Negativ == "Negativ") %>% 
  select(-c(Negativ, Positiv))

write.csv(negativ, "/Users/lena/Documents/R/master_thesis/negativ_en.csv")
write.csv(positiv, "/Users/lena/Documents/R/master_thesis/positiv_en.csv")
