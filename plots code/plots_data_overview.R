# Housekeeping -----------------------------------------------------------------

library(tidyverse)

# define colorblind friendly palette for all plots:
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# import data from python analysis ---------------------------------------------

folder_list <- list.files("/Users/lena/Desktop/faz_data/utf_8")
path <- "/Users/lena/Desktop/faz_data/utf_8"
file_name <- "artikel_df.csv"
artikel_df = NULL

for (f in 1:length(folder_list)) {
  folder_name <- file.path(folder_list[f])
  data_tmp <- read.csv(file.path(path, folder_name, file_name)) %>% 
    rename(artikel_id = X)
  artikel_df <- artikel_df %>% 
    rbind(data_tmp)
}

# plot articles/words in initial corpus ----------------------------------------

artikel_raw_count <- artikel_df %>% 
  mutate(word_count = lengths(strsplit(Tokens_string, ' ')))

average_word_count <- artikel_raw_count %>% 
  group_by(Datum) %>% 
  summarise(mean = mean(word_count, na.rm=TRUE)) %>% 
  mutate(mean = round(mean, digits = 0))

artikel_count <- artikel_raw_count %>% 
  group_by(Datum) %>% 
  summarise(count = n()) %>% 
  rename(artikel_count = count)

artikel_raw_plot <- average_word_count %>% 
  full_join(artikel_count, by = "Datum") %>% 
  mutate(Datum = as.Date(Datum, format = "%Y-%m-%d"))

raw_word_article_plot <- artikel_raw_plot %>% 
  ggplot(aes(x = Datum)) +
  geom_line(aes(y = artikel_count, color = "article_count")) +
  geom_line(aes(y = mean*0.1, color = "mean")) +
  scale_x_date(name = "Year", date_breaks = "2 year", date_labels="%Y") +
  scale_y_continuous(name = "Articles", breaks = seq(20, 120, by = 20),
                     sec.axis = sec_axis(transform =~./0.1, name = "Words", breaks = seq(200, 1200, by = 200))) +
  scale_color_manual(values = c(article_count = "#000000", mean = "#0072B2"),
                     labels = c(article_count = "Number of articles per issue", mean = "Average number of words per issue"),
                     limits = c("article_count", "mean")) +
  theme(legend.title = element_blank(), legend.position = "top")
plot(raw_word_article_plot)

# get data on unique terms in corpus -------------------------------------------

distinct_terms_ini_before <- str_c(artikel_df$Tokens_string, sep = " ") # initialise vocabulary
distinct_terms_before <- paste(distinct_terms_ini_before, collapse = " ")
vocabulary_terms_before <- strsplit(distinct_terms_before, " ")[[1]]
vocabulary_before <- sort(unique(vocabulary_terms_before))

distinct_terms_ini_after <- str_c(artikel_df$Stemming, sep = " ") # initialise vocabulary
distinct_terms_after <- paste(distinct_terms_ini_after, collapse = " ")
vocabulary_terms_after <- strsplit(distinct_terms_after, " ")[[1]]
vocabulary_after <- sort(unique(vocabulary_terms_after)) # get unique terms which constitute the vocabulary, sorting is crucial!





