
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggstream)

library(extrafont)
library(remotes)
# only need to run once; direct install had 'no font name' error
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Filtering & cleaning London Stage data ####
data <- read.csv('LondonStageFull.csv') %>%
  filter(., p_PerformanceTitle != "" & p_WorkId != "0")

# NOTE: removed all entries where performance title was not listed
# NOTE: removed all entries where the Work ID was listed as 0
# e.g., 'Hamlet Prince of Denmark' is listed under 0, Hamlet under 420
# not sure if there's a better way to handle this.

#data_head <- head(data,100)

# parse dates
data$e_EventDate_parsed <- ymd(data$e_EventDate)

# NOTE: removed all entries where date was not complete/could not be parsed
data <- data[!is.na(data$e_EventDate_parsed),]

# we want to collapse across all rows for the same event
# e.g. rows listing individual actors
data_clean <- data %>%
  group_by(e_EventId, e_Season, e_EventDate_parsed, t_TheatreName, p_WorkId, p_PerfTitleClean) %>%
  summarise(count = n()) %>%
  dplyr::select(-count)

# for some reason, Hamlet is listed under two separate Work IDs
# can't find evidence to consider them separately?
data_clean$p_PerfTitleClean <- sub('Hamlet Prince Of Denmark', 'Hamlet', data_clean$p_PerfTitleClean)

# note that the number of rows is not going to match the number of unique event IDs
# because sometimes multiple plays occurred on the same night: same event ID

# get most popular longest running plays
# out of the top 50 longest running plays, we took the 10 most popular ones
# popular = most repeated
list_plays <- data_clean %>%
  group_by(p_PerfTitleClean) %>%
  summarise(Begin = min(e_EventDate_parsed),
            End = max(e_EventDate_parsed),
            count = n(),
            time_dif = End - Begin) %>%
  top_n(n = 50, wt = time_dif) %>%
  top_n(n = 10, wt = count)

# now, we want to go back to the cleaned full dataset
# and take just the events including the 10 plays we're interested in
data_merged <- merge(list_plays, data_clean, by = c('p_PerfTitleClean'))

# we want to just get the year to let us summarise across time
# grab just the first number from each date label
data_merged$year <- as.numeric( sub("\\D*(\\d+).*", "\\1", data_merged$e_EventDate_parsed) )

# summarise by year
data_year <- data_merged %>%
  group_by(p_PerfTitleClean, count, year) %>%
  summarise(count_year = n())

# fill in empty years with zeroes to plot
data_year <- data_year %>%
  complete(year = seq.int(min(year), max(year)), fill = list(count_year = 0)) 

colnames(data_year) <- c('Play', 'CountTotal', 'Year', 'Count')

# define color palette
# original
color_pal <- c('#591f3d','#7b214d','#9e596b','#b27181','#d490a5',
               '#d88166','#dea867','#aca56e','#94adad','#436b5c')

# edits to og
color_pal <- c('#4f1533','#6d213b','#8c3b47','#b06173','#d490a5',
               '#d88166','#dea867','#aca56e','#94adad','#436b5c')

#3rd color: 8a303e; muted is 7e3b45
# 4th color: ab505f

color_pal <- rev(c('#184e61','#5f8c9f','#4f8871','#919187','#d9b85a',
               '#d06061','#e28b93','#9a91ba','#6575b0','#3f4a6c'))

color_pal <- rev(c('#c06b5f','#af3727','#a34561','#d699a8','#e0bec4','#b69081',
               '#c5ad5c','#759f6d','#2f5f48','#44717f'))

data_year$Play <- forcats::fct_reorder(data_year$Play, data_year$Year, max)

# plot preliminary
ggplot(data_year, aes(x = Year, y = Count, fill = Play)) + 
  geom_stream(bw = .45, size = .2, color = '#eaeaea') +
  scale_fill_manual(values = color_pal) +
  scale_x_continuous(breaks = c(1660, 1680, 1700, 1720, 1740, 1760, 1780, 1800)) +
  theme_minimal() +
  theme(legend.position = '',
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_line(size = .1, color = 'grey80'),
        panel.grid.major.x = element_line(size = .2, color = 'grey60'),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 30, color = 'grey60'))


ggsave('plot_overall.png', width = 24, height = 5, dpi = 300)

#### Plays by location ####
# summarise by year & theatre
data_year_theatre <- data_merged %>%
  group_by(p_PerfTitleClean, t_TheatreName, count, year) %>%
  summarise(count_year = n())

colnames(data_year_theatre) <- c('Play', 'Theatre', 'CountTotal', 'Year', 'CountEach')

data_year_theatre <- filter(data_year_theatre, Theatre != "")
# fill in empty years with zeroes to plot
# define min and max overall
min_year = min(data_year_theatre$Year)
max_year = max(data_year_theatre$Year)

# group by theatre and play and fill zeroes where that play never took place
data_theatre <- data_year_theatre %>%
  group_by(Theatre, Play) %>%
  complete(Year = seq.int(min_year, max_year), fill = list(CountEach = 0)) 

ggplot(data = data_theatre, aes(x = Year, y = CountEach, fill = Play)) +
  geom_stream(bw = .45, color = NA) +
  facet_wrap(~Theatre) +
  scale_fill_manual(values = color_pal) +
  theme_minimal() +
  theme(legend.position = 'bottom')

# based on the plot we can see that there are really 5 theatres we're interested in
# others are too few to effectively visualize
data_theatre_select <- data_theatre %>%
  filter(Theatre == "Covent Garden" |
           Theatre == "Drury Lane Theatre" |
           Theatre == "Lincoln's Inn Fields" |
           Theatre == "Goodman's Fields" |
           Theatre == "Haymarket Theatre")

ggplot(data = data_theatre_select, aes(x = Year, y = CountEach, fill = Play)) +
  geom_stream(bw = .45, color = NA) +
  facet_wrap(~Theatre) +
  scale_fill_manual(values = color_pal) +
  theme_minimal() +
  theme(legend.position = 'bottom')

# filter one theatre at a time - if I facet, the proportion is calculated out of 1
dotplot_data <- filter(data_theatre_select, 
                       CountEach != 0,
                       #Theatre == "Covent Garden"
                       #Theatre == "Drury Lane Theatre"
                       #Theatre == "Goodman's Fields"
                       Theatre == "Haymarket Theatre"
                       #Theatre == "Lincoln's Inn Fields"
                       )
dotplot_data <- dotplot_data %>%
  uncount(CountEach)

# add sequential ID by group (i.e. year)
dotplot_data <- dotplot_data %>% 
  group_by(Year) %>% 
  mutate(id = row_number())

ggplot(data = dotplot_data, aes(xmin = Year-1, xmax = Year, ymin = id-1, ymax = id, 
                                fill = Play, color = Play)) +
  geom_rect() +
  #geom_point(size = 4, shape = 15) +
  scale_x_continuous(breaks = c(1660, 1680, 1700, 1720, 1740, 1760, 1780, 1800),
                     limits = c(1660,1800)) +
  scale_fill_manual(values = color_pal) +
  scale_color_manual(values = color_pal) +
  theme_minimal() +
  theme(legend.position = '',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=18, family="InterV"),
        axis.text.x = element_text(size=18, family="InterV"),
        axis.ticks.x = element_line(color = 'black', size = .5),
        axis.line.x = element_line(color = 'grey80')) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 42)) 
                     
ggsave('haymarket_theatre_point.png', width = 7, height = 6, dpi = 300)



## OLDER: geom_dotplot/histogram ##
# abandoned this approach bc the scaling was off; replaced with simpler geom_point approach
ggplot(data = dotplot_data, aes(x = Year, fill = Play, color = Play)) +
  geom_dotplot(stackgroups = T, method = 'histodot', binwidth = 2,
               dotsize = .75, stackratio = 1.1, 
               stackdir = 'up', binpositions= 'all') +
  scale_x_continuous(breaks = c(1660, 1680, 1700, 1720, 1740, 1760, 1780, 1800),
                     limits = c(1660,1800)) +
  #ylim(c(0,1)) +
  scale_fill_manual(values = color_pal) +
  scale_color_manual(values = color_pal) +
  theme_minimal() +
  theme(legend.position = '',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank())

ggsave('dotplot_lincolns_inn_fields.png', width = 12, height = 10, dpi = 300)



# just in case we need to test things:
#testdata <- filter(data_theatre_select,
#                   Play == 'Hamlet' | Play == 'Love For Love' | Play == 'Romeo And Juliet',
#                   Theatre == 'Drury Lane Theatre')
#testdata2 <- filter(testdata, CountEach != 0)
#testdata2 <- testdata2 %>%
#  uncount(CountEach)

#ggplot(data = testdata2, aes(x = Year, fill = Play, color = Play)) +
#  geom_dotplot(stackgroups = T, binwidth = .7, method = 'histodot')

#### Scraping full-text of plays ####
library(rvest)
library(tidytext)

website_list <- c('http://shakespeare.mit.edu/hamlet/full.html',
                  'https://www.gutenberg.org/files/1244/1244-h/1244-h.htm',
                  'http://shakespeare.mit.edu/macbeth/full.html',
                  'http://shakespeare.mit.edu/romeo_juliet/full.html',
                  'https://www.gutenberg.org/files/14549/14549-h/14549-h.htm',
                  'https://archive.org/stream/sirroberthowards00howarich/sirroberthowards00howarich_djvu.txt',
                  'http://shakespeare.mit.edu/merry_wives/full.html',
                  'https://www.gutenberg.ca/ebooks/otway-orphan/otway-orphan-00-h-dir/otway-orphan-00-h.html',
                  'https://archive.org/stream/rehearsal00buckuoft/rehearsal00buckuoft_djvu.txt',
                  'http://shakespeare.mit.edu/tempest/full.html')

title_list <- list('Hamlet',
                'Love For Love',
                'Macbeth',
                'Romeo And Juliet',
                'Rule A Wife And Have A Wife',
                'The Committee',
                'The Merry Wives Of Windsor',
                'The Orphan',
                'The Rehearsal',
                'The Tempest')

# first, build a corpus of all sentiments
full_corpus_list <- list()

for (website in 1:length(website_list)){
  website_full <- read_html(website_list[website])
  text_clean <- website_full %>%
    html_text()
  
  text_df <- as.data.frame(text_clean)
  
  # get all tokens (= words)
  words <- text_df %>% 
    unnest_tokens("word", text_clean)
  
  # count positive and negative sentiments
  bing_word_counts <- words %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  bing_word_counts$play <- title_list[[website]]
  
  full_corpus_list[[website]] <- bing_word_counts
  
}
full_corpus <- do.call(rbind, full_corpus_list)

# now loop through websites again to extract values for each
balance_list <- list()
val_list <- list()
unique_list <- list()

for (website in 1:length(website_list)){
  website_full <- read_html(website_list[website])
  # this could be cleaned more. e.g. the written version contains titles/headings
  # however, those words are emotionally neutral (or by and large neutral)
  # so they won't contribute to our sentiment analysis
  # ALSO some of the websites' 'html_text' contains formatting - this gets removed in removing stop words
  text_clean <- website_full %>%
    html_text()
  
  text_df <- as.data.frame(text_clean)
  
  # get all tokens (= words)
  words <- text_df %>% 
    unnest_tokens("word", text_clean)
  
  ### Sentiment analysis ####
  
  # count positive and negative sentiments
  bing_word_counts <- words %>%
    # remove stop words
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  
  # get positive & negative words NOT in any other play
  bing_word_counts$play <- title_list[[website]]
  
  # get all BUT current play
  corpus_temp <- filter(full_corpus, play != title_list[[website]])
  
  # get all sentiments in this play that do not appear in any other ones
  bing_pos_neg <- bing_word_counts %>%
    anti_join(corpus_temp, by=c('word')) %>%
    group_by(sentiment) %>%
    slice_sample(n=5)

  unique_list[[website]] <- bing_pos_neg
  
  # calculate proportion of positive vs negative words
  word_balance <- bing_word_counts %>%
    mutate(total_n = sum(n)) %>%
    group_by(sentiment, total_n) %>%
    summarise(n_sentiment = sum(n)) %>%
    mutate(sentiment_balance = n_sentiment / total_n)
  
  # grab just negative balance
  neg_balance <- word_balance$sentiment_balance[1]
  
  balance_list[[website]] <- neg_balance
  
  afinn_sentiments <- words %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments('afinn'))
  
  average_val <- mean(afinn_sentiments$value)
  val_list[[website]] <- average_val
  
}
unique_df <- do.call(rbind, unique_list)

balance_df <- do.call(rbind, balance_list)
val_df <- do.call(rbind, val_list)
title_df <- do.call(rbind, title_list)
balance_df <- as.data.frame(cbind(title_df, balance_df, val_df))

colnames(balance_df) <- c('Play', 'sentiment_balance', 'sentiment_val')
balance_df$sentiment_balance <- as.numeric(balance_df$sentiment_balance)
balance_df$sentiment_val <- as.numeric(balance_df$sentiment_val)

# now bind the balance back with full dataset
data_year <- merge(data_year, balance_df, by = c('Play'))

# plot ordered by sentiment
ggplot(data_year, aes(x = Year, y = Count, group = reorder(Play,-sentiment_balance), fill = sentiment_balance)) + 
  geom_stream(bw = .40, color = 'black', size = .05) +
  scale_x_continuous(breaks = c(1660, 1680, 1700, 1720, 1740, 1760, 1780, 1800)) +
  scale_fill_gradientn(colors = color_pal,'') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        plot.background = element_rect(fill = 'grey94', color = 'grey94'),
        panel.background = element_rect(fill = 'grey94', color = 'grey94'))

# OR: CATEGORICAL APPROACH
data_categorical <- data_year
data_year$sentiment_balance <- as.factor(data_year$sentiment_balance)

data_year$Play <- factor(data_year$Play,
                         levels = c('Rule A Wife And Have A Wife',
                                    'The Committee',
                                    'The Rehearsal',
                                    'The Orphan',
                                    'Love For Love',
                                    'Romeo And Juliet',
                                    'Hamlet',
                                    'The Tempest',
                                    'The Merry Wives Of Windsor',
                                    'Macbeth'))

ggplot(data_year, aes(x = Year, y = Count, group = Play, fill = sentiment_balance)) + 
  geom_stream(bw = .40, color = 'black', size = .05) +
  scale_x_continuous(breaks = c(1660, 1680, 1700, 1720, 1740, 1760, 1780, 1800)) +
  scale_fill_manual(values = color_pal) +
  theme_minimal() +
  theme(legend.position = '',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = 'grey94', color = 'grey94'),
        panel.background = element_rect(fill = 'grey94', color = 'grey94'))
