library(quanteda)
library(tidyverse)
install.packages('quanteda.textstats')
library(quanteda.textstats)
install.packages('quanteda.textplots')
library(quanteda.textplots)
library(ggplot2)

# df <- read_delim("speech_2020_raw.txt", 
#                  delim="|")

df <- read_delim("about_pappas.txt", delim="|")

cat("\nmaking a corpus")
# my_corpus <- corpus(df,
#                     text_field = "text", docid_field = "url")

my_corpus <- corpus(df,
                    text_field = "text")

cat("\nmaking tokens")
my_tokens <- tokens(my_corpus, 
                    remove_punct = TRUE,
                    remove_symbols = TRUE,
                    remove_numbers = TRUE,
                    remove_url = TRUE) %>% 
  tokens_tolower() %>%
  #tokens_remove(pattern=c("joe","biden","donald","trump","president","kamala","harris")) %>%
  tokens_remove(pattern=stopwords("en")) %>%
  tokens_wordstem() %>%
  tokens_select(min_nchar=3) %>%
  tokens_ngrams(n=2)

cat("\nmaking a DFM")
# my_dfm <- dfm(my_tokens,
#               groups = "candidate", tolower = TRUE)
my_dfm <- dfm(my_tokens,
               tolower = TRUE)

## quick summaries
tstat_freq <- textstat_frequency(my_dfm, n = 10)

head(tstat_freq, 100)

#textplot_wordcloud(my_dfm, color = c("red", "blue"), comparison = T)
set.seed(132)
textplot_wordcloud(my_dfm, max_words = 10)

my_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


# result_keyness <- textstat_keyness(my_dfm, target = "Donald Trump")
# 
# textplot_keyness(result_keyness)


#### gov 2018 example ####
install.packages('tm')
library(tm)
install.packages('qdapDictionaries')
library(qdapDictionaries)
library(dplyr) # Data preparation and pipes $>$
library(ggplot2) # for plotting word frequencies
library(SnowballC) # for stemming
docs.df <- read_delim("about_pappas.txt", delim="|")
docs <- Corpus(VectorSource(docs.df$text))
docs
inspect(docs)

