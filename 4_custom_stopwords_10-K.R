# Create custom stop words 
# and remove from SEC 10-K forms' textual data

library(tidytext)
library(httr)
library(textclean)
library(lexicon)
data("stop_words") # Multilangual stopwords list
data("sw_fry_1000") # Fry's 1000 Most Commonly Used English Words

# Create a function to automatically remove digit,
# remove punctuations and transform all letters to lower
stopword_funct <- function(x){
  x <- gsub('[[:digit:]]+',' ', x)
  x <- gsub('[[:punct:]]+',' ', x)
  x <- tolower(x)
  return(x)
} 

cik_table_2 <- cik_table

# Security/ company name stopwords
cik_table_2$Security <- stopword_funct(cik_table_2$Security)
security <- cik_table_2 %>% select(Security) %>%
  unnest_tokens(word, Security) %>%
  select(word) %>%
  unique(.)
security_stopwords <- security$word

# GICS Sector/ industry stopwords
cik_table_2$GICS_Sector <- stopword_funct(cik_table_2$GICS_Sector)
industry <- cik_table_2 %>% select(GICS_Sector) %>%
  unnest_tokens(word, GICS_Sector) %>%
  select(word) %>%
  unique(.)
industry_stopwords <- industry$word

# GICS Sub Industry stopwords
cik_table_2$GICS_Sub_Industry <- stopword_funct(cik_table_2$GICS_Sub_Industry)
subindustry <- cik_table_2 %>% select(GICS_Sub_Industry) %>%
  unnest_tokens(word, GICS_Sub_Industry) %>%
  select(word) %>%
  unique(.)
subindustry_stopwords <- subindustry$word

custom_stopwords <- tibble(word = unique(c(security_stopwords, industry_stopwords, subindustry_stopwords)), lexicon = "custom")

# Create custom stop words list
custom_stopwords <- rbind(stop_words, custom_stopwords)