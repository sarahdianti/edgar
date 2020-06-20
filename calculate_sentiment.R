## Calculate sentiment: Bing Liu, Loughran, NRC, Wordnet Affect

library(bit64)

# Code for master_index_b can be seen from 
# downloading_stock_price_BatchGetSymbol.R

# Create an identifier code to ease the analysis (CIK_Year)
master_index_b$CIK_Year <- paste(master_index_b$CIK, master_index_b$Year, sep = "")
master_index_b$CIK_Year <- as.integer64(master_index_b$CIK_Year)

html_text_tokens$CIK_Year <- paste(html_text_tokens$CIK, html_text_tokens$Year, sep = "")
html_text_tokens$CIK_Year <- as.integer64(html_text_tokens$CIK_Year)

master_index_b <- na.omit(master_index_b)

### Bing Liu Dictionary
# Calculate sentiment using Bing Liu Dictionary
bing_liu_sentiment <- html_text_tokens %>% 
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, CIK_Year) %>%
  spread(sentiment, n) %>%
  mutate(bing_liu_sentiment = log(positive/negative)) %>%
  select(CIK_Year, bing_liu_sentiment)


### Loughran Dictionary
# Calculate sentiment using Loughran Dictionary
loughran_sentiment <- html_text_tokens %>%
  inner_join(get_sentiments("loughran")) %>%
  count(sentiment, CIK_Year) %>%
  spread(sentiment, n) %>%
  mutate(loughran_sentiment = log(positive/negative)) %>%
  select(CIK_Year, loughran_sentiment)


### NRC Dictionary
# Calculate sentiment using NRC Dictionary
nrc_sentiment <- html_text_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment, CIK_Year) %>%
  spread(sentiment, n) %>%
  mutate(nrc_sentiment = log(positive/negative)) %>%
  select(CIK_Year, nrc_sentiment)


### Wordnet Affect
library(corpus)
affect_wordnet <- affect_wordnet
affect_wordnet$word <- affect_wordnet$term

# Calculate sentiment using Wordnet Affect Dictionary
affect_wordnet_sentiment <- html_text_tokens %>%
  inner_join(affect_wordnet) %>%
  group_by(CIK_Year, emotion) %>%
  summarise(n = sum(n)) %>%
  spread(emotion, n) %>%
  mutate(wordnet_sentiment = log(Positive/Negative)) %>%
  select(CIK_Year, wordnet_sentiment)


# Bind all sentiment calculation to the big data frame
master_index_b <- master_index_b %>%
  left_join(bing_liu_sentiment) %>%
  left_join(loughran_sentiment) %>%
  left_join(nrc_sentiment) %>%
  left_join(affect_wordnet_sentiment)