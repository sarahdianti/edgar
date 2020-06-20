# Weighting TF-IDF
# Company level - removing common words

company_level <- html_text_tokens %>%
  group_by(CIK, word) %>%
  summarise(n_cik = sum(n)) %>%
  bind_tf_idf(word, CIK, n_cik)

# Filter TF-IDF = 0 
# Store into common words list
common_words <- company_level %>% filter(tf_idf == 0)
common_words <- common_words %>% ungroup() %>% select(-CIK) %>% unique()
common <- common_words$word
common_words <- tibble(word = common, lexicon = "custom")

# Remove common words from tokens
html_text_tokens <- html_text_tokens %>% anti_join(common_words)

dbWriteTable(db_con_10K, "html_text_tokens_clean", html_text_tokens, append = T)

# Update custom stop words
custom_stopwords_updated <- rbind(custom_stopwords, common_words)


## 1. Industry Level (GICS Sector)

html_text_tokens$CIK <- as.integer(html_text_tokens$CIK)
industry_level <- html_text_tokens %>% inner_join(cik_table, by = "CIK")

# Calculate TF-IDF for industry aggregation category
industry_level <- industry_level %>%
  group_by(GICS_Sector, word) %>%
  summarise(n_gics = sum(n)) %>%
  bind_tf_idf(word, GICS_Sector, n_gics)

# Plot TF-IDF distribution in histogram
hist(industry_level$tf_idf, breaks = 100, main = "TF-IDF Plot - Industry Level (1)")

# Right and left trim by 2.5% quantile
industry_level <- industry_level %>% filter(tf_idf < quantile(industry_level$tf_idf, 0.975), tf_idf > 0)

# Re-plot trimmed TF-IDF distribution 
hist(industry_level$tf_idf, breaks = 100, main = "TF-IDF Plot - Industry Level (2)")


## 2. Market Level (Year)

# Calculate TF-IDF for market aggregation category
year_level <- html_text_tokens %>%
  group_by(Year, word) %>%
  summarise(n_year = sum(n)) %>%
  bind_tf_idf(word, Year, n_year)

# Plot TF-IDF distribution in histogram
hist(year_level$tf_idf, breaks = 100, main = "TF-IDF Plot - Year Level (1)")

# Right and left trim by 2.5% quantile
year_level <- year_level %>% filter(tf_idf < quantile(year_level$tf_idf, 0.975), tf_idf > 0)

# Re-plot trimmed TF-IDF distribution
hist(year_level$tf_idf, breaks = 100, main = "TF-IDF Plot - Year Level (2)")