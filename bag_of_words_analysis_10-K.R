# The processed textual data of 10-K forms can now be use for getting insights
# For example, find dominant words per aggregation category

## Find the important words per:
## 1. Industry level (GICS Sector)

# Take the category names with the membership of CIK
sector <- cik_table %>% 
  select(CIK, GICS_Sector) %>%
  unique(.) %>% 
  group_by(GICS_Sector) %>%
  summarise(total = n())

tokens_sector <- cik_table %>% 
  select(CIK, GICS_Sector) %>% 
  unique(.)

# Update the tokens from TF-IDF trim result
html_text_tokens_industry <- html_text_tokens[html_text_tokens$word %in% industry_level$word, ]

sector_type_tokens <- html_text_tokens_industry %>%  
  inner_join(tokens_sector) %>%
  group_by(GICS_Sector, word) %>% 
  summarise(total = sum(n)) %>% 
  arrange(desc(total)) 

# Loop through all industry to get top ten 10 words each
for(gics in 1:nrow(sector)){
  print(paste0("For GICS Sector: ", sector$GICS_Sector[gics]))
  
  toprint <- sector_type_tokens %>% ungroup() %>% 
    filter(GICS_Sector == sector$GICS_Sector[gics]) %>%
    top_n(10, total) %>% select(-total) %>% 
    mutate(rank = row_number())
  print(toprint)
}



## 2. Market level (Year)

# Take the category names with the membership of CIK
year_cat <- master_index_10K %>% 
  select(CIK, Year) %>%
  unique(.) %>% 
  group_by(Year) %>%
  summarise(total = n())

tokens_year <- master_index_10K %>% 
  select(CIK, Year) %>% 
  unique(.)

# Update the tokens from TF-IDF trim result
html_text_tokens_market <- html_text_tokens[html_text_tokens$word %in% year_level$word, ]

year_tokens <- html_text_tokens_market %>%  
  inner_join(tokens_year) %>%
  group_by(Year, word) %>% 
  summarise(total = sum(n)) %>% 
  arrange(desc(total)) 

# Loop through all years to get top ten 10 words each
for(yr in 1:nrow(year_cat)){
  print(paste0("For Market Year: ", year_cat$Year[yr]))
  
  toprint <- year_tokens %>% ungroup() %>% 
    filter(Year == year_cat$Year[yr]) %>%
    top_n(10, total) %>% select(-total) %>% 
    mutate(rank = row_number())
  print(toprint)
}
