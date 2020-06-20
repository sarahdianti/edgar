library(glue)
library(dbplyr)

# Create a connection to a DBMS for storing 10K tokens 
db_con_10K <- dbConnect(SQLite(), dbname="db_10K.db")
#dbDisconnect(db_con_10K)

for(i in 2009:2019){
  filter_query <- glue_sql("SELECT * FROM html_text WHERE Filing_Year = ({i})", .con = db_con_10K)
  
  table_10K <- dbGetQuery(db_con_10K, filter_query)
  
  # Cleaning the textual files (remove non-latin words, digits, puctuations)
  table_10K$Text <- iconv(table_10K$Text, "latin1", "ASCII", "")
  table_10K$Text <- gsub('[[:digit:]]+',' ', table_10K$Text)
  table_10K$Text <- gsub('[[:punct:]]+',' ', table_10K$Text)
  
  table_10K$CIK <- as.factor(table_10K$CIK)
  
  # Tokenize using parallelism, change to lower case and 
  #remove stop words from dictionary
  split_size <- 10000
  tokens_list <- split(table_10K,
                       rep(1:ceiling(nrow(table_10K)
                                     /split_size),
                           each=split_size,
                           length.out=nrow(table_10K)))
  
  tokens_all_10K <- data.frame()
  
  for(c in 1:length(tokens_list)){
    tokens_h <- tokens_list[[c]] %>%
      unnest_tokens(word, Text) 
    tokens_h$word <- tolower(tokens_h$word)
    tokens_h <- tokens_h %>%
      count(word, CIK) %>%
      anti_join(custom_stopwords) %>%
      filter(!(word %in% sw_fry_1000))
    tokens_all_10K <- bind_rows(tokens_all_10K, tokens_h)
    
    print(c)
  }
  
  tokens_all_10K$Year <- paste(i)
  tokens_all_10K$token_length <- nchar(tokens_all_10K$word)
  
  # Store all the information to a table in db
  dbWriteTable(db_con_10K, "html_text_tokens", tokens_all_10K, append = T)
  
}

# Filter tokens with various measure
# Load data frame from db
html_text_tokens <- dbGetQuery(db_con_10K, "SELECT * FROM html_text_tokens")

html_text_tokens %>% group_by(token_length) %>% summarise(total=n())

# Remove too short and too long tokens
html_text_tokens <- html_text_tokens %>%
  filter(between(token_length, 3, 15))

# Spelling check
# Remove words with spelling error
library(hunspell)
html_text_tokens$spelling <- hunspell_check(html_text_tokens$word)

html_text_tokens <- html_text_tokens %>%
  filter(spelling == TRUE)