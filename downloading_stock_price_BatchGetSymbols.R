# Downloading stock prices from BatchGetSymbols
library(chron)
library(BatchGetSymbols)
library(data.table)
library(lubridate)

# Create big data frame to ease the sentiment analysis process
master_index_b <- master_index_10K %>% 
  inner_join(cik_table, by = "CIK") %>%
  mutate(price_adj_ratio = NA)

for (d in 1:length(master_index_b$Symbol)){
  tryCatch({
    sp_tickers <- master_index_b$Symbol[d]
    first.date <- as.Date(master_index_b$Date_Filed[d]) - 7
    last.date <- as.Date(master_index_b$Date_Filed[d]) + 7
    cik_year <- master_index_b$CIK_Year[d]
    
    # Download financial data of S&P 500 
    # Extract prices 7 days before & after filing date
    read_stock_file <- BatchGetSymbols(tickers = sp_tickers,
                                       first.date = first.date,
                                       last.date = last.date,
                                       type.return = "log")
    
    
    # Calculate the price difference
    # Store the output in a column 
    stock_data_filter <- read_stock_file$df.tickers %>%
      filter(ref.date == max(ref.date) | row_number() == 2) %>%
      arrange(desc(ref.date))
    
    price_adj_ratio <- (stock_data_filter$price.adjusted[1] / stock_data_filter$price.adjusted[2]) - 1
    
    master_index_b$price_adj_ratio[d] <- print(price_adj_ratio)
    
  }
  , error = function(ec){
    error_file <- paste0(sp_tickers, "-", cik_year)
    
  }
  )
}

# Store final table into db
dbWriteTable(db_con_10K, "stock_prices", master_index_b, append = T)