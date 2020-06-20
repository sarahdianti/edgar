library(tidytext)
library(sqldf)
library(RSQLite)
library(httr)
library(textclean)
library(tm)

# Create a connection to a DBMS for storing 10K files
db_con_10K <- dbConnect(SQLite(), dbname="db_10K.db")
#dbDisconnect(db_con_10K)

# Download 10-K forms using edgar getFilingsHTML 
# Only download forms for companies listed in cik_table
for(a in 1:length(master_index_10K$CIK)){
  tryCatch({
    cik_id <- master_index_10K$CIK[a]
    filing_year <- as.integer(master_index_10K$Year[a])
    
    full_text_main <- data.frame()
    
    # Download 10K forms and list files from file location
    edgar::getFilingsHTML(cik.no = cik_id, form.type = "10-K", filing.year = filing_year)
    file_location <- paste0("Edgar filings_HTML view/Form 10-K/",cik_id,"/")
    list_file <- list.files(file_location, full.names = T)
    
    # Cleaning textual contents for each form
    # Remove punctuations, digits and white spaces
    for(b in 1:length(list_file)){
      current_file <- list_file[b]
      html_full_text <- read_html(current_file, options = "HUGE") %>%
        html_text() %>%
        tolower() %>%
        removePunctuation() %>%
        removeNumbers() %>%
        stripWhitespace()
      
      # Store cleaned textual contents to a data frame
      single_text <- data.frame(matrix(ncol = 3, nrow = 0))
      single_text <- as.data.frame(cbind(cik_id, html_full_text, filing_year))
      colnames(single_text) <- c("CIK", "Text", "Filing_Year")
      single_text[] <- lapply(single_text, as.character)
      
      full_text_main <- rbind(full_text_main, single_text)
      
    }
    full_text_main$CIK <- as.integer(full_text_main$CIK)
    full_text_main <- full_text_main %>% inner_join(cik_table)
    
    # Store in db
    dbWriteTable(db_con_10K, "html_text", full_text_main, append = T)
    
  }
  , error = function(ec){
    error_file <- paste0(cik_id, "-", filing_year)
    
  }
  # Delete folder path and files inside
  , finally = {
    unlink(paste0(getwd(), "/Edgar filings_HTML view"), recursive = T)
    print(a)
    
  }
  )
}