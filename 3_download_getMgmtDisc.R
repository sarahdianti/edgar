# This code demonstrates on how to download and scrape textual information
# from edgar getMgmtDisc (Section 7: Management Discussion)
# Use master_index_10K data frame from Rscript 2

library(readr)

for(a in 1:length(master_index_10K$CIK)){
  tryCatch({
    cik_id <- master_index_10K$CIK[a]
    filing_year <- as.integer(master_index_10K$Year[a])
    
    mda_text_main <- data.frame()
    
    # Download 10K Management Discussion and list files from file location
    edgar::getMgmtDisc(cik.no = cik_id, filing.year = filing_year)
    file_location <- paste0("MD&A section text/")
    list_file <- list.files(file_location, full.names = T)
    
    # Cleaning textual contents for each form
    # Removing digits, punctuations, white spaces and lower case
    for(b in 1:length(list_file)){
      current_file <- list_file[b]
      mda_full_text <- read_file(current_file) %>%
        tolower() %>%
        removePunctuation() %>%
        removeNumbers() %>%
        stripWhitespace()
      
      # Store cleaned textual contents to a data frame
      single_text <- data.frame(matrix(ncol = 3, nrow = 0))
      single_text <- as.data.frame(cbind(cik_id, mda_full_text, filing_year))
      colnames(single_text) <- c("CIK", "Text", "Filing_Year")
      single_text[] <- lapply(single_text, as.character)
      
      mda_text_main <- rbind(mda_text_main, single_text)
      
    }
    
    mda_text_main$CIK <- as.integer(mda_text_main$CIK)
    mda_text_main <- mda_text_main %>% inner_join(cik_table)
    
    # Store in db
    dbWriteTable(db_con_10K, "mda_text", mda_text_main, append = T)
    
  }
  , error = function(ec){
    error_file <- paste0(cik_id, "-", filing_year)
  }
  # Delete folder path and files inside
  , finally = {
    unlink(paste0(getwd(), "/MD&A section text"), recursive = T)
    unlink(paste0(getwd(),"/Edgar filings_full text"), recursive = T)
    print(a)
  }
  
  )
}