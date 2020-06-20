library(stringr)
library(tidyverse)
library(xml2)
library(rvest)
library(edgar)

# Download Master Indexes of year 2009-2019
edgar::getMasterIndex(filing.year = c(2009:2019))

# Load all information of S&P 500 from Master Index .Rda files into a data frame
master_index <- data.frame()
for (i in 2009:2019){
  file_name <- paste0("Master Indexes/", i, "master.Rda")
  load(file_name)
  index_h <- year.master %>%
    filter(cik %in% c(cik_table$CIK), form.type %in% c("10-K", "10-Q"))
  
  master_index <- rbind(master_index, index_h)
}

# Name the columns
colnames(master_index) <- c("CIK", "Company_Name","Form_Type", "Date_Filed", "Edgar_Link", "Quarter")

# Format column type
master_index$CIK <- as.character(master_index$CIK) %>% as.integer()
master_index$Company_Name <- as.character(master_index$Company_Name)
master_index$Form_Type <- as.character(master_index$Form_Type)
master_index$Date_Filed <- as.Date(master_index$Date_Filed)
master_index$Edgar_Link <- as.character(master_index$Edgar_Link)
master_index <- master_index %>%
  mutate(Year = format(as.Date(master_index$Date_Filed, "%Y, %m, %d"), "%Y"))
master_index$Year <- as.integer(master_index$Year)

# Create a master index data frame for 10K (annual) form type
master_index_10K <- master_index %>%
  filter(master_index$Form_Type == "10-K")

# Check if there any duplicated form grouped by CIK and Filing Year
master_index_10K %>% group_by(CIK, Year) %>% summarise(total = n()) %>% arrange(desc(total))