# All information related to the companies listed in S&P 500
# are listed from the pdf file.
# However, I don't have the permission to share the .pdf file
# Therefore, the cik_table are available in .csv 

library(dplyr)
library(tabulizer)

# Extracting tables from PDF file
pdf <- extract_tables("sp_500_table.pdf")

# Combine element (a matrix representation of a page of the PDF table) 
# into a single data matrix containing all the data
cik_table <- do.call(rbind, pdf)

# Delete table headers that got extracted as rows
cik_table <- as.data.frame(cik_table[2:nrow(cik_table), ])

# Name the columns
colnames(cik_table) <- c("Symbol", "Security", "GICS_Sector", "GICS_Sub_Industry", "CIK")

# Format column types
cik_table <- cik_table %>% 
  mutate(Symbol = as.character(levels(Symbol)[Symbol]))
cik_table <- cik_table %>% 
  mutate(Security = as.character(levels(Security)[Security]))
cik_table <- cik_table %>% 
  mutate(GICS_Sector = as.character(levels(GICS_Sector)[GICS_Sector]))
cik_table <- cik_table %>% 
  mutate(GICS_Sub_Industry = as.character(levels(GICS_Sub_Industry)[GICS_Sub_Industry]))
cik_table <- cik_table %>%
  mutate(CIK = as.integer(levels(CIK)[CIK]))

# Check and remove the duplicated CIK as it belong to the same company 
# (cross-checked from Master Indexes)
cik_table %>% group_by(CIK) %>% summarise(total=n()) %>% arrange(desc(total))

# Delete duplicates by CIK code
cik_table <- cik_table[!duplicated(cik_table$CIK), ]

# Save data frame in .csv file
write.csv(cik_table,file="cik_table.csv",row.names=F,col.names=T)