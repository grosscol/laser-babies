library("readxl")
library("readr")

# Data pre-processing to avoid storing xlsx in version control.

# Read specific sheet from Excel workbook
df <- read_xlsx('SCT Percutaneous Interventions Refined Data.xlsx', 'Database_Refined_2')

# lowercase and substitute spaces from column names
munge_col_name <- function(cname){
  # lower case
  result <- tolower(cname) 
  # replace spaces with underscore
  result <- gsub(' ', '_', result)
  # replace "_y/n" with ?
  result <- gsub('_y/n', '?', result)
  # de-duplicate any ??
  result <- gsub('\\?\\?', '?', result)
  # replace @ with 'at'
  result <- gsub('@','at', result)
  return(result)
}

colnames(df) <- sapply(colnames(df), FUN=munge_col_name)

# Write to csv
write_csv(df, 'SCT_percutaneous_interventions.csv')
