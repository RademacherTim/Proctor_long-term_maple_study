#===============================================================================
# Script to read soluble sugar concentrations from the Proctor Maple Research 
# Center Long-term Study
#-------------------------------------------------------------------------------

# Load dependencies ----
if(!existsFunction("read_excel")) library("readxl")
if(!existsFunction("%>%")) library("tidyverse")

# Name of data file ----
file_name <- "../../Data/LTS Sugar.xlsx"

# Get sample dates from sheet names ----
dates <- excel_sheets(file_name) [-c(1:2)]

# Loop over sample dates to read data for each date ----
for (d in dates){
  
  # Read date-specific data ----
  tmp <- read_excel(
    path = file_name, 
    sheet = d, 
    range = "A9:H102",
    na = "NA",
    col_names = c("tn", "t", "tree", "ssc1", "ssc2", "ssc3", "ssc", "comments"),
    col_types = c("numeric", "text", "text", rep("numeric", 4), "text")
    ) %>% add_column (date = as_date(d))
  
  # Concatenate data ----
  if (d == dates[1]) {
    data <- tmp
  } else {
    data <- rbind (data, tmp)
  }
} # end date loop

#===============================================================================