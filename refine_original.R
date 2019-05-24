#0: Load the data in RStudio
#Save the data set as a CSV file called refine_original.csv and load it in RStudio into a data frame.

library(tidyr)
library(dplyr)
library(readxl)
library(plyr)

refine_original <- read.csv("~/R projects/refine.csv")

#1: Clean up the 'company' column.  Transform the values in the column to be: philips, akzo, van houten and unilever (all lowercase).

refine_original $company

refine_original$company <- tolower(refine_original$company)

refine_original <- refine_original %>%
  mutate(company =ifelse(grepl("^phil|^fil|^phl",company,ignore.case = TRUE),"phillips",company))%>%
  mutate(company =ifelse(grepl("^ak",company,ignore.case = TRUE),"akzo",company))%>%
  mutate(company =ifelse(grepl("^uni",company,ignore.case = TRUE),"unilever",company))

#2: Separate the product code and product number into separate columns i.e. add two new columns called product_code and product_number, containing the product code and number respectively.
refine_original <- refine_original %>%
  separate(`Product.code...number`, into=c("product_code","Product_number"), sep = "-")

names(refine_original)[names(refine_original) == "Product_number"] <- "product_number"


#3: Add product categories
refine_original <- refine_original %>%
  mutate("product_category" = ifelse(product_code == "p", "Smartphone", ""))%>%
  mutate("product_category" = ifelse(product_code == "v", "TV", product_category))%>%
  mutate("product_category" = ifelse(product_code == "x", "Laptop", product_category))%>%
  mutate("product_category" = ifelse(product_code == "q", "Tablet", product_category))

#4: Add full address for geocoding.  Create a new column full_address that concatenates the three address fields (address, city, country), separated by commas.
refine_original <- refine_original %>% unite(full_address, c(address, city, country), sep = ",")

#5.Create dummy variables for company and product category 
refine_original <- refine_original %>% mutate(company_philips = ifelse(company =="philips", 1, 0)) %>%
  mutate(company_akzo = ifelse(company == "akzo", 1, 0)) %>%
  mutate(company_van_houten = ifelse(company == "van_houten", 1, 0)) %>%
  mutate(company_unilever = ifelse(company == "unilever", 1, 0))


refine_original <- refine_original %>%
  mutate(product_smartphone = ifelse(product_category == "Smartphone", 1, 0)) %>%
  mutate(product_TV = ifelse(product_category == "TV", 1, 0)) %>%
  mutate(product_laptop = ifelse(product_category == "Laptop", 1, 0)) %>%
  mutate(product_tablet = ifelse(product_category == "Tablet", 1, 0))


write.csv(refine_original,file = 'refine_original.csv')
