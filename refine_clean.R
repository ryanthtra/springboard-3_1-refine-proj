library(dplyr)
library(readr)
refine_original <- read_csv("refine_original.csv")
View(refine_original)

# Step 1: Clean up brand names
refine_clean <- refine_original %>% 
  mutate(company = replace(company, grepl('zo$|z0$', company, ignore.case = TRUE), 'akzo')) %>% 
  mutate(company = replace(company, grepl('lips$', company, ignore.case = TRUE), 'philips')) %>% 
  mutate(company = replace(company, grepl('houten$', company, ignore.case = TRUE), 'van houten')) %>% 
  mutate(company = replace(company, grepl('ver$', company, ignore.case = TRUE), 'unilever'))

# Step 2: Separate product code and number
refine_clean <- refine_clean %>% 
  separate(`Product code / number`, c('product_code', 'product_number'), sep='-')

# Step 3: Add product categories
refine_clean <- refine_clean %>% 
  mutate(product_category = case_when(
    product_code=="p" ~ "Smartphone", product_code=="v" ~ "TV",
    product_code=="x" ~ "Laptop",
    product_code=="q" ~ "Tablet",
    TRUE ~ as.character(product_code)))

# Step 4: Add full address for geocoding
refine_clean <- refine_clean %>% 
  unite("full_address", address, city, country, sep = ", ")

# Step 5: Create dummy variables for company and product category
# 1. Add four binary columns for company
refine_clean <- refine_clean %>% 
  mutate(
    company_philips = if_else(company=="philips", 1, 0), 
    company_akzo=if_else(company=="akzo", 1, 0), 
    company_van_houten=if_else(company=="van houten", 1, 0), 
    company_unilever=if_else(company=="unilever", 1, 0))

# 2. Add four binary columns for product category
refine_clean <- refine_clean %>% 
  mutate(
    product_smartphone=if_else(product_category=="Smartphone", 1, 0),
    product_tv=if_else(product_category=="TV", 1, 0), 
    product_laptop=if_else(product_category=="Laptop", 1, 0), 
    product_tablet=if_else(product_category=="Tablet", 1, 0))

# Step 6: Submit the project on Github (and create refine_clean.csv)
write_csv(refine_clean, "./refine_clean.csv")
