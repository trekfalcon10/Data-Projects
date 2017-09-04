library(dplyr)
library(readr)
library(tidyr)

refine_original <- read_csv("/Volumes/Backup_Chris/Dropbox/R_Projects/Refine/refine_original.csv")
View(refine_original)

#Order companies alphabetically by name
ordered <- refine_original %>% group_by(company) %>% arrange(company) 

#Put all company names into lowercase
ordered$company <- tolower(ordered$company)

#Take each company name as a subset of Company column and correct spelling
#Akzo
akzo <- subset(ordered$company,
               subset = substr(ordered$company, 1, 2) == "ak")
akzo

akzo[1: length(akzo)] <- "akzo"
akzo

#Philips
philips <- subset(ordered$company, subset = substr(ordered$company, nchar(ordered$company)-1, 
                                                   nchar(ordered$company)) == "ps")
philips

philips[1: length(philips)] <- "philips"
philips

#Unilever
unilever <- subset(ordered$company, 
                   subset = substr(ordered$company, 1, 2) == "un")
unilever

unilever[1: length(unilever)] <- "unilever"
unilever

#Van Houten--needs no corrections
vanhouten <- subset(ordered$company, 
                    subset = substr(ordered$company, 1, 3) == "van")
vanhouten

#Putting all corrected subsets of Company back together
ordered$company <- c(akzo, philips, unilever, vanhouten)
ordered$company

#Separating second column
ordered <- separate(ordered, names(ordered[, 2]), c("product_code", "product_number"), "-")
ordered

#Create new column for product codes as products
clarify_product <- function(x) 
{
  if(x == "p")
    x <- "Smartphone"
  else if(x == "q")
    x <- "Tablet"
  else if(x == "v")
    x <- "TV"
  else
    x <- "Laptop"
  
}

clarified <- ordered$`product_code`[1:25] %>% vapply(clarify_product, character(1))
names(clarified) <- NULL
clarified

#Add new column vector to dataframe
ordered$`product_category` <- clarified

#Reorder column vectors for clarity
ordered <- ordered[c(1,8,2:7)]

#Merge address columns into one full_address column
newcol <- unite(ordered, "full_address", 
                c("address", "city", "country"), sep = ", ")

#Extract full_address column
full_address <- newcol$full_address

#Add full_address column to ordered data frame
ordered$full_address <- full_address

#Reorder columns for clarity
ordered <- ordered[c(1:7, 9, 8)]

#Create binary columns for companies
#Create company_akzo column with binary variables
bool_akzo <- ordered$company == "akzo"
ordered$company_akzo <- as.numeric(bool_akzo)

#Create company_philips column with binary variables
bool_philips <- ordered$company == "philips"
ordered$company_philips <- as.numeric(bool_philips)

#Create company_unilever column with binary variables
bool_unilever <- ordered$company == "unilever"
ordered$company_unilever <- as.numeric(bool_unilever)

#Create company_vanhouten column with binary variables
bool_vanhouten <- ordered$company == "van houten"
ordered$company_van_houten <- as.numeric(bool_vanhouten)

#Create binary columns for products
#Create product_laptop column with binary variables
bool_laptop <- ordered$`product_category` == "Laptop"
ordered$product_laptop <- as.numeric(bool_laptop)

#Create product_smartphone column with binary variables
bool_smartphone <- ordered$`product_category` == "Smartphone"
ordered$product_smartphone <- as.numeric(bool_smartphone)

#Create product_tablet column with binary variables
bool_tablet <- ordered$`product_category` == "Tablet"
ordered$product_tablet <- as.numeric(bool_tablet)

#Create product_tv column with binary variables
bool_tv <- ordered$`product_category` == "TV"
ordered$product_tv <- as.numeric(bool_tv)

View(ordered)

#Write completed clean csv file
write.csv(ordered, "refine_clean.csv", row.names = FALSE)




