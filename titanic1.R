library(readr)
library(dplyr)
library(tidyr)

#Load and view titanic_original.csv
titanic_original <- read.csv("/Volumes/Backup_Chris/Dropbox/R_Projects/Titanic/titanic_original.csv", stringsAsFactors = FALSE)
View(titanic_original)

#Find blanks in embarked and change to "S"
#Separate out blanks from non-blanks
blank <- subset(titanic_original$embarked, titanic_original$embarked[1:length(titanic_original$embarked)] == "")
not_blank <- subset(titanic_original$embarked, titanic_original$embarked != blank)

#Change blanks to "S"
blank[1:length(blank)] <- "S"

#Arrange by embark to line up with new sub-vectors
embark_sort <- arrange(titanic_original, titanic_original$embarked)

#Combine sub-vectors into new embarked column
embark_sort$embarked <- c(blank, not_blank)

#Restore original order
embark_sort <- arrange(embark_sort, embark_sort$name)
embark_sort <- arrange(embark_sort, embark_sort$pclass)

#Age
#Mean
age_mean <- mean(titanic_original$age, na.rm = TRUE)

#Get subset of Age column as NA subvector
#Put age column in proper order to fix
age_sort <- arrange(embark_sort, embark_sort$age)
View(age_sort)

na_age_vector <- subset(age_sort$age, is.na(age_sort$age) == TRUE)

#Get substantive part (not NA) of Age column
age_not_na <- subset(age_sort$age, is.na(age_sort$age) == FALSE)

#Change NA vector to age_mean 
na_age_vector[1:length(na_age_vector)] <- age_mean

#Put age vector back together
age_sort$age <- c(age_not_na, na_age_vector)
View(age_sort)

#Restore original order
age_sort <- arrange(age_sort, age_sort$name)
age_sort <- arrange(age_sort, age_sort$pclass)
View(age_sort)

#Lifeboat
#Arrange by boat column
lifeboat_sort <- arrange(age_sort, age_sort$boat)
View(lifeboat_sort)

#Get subset of boat column consisting of blanks
lifeboat_blanks <- subset(lifeboat_sort$boat, lifeboat_sort$boat == "")

#Get substantive subset of lifeboat column
lifeboat_nums <- subset(lifeboat_sort$boat, lifeboat_sort$boat != "")

#Change blanks to "None"
lifeboat_blanks[1:length(lifeboat_blanks)] <- "None"

#Recombine to form new boats vector
lifeboat_sort$boat <- c(lifeboat_blanks, lifeboat_nums)

#Restore original order
lifeboat_sort <- arrange(lifeboat_sort, lifeboat_sort$name)
lifeboat_sort <- arrange(lifeboat_sort, lifeboat_sort$pclass)
View(lifeboat_sort)

#Cabin
#Arrange by cabin column
cabin_sort <- arrange(lifeboat_sort, lifeboat_sort$cabin)


#Create new boolean column has_cabin_number 
cabin_sort$has_cabin_number <- as.numeric(cabin_sort$cabin[1:length(cabin_sort$cabin)] != "") 

View(cabin_sort)

#Put new column next to cabin column for clarity
cabin_sort <- cabin_sort[c(1:10, 15, 11:14)]

#Restore to original order and create final cleaned dataframe
titanic_clean <- cabin_sort
titanic_clean <- arrange(titanic_clean, titanic_clean$name)
titanic_clean <- arrange(titanic_clean, titanic_clean$pclass)

View(titanic_clean)

#Create cleaned csv file
write.csv(titanic_clean, "titanic_clean.csv", row.names = FALSE)

