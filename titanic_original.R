library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

titanic_original <- read.csv("~/R projects/titanic3.csv")
View(titanic_original)


#see how many missing values exist. It looks like there are three.
summary(titanic_original$embarked)

#replace missing values with "s" 
titanic_original$embarked[titanic_original$embarked == ""] <- "S"


#Calculate the mean of the Age column and use that value to populate the missing values
hist(titanic_original$age)

titanic_original %>% 
  summarise(avg_age = mean(age, na.rm = TRUE))

titanic_original <- titanic_original %>% 
  replace_na(list(age = 30))

summary(titanic_original$age)



# find missing values in cabin,the  place 0 for empty spaces or 1 if cabin column has value
#titanic_original$cabin[titanic_original$cabin == ""] <- "NA"

titanic_original <- titanic_original %>%
  mutate(has_cabin_number = ifelse(cabin == "", 0, 1))


#Fill these empty slots with a dummy value e.g. the string 'None' or 'NA'
titanic_original$boat[titanic_original$boat == ""] <- "NA"





  



  
