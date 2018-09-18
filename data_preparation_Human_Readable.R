#DATA PREPARATION ----
#Human Readable ----

#Load the libraries
library(readxl)
library(tidyverse)
library(forcats)
library(stringr)

#set the path
path_train <- "00_Data/telco_train.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

#Read the dataset
train_raw_tbl <- read_excel(path_train, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names= FALSE)

#Tidy the dataset ----
train_raw_tbl %>% glimpse()
definitions_raw_tbl
#We can see that definitions table has NA values in Education, we can fill that with the top row since that
#is the expected name. It is like Fill feature in Excel. Remove NA values with filter in X__2.

definitions_tbl <- definitions_raw_tbl %>% 
  fill(X__1, .direction = "down") %>%
  filter(!is.na(X__2)) %>%
  separate(X__2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
  rename(column_name = X__1) %>% mutate(key = as.numeric(key)) %>%
  mutate(value = value %>% str_replace(pattern = "'", replacement = ""))
definitions_tbl

definitions_list <- definitions_tbl %>%
                        split(.$column_name)%>%
                          map(~select(., -column_name))%>%
                          map(~mutate(.,value = as.factor(value)))

for (i in seq_along(definitions_list)){
  names_list <- names(definitions_list)[i]
  colnames(definitions_list[[i]]) <- c(names_list, paste0(names_list, "_value"))
}
     
  definitions_list   
     
     
     
data_merged_tbl <- list(HR_Data = train_raw_tbl) %>%
  append(definitions_list, after = 1)%>%
  reduce(left_join)%>%
  select(-one_of(names(definitions_list)))%>%
  set_names(str_replace_all(names(.), pattern = '_value', replacement = ""))%>%
  select(sort(names(.)))


data_merged_tbl %>% select_if(is.character)%>%
  glimpse()

data_merged_tbl%>%distinct(BusinessTravel)

data_merged_tbl%>%mutate_if(is.character, as.factor)%>%select_if(is.factor)%>%
  map(levels)

data_processed_tbl <- data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", "Travel_Rarely", "Travel_Frequently"),
    MaritalStatus  = MaritalStatus %>% fct_relevel("Single", "Married", "Divorced")
  )

data_processed_tbl %>%
  select_if(is.factor) %>%
  map(levels)


#Processing pipeline

definitions_tbl <- definitions_raw_tbl
data <- train_raw_tbl

process_hr_data_readable <- function(data, definitions_tbl){
  definitions_tbl %>% 
    fill(X__1, .direction = "down") %>%
    filter(!is.na(X__2)) %>%
    separate(X__2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
    rename(column_name = X__1) %>% mutate(key = as.numeric(key)) %>%
    mutate(value = value %>% str_replace(pattern = "'", replacement = ""))%>%
    split(.$column_name)%>%
    map(~select(., -column_name))%>%
    map(~mutate(.,value = as.factor(value)))
  
  for (i in seq_along(definitions_list)){
    names_list <- names(definitions_list)[i]
    colnames(definitions_list[[i]]) <- c(names_list, paste0(names_list, "_value"))
  }
  
  data_merged_tbl <- list(HR_Data = data) %>%
    append(definitions_list, after = 1)%>%
    reduce(left_join)%>%
    select(-one_of(names(definitions_list)))%>%
    set_names(str_replace_all(names(.), pattern = '_value', replacement = ""))%>%
    select(sort(names(.))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(
      BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", "Travel_Rarely", "Travel_Frequently"),
      MaritalStatus  = MaritalStatus %>% fct_relevel("Single", "Married", "Divorced")
    )
  
  return(data_merged_tbl)
  
}

process_hr_data_readable(train_raw_tbl, definitions_tbl = definitions_raw_tbl)%>%glimpse()
