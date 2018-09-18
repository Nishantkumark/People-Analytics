# DATA UNDERSTANDING ----

# Libraries 
library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)

# Load Data
path_train            <- "00_Data/telco_train.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

definitions_raw_tbl

train_raw_tbl

glimpse(train_raw_tbl)

#Descriptive, Employment, Compensation, Survey, Performance, Worklife, Training and EDUCATION, Time bsaed features Year in compay

#Exploratory Data Analysis ----

#Data Summarization ---- 

skim(train_raw_tbl)

#Character Raw Data
train_raw_tbl %>% select_if(is.character)%>% glimpse()

#levels - map can be used to iterate over columns, with mutate it iterates over rows
train_raw_tbl %>% select_if(is.character)%>% glimpse() %>% map(unique)

#Take the count and proportions of each level by creating anonlynous fuctions.. here we create  a foctor level coiunt in a table and send that to prop table to get %

train_raw_tbl %>% select_if(is.character) %>% map(~table(.) %>% prop.table()) 

#Numeric data to find the number of unique values in each columns 

train_raw_tbl %>% select_if(is.numeric) %>% map(~unique(.) %>% length()) 

#instead of list we can convert to dataframe less tahn 10 are likely to be factor
train_raw_tbl %>% select_if(is.numeric) %>% map_df(~unique(.) %>% length()) %>% gather() %>%
  arrange(value) %>% filter(value <=10)

# Step 2 : Data Visualization with GGPairs in GGally ---- 

#Feature interactins Lower Triangle- Histogram -   Numeric and Categorical, Barplot - Categorical and Categorical, Scatterplot - Num Num

#Upper Triangle - boxplot for numeric and categorical, correlation for numeric num, bars for categorical cat

#Diagonal - pROPORTION AND DENSITY OF THAT FEATURE

#Investigating Descriptive Features

train_raw_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) 
  
#We use wrap to customize - lets say in diagnoal density we change thickness to 0.5 with alpha

train_raw_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
  ggpairs(aes(color = Attrition), lower = "blank", legend = 1,
          diag  = list(continuous = wrap("densityDiag", alpha = 0.5))) 
  
ggpairs(train_raw_tbl, columns = c("Attrition", "Age", "Gender", "MaritalStatus", 'NumCompaniesWorked', "Over18", "DistanceFromHome"), aes(color = Attrition), lower = "blank", legend = 1,
        diag  = list(continuous = wrap("densityDiag", alpha = 0.5)))

#Creating a plotggpairs custom function so that we may not need ti use the entire code again

plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
  col_expr <- enquo(color)
  if(rlang::quo_is_null(col_expr)){
    g <- data %>% ggpairs(lower = "blank")
  } else {
    color_name <- quo_name(col_expr)
    g <- data %>% ggpairs(mapping = aes_string(color = color_name), lower = "blank", legend = 1,
                          diag  = list(continuous = wrap("densityDiag", alpha = 0.5)))
  }
  return (g)
}

data <- train_raw_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) 
  

plot_ggpairs(data)
plot_ggpairs(data, color = Attrition, density_alpha = 0.8)




# Explore Features by Category ----

#   1. Descriptive features: age, gender, marital status 
train_raw_tbl %>%
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>%
  plot_ggpairs(Attrition)

#   2. Employment features: department, job role, job level
train_raw_tbl %>%
  select(Attrition, contains("employee"), contains("department"), contains("job")) %>%
  plot_ggpairs(Attrition) 

#   3. Compensation features: HourlyRate, MonthlyIncome, StockOptionLevel 
train_raw_tbl %>%
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock")) %>%
  plot_ggpairs(Attrition)

#   4. Survey Results: Satisfaction level, WorkLifeBalance 
train_raw_tbl %>%
  select(Attrition, contains("satisfaction"), contains("life")) %>%
  plot_ggpairs(Attrition)

#   5. Performance Data: Job Involvment, Performance Rating
train_raw_tbl %>%
  select(Attrition, contains("performance"), contains("involvement")) %>%
  plot_ggpairs(Attrition)

#   6. Work-Life Features 
train_raw_tbl %>%
  select(Attrition, contains("overtime"), contains("travel")) %>%
  plot_ggpairs(Attrition)

#   7. Training and Education 
train_raw_tbl %>%
  select(Attrition, contains("training"), contains("education")) %>%
  plot_ggpairs(Attrition)

#   8. Time-Based Features: Years at company, years in current role
train_raw_tbl %>%
  select(Attrition, contains("years")) %>%
  plot_ggpairs(Attrition)