#BUSINESS UNDERSTANDING ----
#load the libraries

library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)
library(timetk)
library(tidyquant)
#install.packages("timetk")
#install.packages("tidyquant")

#Load the data
path_train <- "00_Data/telco_train.xlsx"
train_raw_tbl <- read_excel(path_train, sheet = 1)

#Data Subset
Dept_job_role_tbl <- train_raw_tbl %>% select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

Dept_job_role_tbl
# Business Science Problem Framework ----
#1A. View Business as a Machine ----

#Isolate Business Unit : Department,Jobrole
#Define Objectives : High Performers are retained
#Access Outcomes

#Access Attrition
Dept_job_role_tbl %>% group_by(Attrition) %>%
                     summarise(n = n())%>%
                       ungroup() %>% 
  mutate(perc = n/sum(n))

#2 Understand the Drivers ----

#Investigate the objectives- 16% Attrition
#Synthesize Outcomes- High counts, High %
#Hypothesize Drivers - Jobrole, Department

#Department----
Dept_job_role_tbl %>% group_by(Department, Attrition) %>% summarise(n = n()) %>% ungroup() %>% group_by(Department) %>% mutate(perc = n/sum(n))

#job role----
Dept_job_role_tbl %>% 
  group_by(Department, JobRole, Attrition) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(Department,JobRole) %>%
   mutate(perc = n/sum(n)) %>%
  ungroup() %>%
  filter(Attrition %in% "Yes")

#1C Measeure the Drivers ----

#Collect information on Employee attrition ----


glimpse(train_raw_tbl)
#Descriptive features- Describes the individual
#Employment Features - Dept Jobrole etc
#Compensation Features -
#Survey Feautures - Satisfaction, Worklife balance
#Performance features- perf rating, job involvrmrnt
#Work life features - Overtime, business travel
#Training and Education
#Years at company etc


#Develop KPI's : Industry KPIs

Dept_job_role_tbl %>% 
  group_by(Department, JobRole, Attrition) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(Department,JobRole) %>%
  mutate(perc = n/sum(n)) %>%
  ungroup() %>%
  filter(Attrition %in% "Yes") %>%
  arrange(desc(perc)) %>%
  mutate(above_industry_average = case_when(perc > .088 ~ "Yes", TRUE ~ "No") )


#1D Uncover Problem Opportunities ----

#Attrition cost as seen in excel
calculate_attrition_cost <- function(
  # Employee
  n                    = 1,
  salary               = 80000,
  
  # Direct Costs
  separation_cost      = 500,
  vacancy_cost         = 10000,
  acquisition_cost     = 4900,
  placement_cost       = 3500,
  
  # Productivity Costs
  net_revenue_per_employee = 250000,
  workdays_per_year        = 240,
  workdays_position_open   = 40,
  workdays_onboarding      = 60,
  onboarding_efficiency    = 0.50
){
  # Direct Costs
  direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  # Lost Productivity Costs
  productivity_cost <- net_revenue_per_employee / workdays_per_year * 
    (workdays_position_open + workdays_onboarding * onboarding_efficiency) 
  
  # Savings of Salary & Benefits (Cost Reduction)
  salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
  
  # Estimated Turnover Per Employee
  cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
  
  # Total Cost of Employee Turnover
  total_cost <- n * cost_per_employee
  
  return(total_cost)
  
  
}

calculate_attrition_cost(200)


#Attrition cost by Job role ----
Attritionbyjonrole <-
Dept_job_role_tbl %>% 
  group_by(Department, JobRole, Attrition) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(Department,JobRole) %>% #is done to calc % within groups
  mutate(perc = n/sum(n)) %>%
  ungroup() %>%
  filter(Attrition %in% "Yes") %>%
  arrange(desc(perc)) %>%
  mutate(above_industry_average = case_when(perc > .088 ~ "Yes", TRUE ~ "No") ) %>%
  mutate(cost_of_attrition = calculate_attrition_cost(n = n))

glimpse(Dept_job_role_tbl)

?geom_point


#WorkFlow for Attrition----

#Count Function

Dept_job_role_tbl %>% 
  count(JobRole, Attrition) %>%
  
  count_to_pct(JobRole) %>%
  
  filter(Attrition %in% "Yes") %>%
  arrange(desc(perc)) %>%
  mutate(above_industry_average = case_when(perc > .088 ~ "Yes", TRUE ~ "No") ) %>%
  
  mutate(cost_of_attrition = calculate_attrition_cost(n = n))

#create a new countto% using TidyEval
#... takes the input arguments(in group by)
count_to_pct <- function(data, ..., col = n) {
  grouping_var_expr <- quos(...)
  col_expr <- enquo(col)
  
 ret <-  data %>% 
    group_by(!!! grouping_var_expr) %>%
    mutate(perc = ( !! col_expr)/sum(!! col_expr)) %>%
    ungroup() 
 return(ret)
}

#create a new accessattritin function to  perc to yes and descending order and add flag above industry std
#attritioncol is the heading of the attrition column and value is yes or no 
#attrition col expr captures the attritioncol and stores it as a variable and !! evaluates it


access_attrition <- function(data, attrition_col, attrition_val, baseline_pct){
  attrition_col_expr <- enquo(attrition_col)
  data %>%
    filter((!!attrition_col_expr) %in% attrition_val) %>%
    arrange(desc(perc)) %>%
    mutate(above_industry_average = case_when(perc > baseline_pct ~ "Yes", TRUE ~ "No") ) 
    
}


#Optimising the code with new functions #Data manipulation of texts makes it neat before using ggplot

 Dept_job_role_tbl %>% 
  count(Department,JobRole, Attrition) %>%
  
  count_to_pct(Department,JobRole) %>%
  access_attrition(Attrition, attrition_val = "Yes", baseline_pct = 0.088) %>% 
   mutate(cost_of_attrition = calculate_attrition_cost(n = n)) %>%
    mutate(name = str_c(Department, JobRole, sep = ":") %>% as_factor()) %>%
     mutate(name = fct_reorder(name, cost_of_attrition)) %>%
      mutate(cost_text = str_c("$", format(cost_of_attrition/1e6, digits = 2),
             "M", sep ="")) %>%

   #Data manipulation of texts makes it neat before using ggplot
#Plotting

ggplot(aes(x= cost_of_attrition, y= name)) +
  geom_segment(aes(xend = 0, yend = name),color = palette()[[1]]) +
geom_point(aes(size = cost_of_attrition),color = palette()[[1]]) +
  scale_x_continuous(labels = scales::dollar) +
  geom_label(aes(label = cost_text, size = cost_of_attrition), hjust = "inward", color = palette()[[1]]) +
   scale_size(range = c(3,5)) + 
   labs(title = "Estimated Cost of Attrition : By Dept and Job role", x = "cost of attrition", y = "") 
 

 #plot attrition function
 
 plot_attrition <- function(data, ..., .value, 
                            fct_reorder = TRUE, 
                            fct_rev = FALSE, 
                            include_lbl = TRUE, 
                            color = palette()[[1]], 
                            units = c("0", "K", "M")) {
   
   
   # Inputs
   
   group_vars_expr <- quos(...)
   if (length(group_vars_expr) == 0) 
     group_vars_expr <- quos(rlang::sym(colnames(data)[[1]]))
   
   value_expr <- enquo(.value)
   value_name <- quo_name(value_expr)
   
   units_val <- switch(units[[1]],
                       "M" = 1e6,
                       "K" = 1e3,
                       "0"  = 1)
   if (units[[1]] == "0") units <- ""
   
   
   # Data Manipulation
   usd <- scales::dollar_format(prefix = "$", largest_with_cents = 1e3)
   
   data_manipulated <- data %>%
     mutate(name = str_c(!!! group_vars_expr, sep = ": ") %>% as_factor()) %>% 
     mutate(value_text = str_c(usd(!! value_expr / units_val), 
                               units[[1]], sep = ""))
   
   
   if (fct_reorder) {
     data_manipulated <- data_manipulated %>%
       mutate(name = forcats::fct_reorder(name, !! value_expr)) %>%
       arrange(name)
   }
   
   if (fct_rev) {
     data_manipulated <- data_manipulated %>%
       mutate(name = forcats::fct_rev(name)) %>%
       arrange(name)
   }
   
   # Visualization aes string is used coz  ggplot doesnt work with tidyeval framework
   
   g <- data_manipulated %>%
     ggplot(aes_string(x = value_name, y = "name")) +
     geom_segment(aes(xend = 0, yend = name), color = color) +
     geom_point(aes_string(size = value_name), color = color) +
     scale_x_continuous(labels = scales::dollar) +
     #theme_tq() +
     scale_size(range = c(3, 5)) #+
     #theme(legend.position = "none")
   
   
   if (include_lbl) {
     g <- g +
       geom_label(aes_string(label = "value_text", size = value_name),
                  hjust = "inward", color = color)
   }

   return(g)
   
 }
 
 
 
 
 
 #test the plot attrition function
 Dept_job_role_tbl %>% 
   count(Department,JobRole, Attrition) %>%
   
   count_to_pct(JobRole) %>%
   access_attrition(Attrition, attrition_val = "Yes", baseline_pct = 0.088) %>% 
   mutate(cost_of_attrition = calculate_attrition_cost(n = n)) %>% 
   plot_attrition( JobRole, .value = cost_of_attrition, units ="M")
 
 
 
 
   
   
 
  