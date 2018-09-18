# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)

# Load Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_num2factor(JobLevel, StockOptionLevel) %>%
    prep()

recipe_obj

train_tbl <- bake(recipe_obj, newdata = train_readable_tbl)
test_tbl  <- bake(recipe_obj, newdata = test_readable_tbl)

# 2. Models ----

h2o.init()

#automl_leader <- h2o.getModel("04_Modeling/h2o_models/StackedEnsemble_BestOfFamily_0_AutoML_20180704_223910")

#H2O Automation
h2o.init()
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = (0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o <- as.h2o(test_tbl)

y <- "Attrition"
x <- setdiff(names(train_h2o), y)


automl_models_h2o <- h2o.automl(x = x,
                                y = y,
                                training_frame = train_h2o,
                                validation_frame = valid_h2o,
                                leaderboard_frame = test_h2o ,
                                nfolds = 5,
                                max_runtime_secs = 30)

typeof(automl_models_h2o)
slotNames(automl_models_h2o)
automl_models_h2o@project_name
automl_models_h2o@leaderboard
automl_models_h2o@leader #gets information of top model

#h2o.getModel("GLM_grid_0_AutoML_20180704_223910_model_0")

#h2o.getModel("DeepLearning_0_AutoML_20180704_223910")


# 2 Saving & Loading ----

h2o.getModel("GLM_grid_0_AutoML_20180805_172647_model_0") %>%
  h2o.saveModel(path = "04_Modeling/h2o_models1/")

h2o.getModel("StackedEnsemble_AllModels_0_AutoML_20180805_172647") %>%
  h2o.saveModel(path = "04_Modeling/h2o_models1/")

h2o.getModel("StackedEnsemble_BestOfFamily_0_AutoML_20180805_172647") %>%
  h2o.saveModel(path = "04_Modeling/h2o_models1/")

h2o.getModel("DeepLearning_0_AutoML_20180805_172647") %>%
  h2o.saveModel(path = "04_Modeling/h2o_models1/")


automl_leader <- h2o.loadModel("04_Modeling/h2o_models1/StackedEnsemble_AllModels_0_AutoML_20180805_172647")
automl_leader



#3.1 Making Predictions ----

predictions_tbl <- automl_leader %>% h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
select(Attrition, EmployeeNumber)
)

test_tbl %>%
  slice(5) %>%
  glimpse()

#3.2 Single Explanation for LIME

explainer <- train_tbl %>%
  select(-Attrition) %>%
  lime(model = automl_leader,
       bin_continuous = TRUE,
       n_bins = 4,
       quantile_bins = TRUE)
explainer


explanation <- test_tbl %>%
  slice(5) %>%
  select(-Attrition) %>%
  lime::explain(explainer = explainer, n_labels = 1, n_features = 8, n_permutations = 5000,
                kernel_width = 1)
explanation %>%
  as.tibble() %>%
  select(feature : prediction)

plot_features(explanation = explanation, ncol = 1)

#3.3 Multiple Explanations ----


explanation <- test_tbl %>%
  slice(1:20) %>%
  select(-Attrition) %>%
  lime::explain(explainer = explainer, n_labels = 1, n_features = 8, n_permutations = 5000,
                kernel_width = 1)
explanation %>%
  as.tibble() %>%
  select(feature : prediction)

plot_features(explanation = explanation, ncol = 4)
plot_explanations(explanation = explanation)


#4 Custom Plot Plot_features

plot_features <- function(explanation, ncol = 2) {
  type_pal <- c('Supports', 'Contradicts')
  explanation$type <- factor(ifelse(sign(explanation$feature_weight) == 1, type_pal[1], type_pal[2]), levels = type_pal)
  description <- paste0(explanation$case, '_', explanation$label)
  desc_width <- max(nchar(description)) + 1
  description <- paste0(format(description, width = desc_width), explanation$feature_desc)
  explanation$description <- factor(description, levels = description[order(abs(explanation$feature_weight))])
  explanation$case <- factor(explanation$case, unique(explanation$case))
  explanation$`Explanation fit` <- format(explanation$model_r2, digits = 2)
  
  if (explanation$model_type[1] == 'classification') {
    explanation$probability <- format(explanation$label_prob, digits = 2)
    explanation$label <- factor(explanation$label, unique(explanation$label[order(explanation$label_prob, decreasing = TRUE)]))
    p <- ggplot(explanation) +
      facet_wrap(~ case + label + probability + `Explanation fit`, labeller = label_both_upper, scales = 'free', ncol = ncol)
  } else if (explanation$model_type[1] == 'regression') {
    p <- ggplot(explanation) +
      facet_wrap(~ case + prediction + `Explanation fit`, labeller = label_both_upper, scales = 'free', ncol = ncol)
  }
  p +
    geom_col(aes_(~description, ~feature_weight, fill = ~type)) +
    coord_flip() +
    scale_fill_manual(values = c('forestgreen', 'firebrick'), drop = FALSE) +
    scale_x_discrete(labels = function(lab) substr(lab, desc_width+1, nchar(lab))) +
    labs(y = 'Weight', x = 'Feature', fill = '') +
    theme_lime()
}


#Recreating plot features
explanation %>% as.tibble()

case_1 <- explanation %>%
  filter(case == 1)

case_1 %>% plot_features()

#Transformation
data_transformed <- case_1 %>% as.tibble() %>%
  mutate(feature_desc = as_factor(feature_desc) %>%
           fct_reorder((feature_weight), .desc = FALSE),
         key = ifelse(feature_weight > 0, "Supports", "Contradicts")%>% 
         fct_relevel("Supports"),
          case_text = glue("Case:{case}"),
          label_text = glue("Label:{label}"),
         prob_text    = glue("Probability: {round(label_prob, 2)}"),
         r2_text      = glue("Explanation Fit: {model_r2 %>% round(2)}"))%>%
  select(feature_desc, feature_weight, key, case_text:r2_text)

data_transformed %>% ggplot(aes(feature_desc,feature_weight, fill = key)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_tq() +
  scale_fill_tq() +
  labs(y = "Weight", x = "Feature") +
  facet_wrap(~ case_text + label_text+ prob_text + r2_text ,
             ncol = 1, scales = "free")


plot_features_tq <- function(explanation, ncol){
  data_transformed <- explanation %>% as.tibble() %>%
    mutate(feature_desc = as_factor(feature_desc) %>%
             fct_reorder(abs(feature_weight), .desc = FALSE),
           key = ifelse(feature_weight > 0, "Supports", "Contradicts")%>% 
             fct_relevel("Supports"),
           case_text = glue("Case:{case}"),
           label_text = glue("Label:{label}"),
           prob_text    = glue("Probability: {round(label_prob, 2)}"),
           r2_text      = glue("Explanation Fit: {model_r2 %>% round(2)}"))%>%
    select(feature_desc, feature_weight, key, case_text:r2_text)
  
  data_transformed %>% ggplot(aes(feature_desc,feature_weight, fill = key)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_tq() +
    scale_fill_tq() +
    labs(y = "Weight", x = "Feature") +
    facet_wrap(~ case_text + label_text+ prob_text + r2_text ,
               ncol = ncol, scales = "free")
  
  
  
}

explanation %>%
  filter(case %in% 1) %>%
  plot_features_tq(ncol = 2)

explanation %>%
  filter(case %in% 1:6) %>%
  plot_features(ncol = 2)



# 4.2 Recreating Plot Explanations ----

