#  PREDICTING EMPLOYEE ATTRITION WITH H2O AND LIME ----
# Scripts 2: DATA UNDERSTANDING AND PREPARATION ----
# plot_ggpairs ----

library(tidyverse)
library(tidyquant)
library(GGally)

# ggpairs:  
plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
    
    color_expr <- enquo(color)
    
    if (rlang::quo_is_null(color_expr)) {
        
        g <- data %>%
            ggpairs(lower = "blank") 
        
    } else {
        
        color_name <- quo_name(color_expr)
        
        g <- data %>%
            ggpairs(mapping = aes_string(color = color_name), 
                    lower = "blank", legend = 1,
                    diag = list(continuous = wrap("densityDiag", 
                                                  alpha = density_alpha))) +
            theme(legend.position = "bottom")
    }
    
    return(g)
    
}
