# For the models with the explanatory variables included, use the following model mapping

explanatory_coef_map <- 
    c("(Intercept)" = "Intercept",
      "treated" = "Treatment x Post", 
      "log(total_pop)" = "Log(Total Population)", 
      "log(total_emp)" = "Log(Total Employment)", 
      "log(total_median_wage)" = "Log(Median Employee Wage)", 
      "log(total_average_hours)" = "Log(Average Hours)", 
      "log(ei_claims)" = "Log(EI Claims)", 
      "cpi" = "CPI Level", 
      "log(retail_sales)" = "Log(Retail Sales)", 
      "log(wholesale_sales)" = "Log(Wholesale Sales)", 
      "log(manufacturing_sales)" = "Log(Manufacturing Sales)", 
      "log(international_merchandise_imports)" = "Log(International Merchandise Imports)", 
      "new_housing_price_index" = "New Housing Price Index")