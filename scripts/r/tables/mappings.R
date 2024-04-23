# Stars 

stars <- c('*'= 0.1, '**' = 0.05, '***'= 0.01)

# For the models with the explanatory variables included, use the following model mapping

explanatory_variables_map <- c(
  "(Intercept)" = "Intercept",
  treated = "Treatment x Post",
  ln_total_full_emp = "Ln Full-time employment",
  ln_total_median_wage = "Ln Median wage",
  cpi = "CPI",
  ln1_business_insolvencies = "Ln +1 Business insolvencies",
  ln_exports_all_countries = "Ln Intl. exports",
  ln_imports_all_countries = "Ln Intl. imports",
  ln_retail_sales = "Ln Retail sales",
  ln_wholesale_sales = "Ln Wholesale sales",
  ln_manufacturing_sales = "Ln Manufacturing sales",
  ln_travellers = "Ln International travellers",
  ln_vehicles = "Ln Arriving vehicles",
  ln_electric_power_generation = "Ln Electric power generation",
  ln_average_actual_hours = "Ln Average actual hours",
  new_housing_price_index = "New housing price index",
  ln_food_services_receipts = "Ln Food services receipts",
  ln_total_avg_tenure = "Ln Average job tenure"
)

# Goodness of fit mappings

goodness_of_fit_map <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "adj.r.squared", "clean" = "Adj. $R^2$", "fmt" = 3),
  list("raw" = "r2.within.adjusted", "clean" = "Adj. within $R^2$", "fmt" = 3),
  list("raw" = "rmse", "clean" = "RMSE", "fmt" = 3)
)