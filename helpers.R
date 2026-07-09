
# returns a list of two formatted coefficients
# integrate into a dataframe as two separate rows and your good.

extract_coef = function(model, var) {
  if(!(var %in% rownames(summary(model)$coefficients))) {
    return(c("",""))
  }
  coef_table = summary(model)$coefficients
  
  estimate = coef_table[var, "Estimate"]
  standard_error = coef_table[var, "Std. Error"]
  p_value = coef_table[var, "Pr(>|t|)"]
  
  stars = ifelse(p_value < 0.01, "***", ifelse(
    p_value < 0.05 ,"**", ifelse(
      p_value < 0.1, "*", ""
    )))
  
  return(c(sprintf("%.3f%s", estimate, stars), sprintf("(%.3f)", standard_error)))
}

# generating the results table
results_table_4_5_generator = function(c1, c4) {
  return(data.frame(
    # First Columns with variables
    Variable = c(
      "Eskom Project", "",
      "Poverty rate", "",
      "Female-headed HH's", "",
      "Adult sex-ratio", "",
      "Baseline controls?",
      "District fixed effects?",
      "Change in other services?",
      "N of communities"
    ), 
    
    # Column 1 variables
    "Column 1" = c(
      extract_coef(c1, "treatment"),
      extract_coef(c1, "poverty"),
      extract_coef(c1, "female_hh"),
      extract_coef(c1, "sexratio"),
      "No",
      "No",
      "No",
      "1816"
    ),
    
    # Column 4 variables
    "Column 4" = c(
      extract_coef(c4, "treatment"),
      extract_coef(c4, "poverty"),
      extract_coef(c4, "female_hh"),
      extract_coef(c4, "sexratio"),
      "Yes",
      "Yes",
      "Yes",
      "1816"
    ),
    
    check.names = FALSE
  ))
}
