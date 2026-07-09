
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

