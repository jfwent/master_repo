###model validation and predictor correlation

glmm_validation <- function(my_model){
  
  library(vif)
  
  vif.values <- vif(my_model)
  
  barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
  
  abline(v = 5, lwd = 3, lty = 2)
  
  
}