QSIC = function (y, E = 1, F = 1) {
  # QIC Function
  #
  # This function calculates the QIC (Quadric Information Criterion).
  # If the user does not provide values for E and F the penalty parameters, 
  # they are set to 1 by default.
  #
  # Parameters:
  # - y: An  model object.
  # - E: Penalty parameter for the criterion. Default value is 1.
  # - F: Additional penalty parameter. Default value is 1.
  #
  # Return:
  # - QIC value.
  #
  # Example usage:
  # model <- lm(Fertility ~ . , data = swiss)
  # result <- QIC(model)
  # result
  # For criterion T1 to T6  see: https://sistemas.furg.br/sistemas/sab/arquivos/bdtd/7d3047109fe6538e6b15788714782884.pdf
  # T1 = QSIC(model,1,1)
  # T2 = QSIC(model,1,0.5)
  # T3 = QSIC(model,1,-0.5)
  # T4 = QSIC(model,1,-1)
  # T5 = QSIC(model,0.5,1)
  # T6 = QSIC(model,0.5,-1)
  log_lik <- logLik(y)[1]  
  
  k <- length(coef(y)) + 1  
  
  n <- length(y$fitted.values)  
  
  
  
  QSIC_value <- -2 * log_lik + F * k * n * log(n)/(E * k + F * n)
  
  return(QSIC_value)
}
