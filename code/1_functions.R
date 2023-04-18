# Ricker Functions
# Provided by M. Tyers from 2019 EG Workshop

Ricker <- function(x, lnalpha=input$lnalpha, beta=input$beta) x*exp(lnalpha - beta*x)

fitRicker <- function(S, R) {
  lmy <- log(R/S)
  lmfit <- lm(lmy~S)
  lnalpha_fit <- unname(lmfit$coefficients[1])
  lnalpha_p_fit <- lnalpha_fit + (sigma(lmfit)^2)/2
  beta_fit <- unname(-lmfit$coefficients[2])
  resids <- lmfit$residuals
  fits <- lmfit$fitted.values
  return(list(lnalpha_fit=lnalpha_fit, lnalpha_p_fit=lnalpha_p_fit, beta_fit=beta_fit, resids=resids, fits=fits))
}
