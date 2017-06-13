sdtEst <- function(h, fa){
  # Takes a vector, the first element is hit rate, second is fa rate
  sdt <- data.frame(h=h, fa=fa)
  sdt <- within(sdt, {
    dp <- qnorm(h) - qnorm(fa)
    c <- -.5 * (qnorm(h) + qnorm(fa))
    beta <- exp(-.5 * (qnorm(h)^2 - qnorm(fa)^2))
    beta2 <- exp(c * dp)
  }
  )
  return(sdt)
}
