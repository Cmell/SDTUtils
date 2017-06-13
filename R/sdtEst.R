dpEst <- function (h, fa) {
  #' Calculate d' from hit and false alarm rates.
  #'
  #' @param h The hit rate (as a proportion).
  #' @param fa The false alarm rate (as a proportion).
  #' @return The d' parameter from SDT.
  #' @examples
  #' dpEst(.75, .31)
  #' 
  #' @export dpEst

  return(qnorm(h) - qnorm(fa))
}

cEst <- function (h, fa) {
  #' Calculate criterion from hit and false alarm rates.
  #'
  #' @param h The hit rate (as a proportion).
  #' @param fa The false alarm rate (as a proportion).
  #' @return The c parameter from SDT.
  #' @examples
  #' cEst(.71, .32)
  #' 
  #' @export cEst

  return(-.5 * (qnorm(h) + qnorm(fa)))
}

betaEst <- function (h, fa) {
  #' Calculate sdt-beta from hit and false alarm rates.
  #'
  #' @param h The hit rate (as a proportion).
  #' @param fa The false alarm rate (as a proportion).
  #' @return The beta parameter from SDT.
  #' @examples
  #' betaEst(.71, .32)
  #' 
  #' @export betaEst
  
  return(exp(-.5 * (qnorm(h)^2 - qnorm(fa)^2)))
}

beta2Est <- function (h=NULL, fa=NULL, dp=NULL, c=NULL) {
  #' Calculate sdt-beta2 from hit and false alarm rates.
  #'
  #' @param h The hit rate (as a proportion).
  #' @param fa The false alarm rate (as a proportion).
  #' @param dp d' from signal detection. Optional (see details).
  #' @param c Criterion from signal detection. Optional (see details).
  #' @return The c parameter from SDT.
  #' 
  #' @details 
  #' Either the hit and false alarm rates can be specified, or d' and 
  #' criterion. If either d' or criterion is specified, then both must be
  #' provided. If either hit rate or false alarm rate is specified, then both
  #' must be provided. Note that if hit rate and false alarm rate
  #' are provided, they are first used to calculate d' and c. Thus, it is
  #' faster to provide d' and c if they are available.
  #' 
  #' @examples
  #' beta2Est(.71, .32)
  #' 
  #' @export beta2Est
  
  if (!is.null(h) | !is.null(fa)) {
    if (is.null(h) | is.null(fa)) {
      stop('If either h or fa is provided, both must be provided.')
    }
    c <- cEst(h, fa)
    dp <- dpEst(h, fa)
  } else if (!is.null(dp) | !is.null(c)) {
    if (is.null(dp) | is.null(c)) {
      stop('If either dp or c is provided, both must be provided.')
    }
  }
  
  return(exp(c * dp))
}

sdtEst <- function (h, fa) {
  
  #' Calculate four SDT parameters at once: d', criterion, beta, and beta^2
  #'
  #' @param h The hit rate as a proportion.
  #' @param fa The false alarm rate as a proportion.
  #' @return A dataframe with the hit rate, false alarm rate, and SDT values.
  #' @examples
  #' sdtEst(.72, .44)
  #' 
  #' @export sdtEst

  sdt <- data.frame(h=h, fa=fa)
  sdt <- within(sdt, {
    dp <- dpEst(h, fa)
    c <- cEst(h, fa)
    beta <- betaEst(h, fa)
    beta2 <- beta2Est(dp=dp, c=c)
  }
  )
  return(sdt)
}
