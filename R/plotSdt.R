plotSDT <- function( dp=2, crit=NULL, xlim=c(-4, 4),
                     noiseLab="", signalLab="", noiseLabCex=1, signalLabCex=1,
                     xlab="evidence", ylab="",
                     main="", dpLab=NA, dpLabCex=1,
                     noiseCol="darkblue", signalCol="coral3",
                     critCol="black", critLty=1, critLwd=1,
                     critLab=NA, critLabCex=1,
                     noiseLty=1, signalLty=1, noiseLwd=1, signalLwd=1){
  #' Plot of an SDT analysis
  #'
  #' @param dp The d-prime value to show.
  #' @param crit The criterion value.
  #' @param xlim The limits of the x-axis.
  #' @param noiseLab The label to apply to the noise distribution, if any.
  #' @param signalLab The label to apply to the signal distribution, if any.
  #' @param xlab X-axis label.
  #' @param ylab Y-axis label.
  #' @param main The plot title.
  #' @param dpLab The label to apply to the d-prime representation.
  #' @param noiseCol The color of the noise distribution.
  #' @param signalCol The color of the signal distribution.
  #' @param critCol The color of the criterion line.
  #' @param critLty The type of line to draw for the criterion.
  #' @param critLwd Criterion line width.
  #' @param critLab The criterion label to apply.
  #' @param noiseLty The type of line to draw for the noise distribution.
  #' @param signalLty The type of line to draw for the signal distribution.
  #' @param noiseLwd The width of the noise distribution line.
  #' @param signalLwd the width of the signal distribution line.

  mean1 <- -.5*dp; mean2 <- .5*dp

  seq1 <- seq(xlim[1], xlim[2], .01)
  dens1 <- dnorm(seq1, mean=mean1, sd=1); dens2 <- dnorm(seq1, mean=mean2, sd=1)
  plot(seq1, dens1, type="l", col=noiseCol, ylim=c(0, .5),
       xlab=xlab, ylab=ylab, main=main, lty=noiseLty, lwd=noiseLwd)
  lines(seq1, dens2, col=signalCol, lty=signalLty, lwd=signalLwd)
  if(!is.null(crit)){
    lines(x=c(crit, crit), y=c(0, max(dens1)), col=critCol, lty=critLty, lwd=critLwd)
  }

  arrows(x0=mean1, x1=mean2, y0=.42, angle=90, length=.1, code=3)
  if(is.na(dpLab)){
    dpLab=paste("d' = ", round(dp, 2), sep="")
  }
  if(!is.null(dpLab)){
    text(x=.5*(mean1 + mean2), y=.44, labels=dpLab)
  }

  if(is.na(critLab)){
    dpLab=paste("c = ", round(crit, 2), sep="")
  }
  if(!is.null(critLab)){
    text(x=(crit+1), y=.2, labels=critLab)
  }

  text(x=(mean1-1.5), y=.8*max(dens1), labels=c(noiseLab))
  text(x=(mean2+1.5), y=.8*max(dens1), labels=c(signalLab))
}
