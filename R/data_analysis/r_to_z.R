r_to_z <- function(correlation, N,confidence){
  correlation <- upper.tri(correlation,diag = FALSE)
  SE <- 1 / sqrt(N - 3)

  Zvalue <- qnorm(p= 1 - (1-confidence)/2,
                  mean = 0,
                  sd = 1)

  Zupper <- atanh(ciCors) + Zvalue*SE # fishers z is the arc-tangent of the correlation
  Zlower <- atanh(ciCors) - Zvalue*SE

  rlower <- tanh(Zlower) # use the hyperbolic tangent function to return to r
  rupper <- tanh(Zupper)

  confidence_intervals <- c()

  for(i in 1:8){
    building <- cbind(rlower[,i],rupper[,i])
    confidence_intervals <- cbind(confidence_intervals,building)
  }

  colnames(confidence_intervals) <- paste0(rep(colnames(ciCors),each = 2), rep(c("Lower", "Upper"),8))
  confidence_intervals
}





load("correlations.RData")
correlations
correlations[upper.tri(correlations,diag = FALSE)]
expand.grid(colnames(correlations),colnames(correlations))
?expand.grid
