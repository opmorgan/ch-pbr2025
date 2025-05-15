## -----------------------------------------------------------------------------
msd_to_d <- function(m1, sd1, m2, sd2) {
  d <- (m1 - m2) / (sqrt((sd1^2 + sd2^2) / 2))
  return(d)
}


## -----------------------------------------------------------------------------
## Function to convert F-value to t-value
f_to_t <- function(f) {
  t <- sqrt(f)
  return(t)
}



## -----------------------------------------------------------------------------
## "When an experiment that uses an F-test does not list the MSE, you can calculate
## Cohen’s d as follows using the F statistic. This calculation should only be used
## when the F-test compares one condition to one other condition."
f_to_d <- function(f, n1, n2, sign = "pos") {
  d <- sqrt(f * ((n1+n2)/(n1*n2)) * ((n1+n2)/(n1+n2-2)) )
  if (sign == "pos") {
    return(d)
  } else if (sign == "neg") {return(d*(-1))
  }
}


## -----------------------------------------------------------------------------
## Function to convert t value to Cohen's d, when group n's are available
t_to_d <- function(t, n1, n2) {
  d <- t*(sqrt(1/n1 + 1/n2))
  return(d)
}


## -----------------------------------------------------------------------------
## Function to convert t value to Cohen's d, when only total n is available
t_to_d_totaln <- function(t, n){
  d = (2*t)/(sqrt(n-2))
  return(d)
}

