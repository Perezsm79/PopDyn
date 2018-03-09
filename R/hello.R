

x <- 5


#' Title
#'
#' @param H0 Inital hare population size,
#' @param P0 Initial lynx population size
#' @param z  No known value.
#'
#' @return data.fame of hare and lynx sizes over time.
#' @export
#'
#' @examples
#' sim()
#'
hello <- function(x,y,z) {
  print("Hello, world!")
}

PredGrowth <- function(a,b,P) {
  a-b*P
}
PreyGrowth <- function(c,H,d) {
  c*H-d
}

sim <- function(a = 0.01, b = 0.01, c = 0.01, d = 0.01, H0, P0, t) {
  #intialize
  H <- numeric(t)
  P <- numeric(t)

  H[1] <- H0
  P[1] <- P0

  # loop
  for(i in 1:(t-1)){
    H[i+1] = H[i] + H[i]*PredGrowth(a=a,b=b,P=P[i])
    P[i+1] = P[i] + H[i]*PreyGrowth(c=c, H=H[i], d=d)
  }

  #output
  tibble(year=1:t, Predator = P, Prey = H)
}
