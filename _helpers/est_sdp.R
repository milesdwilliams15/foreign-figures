# -------------------------------------------------------------------------
# est_sdp()
# -------------------------------------------------------------------------


est_sdp <- function(gdp, pop, price) {
  
  # minus subsistence demand from gdp
  x <- gdp - 365 * pop * price
  
  # if non-positive, set to zero
  sdp <- ifelse(x > 0, x, 0)
  
  # return sdp
  sdp
}