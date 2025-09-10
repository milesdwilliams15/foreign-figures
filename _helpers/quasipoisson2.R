
# -------------------------------------------------------------------------
# A log-2 version of a quasipoisson()
# -------------------------------------------------------------------------

# Why the heck would I do this? Because when you convert the predictions
# to predicted percent changes to the outcome and you use a log-2 predictor
# you can interpret the difference as the percent change in the outcome
# per a doubling in the level of the predictor.

quasipoisson2 <- function (link = "log") 
{
  linktemp <- substitute(link)
  if (!is.character(linktemp)) 
    linktemp <- deparse(linktemp)
  okLinks <- c("log", "identity", "sqrt")
  family <- "quasipoisson"
  if (linktemp %in% okLinks) 
    stats <- make.link(linktemp)
  else if (is.character(link)) {
    stats <- make.link(link)
    linktemp <- link
  }
  else {
    if (inherits(link, "link-glm")) {
      stats <- link
      if (!is.null(stats$name)) 
        linktemp <- stats$name
    }
    else {
      stop(gettextf("link \"%s\" not available for %s family; available links are %s", 
                    linktemp, family, paste(sQuote(okLinks), collapse = ", ")), 
           domain = NA)
    }
  }
  variance <- function(mu) mu
  validmu <- function(mu) all(is.finite(mu)) && all(mu > 0)
  dev.resids <- function(y, mu, wt) {
    r <- mu * wt
    p <- which(y > 0)
    r[p] <- (wt * (y * log2(y/mu) - (y - mu)))[p]
    2 * r
  }
  aic <- function(y, n, mu, wt, dev) NA
  initialize <- expression({
    if (any(y < 0)) stop("negative values not allowed for the 'quasiPoisson' family")
    n <- rep.int(1, nobs)
    mustart <- y + 0.1
  })
  structure(list(family = family, link = linktemp, linkfun = stats$linkfun, 
                 linkinv = stats$linkinv, variance = variance, dev.resids = dev.resids, 
                 aic = aic, mu.eta = stats$mu.eta, initialize = initialize, 
                 validmu = validmu, valideta = stats$valideta), class = "family")
}
