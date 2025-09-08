# Fit Beta(alpha, beta) to given quantiles (defaults: 10th & 90th)
# q: vector of target quantile values in (0,1), e.g. c(0.2, 0.6)
# p: corresponding probabilities in (0,1), same length as q, e.g. c(0.1, 0.9)
# returns: list(alpha, beta, mean, var, kappa, fitted_quantiles, input_quantiles, probs)
fit_beta_from_quantiles <- function(q, p = c(0.1, 0.9), verbose = FALSE) {
  stopifnot(length(q) == length(p), length(q) >= 2)
  if (any(!is.finite(q)) || any(!is.finite(p))) stop("Non-finite q or p.")
  if (any(q <= 0 | q >= 1)) stop("All q must be strictly between 0 and 1.")
  if (any(p <= 0 | p >= 1)) stop("All p must be strictly between 0 and 1.")
  if (is.unsorted(p, strictly = TRUE)) stop("p must be strictly increasing.")
  if (is.unsorted(q, strictly = TRUE)) stop("q must be strictly increasing.")
  
  # ---------- Starting guess via normal approximation ----------
  # Use the middle quantile as a crude location, and the p-interval width for scale
  m0 <- mean(q)  # mid-point of provided quantiles
  z <- qnorm(p)
  z_width <- max(z) - min(z)
  width <- max(q) - min(q)
  sigma0 <- width / z_width              # normal-ish sd from quantile width
  var0 <- sigma0^2
  
  # Beta variance: var = m(1-m) / (kappa + 1)  => kappa ≈ m(1-m)/var - 1
  kappa0 <- m0 * (1 - m0) / max(var0, 1e-6) - 1
  if (!is.finite(kappa0) || kappa0 < 2) kappa0 <- 2  # keep it tame
  
  a0 <- m0 * kappa0
  b0 <- (1 - m0) * kappa0
  if (!is.finite(a0) || a0 <= 0 || !is.finite(b0) || b0 <= 0) {
    a0 <- 2; b0 <- 2  # uniform-ish fallback
  }
  
  # ---------- Objective: match target quantiles ----------
  loss <- function(theta) {
    # log-parameterisation to enforce positivity
    a <- exp(theta[1]); b <- exp(theta[2])
    qb <- qbeta(p, a, b)
    sum((qb - q)^2)
  }
  
  opt <- optim(par = log(c(a0, b0)),
               fn  = loss,
               method = "Nelder-Mead",
               control = list(reltol = 1e-12, maxit = 2000))
  
  a <- exp(opt$par[1]); b <- exp(opt$par[2])
  
  if (verbose) {
    message(sprintf("Converged: %s | loss=%.4g | iters=%d",
                    opt$convergence == 0, opt$value, opt$counts[[1]]))
  }
  
  list(
    alpha = a,
    beta  = b,
    mean  = a / (a + b),
    var   = (a * b) / ((a + b)^2 * (a + b + 1)),
    kappa = a + b,
    fitted_quantiles = qbeta(p, a, b),
    input_quantiles  = q,
    probs            = p
  )
}

# Convenience wrapper for 10th/90th specifically
fit_beta_10_90 <- function(p10, p90, verbose = FALSE) {
  if (!(is.finite(p10) && is.finite(p90))) stop("Non-finite p10/p90.")
  if (!(p10 > 0 && p90 < 1 && p10 < p90)) stop("Require 0 < p10 < p90 < 1.")
  fit_beta_from_quantiles(q = c(p10, p90), p = c(0.1, 0.9), verbose = verbose)
}
