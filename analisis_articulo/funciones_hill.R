## ================================================================
## Funciones de diversidad de Hill basadas en abundancia
## Implementación de Chao et al. (2014, Ecol. Monogr. 84: 45-67) y
## Chao & Jost (2012), equivalente a iNEXT (Hsieh et al. 2016).
## Se incluyen aquí para no depender de paquetes no disponibles.
## ================================================================

lchoose_ratio <- function(a, b, n, m) exp(lchoose(a, b) - lchoose(n, m))

f_counts <- function(x) {
  x <- x[x > 0]
  list(n = sum(x), f1 = sum(x == 1), f2 = sum(x == 2), S = length(x))
}

## Completitud del muestreo (cobertura, Chao & Jost 2012)
cobertura_obs <- function(x) {
  k <- f_counts(x); n <- k$n; f1 <- k$f1; f2 <- k$f2
  if (f2 > 0) 1 - f1 / n * ((n - 1) * f1 / ((n - 1) * f1 + 2 * f2))
  else        1 - f1 / n * ((n - 1) * (f1 - 1) / ((n - 1) * (f1 - 1) + 2))
}

f0_hat <- function(x) {
  k <- f_counts(x); n <- k$n
  if (k$f2 > 0) (n - 1) / n * k$f1^2 / (2 * k$f2)
  else          (n - 1) / n * k$f1 * (k$f1 - 1) / 2
}

## Estimadores asintóticos
chao1   <- function(x) f_counts(x)$S + f0_hat(x)
shannon_chao <- function(x) {         # Chao, Wang & Jost (2013)
  x <- x[x > 0]; n <- sum(x); f1 <- sum(x == 1); f2 <- sum(x == 2)
  A <- if (f2 > 0) 2 * f2 / ((n - 1) * f1 + 2 * f2) else
       if (f1 > 0) 2 / ((n - 1) * (f1 - 1) + 2) else 1
  t1 <- sum(sapply(x[x <= n - 1], function(xi) xi / n * sum(1 / (xi:(n - 1)))))
  t2 <- 0
  if (f1 > 0 && A < 1) {
    r  <- 1:(n - 1)
    t2 <- f1 / n * (1 - A)^(1 - n) * (-log(A) - sum((1 - A)^r / r))
  }
  exp(t1 + t2)
}
simpson_inv <- function(x) { x <- x[x > 0]; n <- sum(x); 1 / sum(x * (x - 1) / (n * (n - 1))) }

## Diversidad empírica (observada) de orden q
hill_obs <- function(x, q) {
  p <- x[x > 0] / sum(x)
  if (q == 1) exp(-sum(p * log(p))) else sum(p^q)^(1 / (1 - q))
}

## Rarefacción (m <= n) y extrapolación (m > n)
hill_m <- function(x, q, m) {
  x <- x[x > 0]; n <- sum(x); k <- f_counts(x)
  if (m <= n) {
    if (q == 0) return(sum(1 - lchoose_ratio(n - x, m, n, m)))
    fk <- sapply(1:m, function(kk) sum(exp(lchoose(x, kk) + lchoose(n - x, m - kk) - lchoose(n, m))))
    kk <- 1:m
    if (q == 1) return(exp(-sum(kk / m * log(kk / m) * fk)))
    return(sum((kk / m)^q * fk)^(1 / (1 - q)))
  }
  ms <- m - n
  f0 <- f0_hat(x)
  A  <- if (f0 > 0) 1 - k$f1 / (n * f0 + k$f1) else 1
  if (q == 0) return(k$S + f0 * (1 - A^ms))
  if (q == 1) {
    Hobs <- log(hill_obs(x, 1)); Hest <- log(shannon_chao(x))
    return(exp(Hobs + (Hest - Hobs) * (1 - A^ms)))
  }
  if (q == 2) {
    s2 <- sum(x * (x - 1)) / (n * (n - 1))
    return(1 / (1 / m + (m - 1) / m * s2))
  }
}

cobertura_m <- function(x, m) {
  x <- x[x > 0]; n <- sum(x); k <- f_counts(x)
  if (m < n) return(1 - sum(x / n * exp(lchoose(n - x, m) - lchoose(n - 1, m))))
  if (m == n) return(cobertura_obs(x))
  f1 <- k$f1; f2 <- k$f2
  a <- if (f2 > 0) (n - 1) * f1 / ((n - 1) * f1 + 2 * f2) else (n - 1) * (f1 - 1) / ((n - 1) * (f1 - 1) + 2)
  1 - f1 / n * a^(m - n + 1)
}

## Comunidad estimada para bootstrap (Chao et al. 2014, Apéndice)
comunidad_boot <- function(x) {
  x <- x[x > 0]; n <- sum(x); C <- cobertura_obs(x)
  lambda <- (1 - C) / sum(x / n * (1 - x / n)^n)
  p  <- x / n * (1 - lambda * (1 - x / n)^n)
  f0 <- ceiling(f0_hat(x))
  if (f0 > 0 && C < 1) p <- c(p, rep((1 - C) / f0, f0))
  p / sum(p)
}

## Curva completa con IC 95 % por bootstrap
curva_hill <- function(x, qs = 0:2, m_max = 2 * sum(x), n_pts = 40, B = 200, seed = 1) {
  set.seed(seed)
  x <- x[x > 0]; n <- sum(x)
  ms <- sort(unique(c(round(seq(1, m_max, length.out = n_pts)), n)))
  pb <- comunidad_boot(x)
  out <- list()
  for (q in qs) {
    est  <- sapply(ms, function(m) hill_m(x, q, m))
    boot <- replicate(B, {
      xb <- as.vector(rmultinom(1, n, pb)); xb <- xb[xb > 0]
      sapply(ms, function(m) hill_m(xb, q, m))
    })
    sdv <- apply(boot, 1, sd)
    out[[length(out) + 1]] <- data.frame(
      m = ms, q = q, qD = est, LCL = pmax(est - 1.96 * sdv, 0), UCL = est + 1.96 * sdv,
      SC = sapply(ms, function(m) cobertura_m(x, m)),
      metodo = ifelse(ms < n, "Rarefacción", ifelse(ms == n, "Observado", "Extrapolación")))
  }
  do.call(rbind, out)
}

## Resumen asintótico con IC por bootstrap
asintoticos <- function(x, B = 200, seed = 1) {
  set.seed(seed)
  x <- x[x > 0]; n <- sum(x); pb <- comunidad_boot(x)
  f <- function(y) c(chao1(y), shannon_chao(y), simpson_inv(y))
  est <- f(x)
  bt  <- replicate(B, { y <- as.vector(rmultinom(1, n, pb)); f(y[y > 0]) })
  sdv <- apply(bt, 1, sd)
  data.frame(q = 0:2,
             Observado = c(length(x), hill_obs(x, 1), hill_obs(x, 2)),
             Estimado = est, LCL = est - 1.96 * sdv, UCL = est + 1.96 * sdv)
}
