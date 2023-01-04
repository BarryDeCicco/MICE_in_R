> mediation::amelidiate


function (g) 
{
  ob <- names(g)
  d.avg <- n.avg <- z.avg <- d1.sims <- d0.sims <- z1.sims <- z0.sims <- n1.sims <- n0.sims <- tau.sims <- d.sims <- z.sims <- list()
  for (i in 1:length(g)) {
    k <- sprintf("g$%s", ob[i])
    k <- eval(parse(text = k))
    conf.level <- as.numeric(k$conf.level)
    model.y <- k$model.y
    model.m <- k$model.m
    boot = k$boot
    boot.ci.type = k$boot.ci.type
    treat = k$treat
    mediator = k$mediator
    nobs <- k$nobs
    sims <- k$sims
    INT <- paste(treat, mediator, sep = ":") %in% attr(model.y$terms, 
                                                       "term.labels") | paste(mediator, treat, sep = ":") %in% 
      attr(model.y$terms, "term.labels")
    d1.sims[[i]] <- k$d1.sims
    d0.sims[[i]] <- k$d0.sims
    z1.sims[[i]] <- k$z1.sims
    z0.sims[[i]] <- k$z0.sims
    n1.sims[[i]] <- k$n1.sims
    n0.sims[[i]] <- k$n0.sims
    tau.sims[[i]] <- k$tau.sims
    d.sims[[i]] <- k$d.avg.sims
    z.sims[[i]] <- k$z.avg.sims
    d.avg[[i]] <- k$d.avg
    z.avg[[i]] <- k$z.avg
    n.avg[[i]] <- k$n.avg
  }
  r <- c("d.sims", "d1.sims", "d0.sims", "z.sims", "z1.sims", 
         "z0.sims", "n1.sims", "n0.sims", "tau.sims", "d.avg", 
         "z.avg", "n.avg")
  store <- list()
  for (i in 1:length(r)) {
    store[[i]] <- unlist(lapply(eval(parse(text = r[i])), 
                                function(l) l[sapply(l, is.atomic)]))
  }
  names(store) <- r
  x <- as.data.frame(store)
  low <- (1 - conf.level)/2
  high <- 1 - low
  cis <- as.data.frame(apply(x, 2, quantile, c(low, high)))
  d1.ci <- cis$d1.sims
  d0.ci <- cis$d0.sims
  d.avg.ci <- cis$d.sims
  tau.ci <- cis$tau.sims
  z1.ci <- cis$z1.sims
  z0.ci <- cis$z0.sims
  z.avg.ci <- cis$z.sims
  n.avg.ci <- cis$n.avg
  n1.ci <- cis$n1.sims
  n0.ci <- cis$n0.sims
  cis <- as.data.frame(apply(x, 2, mean))
  h <- row.names(cis)
  cis <- as.data.frame(t(cis))
  names(cis) <- h
  d1 <- cis$d1.sims
  d0 <- cis$d0.sims
  tau <- cis$tau.sims
  z1 <- cis$z1.sims
  z0 <- cis$z0.sims
  n1 <- cis$n1
  n0 <- cis$n0
  d.avg <- cis$d.avg
  z.avg <- cis$z.avg
  n.avg <- cis$n.avg
  n0.p <- n1.p <- d0.p <- d1.p <- z1.p <- z0.p <- tau.p <- d.avg.p <- z.avg.p <- n.avg.p <- 1
  out <- list(d0 = d0, d1 = d1, z0 = z1, tau = tau, d0.ci = d0.ci, 
              d1.ci = d1.ci, z1.ci = z1.ci, z0.ci = z0.ci, tau.ci = tau.ci, 
              d0.sims = d0.sims, d1.sims = d1.sims, z1.sims = z1.sims, 
              z0.sims = z0.sims, tau.sims = tau.sims, n1.sims = n1.sims, 
              n0.sims = n0.sims)
  x <- plot.process(out)
  out <- list(d0 = d0, d1 = d1, z0 = z0, z1 = z1, n1 = n1, 
              n0 = n0, tau = tau, d0.ci = d0.ci, d1.ci = d1.ci, z1.ci = z1.ci, 
              z0.ci = z0.ci, n1.ci = n1.ci, n0.ci = n0.ci, tau.ci = tau.ci, 
              d0.sims = d0.sims, d1.sims = d1.sims, z1.sims = z1.sims, 
              z0.sims = z0.sims, tau.sims = tau.sims, n1.sims = n1.sims, 
              n0.sims = n0.sims, coef.vec.1 = x$coef.vec.1, lower.vec.1 = x$lower.vec.1, 
              upper.vec.1 = x$upper.vec.1, coef.vec.0 = x$coef.vec.0, 
              lower.vec.0 = x$lower.vec.0, upper.vec.0 = x$upper.vec.0, 
              tau.vec = x$tau.vec, tau.coef = tau, range.1 = x$range.1, 
              range.0 = x$range.0, nobs = nobs, sims = sims, d.avg = d.avg, 
              z.avg = z.avg, n.avg = n.avg, d.avg.ci = d.avg.ci, z.avg.ci = z.avg.ci, 
              n.avg.ci = n.avg.ci, d0.p = d0.p, d1.p = d1.p, z1.p = z1.p, 
              z0.p = z0.p, tau.p = tau.p, n0.p = n0.p, n1.p = n1.p, 
              d.avg.p = d.avg.p, z.avg.p = z.avg.p, n.avg.p = n.avg.p, 
              INT = INT, boot = boot, boot.ci.type = boot.ci.type, 
              model.y = model.y, model.m = model.m)
  class(out) <- "mediate"
  return(out)
}
<bytecode: 0x000001dbc25e0d30>
  <environment: namespace:mediation>
  > 