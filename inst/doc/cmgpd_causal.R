## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.8,
  out.width = "85%",
  fig.align = "center",
  message = FALSE,
  warning = FALSE
)
.pkg_extdata <- function(name) {
  f <- system.file("extdata", name, package = "CausalMixGPD")
  if (nzchar(f)) {
    return(f)
  }
  vdir <- knitr::current_input(dir = TRUE)
  pkg_root <- normalizePath(file.path(vdir, ".."), winslash = "/", mustWork = TRUE)
  alt <- file.path(pkg_root, "inst", "extdata", name)
  if (file.exists(alt)) {
    return(normalizePath(alt, winslash = "/", mustWork = TRUE))
  }
  stop("Missing extdata: ", name, ".", call. = FALSE)
}

## ----libraries----------------------------------------------------------------
library(CausalMixGPD)
library(ggplot2)

## ----mcmc-settings------------------------------------------------------------
mcmc_vig <- list(
  niter = 1200,
  nburnin = 300,
  thin = 2,
  nchains = 2,
  seed = 2026
)
# Settings used when regenerating `inst/extdata/causal_*.csv` (see tools/.Rscripts/build_vignette_fits.R).
mcmc_out <- list(
  niter = 1200,
  nburnin = 300,
  thin = 2,
  nchains = 2,
  seed = 2027
)
mcmc_ps <- list(
  niter = 1000,
  nburnin = 250,
  thin = 2,
  nchains = 2,
  seed = 2028
)

## ----load-data----------------------------------------------------------------
data("causal_pos500_p3_k2", package = "CausalMixGPD")
causal_dat <- data.frame(
  y = causal_pos500_p3_k2$y,
  A = causal_pos500_p3_k2$A,
  causal_pos500_p3_k2$X
)

## ----exploratory-plots, fig.height=4.8, fig.width=7---------------------------
ggplot(causal_dat, aes(x = y, colour = factor(A), fill = factor(A))) +
  geom_histogram(aes(y = after_stat(density)), bins = 25,
                 alpha = 0.25, position = "identity") +
  labs(
    x = "y",
    y = "Density",
    colour = "A",
    fill = "A",
    title = "Observed outcome distributions by treatment arm"
  ) +
  theme_minimal()

## ----fit-causal-display, eval=FALSE-------------------------------------------
# fit_causal <- dpmgpd.causal(
#   formula = y ~ x1 + x2 + x3,
#   data = causal_dat,
#   treat = "A",
#   backend = "crp",
#   kernel = "gamma",
#   components = 6,
#   PS = "logit",
#   ps_scale = "logit",
#   ps_summary = "mean",
#   mcmc_outcome = mcmc_out,
#   mcmc_ps = mcmc_ps,
#   parallel_arms = FALSE
# )

## ----ate-summary-display, eval=FALSE------------------------------------------
# ate_fit <- ate(fit_causal, interval = "hpd", level = 0.95, show_progress = FALSE)
# knitr::kable(summary(ate_fit)$effect_table, format = "html", digits = 4)

## ----ate-summary, echo=FALSE--------------------------------------------------
ate_tab <- read.csv(.pkg_extdata("causal_ate.csv"))
knitr::kable(ate_tab, format = "html", digits = 4)

## ----qte-summary-display, eval=FALSE------------------------------------------
# qte_fit <- qte(
#   fit_causal,
#   probs = c(0.25, 0.50, 0.75, 0.90, 0.95),
#   interval = "credible",
#   level = 0.95,
#   show_progress = FALSE
# )
# knitr::kable(summary(qte_fit)$effect_table, format = "html", digits = 4)

## ----qte-summary, echo=FALSE--------------------------------------------------
qte_tab <- read.csv(.pkg_extdata("causal_qte.csv"))
knitr::kable(qte_tab, format = "html", digits = 4)

## ----qte-plot-display, eval=FALSE---------------------------------------------
# qte_fit <- qte(
#   fit_causal,
#   probs = c(0.25, 0.50, 0.75, 0.90, 0.95),
#   interval = "credible",
#   level = 0.95,
#   show_progress = FALSE
# )
# plot(qte_fit, type = "effect")

## ----qte-plot, echo=FALSE, fig.height=5, fig.width=7--------------------------
plot(
  qte_tab$prob,
  qte_tab$estimate,
  type = "b",
  pch = 19,
  xlab = "Quantile level",
  ylab = "Estimated QTE",
  ylim = range(c(qte_tab$lower, qte_tab$upper)),
  main = "Quantile treatment effect curve"
)
segments(qte_tab$prob, qte_tab$lower, qte_tab$prob, qte_tab$upper, lwd = 1.2)
abline(h = 0, lty = 2, col = "grey50")

## ----newdata-grid-------------------------------------------------------------
qs <- c(0.25, 0.50, 0.75)
Xgrid <- expand.grid(lapply(causal_dat[, c("x1", "x2", "x3")], quantile, probs = qs))
head(Xgrid)

## ----conditional-qte-table-display, eval=FALSE--------------------------------
# Xgrid <- as.data.frame(lapply(
#   causal_dat[, c("x1", "x2", "x3")],
#   stats::quantile,
#   probs = c(0.25, 0.50, 0.75),
#   na.rm = TRUE
# ))
# rownames(Xgrid) <- c("low", "mid", "high")
# cqte_fit <- cqte(
#   fit_causal,
#   newdata = Xgrid,
#   probs = c(0.25, 0.50, 0.75, 0.90, 0.95),
#   interval = "credible",
#   level = 0.95,
#   show_progress = FALSE
# )
# knitr::kable(summary(cqte_fit)$effect_table, format = "html", digits = 4)

## ----conditional-qte-table, echo=FALSE----------------------------------------
cond_q <- read.csv(.pkg_extdata("conditional_quantiles.csv"))
knitr::kable(cond_q, format = "html", digits = 4)

## ----session-info-------------------------------------------------------------
sessionInfo()

