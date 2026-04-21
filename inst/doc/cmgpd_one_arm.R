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
  stop("Missing extdata: ", name, ". Run data-raw/build_vignette_fits.R", call. = FALSE)
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

## ----load-data----------------------------------------------------------------
data("nc_posX100_p3_k2", package = "CausalMixGPD")
onearm_dat <- data.frame(
  y = nc_posX100_p3_k2$y,
  nc_posX100_p3_k2$X
)


## ----exploratory-plots, fig.height=4.5, fig.width=7---------------------------
p1 <- ggplot(onearm_dat, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25,
                 fill = "grey85", colour = "grey35") +
  geom_density(linewidth = 0.9) +
  labs(x = "y", y = "Density",
       title = "Observed response distribution") +
  theme_minimal()

p2 <- ggplot(onearm_dat, aes(sample = y)) +
  stat_qq() +
  stat_qq_line(colour = 2) +
  labs(title = "Normal Q-Q plot for the response") +
  theme_minimal()

p1
p2

## ----fit-spliced-display, eval=FALSE------------------------------------------
# fit_spliced <- dpmgpd(
#   formula = y ~ x1 + x2 + x3,
#   data = onearm_dat,
#   backend = "sb",
#   kernel = "lognormal",
#   components = 5,
#   mcmc = mcmc_vig
# )

## ----fit-spliced, echo=FALSE--------------------------------------------------
one_arm_out <- readRDS(.pkg_extdata("one_arm_outputs.rds"))

## ----summary-spliced-display, eval=FALSE--------------------------------------
# summary(fit_spliced)

## ----summary-spliced, echo=FALSE----------------------------------------------
fsum <- system.file("extdata", "one_arm_fit_summary.txt", package = "CausalMixGPD")
if (!nzchar(fsum)) {
  alt <- normalizePath(
    file.path(knitr::current_input(dir = TRUE), "..", "inst", "extdata", "one_arm_fit_summary.txt"),
    winslash = "/",
    mustWork = FALSE
  )
  if (file.exists(alt)) fsum <- alt
}
if (nzchar(fsum) && file.exists(fsum)) {
  cat(paste(readLines(fsum, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
} else {
  cat("Precomputed fit summary not found; run tools/.Rscripts/build_vignette_fits.R.\n")
}

## ----plot-spliced-trace-display, eval=FALSE-----------------------------------
# plot(fit_spliced, family = c("traceplot", "density"), params = "alpha")

## ----plot-spliced-trace, echo=FALSE, fig.height=5.2, fig.width=7--------------
knitr::include_graphics("assets/one_arm_alpha_trace.png")

## ----plot-spliced-density-display, eval=FALSE---------------------------------
# # Posterior density for monitored parameters (e.g. alpha); see plot.mixgpd_fit().
# plot(fit_spliced, family = "density", params = "alpha")

## ----plot-spliced-density, echo=FALSE, fig.height=5, fig.width=7--------------
knitr::include_graphics("assets/one_arm_alpha_density.png")

## ----newdata-grid-display, eval=FALSE-----------------------------------------
# x_new <- as.data.frame(lapply(
#   onearm_dat[, c("x1", "x2", "x3")],
#   stats::quantile,
#   probs = c(0.25, 0.50, 0.75),
#   na.rm = TRUE
# ))
# rownames(x_new) <- c("q25", "q50", "q75")
# x_new

## ----newdata-grid, echo=FALSE-------------------------------------------------
x_new <- one_arm_out$x_new
x_new

## ----density-prediction-display, eval=FALSE-----------------------------------
# y_grid <- seq(min(onearm_dat$y), quantile(onearm_dat$y, 0.99), length.out = 120)
# pred_dens <- predict(
#   fit_spliced,
#   newdata = x_new,
#   y = y_grid,
#   type = "density",
#   interval = "credible",
#   level = 0.95,
#   store_draws = FALSE
# )
# plot(pred_dens, type = "density", facet = "covariate")

## ----density-prediction, echo=FALSE-------------------------------------------
pred_dens <- one_arm_out$pred_dens
plot(pred_dens, type = "density", facet = "covariate")

## ----survival-prediction-display, eval=FALSE----------------------------------
# y_grid <- seq(min(onearm_dat$y), quantile(onearm_dat$y, 0.99), length.out = 120)
# pred_surv <- predict(
#   fit_spliced,
#   newdata = x_new,
#   y = y_grid,
#   type = "survival",
#   interval = "credible",
#   level = 0.95,
#   store_draws = FALSE
# )
# plot(pred_surv)

## ----survival-prediction, echo=FALSE------------------------------------------
pred_surv <- one_arm_out$pred_surv
plot(pred_surv)

## ----quantile-prediction-display, eval=FALSE----------------------------------
# pred_quant <- predict(
#   fit_spliced,
#   newdata = x_new,
#   type = "quantile",
#   index = c(0.25, 0.50, 0.75, 0.90, 0.95),
#   interval = "credible",
#   level = 0.95,
#   store_draws = FALSE
# )

## ----quantile-prediction, echo=FALSE------------------------------------------
pred_quant <- one_arm_out$pred_quant

## ----quantile-prediction-plot-display, eval=FALSE-----------------------------
# plot(pred_quant)

## ----quantile-prediction-plot, echo=FALSE, fig.height=5.5, fig.width=7--------
plot(pred_quant)

## ----mean-prediction-display, eval=FALSE--------------------------------------
# pred_mean <- predict(
#   fit_spliced,
#   newdata = x_new,
#   type = "mean",
#   interval = "hpd",
#   level = 0.90,
#   store_draws = FALSE
# )

## ----mean-prediction, echo=FALSE----------------------------------------------
pred_mean <- one_arm_out$pred_mean

## ----mean-prediction-plot-display, eval=FALSE---------------------------------
# plot(pred_mean)

## ----mean-prediction-plot, echo=FALSE, fig.height=5.2, fig.width=7------------
plot(pred_mean)

## ----rmean-prediction-display, eval=FALSE-------------------------------------
# cutoff_val <- as.numeric(stats::quantile(onearm_dat$y, 0.95))
# pred_rmean <- predict(
#   fit_spliced,
#   newdata = x_new,
#   type = "rmean",
#   cutoff = cutoff_val,
#   interval = "hpd",
#   level = 0.90,
#   store_draws = FALSE
# )

## ----rmean-prediction, echo=FALSE---------------------------------------------
cutoff_val <- one_arm_out$cutoff_val

## ----rmean-table-display, eval=FALSE------------------------------------------
# knitr::kable(pred_rmean$fit_df, format = "html", row.names = FALSE, digits = 4)

## ----rmean-table, echo=FALSE--------------------------------------------------
knitr::kable(
  one_arm_out$pred_rmean_fit_df,
  format = "html",
  row.names = FALSE,
  digits = 4
)

## ----fit-bulk-only-display, eval=FALSE----------------------------------------
# fit_bulk <- dpmix(
#   formula = y ~ x1 + x2 + x3,
#   data = onearm_dat,
#   backend = "sb",
#   kernel = "lognormal",
#   components = 5,
#   mcmc = mcmc_vig
# )

## ----fit-bulk-only, echo=FALSE------------------------------------------------
cat("Bulk-only fit computed offline; see quantile comparison below.\n")

## ----compare-quantiles-display, eval=FALSE------------------------------------
# quant_bulk_fit_df <- predict(
#   fit_bulk,
#   newdata = x_new,
#   type = "quantile",
#   index = c(0.90, 0.95),
#   interval = "credible",
#   level = 0.95,
#   store_draws = FALSE
# )$fit_df
# quant_spliced_fit_df <- predict(
#   fit_spliced,
#   newdata = x_new,
#   type = "quantile",
#   index = c(0.90, 0.95),
#   interval = "credible",
#   level = 0.95,
#   store_draws = FALSE
# )$fit_df

## ----compare-quantiles, echo=FALSE--------------------------------------------
quant_bulk_fit_df <- one_arm_out$quant_bulk_fit_df
quant_spliced_fit_df <- one_arm_out$quant_spliced_fit_df

## ----compare-quantiles-plot-display, eval=FALSE-------------------------------
# qb <- quant_bulk_fit_df
# qb$model <- "dpmix (bulk only)"
# qs <- quant_spliced_fit_df
# qs$model <- "dpmgpd (spliced)"
# qc <- rbind(qb, qs)
# qc$profile <- rownames(x_new)[as.integer(qc$id)]
# ggplot(qc, aes(x = profile, y = estimate, colour = model, group = model)) +
#   geom_line(linewidth = 0.8) +
#   geom_point(size = 2) +
#   facet_wrap(~index, scales = "free_y", ncol = 2) +
#   labs(
#     x = "Covariate profile",
#     y = "Posterior mean quantile",
#     title = "Upper quantiles: bulk-only vs spliced tail"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 20, hjust = 1))

## ----compare-quantiles-plot, echo=FALSE, fig.height=5, fig.width=7------------
qb <- quant_bulk_fit_df
qb$model <- "dpmix (bulk only)"
qs <- quant_spliced_fit_df
qs$model <- "dpmgpd (spliced)"
qc <- rbind(qb, qs)
qc$profile <- rownames(x_new)[as.integer(qc$id)]
ggplot(qc, aes(x = profile, y = estimate, colour = model, group = model)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~index, scales = "free_y", ncol = 2) +
  labs(
    x = "Covariate profile",
    y = "Posterior mean quantile",
    title = "Upper quantiles: bulk-only vs spliced tail"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

## ----session-info-------------------------------------------------------------
sessionInfo()

