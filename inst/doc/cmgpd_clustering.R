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
  stop("Missing extdata: ", name, ". Run tools/.Rscripts/build_vignette_fits.R", call. = FALSE)
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
# Used when regenerating clustering vignette artifacts (tools/.Rscripts/build_vignette_fits.R).
mcmc_clust <- list(
  niter = 1200,
  nburnin = 300,
  thin = 2,
  nchains = 2,
  seed = 2029
)

## ----load-data----------------------------------------------------------------
data("nc_realX100_p3_k2", package = "CausalMixGPD")
dat_cl <- data.frame(
  y = nc_realX100_p3_k2$y,
  nc_realX100_p3_k2$X
)

## ----split-data-display, eval=FALSE-------------------------------------------
# set.seed(123)
# idx_train <- sample(seq_len(nrow(dat_cl)), size = 80)
# train_dat <- dat_cl[idx_train, ]
# test_dat <- dat_cl[-idx_train, ]
# nrow(train_dat)
# nrow(test_dat)

## ----split-data, echo=FALSE---------------------------------------------------
clust_out <- readRDS(.pkg_extdata("clustering_outputs.rds"))
train_dat <- clust_out$train_dat
test_dat <- clust_out$test_dat

nrow(train_dat)
nrow(test_dat)

## ----exploratory-plots, fig.height=4.8, fig.width=7---------------------------
ggplot(train_dat, aes(x = y)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25,
                 fill = "grey85", colour = "grey35") +
  geom_density(linewidth = 0.9) +
  labs(x = "y", y = "Density", title = "Training-sample response distribution") +
  theme_minimal()

## ----fit-cluster-display, eval=FALSE------------------------------------------
# fit_cluster <- dpmix.cluster(
#   formula = y ~ x1 + x2 + x3,
#   data = train_dat,
#   kernel = "laplace",
#   type = "both",
#   components = 8,
#   mcmc = mcmc_clust
# )

## ----fit-cluster, echo=FALSE--------------------------------------------------
fit_cluster <- clust_out$fit_cluster

## ----psm-prediction-display, eval=FALSE---------------------------------------
# z_train_psm <- predict(fit_cluster, type = "psm")
# z_train_psm

## ----psm-prediction, echo=FALSE-----------------------------------------------
z_train_psm <- clust_out$psm_obj
z_train_psm

## ----psm-plot-display, eval=FALSE---------------------------------------------
# plot(z_train_psm, type = "summary")

## ----psm-plot, echo=FALSE, fig.height=5.2, fig.width=7------------------------
plot(z_train_psm, type = "summary")

## ----label-prediction-train-display, eval=FALSE-------------------------------
# z_train_lab <- predict(fit_cluster, type = "label", return_scores = TRUE)
# z_train_lab

## ----label-prediction-train, echo=FALSE---------------------------------------
z_train_lab <- clust_out$train_lab
z_train_lab

## ----label-plot-train-display, eval=FALSE-------------------------------------
# plot(z_train_lab, type = "summary")

## ----label-plot-train, echo=FALSE, fig.height=5, fig.width=7------------------
plot(z_train_lab, type = "summary")

## ----label-prediction-test-display, eval=FALSE--------------------------------
# z_test <- predict(fit_cluster, newdata = test_dat, type = "label", return_scores = TRUE)
# z_test

## ----label-prediction-test, echo=FALSE----------------------------------------
z_test <- clust_out$test_lab
z_test

## ----test-cluster-profiles-display, eval=FALSE--------------------------------
# summary(
#   predict(fit_cluster, newdata = test_dat, type = "label", return_scores = TRUE)
# )$cluster_profiles

## ----test-cluster-profiles, echo=FALSE----------------------------------------
clust_out$cluster_profiles

## ----sizes-plot-test-display, eval=FALSE--------------------------------------
# plot(z_test, type = "sizes")

## ----sizes-plot-test, echo=FALSE, fig.height=4.8, fig.width=7-----------------
plot(z_test, type = "sizes")

## ----session-info-------------------------------------------------------------
sessionInfo()

