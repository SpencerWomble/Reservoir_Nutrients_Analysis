# =============================================================
# Full model validation workflow
# Test model: brms lognormal regression with a left-censored response
#   fit <- brm(nitrate_obs | cens(cens) ~ veg,
#              family = lognormal(), backend = "cmdstanr", ...)
#
# Run top to bottom. Sections 1-3 and 5 apply to this toy model as is.
# Section 4 is scaffolding for your real (multi-predictor, time-indexed)
# data. Section 6 only works because this is simulated data with a
# known truth.
# =============================================================

# ---- 0. Packages and constants -----------------------------
library(brms)
library(posterior)
library(bayesplot)
library(loo)
library(DHARMa)
library(dplyr)
library(ggplot2)

# one-time setup
# install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
# cmdstanr::install_cmdstan()




dl <- 0.05   # the detection limit the data were censored at


# SIMULATED DATA AND MODEL
library(tidyverse)

set.seed(123)

n <- 100

dat <- tibble(
  veg = rep(c("Control", "Vegetated"), each = n/2),
  nitrate_true = c(
    rlnorm(n/2, meanlog = -2.5, sdlog = 0.7),
    rlnorm(n/2, meanlog = -3.2, sdlog = 0.7)
  )
)

dl <- 0.05

dat <- dat %>%
  mutate(
    nitrate_obs = ifelse(nitrate_true < dl, dl, nitrate_true),
    cens = ifelse(nitrate_true < dl, "left", "none")
  )


library(brms)

fit <- brm(
  bf(nitrate_obs | cens(cens) ~ veg),
  data = dat,
  family = lognormal(),
  chains = 4,
  iter = 4000,
  cores = 4,
#  open_progress = FALSE,
  backend = "cmdstanr"
)


# =============================================================
# 1. SAMPLER DIAGNOSTICS  (did the MCMC actually work?)
#    Always first. If the sampler failed, every downstream
#    check below is meaningless.
# =============================================================

# ---- summary(fit) ----
# Not a plot, but the first thing to read. In the two coefficient tables,
# scan the last three columns:
#   Rhat      : want 1.00 on every row, 1.01 at the very most.
#   Bulk_ESS  : effective sample size for the center (mean/median). Want it
#               in the hundreds at an absolute minimum, ideally 1000+.
#   Tail_ESS  : same idea for the distribution's tails, which drive your
#               95% interval endpoints. Also want hundreds to thousands.
# A single bad row is enough to distrust that one parameter.
summary(fit)

# ---- plot(fit): trace plots (left column) + densities (right column) ----
# TRACE PLOTS show each chain's value for a parameter across iterations.
#   GOOD: a "fuzzy caterpillar." All 4 chains overlap into one fat band,
#         the band is flat (no upward or downward drift), and the line jumps
#         rapidly up and down (good mixing). You should not be able to tell
#         the individual chains apart.
#   BAD, and what each pattern means:
#     - Chains sit in separate parallel bands that never overlap:
#         the chains disagree, i.e. non-convergence. Same thing a high Rhat
#         is telling you.
#     - The band drifts or trends across the window (still climbing or
#         falling): not stationary, warmup was too short. Increase iter/warmup.
#     - Flat horizontal stretches where a chain gets "stuck" before jumping:
#         poor mixing / a sticky sampler, often paired with divergences.
#         Reparameterize or raise adapt_delta.
#     - Slow, snaking wander instead of rapid jumps: high autocorrelation (within the MCMC sampler),
#         which shows up as low ESS below. More iterations help.
#     - One chain exploring a totally different level than the other three:
#         multimodality, or a chain that never found the bulk of the posterior.
# DENSITY PLOTS (right) show the marginal posterior, one curve per chain.
#   GOOD: the 4 per-chain densities lie on top of each other, smooth and
#         (here) unimodal.
#   BAD: chain densities that are offset or differently shaped (chains
#        disagree), or unexpected extra bumps (possible multimodality).
plot(fit)

# ---- mcmc_plot(fit, type = "rhat"): Rhat dot plot ----
# One dot per parameter, shaded by severity.
#   GOOD: every dot in the leftmost (lightest) zone, at or below 1.05, and
#         really you want them hugging 1.00.
#   BAD: dots creeping right into the 1.05-1.1 zone or beyond means those
#        parameters' chains have not converged to a common distribution. Do
#        not interpret those parameters; run longer or fix the model.
mcmc_plot(fit, type = "rhat")

# ---- mcmc_plot(fit, type = "neff"): effective sample size ratio ----
# Plots ratio = effective sample size / total draws, per parameter.
#   GOOD: ratios above 0.5 (lightest zone). Anything above ~0.1 is usable.
#   BAD: ratios below 0.1 (darkest zone) flag heavy autocorrelation (in the MCMC sampler). Your
#        8000 draws are then worth far fewer independent ones, so estimates
#        (especially interval endpoints) get noisy. Increase iter, or
#        reparameterize the part of the model causing it.
mcmc_plot(fit, type = "neff")

# ---- mcmc_plot(fit, type = "rank_overlay"): rank histograms ----
# Pools all draws, ranks them, then histograms each chain's ranks. More
# sensitive than trace plots for subtle non-convergence.
#   GOOD: every chain's histogram is roughly flat (uniform) and the chains
#         overlap. No chain is consistently high or low in any rank bin.
#   BAD: a chain sitting above the others on one side and below on the other
#        (it favors high or low values, so it is exploring a different region),
#        or systematic U / staircase shapes (trends, non-stationarity).
mcmc_plot(fit, type = "rank_overlay")

# ---- divergent transitions ----
# Prints the number of divergent transitions, an HMC-specific failure with
# no frequentist analog. A divergence means the sampler could not follow the
# posterior's curvature and bailed, leaving a region under-explored.
#   GOOD: 0.
#   BAD: any nonzero count. A few scattered ones are sometimes tolerable;
#        many, or divergences that cluster in one region of the posterior,
#        bias your estimates. First fix: control = list(adapt_delta = 0.95)
#        (or 0.99). If they persist, reparameterize or simplify the model.
np <- nuts_params(fit)
sum(subset(np, Parameter == "divergent__")$Value)

# =============================================================
# 2. POSTERIOR PREDICTIVE CHECKS  (does the model describe the data?)
#    Every plot here overlays your real data (dark) on datasets simulated
#    from the fitted model (light). The question is always the same: is the
#    real data unremarkable when set against the simulated data?
# =============================================================

# ---- pp_check density overlay ----
# Dark line = observed density of nitrate_obs. Light lines = densities of
# datasets drawn from the posterior.
#   GOOD: the dark line sits in the middle of the light bundle and matches it
#         on location (where the peak is), spread (how wide), and shape (skew,
#         number of modes).
#   BAD, and what it points to:
#     - Dark peak taller and narrower than the sims: model overstates the
#         spread (predicts more variability than there is).
#     - Dark peak shorter and wider than the sims: model understates spread.
#     - Dark line shifted left or right of the bundle: location bias, the mean
#         structure is off.
#     - Dark line has a second hump the sims miss: an unmodeled subgroup or
#         mixture (here, possibly a veg effect needing more than a shift).
#     - Dark line clearly more skewed than the sims: wrong family or link.
#   EXPECTED here: a small mismatch right at the detection limit (~0.05),
#         because the observed data piles up there and the latent sims do not.
#         That is the censoring artifact, not misfit. Judge the fit on the bulk
#         and tail, not the left edge.
pp_check(fit, ndraws = 100)

# ---- pp_check ECDF overlay ----
# The same comparison as cumulative curves. Easier to read the tails and any
# point masses than the density version.
#   GOOD: the dark ECDF stays inside the light bundle across the whole range.
#   BAD: dark curve riding consistently above or below the bundle (location
#        shift); crossing the bundle with a clearly steeper or shallower slope
#        (variance too small or too large); a vertical step the sims lack (a
#        point mass, e.g. the detection-limit pile-up). Same left-edge caveat.
pp_check(fit, type = "ecdf_overlay", ndraws = 100)

# ---- grouped density overlay ----
# Splits the density overlay into one panel per veg level. This is how you
# catch a model that looks fine overall but fails inside a group.
#   GOOD: within every panel, the dark line sits in the light bundle.
#   BAD: one group fits and another is shifted or wrong-shaped. If, say,
#        Vegetated is fine but Control is too wide, the groups likely need
#        their own sigma (a distributional model: bf(..., sigma ~ veg)).
pp_check(fit, type = "dens_overlay_grouped", group = "veg", ndraws = 100)

# ---- test-statistic checks ----
# Instead of the whole distribution, these reduce each simulated dataset to a
# single number (here the mean) and histogram those, with your observed value
# drawn as a vertical line.
#   GOOD: the observed line falls in the bulk of the histogram (the Bayesian
#         "p-value", the fraction of sims more extreme, sits comfortably
#         mid-range, not near 0 or 1).
#   BAD: the observed line out in a tail or off the edge of the histogram means
#        the model systematically misses that feature of the data.
#   TIP: rerun with stat = "sd", "max", or a custom function to probe spread and
#        extremes, not just central tendency.
pp_check(fit, type = "stat", stat = "mean")
    # veg looks off

# The same statistic computed within each veg level. Isolates which group (if
# any) is responsible when the pooled check looks off.
pp_check(fit, type = "stat_grouped", stat = "mean", group = "veg")
    # veg is the level of the group causing the mismatch in the previous pp_check

# ---- censoring-specific check (custom) ----
# The standard pp_check drops censored points, so it never tests the thing you
# most need to trust. This does: it compares the KNOWN fraction of nondetects
# in your data against the fraction the model predicts below the detection limit.
#   GOOD: the red line (observed censoring rate) sits inside the bulk of the
#         predicted histogram. The model reproduces how much data falls below
#         detection, so the censoring machinery is calibrated.
#   BAD: the red line in the tail or past the edge means the model predicts too
#        many or too few nondetects. That points to the location or scale being
#        off near the detection limit, which is exactly where censored data is
#        most sensitive. Revisit the family or priors.
yrep <- posterior_predict(fit)
prop_below_pred <- apply(yrep, 1, function(y) mean(y < dl))  # one per draw
prop_below_obs  <- mean(dat$cens == "left")                  # known from data

ggplot(data.frame(prop = prop_below_pred), aes(prop)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = prop_below_obs, linewidth = 1, colour = "red") +
  labs(x = "Predicted proportion below detection limit",
       title = "Observed censoring rate (red) vs posterior prediction")

# =============================================================
# 3. DHARMa RESIDUAL DIAGNOSTICS  (adapted for a Bayesian model)
#
#    DHARMa was built for frequentist GLMMs, but it only needs three
#    inputs, all of which a brms fit can provide:
#      simulatedResponse:       matrix [n_obs x n_sims] of simulated y
#      observedResponse:        the observed y vector
#      fittedPredictedResponse: one predicted value per obs (plot axis)
#
#    Two gotchas:
#    (a) posterior_predict returns [n_draws x n_obs], so TRANSPOSE it.
#    (b) CENSORING: posterior_predict gives the latent lognormal draws,
#        which can fall below the detection limit. Your observed values
#        were censored to dl. Apply the SAME censoring to the simulations
#        so DHARMa compares like with like. Skip this and the residuals
#        for censored points are meaningless.
# =============================================================

sims      <- t(posterior_predict(fit, ndraws = 1000))  # [n_obs x 1000] - transposing
sims_cens <- ifelse(sims < dl, dl, sims)               # mirror the observation process (add censoring)

fitted_med <- apply(posterior_epred(fit), 2, median)   # expected value per obs
  # posterior_epred(fit) gets the posterior expected values
  # 2 means operate across columns (1 for rows) - this is part of the apply() function
  # Returns a single median fitted value for every observation (remember the sims are distributions).
  # So, the function reads: For each column (observation), compute the median across all posterior draws.
  # These are the posterior median estimates of the expected response.

  # Why posterior_epred() instead of posterior_predict()
  # posterior_epred() includes uncertainty in parameters only. posterior_predict() includes uncertainty in parameters and residual/error variation

dh <- createDHARMa(
  simulatedResponse       = sims_cens,
  observedResponse        = dat$nitrate_obs, # actual observed data
  fittedPredictedResponse = fitted_med,
  integerResponse         = FALSE      # lognormal is continuous
)

# Standard DHARMa panel:
#   left  = QQ plot of scaled residuals; should be uniform, so points on the line
#   right = residual vs predicted; should be a flat, structureless band
plot(dh)

# Formal tests, exactly as you'd run for a glmmTMB model:
testUniformity(dh)   # KS test: are the scaled residuals uniform?
testDispersion(dh)   # over- or under-dispersion
testOutliers(dh)     # more extreme points than the model expects?

# Residuals against a predictor. Here veg (categorical). On real data with
# a continuous predictor, structure in this plot is your nonlinearity flag.
plotResiduals(dh, form = dat$veg)

# =============================================================
# 4. STRUCTURE CHECKS  (templates for your real reservoir data)
#    The toy data has one categorical predictor and no time index,
#    so these are scaffolding, ready to uncomment on the real model.
# =============================================================

# ---- 4a. Temporal autocorrelation ----
# 1. test the residuals (DHARMa understands the simulated residuals):
# testTemporalAutocorrelation(dh, time = dat$date)
# 2. if present, MODEL it rather than just flagging it:
# fit_ar <- brm(bf(nitrate_obs | cens(cens) ~ veg + ar(time, gr = site, p = 1)),
#               data = dat, family = lognormal(), backend = "cmdstanr")
# 3. compare with and without the AR(1) term:
# loo_compare(loo(fit), loo(fit_ar))

# ---- 4b. Non-linearity ----
# For a continuous predictor x, residual-vs-predictor is the check:
# pp_check(fit, type = "error_scatter_avg_vs_x", x = "x")
# then model curvature with a smooth (mgcv lives inside brms):
# brm(bf(nitrate_obs | cens(cens) ~ s(x)), ...)

# ---- 4c. Multicollinearity ----
# Pre-fit, on the design matrix (estimator-agnostic, your usual VIF):
# car::vif(lm(nitrate_obs ~ pred1 + pred2 + pred3, data = dat))
# Post-fit, see the correlated posteriors directly (a tight diagonal ridge):
# mcmc_plot(fit, type = "pairs", variable = c("b_pred1", "b_pred2"))

# =============================================================
# 5. INFLUENCE & OUTLIERS  (LOO cross-validation)
#    Pareto k is the Bayesian analog of leverage / Cook's distance.
# =============================================================

loo_fit <- loo(fit)
print(loo_fit)   # read the Pareto k diagnostic table
plot(loo_fit)    # k per observation; flag anything with k > 0.7

# Which observations (if any) are too influential to trust?
which(loo_fit$diagnostics$pareto_k > 0.7)

# =============================================================
# 6. PARAMETER RECOVERY  (only valid because this is SIMULATED data)
#    You know the truth, so confirm the model returns it.
#    From your simulation:
#      Control meanlog  = -2.5  -> Intercept
#      Vegetated meanlog = -3.2 -> Intercept + b_vegVegetated, so diff = -0.7
#      sdlog            =  0.7  -> sigma
# =============================================================

post  <- as_draws_df(fit)
truth <- c(b_Intercept = -2.5, b_vegVegetated = -0.7, sigma = 0.7)

recovery <- tibble(
  parameter = names(truth),
  truth     = truth,
  estimate  = c(mean(post$b_Intercept),
                mean(post$b_vegVegetated),
                mean(post$sigma)),
  q2.5      = c(quantile(post$b_Intercept,    0.025),
                quantile(post$b_vegVegetated, 0.025),
                quantile(post$sigma,          0.025)),
  q97.5     = c(quantile(post$b_Intercept,    0.975),
                quantile(post$b_vegVegetated, 0.975),
                quantile(post$sigma,          0.975))
) %>%
  mutate(recovered = truth >= q2.5 & truth <= q97.5)

print(recovery)