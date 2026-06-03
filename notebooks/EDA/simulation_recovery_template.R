# =============================================================
# Parameter-recovery simulation template
#
# Purpose: before trusting the model on real data, confirm that your
# chosen structure + your actual sampling design can recover known
# parameters. You invent the truth, simulate data from it ON YOUR REAL
# DESIGN, fit the same model, and check the credible intervals capture
# the truth.
#
# Generative model (lognormal, log scale):
#   log(nox) ~ Normal(mu, sigma)
#   mu = b0 + b_site[site] + b_time * time_std + u_id
#   u_id ~ Normal(0, sd_id)
# then left-censor anything below the detection limit, exactly like the
# real data pipeline.
# =============================================================

library(brms)
library(dplyr)
library(posterior)

set.seed(2024)   # change this and rerun to confirm recovery is not a fluke

# ---------------------------------------------------------------
# A. CHOOSE THE TRUTH
#    Plausible values on the LOG scale (because lognormal). These are
#    the numbers the fitted model has to recover. site is a factor with
#    levels center_hill < dale_hollow < percy_priest, so center_hill is
#    the reference and its effect is fixed at 0.
# ---------------------------------------------------------------
true <- list(
  b0     = -2.0,                          # intercept: exp(-2) = 0.14 mg/L center
  b_site = c(center_hill  =  0.0,         # reference level
             dale_hollow  =  0.4,         # +0.4 on log scale vs center_hill
             percy_priest = -0.3),
  b_time =  0.15,                         # trend per 1 SD of time
  sd_id  =  0.5,                          # among-location sd (log scale)
  sigma  =  0.6                           # residual sd (log scale)
)

# ---------------------------------------------------------------
# B. BUILD THE SIMULATED DATA ON YOUR REAL DESIGN
#    Reuse the actual site / id / time / detection-limit structure from
#    mod_df. Only the response is invented.
# ---------------------------------------------------------------
sim_df <- mod_df %>%
  transmute(
    site,
    id,
    mdl,
    time_std = as.numeric(scale(decimal_date))   # center and scale the time var
  )

# one random intercept per location (id), drawn from N(0, sd_id)
ids  <- unique(sim_df$id)
u_id <- tibble(id = ids, u = rnorm(length(ids), 0, true$sd_id))

sim_df <- sim_df %>%
  left_join(u_id, by = "id") %>%
  mutate(
    # linear predictor on the log scale (= meanlog of the lognormal)
    mu = true$b0 +
         unname(true$b_site[as.character(site)]) +
         true$b_time * time_std +
         u,
    # draw the TRUE (uncensored) concentration
    nox_true = rlnorm(n(), meanlog = mu, sdlog = true$sigma),
    # apply the detection-limit censoring exactly like the real pipeline:
    # below the per-row mdl becomes a left-censored observation at the mdl
    measure_value = ifelse(nox_true < mdl, mdl, nox_true),
    cens          = ifelse(nox_true < mdl, "left", "none")
  )

# sanity check: the simulated censoring rate should be realistic for your
# data. If it is wildly off (e.g. 0 or 0.95), adjust b0 / sigma in the truth.
mean(sim_df$cens == "left")

# ---------------------------------------------------------------
# C. FIT THE SAME MODEL YOU INTEND TO USE ON REAL DATA
#    Use the priors you settled on via prior predictive simulation.
# ---------------------------------------------------------------
priors <- c(
  prior(normal(-2, 1),  class = "Intercept"),
  prior(normal(0, 0.5), class = "b"),
  prior(exponential(2), class = "sigma"),
  prior(exponential(2), class = "sd")
)

sim_fit <- brm(
  measure_value | cens(cens) ~ site + time_std + (1 | id),
  data    = sim_df,
  family  = lognormal(),
  prior   = priors,
  chains  = 4, iter = 4000, cores = 4,
  backend = "cmdstanr",
  seed    = 2024
)

# Always glance at the diagnostics on the simulated fit too. Recovery is
# only meaningful if the sampler converged here (Rhat 1.00, healthy ESS).
summary(sim_fit)

# ---------------------------------------------------------------
# D. CHECK RECOVERY
#    Did each 95% credible interval capture the known truth?
#    Parameter names follow brms conventions: factor contrasts are
#    b_site<level>, the random-effect sd is sd_id__Intercept, etc.
# ---------------------------------------------------------------
post <- as_draws_df(sim_fit)

recovery <- tibble::tribble(
  ~parameter,            ~truth,
  "b_Intercept",          unname(true$b0 + true$b_site["center_hill"]),
  "b_sitedale_hollow",    unname(true$b_site["dale_hollow"]  - true$b_site["center_hill"]),
  "b_sitepercy_priest",   unname(true$b_site["percy_priest"] - true$b_site["center_hill"]),
  "b_time_std",           true$b_time,
  "sd_id__Intercept",     true$sd_id,
  "sigma",                true$sigma
) %>%
  rowwise() %>%
  mutate(
    estimate  = mean(post[[parameter]]),
    q2.5      = quantile(post[[parameter]], 0.025),
    q97.5     = quantile(post[[parameter]], 0.975),
    recovered = truth >= q2.5 & truth <= q97.5
  ) %>%
  ungroup()

print(recovery)

# ---------------------------------------------------------------
# E. HOW TO READ THE RESULT
#   - All TRUE / intervals tight around truth: your design can estimate
#       this structure. Proceed to the real data with confidence.
#   - A parameter's interval is huge (covers truth but uselessly wide):
#       your design is underpowered for that effect. More data, or a
#       simpler structure for that piece.
#   - A parameter is systematically missed (truth outside the interval,
#       repeatedly across seeds): a real problem. Common causes are an
#       identifiability clash (e.g. a trend and the random effect soaking
#       up the same variation) or priors too tight to let the data speak.
#   - sd_id specifically failing: the among-location variance is not well
#       identified by your number of locations / replicates per location.
#
#   IMPORTANT: rerun the whole script under several set.seed() values (and
#   try smaller, more realistic effect sizes for b_site / b_time). One pass
#   recovering the truth can be luck; reliable recovery across seeds is the
#   evidence you want. This is a lightweight version of what McElreath later
#   calls simulation-based calibration.
# ---------------------------------------------------------------
