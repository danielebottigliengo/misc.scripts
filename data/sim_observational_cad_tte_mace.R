# This script simulates an hypothetical non-randomized clinical trial
# that aims at comparing two treatments for coronary artery disease
# (CAD) in terms of reduction of major adverse cardiac or cerebrovascular 
# event, defined as death from any cause, myocardial infarction, stroke, 
# or repeat revascularization.

# References for the simulation are:
# - https://www.nejm.org/doi/full/10.1056/NEJMoa2112299
# - https://academic.oup.com/ejcts/article/55/6/1152/5248188

# The main goal of this is to have realistic datasets to
# be used when practicing with causal inference methods for
# time-to-event endpoints in observational/non-randomized clinical trials

# We consider an hypothetical trial where a new treatment is
# compared with the standard one. The new treatment consists of a
# surgical procedure that allows a less invasive intervention on
# the patient than the standard. Clinicians are more likely to treat
# patients with poor prognosis with the standard procedure, with which
# they have established and successful experience. The aim of the
# trial is to establish non-inferiority of the new treatment in terms
# of 5-years MACE risk and average time-free-from MACE.

# Load packages --------------------------------------------------------
library(MASS)

# 1) Simulate 12 clinical baseline covariates --------------------------
# Simulation from a latent multivariate normal to mimic a CAD population
# with correlated prognostic/baseline covariates. Marginal covariate
# distribution are then obtained with the gaussian copula.

# The following baseline clinical factors are considered:
# 1) Age
# 2) Sex (female)
# 3) BMI
# 4) Diabetes
# 5) Hypertension
# 6) Smoking status (Non-smoker, previous/current smoker)
# 7) Family history of CAD
# 8) Previous MI
# 9) Previous PCI
# 10) History of TIA or CVA
# 11) LVEF
# 12) Dyslipidemia

# Function for simulating the datasets ---------------------------------
sim_data <- function(seed) {
  
  set.seed(seed)
  
  # First define the correlation matrix
  corr_matrix <- matrix(
    data = c(
      1, 0.2, 0.25, 0.5, 0.6, 0.4, 0, 0.3, 0.4, 0.5, -0.6, 0.4,
      0.2, 1, 0.05, -0.2, -0.1, -0.3, 0, -0.1, -0.1, -0.2, 0, -0.1,
      0.25, 0.05, 1, 0.7, 0.8, 0.4, 0, 0.3, 0.3, 0.3, -0.2, 0.5,
      0.5, -0.2, 0.7, 1, 0.7, 0.45, 0, 0.3, 0.3, 0.3, -0.45, 0.8,
      0.6, -0.1, 0.8, 0.7, 1, 0.6, 0.2, 0.55, 0.55, 0.55, -0.5, 0.65,
      0.4, -0.1, 0.4, 0.45, 0.6, 1, 0.2, 0.6, 0.4, 0.65, -0.25, 0.4,
      0, 0, 0, 0, 0.2, 0.2, 1, 0.25, 0.25, 0.25, -0.2, 0.3,
      0.3, -0.1, 0.3, 0.3, 0.55, 0.6, 0.25, 1, 0.6, 0.5, -0.5, 0.4,
      0.4, -0.1, 0.3, 0.3, 0.55, 0.4, 0.25, 0.6, 1, 0.5, -0.5, 0.4,
      0.5, -0.2, 0.3, 0.3, 0.55, 0.65, 0.25, 0.5, 0.5, 1, -0.3, 0.3,
      -0.6, 0, -0.2, -0.45, -0.5, -0.25, -0.2, -0.5, -0.5, -0.3, 1, -0.35,
      0.4, -0.1, 0.5, 0.8, 0.65, 0.4, 0.3, 0.4, 0.4, 0.3, -0.35, 1
    ),
    nrow = 12,
    ncol = 12
  )
  
  covs <- c(
    "age", "female", "bmi", "diabetes", "hypertension", "smoking",
    "family_cad", "previous_mi", "previous_pci", "history_tia_cva",
    "lvef", "dyslipidemia"
  )
  colnames(corr_matrix) <- covs
  rownames(corr_matrix) <- covs
  
  # Simulate baseline covariates from a standard multivariate normal
  sds <- rep(1, ncol(corr_matrix))
  mus <- rep(0, ncol(corr_matrix))
  
  # # From correlation to covariance following this:
  # # https://stats.stackexchange.com/questions/62850/obtaining-covariance-matrix-from-correlation-matrix
  # cov_matrix <- sweep(sweep(corr_matrix, 1, sds, "*"), 2, sds, "*")
  
  # In our case, since this is a multivariate standard normal, the
  # covariance matrix equals the correlation matrix
  cov_matrix <- corr_matrix
  
  n <- 1500L
  mv_bs <- mvrnorm(n = n, mu = mus, Sigma = cov_matrix)
  
  # Get the uniform distributions
  u_bs <- pnorm(mv_bs)
  
  # Get the marginal distributions for each covariante and store
  cov_bs <- u_bs
  cov_bs[, 1] <- qnorm(p = u_bs[, 1], mean = 65, sd = 8.2)
  cov_bs[, 2] <- qbinom(p = u_bs[, 2], size = 1, prob = 0.18)
  cov_bs[, 3] <- qnorm(p = u_bs[, 3], mean = 29, sd = 1.8)
  cov_bs[, 4] <- qbinom(p = u_bs[, 4], size = 1, prob = 0.28)
  cov_bs[, 5] <- qbinom(p = u_bs[, 5], size = 1, prob = 0.73)
  cov_bs[, 6] <- qbinom(p = u_bs[, 6], size = 1, prob = 0.60)
  cov_bs[, 7] <- qbinom(p = u_bs[, 7], size = 1, prob = 0.30)
  cov_bs[, 8] <- qbinom(p = u_bs[, 8], size = 1, prob = 0.33)
  cov_bs[, 9] <- qbinom(p = u_bs[, 9], size = 1, prob = 0.14)
  cov_bs[, 10] <- qbinom(p = u_bs[, 10], size = 1, prob = 0.07)
  cov_bs[, 11] <- qnorm(p = u_bs[, 11], mean = 55, sd = 10)
  cov_bs[, 12] <- qbinom(p = u_bs[, 10], size = 1, prob = 0.70)
  colnames(cov_bs) <- covs
  
  # 2) Simulate treatment assignment -------------------------------------
  # More severe patients are more likely to be assigned to the standard
  # treatment. Treatment assignment probabilities follows a logistic
  # regression model with main effects only
  gammas <- c(
    log(0.15), # Intercept fixed to have roughly 25% in the new treatment
    log(0.955),
    log(1.4), log(0.95), log(0.85), log(0.7), log(0.85),
    log(0.8), log(0.75), log(0.75), log(0.75), 
    log(1.1),
    log(0.9)
  )
  tr_mat <- cbind(rep(1, n), cov_bs)
  ps <- plogis(tr_mat %*% gammas)
  treat <- rbinom(n = n, size = 1, prob = ps)
  mean(treat)
  tr_cov_mat <- cbind(treat, cov_bs)
  
  # 3) Simulate the outcome ----------------------------------------------
  # Simulate the time-to-MACE from an accelerated failure time
  # model to allows non proportional hazards. We consider a situation
  # where the non-inferiority of the new treatment shows up later in the
  # follow-up time.
  
  # Four scenarios are considered, depending on the following two
  # factors:
  
  # 1) Homogeneous and heterogeneous treatment effect. For the second
  #    situation, we consider the new treatment as being more beneficial
  #    for patients with a better prognosis.
  
  # 2) Independent and dependent-on-baseline covariates censoring. For the
  #    latter, we allow patients with better prognosis and assigned to
  #    the experimental treatment to have earlier drop-out. This would
  #    mimic a situation where resources for data collection on the
  #    follow-up are limited and most of them are used for patients with
  #    poor prognoses and assigned to the new treatment.
  
  # Time-to-MACE are generated from a log-normal accelerated failure time
  # model. Time-to-censoring from a proportional hazards exponential model.
  
  # 3A) Homogenous treatment effect and indep censoring ------------------
  betas <- c(
    log(10), # Intercept fixed to have 20% of MACE in the control
    log(1.1),
    log(0.9), log(1.08), log(1.25), log(1.6), log(1.4),
    log(1.8), log(1.8), log(1.8), log(1.5), 
    log(0.95),
    log(1.5)
  )
  
  y_mat <- cbind(rep(1, n), cov_bs)
  
  eta <- y_mat %*% betas
  eff <- -0.05
  tte_0 <- exp(eta + rnorm(n = n))
  tte_1 <- exp(eta + eff + rnorm(n = n))
  tte <- ifelse(treat == 0, tte_0, tte_1)
  fup <- ifelse(tte <= 365.25 * 5, tte, 365.25 * 5)
  mace <- ifelse(tte <= 365.25 * 5, 1, 0)
  mean(mace)
  
  d1 <- as.data.frame(cbind(fup, mace, tr_cov_mat))
  d1$id <- seq_len(n)
  
  # # Check the model
  # summary(
  #   survival::survreg(
  #     survival::Surv(fup, mace) ~ treat + age + age^2 + age^3 + 
  #       female + bmi + diabetes + hypertension + smoking + family_cad + 
  #       previous_mi + history_tia_cva + lvef + lvef^2 + lvef^3 +
  #       dyslipidemia, data = d1, dist = "lognormal"
  #   )
  # )
  
  # 3B) Heterogeneous treatment effect and indep censoring ---------------
  betas <- c(
    log(4.9), # Intercept fixed to have 20% of MACE in the control
    log(1.1),
    log(0.9), log(1.08), log(1.25), log(1.6), log(1.4),
    log(1.8), log(1.8), log(1.8), log(1.5), 
    log(0.95),
    log(1.5)
  )
  
  y_mat <- cbind(rep(1, n), cov_bs)
  
  eta <- y_mat %*% betas
  eff <- -0.05 + log(0.96) * cov_bs[, "age"] + 
    log(0.91) * cov_bs[, "diabetes"] +
    log(0.88) * cov_bs[, "previous_pci"] +
    log(1.09) * cov_bs[, "lvef"]
  tte_0 <- exp(eta + rnorm(n = n))
  tte_1 <- exp(eta + eff + rnorm(n = n))
  tte <- ifelse(treat == 0, tte_0, tte_1)
  fup <- ifelse(tte <= 365.25 * 5, tte, 365.25 * 5)
  mace <- ifelse(tte <= 365.25 * 5, 1, 0)
  mean(mace)
  
  d2 <- as.data.frame(cbind(fup, mace, tr_cov_mat))
  d2$id <- seq_len(n)
  
  # 3C) Homogeneous treatment effect and dependent censoring -------------
  # Outcome model
  betas <- c(
    log(5), # Intercept fixed to have 20% of MACE in the control
    log(1.1),
    log(0.9), log(1.08), log(1.25), log(1.6), log(1.4),
    log(1.8), log(1.8), log(1.8), log(1.5), 
    log(0.95),
    log(1.5)
  )
  
  y_mat <- cbind(rep(1, n), cov_bs)
  
  eta <- y_mat %*% betas
  eff <- -0.05
  tte_0 <- exp(eta + rnorm(n = n))
  tte_1 <- exp(eta + eff + rnorm(n = n))
  
  # Censoring: a scenario where follow-up data on higher-risk patients
  # and patients treated with the new treatment are more likely to
  # be collected.
  cens_parm <- c(
    log(0.00006), # Intercept fixed to have 20% of MACE in the control
    log(1.05),
    log(1), log(1.04), log(1.3), log(1.25), log(1.8),
    log(1.5), log(1.5), log(1.3), log(1.7), 
    log(0.96),
    log(1.6)
  )
  
  eta_cens <- y_mat %*% cens_parm
  cens_eff <- log(0.85)
  cens_0 <- rexp(n = n, rate = exp(eta_cens))
  cens_1 <- rexp(n = n, rate = exp(eta_cens + cens_eff))
  
  # Observed follow-up
  tte <- ifelse(treat == 0, tte_0, tte_1)
  cens_time <- ifelse(treat == 0, cens_0, cens_1)
  tt <- pmin(tte, cens_time)
  fup <- ifelse(tt <= 365.25 * 5, tt, 365.25 * 5)
  mace <- ifelse(tt == tte, 1, 0)
  mean(mace)
  
  d3 <- as.data.frame(cbind(fup, mace, tr_cov_mat))
  d3$id <- seq_len(n)
  
  # 3D) Heterogeneous treatment effect and dep censoring -----------------
  betas <- c(
    log(5), # Intercept fixed to have 20% of MACE in the control
    log(1.1),
    log(0.9), log(1.08), log(1.25), log(1.6), log(1.4),
    log(1.8), log(1.8), log(1.8), log(1.5), 
    log(0.95),
    log(1.5)
  )
  
  y_mat <- cbind(rep(1, n), cov_bs)
  
  eta <- y_mat %*% betas
  eff <- -0.05 + log(0.96) * cov_bs[, "age"] + 
    log(0.91) * cov_bs[, "diabetes"] +
    log(0.88) * cov_bs[, "previous_pci"] +
    log(1.09) * cov_bs[, "lvef"]
  
  # Censoring: a scenario where follow-up data on higher-risk patients
  # and patients treated with the new treatment are more likely to
  # be collected.
  cens_parm <- c(
    log(0.00009), # Intercept fixed to have 20% of MACE in the control
    log(1.05),
    log(1), log(1.04), log(1.3), log(1.25), log(1.8),
    log(1.5), log(1.5), log(1.3), log(1.7), 
    log(0.96),
    log(1.6)
  )
  
  # Censoring: a scenario where follow-up data on higher-risk patients
  # and patients treated with the new treatment are more likely to
  # be collected.
  eta_cens <- y_mat %*% cens_parm
  cens_eff <- log(0.85)
  cens_0 <- rexp(n = n, rate = exp(eta_cens))
  cens_1 <- rexp(n = n, rate = exp(eta_cens + cens_eff))
  
  # Observed follow-up
  tte <- ifelse(treat == 0, tte_0, tte_1)
  cens_time <- ifelse(treat == 0, cens_0, cens_1)
  tt <- pmin(tte, cens_time)
  fup <- ifelse(tt <= 365.25 * 5, tt, 365.25 * 5)
  mace <- ifelse(tt == tte, 1, 0)
  mean(mace)
  
  d4 <- as.data.frame(cbind(fup, mace, tr_cov_mat))
  d4$id <- seq_len(n)
  
  # 4) Save datasets -----------------------------------------------------
  d <- list(
    "hom_indep" = d1,
    "het_indep" = d2,
    "hom_dep" = d3,
    "het_dep" = d4
  )
  
  
}

# Simulate each scenario 1000 times ------------------------------------
n_sims <- 1000L
seeds <- seq_len(n_sims)
sims <- lapply(seeds, sim_data)
cad_mace <- sims[[500]]

# 5) Save the datasets into rda ----------------------------------------
save(sims, file = "data/observational_cad_tte_mace_sims.rda")
save(cad_mace, file = "data/observational_cad_tte_mace_sample.rda")

