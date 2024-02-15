# This script simulates an hypothetical non-randomized clinical trial
# that aims at comparing two treatments for coronary artery disease
# (CAD) in terms of reduction of major adverse cardiac or cerebrovascular 
# event, defined as death from any cause, myocardial infarction, stroke, 
# or repeat revascularization.

# References for the simulation are:
# - https://www.nejm.org/doi/full/10.1056/NEJMoa2112299
# - https://academic.oup.com/ejcts/article/55/6/1152/5248188

# The main goal of this is to have a somehow realistic dataset to
# be used when practicing with causal inference methods for
# time-to-event endpoints in observational/non-randomized clinical trials

# Load packages --------------------------------------------------------
library(tidyverse)

# 1) Simulate 12 clinical baseline covariates --------------------------
# Simulation from a multivariate normal to mimic a CAD population
# with correlated prognostic/baseline covariates. The following
# baseline clinical factors are considered:
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

corr_matrix <- matrix(
  data = c(
    1, 0.2, 0.25, 0.5, 0.6, 0.4, 0, 0.3, 0.4, 0.5, -0.6, 0.4,
    0.2, 1, 0.05, -0.2, -0.1, -0.3, 0, -0.1, -0.1, -0.2, 0, -0.1,
    0.25, 0.05, 1, 0.7, 0.8, 0.4, 0, 0.3, 0.3, 0.3, -0.2, 0.5,
    0.5, -0.2, 0.7, 1, 0.7, 0.45, 0, 0.3, 0.3, 0.3, -0.45, 0.8,
    
  ),
  nrow = 12,
  ncol = 12
)








