# History

## 1st May, 2024

* Add the folder `pi_hiv` with the raw-data (`.xlsx` format) of the
  pulmonary impairment in HIV-infected South African adults case study.

## 6th April, 2024

* `sim_observational_cad_tte_mace.R` and related files have been
  grouped into the `sim_observational_cad_tte_mace` folder.
  
* The folder `ccasanet` contains simulated datasets from the
  [CCASAnet cohort](https://www.ccasanet.org/). The simulated 
  datasets are available [here](https://biostat.app.vumc.org/wiki/Main/ArchivedAnalyses). More
  info on the simulated datasets can be found in [Shepherd et al. (2017)](https://academic.oup.com/aje/article/186/4/387/3813218?login=false).

## 25th March, 2024

* `sim_observational_cad_tte_mace.R`: script to simulate an
  hypothetical observational clinical study that aims at comparing
  two treatments for the treatment of coronary artery disease (CAD)
  patients. The time-to-first MACE is the endpoint of interest. Four
  scenarios are simulated. Overall, 1000 datasets are simulated for 
  all scenarios. Moreover, a sample dataset to be used as reference has
  been selected for all scenarios. The following four scenarios are
  considered:
  
  1. Homogeneous treatment effect and independent censoring
  2. Heterogeneous treatment effect and independent censoring
  3. Homogeneous treatment effect and covariate-dependent censoring
  4. Heterogeneous treatment effect and covariate-dependent censoring
  
 
