Pulmonary Impairment in HIV-infected South African Adults
================
2024-05-28

The dataset of the case studies is publicly available
[here](https://datadryad.org/stash/dataset/doi:10.5061/dryad.st5rk).

Load the packages:

``` r
library(tidyverse)
library(here)
library(readxl)
```

Import the dataset:

``` r
raw <- read_xlsx(
  path = here("data", "pi_hiv", "pulm_impairment_HIV.xlsx")
)
```

# Data dictionary

The following can be used as the dictionary of the dataset:

- `ptid` is the HIV-infected adult identificative number. The dataset
  contains collected data for $749$ subjects.
- `visit` is the month at which the follow-up visit was performed.
  Visits where performed every $6$ months for $3$ years, i.e., the last
  $36$ planned visit was at Month-$36$.
- `ca_sex` is the sex identificative: `1` for males and `2` for females
- `ca_educ` is the level of education level, with increasing values
  denoting higher education level.
- `ca_evrsmk`: has the subject ever smoked? `1` if yes, `2` if no.
- `ca_smkdly`: not clear what it refers to.
- `ca_agesmk`: age at which the subject started to smoke.
- `ca_smkstop`: did the subject stop smoking? `1` for no, `2` for yes
- `ca_yrsstpsmk`: it seems to be the tobacco pack-years smoked, but the
  summary statistics for the baseline data do not match those in the
  publication.
- `ca_nmcignow`: it seems to be the number of cigarettes that the smoker
  was smoking at the enrollment. Though it likely indicates the number
  of smoked cigarettes per day, it is not $100 \%$ clear.
- `ca_typfuel`: exposure to biomass fuel. It is not clear what each
  value refers to. However, in the publication, the authors stated that
  nobody reported exposure to biomass fuels. Therefore, the variable can
  be ignored in the subsequent analyses.
- `ca_mine`: has the subject worked in mine? `1` for yes, `2` for no.
- `ca_hadtb`: has the subject ever had turberculosis? `1` for yes, `2`
  for no
- `ca_evrarv`: is the subject on antiretroviral therapy (ART)? `1` if
  yes, `2` if no.
- `ca_arv6m`: was the subject on ART from less than 6 months? `1` if
  yes, `2` if no.
- `ce_prefev1`: FEV1 at the visit measured in $L$
- `ce_prefvc`: FVC at the visit measures in $L$
- `crp`: C-reactive protein (CRP) at the visit measured in $mg/L$
- `cd4`: CD4 cell count at the visit measured in $cells/mm^{3}$
- `vl`: viral load at the visit measured in $copies/mL$
- `age`: age at the visit in years
- `bmi`: BMI at the visit measured in $kg/m^{2}$

# Data cleaning

The dataset is prepared for the analysis taking the following into
account:

- from the available information, it is not clear how the variables
  tobacco pack-years and second-hand smoking could be derived.
  Therefore, in the subsequent analyses, only the variable that captures
  whether the subject has ever smoked and is currently smoking will be
  used.

- a unique variable on ART is derived as in the publication.

- exposure to biomass fuels and working in mine will not be considered

- education level will not be considered

- for each subject, the dataset will be forced to have a row for each
  planned visit. Visits with no information for a variable will have
  missing values

- the viral loads has the detection limit (DL) at $49 copies/mL$. A
  variable will be added to flag which viral loads observation is below
  the DL.

- create a variable with the ratio $FEV1/FVC$

``` r
d_raw <- raw |> 
  mutate(
    across(c(ptid, visit, ca_educ), ~ as.integer(.x)),
    ca_sex = factor(
      if_else(ca_sex == 1, "male", "female"), 
      levels = c("male", "female")
    ), 
    ca_smk = factor(
      case_when(
        ca_evrsmk == 2 ~ "never",
        ca_evrsmk == 1 & ca_smkstop == 1 ~ "former",
        ca_evrsmk == 1 & ca_smkstop == 2 ~ "current",
      ),
      levels = c("never", "former", "current")
    ),
    ca_hadtb = factor(
      if_else(ca_hadtb == 1, "yes", "no"), levels = c("no", "yes")
    ),
    ca_art = factor(
      case_when(
        ca_evrarv == 2 ~ "never",
        ca_evrarv == 1 & ca_arv6m == 1 ~ "recent",
        ca_evrarv == 1 & ca_arv6m == 2 ~ "chronic"
      ),
      levels = c("never", "recent", "chronic")
    ),
    vl_dl = factor(
      if_else(vl == 49, "below LD", "above LD"),
      levels = c("above LD", "below LD")
    ),
    ce_prefev1_fvc = ce_prefev1/ce_prefvc
  ) |> 
  dplyr::select(
    ptid, 
    visit,
    age,
    ca_sex,
    ca_smk,
    ca_hadtb,
    bmi,
    ca_art,
    crp,
    cd4,
    vl, 
    vl_dl,
    ce_prefev1,
    ce_prefvc,
    ce_prefev1_fvc 
  )

# For each participant, there should be one row for each planned visit
d_miss <- d_raw |> 
  tidyr::complete(ptid, tidyr::nesting(visit)) |> 
  group_by(ptid) |> 
  mutate(
    age = if_else(is.na(age), mean(age, na.rm = TRUE), age),
    across(
      c(ca_sex, ca_smk, ca_hadtb, ca_art), 
      ~ if_else(
        is.na(.x),
        names(which.max(table(.x))),
        .x
      )
    )
  ) |> 
  ungroup() |> 
  mutate(
    ca_sex = factor(ca_sex, levels = c("male", "female")),
    ca_smk = factor(ca_smk, levels = c("never", "former", "current")),
    ca_hadtb = factor(ca_hadtb, levels = c("no", "yes")),
    ca_art = factor(ca_art, levels = c("never", "recent", "chronic"))
  )
```

Glimpse of the dataset:

``` r
glimpse(d_miss)
```

    ## Rows: 5,243
    ## Columns: 15
    ## $ ptid           <int> 170001, 170001, 170001, 170001, 170001, 170001, 170001,…
    ## $ visit          <int> 0, 6, 12, 18, 24, 30, 36, 0, 6, 12, 18, 24, 30, 36, 0, …
    ## $ age            <dbl> 30.57906, 30.57906, 30.57906, 30.57906, 30.57906, 30.57…
    ## $ ca_sex         <fct> female, female, female, female, female, female, female,…
    ## $ ca_smk         <fct> former, former, former, former, former, former, former,…
    ## $ ca_hadtb       <fct> no, no, no, no, no, no, no, no, no, no, no, no, no, no,…
    ## $ bmi            <dbl> 19.87910, 21.01505, 19.87910, 22.23214, 21.82644, 21.33…
    ## $ ca_art         <fct> never, never, never, never, never, never, never, recent…
    ## $ crp            <dbl> 3.00, NA, 4.00, 1.50, 0.90, 4.50, 1.00, 2.95, NA, 20.00…
    ## $ cd4            <dbl> 700.0, NA, 596.0, 665.0, 729.0, 636.0, 530.0, 86.5, NA,…
    ## $ vl             <dbl> 1692.0, NA, 3303.0, 1621.0, 1359.0, 3874.0, 4628.0, 250…
    ## $ vl_dl          <fct> above LD, NA, above LD, above LD, above LD, above LD, a…
    ## $ ce_prefev1     <dbl> 2.92, NA, 2.86, NA, 2.86, NA, 2.77, 3.25, NA, 3.06, NA,…
    ## $ ce_prefvc      <dbl> 3.49, NA, 3.48, NA, 3.44, NA, 3.33, 3.74, NA, 3.72, NA,…
    ## $ ce_prefev1_fvc <dbl> 0.8366762, NA, 0.8218390, NA, 0.8313953, NA, 0.8318318,…

Save the dataset into a rds:

``` r
write_rds(
  d_miss, here::here("case_studies", "pi_hiv", "pi_hiv_data.rds")
)
```

# Session Info

``` r
si <- sessionInfo()
print(si, locale = FALSE)
```

    ## R version 4.4.0 (2024-04-24 ucrt)
    ## Platform: x86_64-w64-mingw32/x64
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] readxl_1.4.3    here_1.0.1      lubridate_1.9.3 forcats_1.0.0  
    ##  [5] stringr_1.5.1   dplyr_1.1.4     purrr_1.0.2     readr_2.1.5    
    ##  [9] tidyr_1.3.1     tibble_3.2.1    ggplot2_3.5.1   tidyverse_2.0.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.5      compiler_4.4.0    tidyselect_1.2.1  scales_1.3.0     
    ##  [5] yaml_2.3.8        fastmap_1.2.0     R6_2.5.1          generics_0.1.3   
    ##  [9] knitr_1.46        rprojroot_2.0.4   munsell_0.5.1     pillar_1.9.0     
    ## [13] tzdb_0.4.0        rlang_1.1.3       utf8_1.2.4        stringi_1.8.4    
    ## [17] xfun_0.44         timechange_0.3.0  cli_3.6.2         withr_3.0.0      
    ## [21] magrittr_2.0.3    digest_0.6.35     grid_4.4.0        rstudioapi_0.16.0
    ## [25] hms_1.1.3         lifecycle_1.0.4   vctrs_0.6.5       evaluate_0.23    
    ## [29] glue_1.7.0        cellranger_1.1.0  fansi_1.0.6       colorspace_2.1-0 
    ## [33] rmarkdown_2.27    tools_4.4.0       pkgconfig_2.0.3   htmltools_0.5.8.1
