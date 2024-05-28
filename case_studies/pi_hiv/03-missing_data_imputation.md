Pulmonary Impairment in HIV-infected South African Adults
================
2024-05-28

Load the packages:

``` r
library(tidyverse)
library(here)
library(Hmisc)
library(mice)
library(patchwork)
library(gridExtra)
theme_set(theme_bw(12))
```

Import the datasets in the wide and long formats:

``` r
long <- read_rds(here("case_studies", "pi_hiv", "pi_hiv_data_long.rds"))
wide <- read_rds(here("case_studies", "pi_hiv", "pi_hiv_data_wide.rds"))
```

# Missing data imputation strategy

# Missing data imputation

# Checking imputed data

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
    ##  [1] gridExtra_2.3   patchwork_1.2.0 mice_3.16.0     Hmisc_5.1-2    
    ##  [5] here_1.0.1      lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1  
    ##  [9] dplyr_1.1.4     purrr_1.0.2     readr_2.1.5     tidyr_1.3.1    
    ## [13] tibble_3.2.1    ggplot2_3.5.1   tidyverse_2.0.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] gtable_0.3.5      shape_1.4.6.1     xfun_0.44         htmlwidgets_1.6.4
    ##  [5] lattice_0.22-6    tzdb_0.4.0        vctrs_0.6.5       tools_4.4.0      
    ##  [9] generics_0.1.3    fansi_1.0.6       pan_1.9           cluster_2.1.6    
    ## [13] jomo_2.7-6        pkgconfig_2.0.3   Matrix_1.7-0      data.table_1.15.4
    ## [17] checkmate_2.3.1   lifecycle_1.0.4   compiler_4.4.0    munsell_0.5.1    
    ## [21] codetools_0.2-20  htmltools_0.5.8.1 yaml_2.3.8        glmnet_4.1-8     
    ## [25] htmlTable_2.4.2   Formula_1.2-5     nloptr_2.0.3      pillar_1.9.0     
    ## [29] MASS_7.3-60.2     iterators_1.0.14  boot_1.3-30       mitml_0.4-5      
    ## [33] rpart_4.1.23      foreach_1.5.2     nlme_3.1-164      tidyselect_1.2.1 
    ## [37] digest_0.6.35     stringi_1.8.4     splines_4.4.0     rprojroot_2.0.4  
    ## [41] fastmap_1.2.0     grid_4.4.0        colorspace_2.1-0  cli_3.6.2        
    ## [45] magrittr_2.0.3    base64enc_0.1-3   survival_3.5-8    utf8_1.2.4       
    ## [49] broom_1.0.6       foreign_0.8-86    withr_3.0.0       scales_1.3.0     
    ## [53] backports_1.5.0   timechange_0.3.0  rmarkdown_2.27    lme4_1.1-35.3    
    ## [57] nnet_7.3-19       hms_1.1.3         evaluate_0.23     knitr_1.46       
    ## [61] rlang_1.1.3       Rcpp_1.0.12       glue_1.7.0        minqa_1.2.7      
    ## [65] rstudioapi_0.16.0 R6_2.5.1
