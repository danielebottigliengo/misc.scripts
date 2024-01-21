library(survival)

# KM fit ---------------------------------------------------------------
km_fit <- survfit(Surv(time, status) ~ trt, data = veteran)

# 1) RMST with trapezoidal ---------------------------------------------
my_rmst <- function(survfit_object, time) {

  rmst <- rep(NA_real_, length(survfit_object$strata))
  names(rmst) <- names(survfit_object$strata)

  for(i in seq_len(length(survfit_object$strata))) {

    # Get the number of observations for the group
    n_obs_pr <- ifelse(i == 1, 0, unname(survfit_object$strata[i - 1]))
    n_obs <- unname(survfit_object$strata[i])

    # Define indexes for the group
    idx <- seq(from = n_obs_pr + 1, to = n_obs_pr + n_obs, by = 1)

    # Get times and survival probabilities
    times <- survfit_object$time[idx]
    survs <- survfit_object$surv[idx]

    # Get all observations up to the define follow-up time
    time_fup <- times[times <= time]
    survs_fup <- c(1, survs[seq_len(length.out = length(time_fup))])

    # Compute RMST with trapezoidal rule
    diff_time_fup <- diff(c(0, time_fup))
    # diff_time_fup <- diff(time_fup)

    rmst_trp <- sum(diff_time_fup * survs_fup[-length(survs_fup)])

    rmst[[i]] <- rmst_trp

  }

  return(rmst)

}

# Comparison with survival package -------------------------------------
tt <- 100

print(km_fit, rmean = tt)
my_rmst(survfit_object = km_fit, time = tt)


