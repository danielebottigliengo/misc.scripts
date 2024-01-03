# Bootstrap Wilcoxon Signed Rank test for paired data ------------------
library(tidyverse)
library(boot)
library(MASS)
library(rms)
options(datadist = "dd")
n_cores <- parallel::detectCores() - 1
seed <- 280191
set.seed(seed)


# Simulate pre-post data -----------------------------------------------
n <- 100L
mus <- c(10, 15)
sds <- c(8, 12)
corr <- matrix(
  data = c(1, 0.7, 0.7, 1), nrow = 2L, ncol = 2L, byrow = TRUE
)
cov_mat <- diag(sds) %*% corr %*% diag(sds)

# Round the data to make it looks like a score from a test
pre_post <- round(mvrnorm(n = n, mu = mus, Sigma = cov_mat))

d <- data.frame(
  id = seq_len(n),
  # Bound pre and post scores to be between 0 and 30
  pre = ifelse(
    as.double(pre_post[, 1]) < 0, 0, ifelse(
      as.double(pre_post[, 1]) > 30, 30, as.double(pre_post[, 1])
    )
  ),
  post = ifelse(
    as.double(pre_post[, 2]) < 0, 0, ifelse(
      as.double(pre_post[, 2]) > 30, 30, as.double(pre_post[, 2])
    )
  )
)

# Wilcoxon signed rank test --------------------------------------------
wsr <- wilcox.test(
  x = d$pre, y = d$post,
  alternative = "two.side",
  paired = TRUE,
  exact = FALSE,
  correct = TRUE,
  conf.int = TRUE
)

wsr

# Bootstrap to get quantities of interest ------------------------------
wsr_function <- function(data, pre, post, indices) {

  df <- data[indices, ]

  # Compute the differences
  diff <- df[[post]] - df[[pre]]

  # Compute the probablity of post being higher than pre
  p_diff <- mean(diff > 0)

  # Compute quantiles of the post-pre difference
  q_diff <- quantile(diff, probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

  # Compute pseudo-median
  pseudo_median <- wilcox.test(
    x = df[["post"]], y = df[["pre"]],
    alternative = "two.side",
    paired = TRUE,
    exact = FALSE,
    correct = TRUE,
    conf.int = TRUE
  )$estimate

  # Values
  c(
    "P(Post > Pre)" = p_diff,
    "10th percentile Post-Pre" = q_diff[1],
    "25th percentile Post-Pre" = q_diff[2],
    "50th (median) percentile Post-Pre" = q_diff[3],
    "75th percentile Post-Pre" = q_diff[4],
    "90th percentile Post-Pre" = q_diff[5],
    "Pseudo-median" = pseudo_median
  )

}

# Boostrap the function

boot_wsr <- boot(
  d,
  wsr_function,
  R = 999,
  pre = "pre",
  post = "post",
  parallel = "snow",
  cl = n_cores
)

# Inference on summary statistics --------------------------------------
# Plot bootstrap distributions
boot_df <- map(
  seq_len(ncol(boot_wsr$t)),
  \(x) {

    tibble(
      stat = rep(names(boot_wsr$t0[x]), nrow(boot_wsr$t)),
      value = boot_wsr$t[, x]
    )


  }
) |>
  list_rbind()

boot_df |>
  ggplot(mapping = aes(x = value)) +
  geom_histogram(colour = "dodgerblue", alpha = 0.2) +
  facet_wrap(~ stat, scales = "free_x") +
  xlab(" ") +
  ylab(" ")


# Boostrap CI
boot_ci_stats <- map(
  seq_len(ncol(boot_wsr$t)),
  \(x) {

    boot_ci <- boot::boot.ci(
      boot_wsr,
      conf = 0.95,
      type = c("perc", "bca"),
      index = x
    )

    tibble::tibble(
      stat = names(boot_ci$t0),
      est_bca = glue::glue(
        "{round(boot_ci$t0, 2L)} [{round(boot_ci$bca[4], 2L)}; {round(boot_ci$bca[5], 2L)}]"
      ),
      est_perc = glue::glue(
        "{round(boot_ci$t0, 2L)} [{round(boot_ci$perc[4], 2L)}; {round(boot_ci$perc[5], 2L)}]"
      )

    )

  }
) |>
  list_rbind()

# P-value to test evidence of post being greater than control
prob <- mean(boot_wsr$t[, 1] > 0)
pval <- 2 * min(prob, 1 - prob)
pval

# Model-based inference with proportional odds model -------------------
# Reshape dataset
d_long <- d |>
  pivot_longer(
    cols = c(pre, post),
    values_to = "score",
    names_to = "time"
  ) |>
  mutate(time = factor(time, levels = c("pre", "post")))

# Fit PO model
dd <- datadist(d_long)
po_fit <- orm(
  score ~ time,
  data = d_long,
  family = logistic,
  x = TRUE,
  y = TRUE
)

boot_po <- bootcov(
  po_fit,
  cluster = d_long$id,
  B = 1000
)

boot_or_pre_post <- tibble(or = exp(boot_po$boot.Coef[, 2]))

boot_or_pre_post |>
  ggplot(mapping = aes(x = or)) +
  geom_density(
    fill = "firebrick", colour = "firebrick", alpha = 0.2
  ) +
  scale_x_continuous(trans = scales::log_trans()) +
  xlab("OR (Post vs Pre)") +
  ylab(" ")

mean(boot_or_pre_post > 1)


