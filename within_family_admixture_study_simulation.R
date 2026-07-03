library(fixest)

# Set seed for reproducibility
set.seed(42)

n_sim        <- 10000
ancestry_var <- 0.001291 # Within-family variance from paper
residual_var <- 1.424   # Within-family residual variance from paper
beta_true    <- 1.35     # Minimum effect size of interest
n            <- 4745     # Number of pairs

# we test 3 different implementations of sibship fixed effects: the implementation in the fixest package, the implementation of the demeaned regression (using one observation from each pair), and the sibling differences implementation
beta <- list(
  "fixest" = numeric(n_sim), 
  "demeaned" = numeric(n_sim), 
  "sib_diff" = numeric(n_sim)
)

p_values <- list(
  "fixest" = numeric(n_sim), 
  "demeaned" = numeric(n_sim), 
  "sib_diff" = numeric(n_sim)
)

for(i in 1:n_sim) {
  dev_ancestry <- rnorm(n, mean = 0, sd = sqrt(ancestry_var))
  dev_residual <- rnorm(n, mean = 0, sd = sqrt(residual_var))
  
  sib1_ancestries <- 0.5 + dev_ancestry
  sib2_ancestries <- 0.5 - dev_ancestry
  sib1_ancestry
  
  sib1_residuals <- dev_residual
  sib2_residuals <- -dev_residual
  
  sib1_iqs <- beta_true * sib1_ancestries + sib1_residuals 
  sib2_iqs <- beta_true * sib2_ancestries + sib2_residuals 
  
  # fixest model
  fixed_effects_df <- data.frame(
    sibship_id = rep(1:n, each = 2),
    iq        = c(sib1_iqs, sib2_iqs),
    ancestry  = c(sib1_ancestries, sib2_ancestries)
  )

  fixest_model <- feols(iq ~ ancestry | sibship_id, data = fixed_effects_df)
  
  beta$fixest[i] <- coeftable(fixest_model)["ancestry", "Estimate"]
  p_values$fixest[i] <- coeftable(fixest_model)["ancestry", "Pr(>|t|)"]

  # demeaned regression model
  demeaned_df <- data.frame(
    iq = sib1_iqs - (sib1_iqs + sib2_iqs) / 2, 
    ancestry = sib1_ancestries - (sib1_ancestries + sib2_ancestries) / 2 
  )

  demeaned_model <- lm(iq ~ ancestry, data = demeaned_df)

  beta$demeaned[i] <- coeftable(demeaned_model)["ancestry", "Estimate"]
  p_values$demeaned[i] <- coeftable(demeaned_model)["ancestry", "Pr(>|t|)"]

  # sibling differences model 
  sib_diff_df <- data.frame(
    iq = sib1_iqs - sib2_iqs,
    ancestry = sib1_ancestries - sib2_ancestries 
  )
  
  sib_diff_model <- lm(iq ~ ancestry, data = sib_diff_df)

  beta$sib_diff[i] <- coeftable(sib_diff_model)["ancestry", "Estimate"]
  p_values$sib_diff[i] <- coeftable(sib_diff_model)["ancestry", "Pr(>|t|)"]
}

print(paste("[Fixest] Average Estimate: ", mean(beta$fixest), " Empirical Power:", mean(p_values$fixest < 0.05)))
print(paste("[Demeaned Regression] Average Estimate: ", mean(beta$demeaned), " Empirical Power:", mean(p_values$demeaned < 0.05)))
print(paste("[Sibling Differences] Average Estimate: ", mean(beta$sib_diff), " Empirical Power:", mean(p_values$sib_diff < 0.05)))
