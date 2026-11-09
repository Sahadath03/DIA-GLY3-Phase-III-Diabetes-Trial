
# ============================================================
# DIA-GLY3 
# Key Secondary Endpoint 1
# HbA1c < 7% at Week 26
# Multiple Imputation + Logistic Regression
# ============================================================

library(mice)

# 1. Load datasets and imputation models
adsl <- read.csv("data/adam/adsl.csv")
adeff <- read.csv("data/adam/adeff.csv")

models <- readRDS(
  "outputs/diagnostics/primary_mi_models.rds"
)

# 2. Prepare analysis population
week26 <- adeff[
  adeff$WEEK == 26,
  c("USUBJID", "HBA1C", "CHG_HBA1C")
]

analysis <- merge(
  adsl,
  week26,
  by = "USUBJID",
  all.x = TRUE,
  sort = FALSE
)

analysis <- analysis[analysis$FASFL == "Y", ]

analysis$TRT <- factor(
  analysis$TRT01P,
  levels = c("Placebo", "GLY-301")
)

analysis$STRATUM <- factor(
  ifelse(analysis$HBA1C_BL < 9, "<9%", ">=9%"),
  levels = c("<9%", ">=9%")
)

stopifnot(
  nrow(analysis) == 478,
  sum(is.na(analysis$HBA1C)) == 96,
  !anyNA(analysis$TRT),
  !anyNA(analysis$HBA1C_BL)
)

# 3. Observed responder summaries
analysis$RESP7 <- ifelse(
  is.na(analysis$HBA1C),
  NA_integer_,
  as.integer(analysis$HBA1C < 7)
)

observed_summary <- do.call(
  rbind,
  lapply(levels(analysis$TRT), function(arm) {
    
    d <- analysis[analysis$TRT == arm, ]
    
    n_obs <- sum(!is.na(d$RESP7))
    n_resp <- sum(d$RESP7 == 1, na.rm = TRUE)
    
    data.frame(
      Treatment = arm,
      Randomized = nrow(d),
      Observed = n_obs,
      Missing = sum(is.na(d$RESP7)),
      Responders = n_resp,
      Observed_Rate = n_resp / n_obs
    )
  })
)

cat("\n========== OBSERVED RESPONDER RATES ==========\n")
print(observed_summary, row.names = FALSE)

# 4. Reconstruct treatment-specific imputed datasets
# Match the same row ordering used in 09_primary_efficacy.R.
# The saved mice objects contain CHANGE, HBA1C_BL, STRATUM.

placebo <- analysis[analysis$TRT == "Placebo", ]
active  <- analysis[analysis$TRT == "GLY-301", ]

stopifnot(
  nrow(placebo) == nrow(models$placebo$data),
  nrow(active) == nrow(models$active$data),
  isTRUE(all.equal(
    placebo$HBA1C_BL,
    models$placebo$data$HBA1C_BL
  )),
  isTRUE(all.equal(
    active$HBA1C_BL,
    models$active$data$HBA1C_BL
  ))
)

m <- models$placebo$m
stopifnot(models$active$m == m)

completed <- vector("list", m)
fits <- vector("list", m)

for (i in seq_len(m)) {
  
  p <- complete(models$placebo, action = i)
  a <- complete(models$active, action = i)
  
  placebo_i <- placebo
  active_i <- active
  
  placebo_i$CHANGE <- p$CHANGE
  active_i$CHANGE <- a$CHANGE
  
  d <- rbind(placebo_i, active_i)
  
  # Derive HbA1c at Week 26 from baseline + imputed change
  d$HBA1C_MI <- d$HBA1C_BL + d$CHANGE
  
  # Retain the actual observed HbA1c for observed participants.
  # This avoids reclassifying a participant due to baseline rounding.
  observed <- !is.na(d$HBA1C)
  d$HBA1C_MI[observed] <- d$HBA1C[observed]
  
  d$RESP7_MI <- as.integer(d$HBA1C_MI < 7)
  
  stopifnot(
    !anyNA(d$RESP7_MI),
    all(d$RESP7_MI[observed] == d$RESP7[observed])
  )
  
  completed[[i]] <- d
  
  fits[[i]] <- glm(
    RESP7_MI ~ TRT + HBA1C_BL + STRATUM,
    family = binomial(),
    data = d
  )
}

# 5. Pool logistic regression coefficients
pooled <- pool(as.mira(fits))

logit_results <- summary(
  pooled,
  conf.int = TRUE,
  conf.level = 0.95
)

cat("\n========== POOLED LOGISTIC REGRESSION ==========\n")
print(logit_results)

trt <- logit_results[
  logit_results$term == "TRTGLY-301",
]

stopifnot(nrow(trt) == 1)

# Convert log odds ratio to odds ratio
OR <- exp(trt$estimate)
OR_low <- exp(trt$conf.low)
OR_high <- exp(trt$conf.high)

# 6. Average imputed responder proportions
rates <- do.call(
  rbind,
  lapply(seq_len(m), function(i) {
    
    d <- completed[[i]]
    
    data.frame(
      Imputation = i,
      Placebo = mean(
        d$RESP7_MI[d$TRT == "Placebo"]
      ),
      GLY301 = mean(
        d$RESP7_MI[d$TRT == "GLY-301"]
      )
    )
  })
)

mi_placebo <- mean(rates$Placebo)
mi_active <- mean(rates$GLY301)

# 7. Save analysis results
dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "outputs/diagnostics",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  observed_summary,
  "outputs/tables/T14_2_2_observed_responders.csv",
  row.names = FALSE
)

write.csv(
  logit_results,
  "outputs/tables/T14_2_2_logistic_results.csv",
  row.names = FALSE
)

write.csv(
  rates,
  "outputs/diagnostics/responder_imputed_rates.csv",
  row.names = FALSE
)

saveRDS(
  pooled,
  "outputs/diagnostics/responder_pooled_model.rds"
)

# 8. Print summary
cat("\n========== RESPONDER ANALYSIS SUMMARY ==========\n")

cat(
  "MI response rate - Placebo:",
  sprintf("%.1f%%", 100 * mi_placebo), "\n"
)

cat(
  "MI response rate - GLY-301:",
  sprintf("%.1f%%", 100 * mi_active), "\n"
)

cat(
  "Adjusted odds ratio:",
  round(OR, 3), "\n"
)

cat(
  "95% CI:",
  round(OR_low, 3),
  "to",
  round(OR_high, 3), "\n"
)

cat(
  "P-value:",
  signif(trt$p.value, 4), "\n"
)

cat("\nResponder analysis completed.\n")
