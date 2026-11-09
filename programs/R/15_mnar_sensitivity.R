

# ============================================================
# DIA-GLY3 
# MNAR Delta-Adjustment Sensitivity Analysis
# Primary Endpoint: Week-26 HbA1c Change
#
# Delta is applied ONLY to originally missing CHANGE
# values in the GLY-301 arm.
#
# Primary model:
# CHANGE ~ TRT + HBA1C_BL + STRATUM
# ============================================================

library(mice)

# 1. Load original imputation models
mi <- readRDS(
  "outputs/diagnostics/primary_mi_models.rds"
)

stopifnot(
  inherits(mi$placebo, "mids"),
  inherits(mi$active, "mids"),
  mi$placebo$m == mi$active$m
)

M <- mi$active$m

cat("\nNumber of imputations:", M, "\n")

# 2. Identify originally missing outcomes
missing_active <- is.na(mi$active$data$CHANGE)
missing_placebo <- is.na(mi$placebo$data$CHANGE)

cat("\n========== ORIGINAL MISSINGNESS ==========\n")
cat("GLY-301 missing:", sum(missing_active), "\n")
cat("Placebo missing:", sum(missing_placebo), "\n")

stopifnot(
  M == 50,
  sum(missing_active) == 53,
  sum(missing_placebo) == 43
)

# 3. Delta scenarios

# Originally prespecified in SAP Section 5.4.1
sap_deltas <- c(
  0, 0.2, 0.4, 0.6, 0.8, 1.0
)

# Additional exploratory sensitivity scenarios
exploratory_deltas <- c(
  0.25, 0.50, 0.75, 1.50, 2.00
)

# Combine scenarios without duplicates
deltas <- sort(unique(c(
  sap_deltas,
  exploratory_deltas
)))

# 4. Prepare output directory
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

# 5. Fit ANCOVA and pool estimates for one delta
run_delta <- function(delta) {
  
  models <- vector("list", M)
  
  for (i in seq_len(M)) {
    
    # Original completed datasets
    active <- mice::complete(mi$active, i)
    placebo <- mice::complete(mi$placebo, i)
    
    # Apply delta only to originally missing
    # GLY-301 outcomes
    active$CHANGE[missing_active] <-
      active$CHANGE[missing_active] + delta
    
    # Treatment coding
    active$TRT <- "GLY-301"
    placebo$TRT <- "Placebo"
    
    # Combine arms
    dat <- rbind(placebo, active)
    
    dat$TRT <- factor(
      dat$TRT,
      levels = c("Placebo", "GLY-301")
    )
    
    dat$STRATUM <- factor(dat$STRATUM)
    
    # Primary ANCOVA model
    models[[i]] <- lm(
      CHANGE ~ TRT + HBA1C_BL + STRATUM,
      data = dat
    )
  }
  
  # Rubin pooling
  pooled <- mice::pool(
    mice::as.mira(models)
  )
  
  result <- summary(
    pooled,
    conf.int = TRUE,
    conf.level = 0.95
  )
  
  # Extract treatment coefficient
  trt_row <- result[
    grepl("^TRT", result$term),
  ]
  
  if (nrow(trt_row) != 1) {
    stop("Unable to identify treatment coefficient.")
  }
  
  data.frame(
    Delta = delta,
    Estimate = trt_row$estimate,
    SE = trt_row$std.error,
    Lower95 = trt_row$`2.5 %`,
    Upper95 = trt_row$`97.5 %`,
    P_value = trt_row$p.value
  )
}

# 6. Run all scenarios
cat("\n========== MNAR DELTA ANALYSIS ==========\n")

results <- do.call(
  rbind,
  lapply(deltas, run_delta)
)

results$Significant_0_05 <-
  results$P_value < 0.05

print(results, row.names = FALSE)

# 7. Validate delta = 0 against primary analysis
cat("\n========== DELTA ZERO VALIDATION ==========\n")

primary_estimate <- -0.55778880

difference <- abs(
  results$Estimate[results$Delta == 0] -
    primary_estimate
)

cat(
  "Difference from primary estimate:",
  difference,
  "\n"
)

if (difference > 0.001) {
  warning(
    "Delta = 0 does not reproduce the primary ",
    "ANCOVA estimate. Check model specification."
  )
}

# 8. Identify first tested loss of significance
not_significant <- results[
  results$P_value >= 0.05,
]

cat("\n========== TIPPING-POINT ASSESSMENT ==========\n")

if (nrow(not_significant) > 0) {
  cat(
    "First tested delta with p >= 0.05:",
    min(not_significant$Delta),
    "\n"
  )
} else {
  cat(
    "No loss of statistical significance ",
    "within the tested delta scenarios.\n"
  )
}

# 9. Save results
write.csv(
  results,
  "outputs/tables/T14_2_6_mnar_delta_sensitivity.csv",
  row.names = FALSE
)

saveRDS(
  results,
  "outputs/diagnostics/mnar_delta_results.rds"
)

cat("\nMNAR sensitivity analysis completed.\n")



# Refine the tipping-point interval
fine_deltas <- seq(1.00, 1.50, by = 0.01)

fine_results <- do.call(
  rbind,
  lapply(fine_deltas, run_delta)
)

first_loss <- fine_results[
  fine_results$P_value >= 0.05,
]

cat("\n========== REFINED TIPPING POINT ==========\n")

if (nrow(first_loss) > 0) {
  print(first_loss[1, ])
} else {
  cat("No loss of significance in refined grid.\n")
}

write.csv(
  fine_results,
  "outputs/tables/T14_2_6_mnar_refined_tipping.csv",
  row.names = FALSE
)
