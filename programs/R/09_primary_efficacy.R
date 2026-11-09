

# ============================================================
# DIA-GLY3 
# Primary Efficacy Analysis
# Treatment-specific MI + ANCOVA + Rubin's Rules
# ============================================================

library(mice)

set.seed(2026)

# 1. Load datasets
adsl <- read.csv("data/adam/adsl.csv")
adeff <- read.csv("data/adam/adeff.csv")

stopifnot(
  !anyDuplicated(adsl$USUBJID),
  !anyDuplicated(adeff[c("USUBJID", "WEEK")])
)

# 2. Extract Week 26 endpoint
week26 <- adeff[
  adeff$WEEK == 26,
  c("USUBJID", "CHG_HBA1C")
]

names(week26)[2] <- "CHANGE"

analysis <- merge(
  adsl, week26,
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
  !anyNA(analysis$TRT),
  !anyNA(analysis$HBA1C_BL),
  !anyNA(analysis$STRATUM),
  sum(is.na(analysis$CHANGE)) == 96
)

cat("\n========== ANALYSIS POPULATION ==========\n")
print(table(analysis$TRT))
cat("Missing outcomes:", sum(is.na(analysis$CHANGE)), "\n")

# 3. Impute separately within treatment arms
# Include baseline HbA1c and randomization stratum.
# Use PMM, 50 imputations, 20 iterations, 5 donors.

m <- 50

impute_arm <- function(data, arm_seed) {
  
  imp_data <- data[
    , c("CHANGE", "HBA1C_BL", "STRATUM")
  ]
  
  methods <- c(
    CHANGE = "pmm",
    HBA1C_BL = "",
    STRATUM = ""
  )
  
  predictor <- make.predictorMatrix(imp_data)
  predictor[,] <- 0
  
  predictor["CHANGE", "HBA1C_BL"] <- 1
  predictor["CHANGE", "STRATUM"] <- 1
  
  mice(
    imp_data,
    m = m,
    maxit = 20,
    method = methods,
    predictorMatrix = predictor,
    donors = 5,
    seed = arm_seed,
    printFlag = FALSE
  )
}

placebo <- analysis[analysis$TRT == "Placebo", ]
active <- analysis[analysis$TRT == "GLY-301", ]

cat("\nImputing Placebo group...\n")
imp_placebo <- impute_arm(placebo, 2026)

cat("Imputing GLY-301 group...\n")
imp_active <- impute_arm(active, 2027)

# 4. Build 50 complete analysis datasets
completed <- vector("list", m)

for (i in seq_len(m)) {
  
  p <- complete(imp_placebo, action = i)
  a <- complete(imp_active, action = i)
  
  placebo_i <- placebo
  active_i <- active
  
  placebo_i$CHANGE <- p$CHANGE
  active_i$CHANGE <- a$CHANGE
  
  completed[[i]] <- rbind(placebo_i, active_i)
}

# 5. Fit ANCOVA to each completed dataset
fits <- lapply(
  completed,
  function(d) {
    lm(
      CHANGE ~ TRT + HBA1C_BL + STRATUM,
      data = d
    )
  }
)

# 6. Pool estimates using Rubin's rules
pooled <- pool(as.mira(fits))

results <- summary(
  pooled,
  conf.int = TRUE,
  conf.level = 0.95
)

cat("\n========== FINAL PRIMARY ANCOVA ==========\n")
print(results)

trt_result <- results[
  results$term == "TRTGLY-301",
]

stopifnot(nrow(trt_result) == 1)

# 7. Descriptive summaries
# Observed data only (not imputed)
summary_arm <- do.call(
  rbind,
  lapply(levels(analysis$TRT), function(arm) {
    
    d <- analysis[analysis$TRT == arm, ]
    
    data.frame(
      Treatment = arm,
      N = nrow(d),
      Observed = sum(!is.na(d$CHANGE)),
      Missing = sum(is.na(d$CHANGE)),
      Mean_Change_Observed = mean(
        d$CHANGE, na.rm = TRUE
      ),
      SD_Change_Observed = sd(
        d$CHANGE, na.rm = TRUE
      )
    )
  })
)

cat("\n========== OBSERVED DESCRIPTIVES ==========\n")
print(summary_arm, row.names = FALSE)

# 8. Save results and diagnostics
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
  results,
  "outputs/tables/T14_2_1_primary_ancova.csv",
  row.names = FALSE
)

write.csv(
  summary_arm,
  "outputs/tables/T14_2_1_arm_descriptives.csv",
  row.names = FALSE
)

saveRDS(
  list(
    placebo = imp_placebo,
    active = imp_active,
    pooled = pooled
  ),
  "outputs/diagnostics/primary_mi_models.rds"
)

# 9. Check imputation convergence
pdf(
  "outputs/diagnostics/primary_mi_traceplots.pdf",
  width = 10,
  height = 7
)

print(plot(imp_placebo, c("CHANGE")))
print(plot(imp_active, c("CHANGE")))

dev.off()

# 10. Final summary
cat("\n========== PRIMARY ENDPOINT SUMMARY ==========\n")

cat("Contrast: GLY-301 minus Placebo\n")
cat(
  "Adjusted difference:",
  round(trt_result$estimate, 3), "\n"
)

cat(
  "95% CI:",
  round(trt_result$`2.5 %`, 3),
  "to",
  round(trt_result$`97.5 %`, 3), "\n"
)

cat(
  "P-value:",
  signif(trt_result$p.value, 4), "\n"
)

cat("\nTreatment-specific MI analysis completed.\n")



file.exists("outputs/diagnostics/primary_mi_traceplots.pdf")

