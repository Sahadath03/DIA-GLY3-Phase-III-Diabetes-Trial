
# ============================================================
# DIA-GLY3 
# Key Secondary Endpoint 2
# FPG Change from Baseline to Week 26
# Treatment-specific MI + ANCOVA
# ============================================================

library(mice)

set.seed(2026)

# 1. Load datasets
adsl <- read.csv("data/adam/adsl.csv")
adeff <- read.csv("data/adam/adeff.csv")

# 2. Extract Week 26 FPG
week26 <- adeff[
  adeff$WEEK == 26,
  c("USUBJID", "FPG", "CHG_FPG")
]

names(week26)[3] <- "CHANGE"

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
  !anyNA(analysis$FPG_BL),
  !anyNA(analysis$STRATUM)
)

cat("\n========== FPG ANALYSIS POPULATION ==========\n")
print(table(analysis$TRT))
cat(
  "Missing Week 26 FPG:",
  sum(is.na(analysis$FPG)), "\n"
)

# 3. Observed summaries
summarize_arm <- function(d, arm) {
  data.frame(
    Treatment = arm,
    N = nrow(d),
    Observed = sum(!is.na(d$FPG)),
    Missing = sum(is.na(d$FPG)),
    Baseline_Mean = mean(d$FPG_BL),
    Baseline_SD = sd(d$FPG_BL),
    Week26_Mean = mean(d$FPG, na.rm = TRUE),
    Week26_SD = sd(d$FPG, na.rm = TRUE),
    Change_Mean = mean(d$CHANGE, na.rm = TRUE),
    Change_SD = sd(d$CHANGE, na.rm = TRUE)
  )
}

observed_summary <- rbind(
  summarize_arm(
    analysis[analysis$TRT == "Placebo", ],
    "Placebo"
  ),
  summarize_arm(
    analysis[analysis$TRT == "GLY-301", ],
    "GLY-301"
  )
)

cat("\n========== OBSERVED FPG SUMMARIES ==========\n")
print(observed_summary, row.names = FALSE)

# 4. Treatment-specific multiple imputation
m <- 50

impute_arm <- function(d, arm_seed) {
  
  mi_data <- d[
    , c("CHANGE", "FPG_BL", "STRATUM")
  ]
  
  methods <- c(
    CHANGE = "pmm",
    FPG_BL = "",
    STRATUM = ""
  )
  
  predictor <- make.predictorMatrix(mi_data)
  predictor[,] <- 0
  
  predictor["CHANGE", "FPG_BL"] <- 1
  predictor["CHANGE", "STRATUM"] <- 1
  
  mice(
    mi_data,
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

cat("\nImputing Placebo FPG outcomes...\n")
imp_placebo <- impute_arm(placebo, 2028)

cat("Imputing GLY-301 FPG outcomes...\n")
imp_active <- impute_arm(active, 2029)

# 5. Fit ANCOVA across completed datasets
fits <- vector("list", m)

for (i in seq_len(m)) {
  
  p <- complete(imp_placebo, action = i)
  a <- complete(imp_active, action = i)
  
  placebo_i <- placebo
  active_i <- active
  
  placebo_i$CHANGE <- p$CHANGE
  active_i$CHANGE <- a$CHANGE
  
  d <- rbind(placebo_i, active_i)
  
  fits[[i]] <- lm(
    CHANGE ~ TRT + FPG_BL + STRATUM,
    data = d
  )
}

# 6. Pool results
pooled <- pool(as.mira(fits))

results <- summary(
  pooled,
  conf.int = TRUE,
  conf.level = 0.95
)

cat("\n========== POOLED FPG ANCOVA ==========\n")
print(results)

trt <- results[
  results$term == "TRTGLY-301",
]

stopifnot(nrow(trt) == 1)

# 7. Save outputs
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
  "outputs/tables/T14_2_3_fpg_descriptives.csv",
  row.names = FALSE
)

write.csv(
  results,
  "outputs/tables/T14_2_3_fpg_ancova.csv",
  row.names = FALSE
)

saveRDS(
  list(
    placebo = imp_placebo,
    active = imp_active,
    pooled = pooled
  ),
  "outputs/diagnostics/fpg_mi_models.rds"
)

# 8. Generate convergence diagnostics
pdf(
  "outputs/diagnostics/fpg_mi_traceplots.pdf",
  width = 10,
  height = 7
)

print(plot(imp_placebo, c("CHANGE")))
print(plot(imp_active, c("CHANGE")))

dev.off()

# 9. Final summary
cat("\n========== FPG ENDPOINT SUMMARY ==========\n")

cat("Contrast: GLY-301 minus Placebo\n")

cat(
  "Adjusted difference:",
  round(trt$estimate, 3), "mg/dL\n"
)

cat(
  "95% CI:",
  round(trt$conf.low, 3),
  "to",
  round(trt$conf.high, 3),
  "mg/dL\n"
)

cat(
  "P-value:",
  signif(trt$p.value, 4), "\n"
)

cat("\nFPG analysis completed.\n")


browseURL(
  normalizePath(
    "outputs/diagnostics/fpg_mi_traceplots.pdf"
  )
)

models <- readRDS(
  "outputs/diagnostics/fpg_mi_models.rds"
)

models$placebo$m
models$active$m
