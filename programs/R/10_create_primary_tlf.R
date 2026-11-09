
# ============================================================
# DIA-GLY3 
# Table T14.2.1 - Primary Efficacy Analysis
# HbA1c Change from Baseline to Week 26
# ============================================================

# 1. Load data and model results
adsl <- read.csv("data/adam/adsl.csv")
adeff <- read.csv("data/adam/adeff.csv")

models <- readRDS(
  "outputs/diagnostics/primary_mi_models.rds"
)

results <- summary(
  models$pooled,
  conf.int = TRUE,
  conf.level = 0.95
)

trt <- results[
  results$term == "TRTGLY-301", 
]

stopifnot(nrow(trt) == 1)

# 2. Build Week 26 analysis dataset
week26 <- adeff[
  adeff$WEEK == 26,
  c("USUBJID", "HBA1C", "CHG_HBA1C")
]

analysis <- merge(
  adsl,
  week26,
  by = "USUBJID",
  all.x = TRUE
)

analysis <- analysis[
  analysis$FASFL == "Y",
]

analysis$TRT <- factor(
  analysis$TRT01P,
  levels = c("Placebo", "GLY-301")
)

# 3. Calculate observed summaries
summarize_arm <- function(d) {
  
  data.frame(
    N = nrow(d),
    Observed = sum(!is.na(d$HBA1C)),
    Missing = sum(is.na(d$HBA1C)),
    Baseline_Mean = mean(d$HBA1C_BL),
    Baseline_SD = sd(d$HBA1C_BL),
    Week26_Mean = mean(d$HBA1C, na.rm = TRUE),
    Week26_SD = sd(d$HBA1C, na.rm = TRUE),
    Change_Mean = mean(d$CHG_HBA1C, na.rm = TRUE),
    Change_SD = sd(d$CHG_HBA1C, na.rm = TRUE)
  )
}

placebo <- summarize_arm(
  analysis[analysis$TRT == "Placebo", ]
)

active <- summarize_arm(
  analysis[analysis$TRT == "GLY-301", ]
)

# 4. Format table
fmt <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_mean_sd <- function(mean, sd) {
  paste0(fmt(mean), " (", fmt(sd), ")")
}

table_data <- data.frame(
  Parameter = c(
    "Number of participants (FAS)",
    "Week 26 observed, n",
    "Week 26 missing, n",
    "Baseline HbA1c, mean (SD)",
    "Week 26 HbA1c, mean (SD)",
    "Change from baseline, mean (SD)"
  ),
  
  Placebo = c(
    placebo$N,
    placebo$Observed,
    placebo$Missing,
    fmt_mean_sd(
      placebo$Baseline_Mean,
      placebo$Baseline_SD
    ),
    fmt_mean_sd(
      placebo$Week26_Mean,
      placebo$Week26_SD
    ),
    fmt_mean_sd(
      placebo$Change_Mean,
      placebo$Change_SD
    )
  ),
  
  GLY301 = c(
    active$N,
    active$Observed,
    active$Missing,
    fmt_mean_sd(
      active$Baseline_Mean,
      active$Baseline_SD
    ),
    fmt_mean_sd(
      active$Week26_Mean,
      active$Week26_SD
    ),
    fmt_mean_sd(
      active$Change_Mean,
      active$Change_SD
    )
  ),
  
  stringsAsFactors = FALSE
)

# 5. Append adjusted treatment comparison
comparison <- data.frame(
  Parameter = c(
    "Adjusted treatment difference",
    "95% confidence interval",
    "P-value"
  ),
  
  Placebo = c("", "", ""),
  
  GLY301 = c(
    fmt(trt$estimate, 3),
    paste0(
      "(",
      fmt(trt$conf.low, 3),
      ", ",
      fmt(trt$conf.high, 3),
      ")"
    ),
    format.pval(
      trt$p.value,
      digits = 3,
      eps = 0.0001
    )
  ),
  
  stringsAsFactors = FALSE
)

table_data <- rbind(table_data, comparison)

# 6. Save the final table
dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  table_data,
  "outputs/tables/T14_2_1_primary_efficacy.csv",
  row.names = FALSE
)

# 7. Print to Console
cat("\n")
cat("================================================\n")
cat("TABLE T14.2.1\n")
cat("Change in HbA1c from Baseline to Week 26\n")
cat("Full Analysis Set\n")
cat("================================================\n\n")

print(table_data, row.names = FALSE, right = FALSE)

cat("\nNotes:\n")
cat("Observed summaries exclude missing Week 26 values.\n")
cat("Treatment difference: GLY-301 minus Placebo.\n")
cat("ANCOVA adjusted for baseline HbA1c and stratum.\n")
cat("Missing Week 26 outcomes imputed separately by arm.\n")
cat("50 imputations; PMM; Rubin's rules.\n")
cat("Negative treatment differences favor GLY-301.\n")
cat("All data are simulated.\n")

cat("\nTable T14.2.1 generated successfully.\n")

