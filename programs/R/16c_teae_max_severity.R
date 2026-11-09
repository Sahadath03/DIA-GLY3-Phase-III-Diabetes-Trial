
# ============================================================
# DIA-GLY3 | SAFETY ANALYSIS
# Table T14.3.3 - TEAEs by Maximum Severity
# ============================================================

# 1. Load datasets
adsl <- read.csv("data/adam/adsl.csv")
adae <- read.csv("data/adam/adae.csv")

# 2. Define safety population
safety <- subset(adsl, SAFFL == "Y")

# 3. Select treatment-emergent adverse events
ae <- subset(
  adae,
  TRTEMFL == "Y" & SAFFL == "Y"
)

# 4. Validate severity coding
severity_levels <- c("MILD", "MODERATE", "SEVERE")

stopifnot(
  !anyDuplicated(safety$USUBJID),
  all(ae$USUBJID %in% safety$USUBJID),
  all(!is.na(ae$AESEV)),
  all(ae$AESEV %in% severity_levels)
)

# 5. Assign numeric severity scores
ae$SEVERITY_SCORE <- match(
  ae$AESEV,
  severity_levels
)

# 6. Determine maximum severity per participant
max_severity <- aggregate(
  SEVERITY_SCORE ~ USUBJID,
  data = ae,
  FUN = max
)

# 7. Add actual treatment from ADSL
max_severity <- merge(
  max_severity,
  safety[, c("USUBJID", "TRT01A")],
  by = "USUBJID"
)

max_severity$MAX_SEVERITY <- severity_levels[
  max_severity$SEVERITY_SCORE
]

# 8. Calculate participant counts
trts <- c("Placebo", "GLY-301")

N <- sapply(
  trts,
  function(t) sum(safety$TRT01A == t)
)

format_count <- function(count, treatment) {
  sprintf(
    "%d (%.1f%%)",
    count,
    100 * count / N[treatment]
  )
}

categories <- c(
  "MILD",
  "MODERATE",
  "SEVERE"
)

results <- data.frame(
  Maximum_Severity = categories,
  stringsAsFactors = FALSE
)

for (trt in trts) {
  
  counts <- vapply(
    categories,
    function(sev) {
      sum(
        max_severity$TRT01A == trt &
          max_severity$MAX_SEVERITY == sev
      )
    },
    integer(1)
  )
  
  results[[trt]] <- vapply(
    counts,
    format_count,
    character(1),
    treatment = trt
  )
}

# 9. Add total participants with TEAEs
total_row <- data.frame(
  Maximum_Severity = "ANY TEAE",
  Placebo = format_count(
    sum(max_severity$TRT01A == "Placebo"),
    "Placebo"
  ),
  `GLY-301` = format_count(
    sum(max_severity$TRT01A == "GLY-301"),
    "GLY-301"
  ),
  check.names = FALSE
)

names(total_row) <- names(results)

results <- rbind(total_row, results)

# 10. Quality control
for (trt in trts) {
  
  expected <- sum(
    max_severity$TRT01A == trt
  )
  
  observed <- sum(
    max_severity$TRT01A == trt &
      max_severity$MAX_SEVERITY %in% categories
  )
  
  stopifnot(expected == observed)
}

stopifnot(
  sum(max_severity$TRT01A == "Placebo") == 82,
  sum(max_severity$TRT01A == "GLY-301") == 111
)

# 11. Rename columns
names(results)[2:3] <- c(
  paste0("Placebo (N=", N["Placebo"], ")"),
  paste0("GLY-301 (N=", N["GLY-301"], ")")
)

# 12. Print results
cat("\n========== TABLE T14.3.3 ==========\n")

print(
  results,
  row.names = FALSE,
  right = FALSE
)

# 13. Save table
dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  results,
  "outputs/tables/T14_3_3_teae_max_severity.csv",
  row.names = FALSE
)

cat("\nTable T14.3.3 saved successfully.\n")
