
# ============================================================
# DIA-GLY3 | SAFETY ANALYSIS
# Table T14.3.1 - Overall Summary of TEAEs
# ============================================================

# 1. Load ADaM datasets
adsl <- read.csv("data/adam/adsl.csv")
adae <- read.csv("data/adam/adae.csv")

# 2. Define safety population
safety <- subset(adsl, SAFFL == "Y")

# Check unique participants and treatment groups
stopifnot(
  !anyDuplicated(safety$USUBJID),
  all(c("GLY-301", "Placebo") %in% safety$TRT01A)
)

# 3. Restrict to treatment-emergent events
ae <- subset(
  adae,
  TRTEMFL == "Y" & SAFFL == "Y"
)

# Verify AE participants belong to safety population
stopifnot(
  all(ae$USUBJID %in% safety$USUBJID)
)

# Check consistency of actual treatment
treatment_check <- merge(
  ae[, c("USUBJID", "TRT01A")],
  safety[, c("USUBJID", "TRT01A")],
  by = "USUBJID",
  suffixes = c("_AE", "_ADSL")
)

stopifnot(
  all(
    treatment_check$TRT01A_AE ==
      treatment_check$TRT01A_ADSL
  )
)

# 4. Define safety categories
categories <- list(
  "Any TEAE" = rep(TRUE, nrow(ae)),
  "Treatment-related TEAE" = ae$AEREL == "RELATED",
  "Mild TEAE" = ae$AESEV == "MILD",
  "Moderate TEAE" = ae$AESEV == "MODERATE",
  "Severe TEAE" = ae$AESEV == "SEVERE",
  "Any serious TEAE" = ae$AESER == "Y",
  "TEAE leading to treatment discontinuation" =
    ae$AEDISFL == "Y",
  "TEAE leading to dose interruption" =
    ae$AEACN == "DOSE INTERRUPTED"
)

# 5. Calculate participant incidence
count_category <- function(flag, treatment) {
  
  event_ids <- unique(
    ae$USUBJID[
      !is.na(flag) &
        flag &
        ae$TRT01A == treatment
    ]
  )
  
  n <- sum(safety$TRT01A == treatment)
  
  count <- length(event_ids)
  
  sprintf(
    "%d (%.1f%%)",
    count,
    100 * count / n
  )
}

# 6. Build table
treatments <- c("Placebo", "GLY-301")

results <- data.frame(
  Category = names(categories),
  stringsAsFactors = FALSE
)

for (trt in treatments) {
  
  results[[trt]] <- vapply(
    categories,
    count_category,
    character(1),
    treatment = trt
  )
}

# 7. Add denominators to column headings
names(results)[2:3] <- c(
  paste0("Placebo (N=", sum(safety$TRT01A == "Placebo"), ")"),
  paste0("GLY-301 (N=", sum(safety$TRT01A == "GLY-301"), ")")
)

# 8. Print results
cat("\n========== TABLE T14.3.1 ==========\n")
print(results, row.names = FALSE, right = FALSE)

# 9. Save table
dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  results,
  "outputs/tables/T14_3_1_overall_teae.csv",
  row.names = FALSE
)

cat("\nTable T14.3.1 saved successfully.\n")
