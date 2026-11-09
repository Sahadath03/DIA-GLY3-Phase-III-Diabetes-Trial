
# ============================================================
# DIA-GLY3 
# Safety Analysis - Dataset Inspection
# ============================================================

# 1. Load datasets
adsl <- read.csv("data/adam/adsl.csv")
adae <- read.csv("data/adam/adae.csv")

# 2. Dataset dimensions
cat("\n========== DATASET DIMENSIONS ==========\n")
cat("ADSL:", nrow(adsl), "participants\n")
cat("ADAE:", nrow(adae), "adverse-event records\n")

# 3. Variable names
cat("\n========== ADSL VARIABLES ==========\n")
print(names(adsl))

cat("\n========== ADAE VARIABLES ==========\n")
print(names(adae))

# 4. Safety population
cat("\n========== SAFETY POPULATION ==========\n")
print(table(adsl$SAFFL, useNA = "ifany"))

cat("\n========== ACTUAL TREATMENT ==========\n")
print(table(adsl$TRT01A, useNA = "ifany"))

# 5. Adverse-event sample
cat("\n========== ADAE SAMPLE ==========\n")
print(head(adae, 5))

# 6. Inspect categorical variables
cat("\n========== CATEGORICAL VARIABLES ==========\n")

candidate_vars <- c(
  "TRTEMFL", "AESER", "AESEV",
  "AEREL", "AEDECOD", "AEBODSYS"
)

for (v in candidate_vars) {
  if (v %in% names(adae)) {
    cat("\n", v, ":\n")
    print(table(adae[[v]], useNA = "ifany"))
  }
}

# 7. Duplicate event records
cat("\n========== EVENT ID CHECK ==========\n")

if ("AESEQ" %in% names(adae)) {
  cat(
    "Duplicate subject-event IDs:",
    sum(duplicated(adae[, c("USUBJID", "AESEQ")])),
    "\n"
  )
}

cat("\nSafety dataset inspection completed.\n")
