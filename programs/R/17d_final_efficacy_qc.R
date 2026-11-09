
# ============================================================
# DIA-GLY3 | FINAL EFFICACY QUALITY-CONTROL AUDIT
# ============================================================

# 1. Identify the six efficacy result files
files <- c(
  Primary_HbA1c = "T14_2_1_primary_ancova.csv",
  HbA1c_Responder = "T14_2_2_logistic_results.csv",
  FPG = "T14_2_3_fpg_ancova.csv",
  Body_Weight = "T14_2_4_weight_ancova.csv",
  MMRM = "T14_2_5_mmrm_by_visit.csv",
  MNAR = "T14_2_6_mnar_refined_tipping.csv"
)

folder <- "outputs/tables"

# 2. Check that files exist
paths <- file.path(folder, files)

if (!all(file.exists(paths))) {
  stop(
    "Missing files: ",
    paste(
      names(files)[!file.exists(paths)],
      collapse = ", "
    )
  )
}

# 3. Load each table
results <- lapply(paths, read.csv, check.names = FALSE)
names(results) <- names(files)

# 4. Display column names and results
for (name in names(results)) {
  
  cat("\n========================================\n")
  cat("ANALYSIS:", name, "\n")
  cat("========================================\n")
  
  cat("\nCOLUMN NAMES:\n")
  print(names(results[[name]]))
  
  cat("\nTABLE CONTENT:\n")
  print(results[[name]], row.names = FALSE)
}

# 5. Check that expected output tables contain data
qc <- data.frame(
  Analysis = names(results),
  File_Exists = file.exists(paths),
  Has_Rows = vapply(
    results,
    function(x) nrow(x) > 0,
    logical(1)
  ),
  Has_Columns = vapply(
    results,
    function(x) ncol(x) > 0,
    logical(1)
  ),
  stringsAsFactors = FALSE
)

qc$Result <- ifelse(
  qc$File_Exists & qc$Has_Rows & qc$Has_Columns,
  "PASS",
  "FAIL"
)

cat("\n========== EFFICACY FILE QC ==========\n")
print(qc, row.names = FALSE)

cat("\n========== QC SUMMARY ==========\n")
print(table(qc$Result))

# 6. Save QC report
dir.create(
  "outputs/qc",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  qc,
  "outputs/qc/final_efficacy_file_qc.csv",
  row.names = FALSE
)

cat("\nEfficacy file QC report saved.\n")
