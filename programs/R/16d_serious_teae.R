
# ============================================================
# DIA-GLY3 | SAFETY ANALYSIS
# Table T14.3.4 - Serious TEAEs
# ============================================================

adsl <- read.csv("data/adam/adsl.csv")
adae <- read.csv("data/adam/adae.csv")

# Safety population
safety <- subset(adsl, SAFFL == "Y")

# Serious treatment-emergent events
sae <- subset(
  adae,
  TRTEMFL == "Y" &
    SAFFL == "Y" &
    AESER == "Y"
)

# Validate population and treatment
stopifnot(
  !anyDuplicated(safety$USUBJID),
  all(sae$USUBJID %in% safety$USUBJID),
  !anyNA(sae[, c("AESOC", "AETERM", "AESEV")])
)

trts <- c("Placebo", "GLY-301")

# Use actual treatment from ADSL
sae$TRT01A <- safety$TRT01A[
  match(sae$USUBJID, safety$USUBJID)
]

N <- sapply(
  trts,
  function(t) sum(safety$TRT01A == t)
)

# Participant-level incidence
count_ids <- function(ids, trt) {
  n <- length(unique(ids))
  sprintf("%d (%.1f%%)", n, 100 * n / N[trt])
}

# Overall row
overall <- data.frame(
  Category = "Any serious TEAE",
  Placebo = count_ids(
    sae$USUBJID[sae$TRT01A == "Placebo"],
    "Placebo"
  ),
  GLY301 = count_ids(
    sae$USUBJID[sae$TRT01A == "GLY-301"],
    "GLY-301"
  )
)

# By system organ class and event term
groups <- unique(sae[, c("AESOC", "AETERM")])
groups <- groups[order(groups$AESOC, groups$AETERM), ]

detail <- do.call(
  rbind,
  lapply(seq_len(nrow(groups)), function(i) {
    
    x <- subset(
      sae,
      AESOC == groups$AESOC[i] &
        AETERM == groups$AETERM[i]
    )
    
    data.frame(
      Category = paste(
        groups$AESOC[i],
        "-",
        groups$AETERM[i]
      ),
      Placebo = count_ids(
        x$USUBJID[x$TRT01A == "Placebo"],
        "Placebo"
      ),
      GLY301 = count_ids(
        x$USUBJID[x$TRT01A == "GLY-301"],
        "GLY-301"
      )
    )
  })
)

results <- rbind(overall, detail)

# QC against previous table
stopifnot(
  length(unique(sae$USUBJID[
    sae$TRT01A == "Placebo"
  ])) == 2,
  length(unique(sae$USUBJID[
    sae$TRT01A == "GLY-301"
  ])) == 7
)

names(results)[2:3] <- c(
  "Placebo (N=239)",
  "GLY-301 (N=239)"
)

cat("\n========== TABLE T14.3.4 ==========\n")
print(results, row.names = FALSE, right = FALSE)

cat("\n========== SAE EVENT CHARACTERISTICS ==========\n")
print(
  table(
    Treatment = sae$TRT01A,
    Severity = sae$AESEV
  )
)

cat("\n========== SAE RELATEDNESS ==========\n")
print(
  table(
    Treatment = sae$TRT01A,
    Relatedness = sae$AEREL
  )
)

cat("\n========== SAE RECORD COUNTS ==========\n")
print(table(sae$TRT01A))

dir.create(
  "outputs/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  results,
  "outputs/tables/T14_3_4_serious_teae.csv",
  row.names = FALSE
)

cat("\nTable T14.3.4 saved successfully.\n")
