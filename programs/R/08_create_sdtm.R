
# ============================================================
# DIA-GLY3
# Educational SDTM-style domains
# ============================================================

# 1. Load source datasets
required <- c(
  "data/raw/participants_baseline.csv",
  "data/raw/participant_events.csv",
  "data/raw/efficacy_observed.csv",
  "data/raw/treatment_exposure.csv",
  "data/raw/adverse_events.csv",
  "data/adam/adsl.csv"
)

stopifnot(all(file.exists(required)))

p <- read.csv(required[1])
ev <- read.csv(required[2])
eff <- read.csv(required[3])
exposure <- read.csv(required[4])
ae_raw <- read.csv(required[5])
adsl <- read.csv(required[6])

dir.create("data/sdtm", recursive = TRUE, showWarnings = FALSE)

studyid <- "DIA-GLY3"

save_domain <- function(data, domain) {
  write.csv(
    data,
    file.path("data/sdtm", paste0(tolower(domain), ".csv")),
    row.names = FALSE,
    na = ""
  )
  cat(domain, ":", nrow(data), "records\n")
}

# 2. DM: Demographics
dm <- data.frame(
  STUDYID = studyid,
  DOMAIN = "DM",
  USUBJID = p$USUBJID,
  SUBJID = p$USUBJID,
  SITEID = p$SITEID,
  AGE = p$AGE,
  AGEU = "YEARS",
  SEX = toupper(p$SEX),
  ARM = p$TRT,
  ACTARM = p$TRT,
  RFSTDTC = "DAY 1",
  stringsAsFactors = FALSE
)
save_domain(dm, "DM")

# 3. LB: HbA1c and fasting plasma glucose
# One record per participant, parameter, and visit.
make_lb <- function(code, name, value, unit) {
  data.frame(
    STUDYID = studyid,
    DOMAIN = "LB",
    USUBJID = eff$USUBJID,
    LBTESTCD = code,
    LBTEST = name,
    LBORRES = value,
    LBORRESU = unit,
    LBSTRESN = value,
    LBSTRESU = unit,
    VISIT = ifelse(
      eff$WEEK == 0,
      "Baseline",
      paste("Week", eff$WEEK)
    ),
    VISITNUM = eff$WEEK,
    stringsAsFactors = FALSE
  )
}

lb <- rbind(
  make_lb("HBA1C", "Hemoglobin A1c", eff$HBA1C, "%"),
  make_lb("GLUC", "Fasting Plasma Glucose", eff$FPG, "mg/dL")
)

lb <- lb[order(lb$USUBJID, lb$LBTESTCD, lb$VISITNUM), ]
lb$LBSEQ <- ave(
  seq_len(nrow(lb)),
  lb$USUBJID,
  FUN = seq_along
)

save_domain(lb, "LB")

# 4. AE: Adverse events
ae <- data.frame(
  STUDYID = studyid,
  DOMAIN = "AE",
  USUBJID = ae_raw$USUBJID,
  AESEQ = ae_raw$AESEQ,
  AETERM = ae_raw$AETERM,
  AEBODSYS = ae_raw$AESOC,
  AESEV = ae_raw$AESEV,
  AESER = ae_raw$AESER,
  AEREL = ae_raw$AEREL,
  AEACN = ae_raw$AEACN,
  AESTDY = ae_raw$AESTDY,
  AEENDY = ae_raw$AEENDY,
  stringsAsFactors = FALSE
)
save_domain(ae, "AE")

# 5. EX: Treatment exposure
ex <- data.frame(
  STUDYID = studyid,
  DOMAIN = "EX",
  USUBJID = exposure$USUBJID,
  EXSEQ = 1L,
  EXTRT = exposure$TRT,
  EXSTDY = exposure$FIRST_DOSE_DAY,
  EXENDY = exposure$LAST_DOSE_DAY,
  EXDUR = exposure$DURATION_DAYS,
  TABLETS_TAKEN = exposure$TABLETS_TAKEN,
  ADHERENCE_PCT = exposure$ADHERENCE_PCT,
  stringsAsFactors = FALSE
)
save_domain(ex, "EX")

# 6. VS: Body weight
# Week 4 is excluded because no weight assessment was scheduled.
weight <- eff[
  eff$WEEK %in% c(0, 8, 12, 18, 26),
  c("USUBJID", "WEEK", "WEIGHT")
]

vs <- data.frame(
  STUDYID = studyid,
  DOMAIN = "VS",
  USUBJID = weight$USUBJID,
  VSTESTCD = "WEIGHT",
  VSTEST = "Body Weight",
  VSSTRESN = weight$WEIGHT,
  VSSTRESU = "kg",
  VISIT = ifelse(
    weight$WEEK == 0,
    "Baseline",
    paste("Week", weight$WEEK)
  ),
  VISITNUM = weight$WEEK,
  stringsAsFactors = FALSE
)

vs <- vs[order(vs$USUBJID, vs$VISITNUM), ]
vs$VSSEQ <- ave(
  seq_len(nrow(vs)),
  vs$USUBJID,
  FUN = seq_along
)

save_domain(vs, "VS")

# 7. DS: Disposition
# These records describe treatment discontinuation,
# not necessarily withdrawal from the entire study.
ds <- data.frame(
  STUDYID = studyid,
  DOMAIN = "DS",
  USUBJID = ev$USUBJID,
  DSSEQ = 1L,
  DSCAT = "DISPOSITION EVENT",
  DSSCAT = "STUDY TREATMENT",
  DSTERM = ifelse(
    is.na(ev$DISCONT_WEEK),
    "COMPLETED PLANNED TREATMENT",
    "DISCONTINUED STUDY TREATMENT"
  ),
  DSSTDY = ev$LAST_DOSE_DAY,
  stringsAsFactors = FALSE
)

save_domain(ds, "DS")

# 8. CM: Rescue medication
# Drug name and dose are not simulated.
# Do not fabricate them.
rescued <- ev[!is.na(ev$RESCUE_WEEK), ]

cm <- data.frame(
  STUDYID = studyid,
  DOMAIN = "CM",
  USUBJID = rescued$USUBJID,
  CMSEQ = 1L,
  CMCAT = "RESCUE MEDICATION",
  CMTRT = "UNSPECIFIED GLUCOSE-LOWERING RESCUE",
  CMSTDY = 1 + 7 * rescued$RESCUE_WEEK,
  RESCUE_WEEK = rescued$RESCUE_WEEK,
  stringsAsFactors = FALSE
)

save_domain(cm, "CM")

# 9. SV: Scheduled participant visits
# These represent scheduled efficacy visit records.
# Actual attendance dates were not simulated.
sv_source <- eff[
  , c("USUBJID", "WEEK")
]

sv <- data.frame(
  STUDYID = studyid,
  DOMAIN = "SV",
  USUBJID = sv_source$USUBJID,
  VISIT = ifelse(
    sv_source$WEEK == 0,
    "Baseline",
    paste("Week", sv_source$WEEK)
  ),
  VISITNUM = sv_source$WEEK,
  SVSTDTC = NA_character_,
  SVENDTC = NA_character_,
  stringsAsFactors = FALSE
)

sv <- sv[order(sv$USUBJID, sv$VISITNUM), ]
save_domain(sv, "SV")

# 10. Cross-domain validation
domains <- list(
  DM = dm, LB = lb, AE = ae, EX = ex,
  VS = vs, DS = ds, CM = cm, SV = sv
)

checks <- data.frame(
  Domain = names(domains),
  Records = vapply(domains, nrow, integer(1)),
  IDs_Valid = vapply(
    domains,
    function(d) all(d$USUBJID %in% dm$USUBJID),
    logical(1)
  ),
  stringsAsFactors = FALSE
)

stopifnot(
  nrow(dm) == 478,
  !anyDuplicated(dm$USUBJID),
  all(checks$IDs_Valid),
  nrow(lb) == 5736,
  nrow(vs) == 478 * 5,
  nrow(sv) == 478 * 6,
  nrow(cm) == sum(!is.na(ev$RESCUE_WEEK)),
  nrow(ae) == nrow(ae_raw),
  nrow(ex) == 478
)

cat("\n========== SDTM-STYLE VALIDATION ==========\n")
print(checks, row.names = FALSE)
cat("\nAll initial domain checks passed.\n")

write.csv(
  checks,
  "docs/validation/sdtm_domain_validation.csv",
  row.names = FALSE
)

