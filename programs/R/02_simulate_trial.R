
# ============================================================
# DIA-GLY3 | Phase III Simulated Diabetes Clinical Trial
# Script: 02_simulate_trial.R
# Step 1: Randomization and baseline characteristics
# All participants and treatments are fictional.
# ============================================================

set.seed(20261105)

n_total <- 478
n_sites <- 12

# Generate baseline participant characteristics
participants <- data.frame(
  USUBJID = sprintf("DIA%04d", 1:n_total),
  SITEID = sprintf("SITE%02d",
                   sample(1:n_sites, n_total, replace = TRUE)),
  AGE = sample(18:75, n_total, replace = TRUE),
  SEX = sample(c("Female", "Male"), n_total,
               replace = TRUE, prob = c(0.48, 0.52)),
  HEIGHT_CM = pmin(pmax(rnorm(n_total, 168, 10), 145), 195),
  BMI = pmin(pmax(rnorm(n_total, 31, 5), 20), 45),
  HBA1C_BL = pmin(pmax(rnorm(n_total, 8.7, 0.7),
                       7.5), 10.5)
)

# Derive baseline weight and fasting plasma glucose
participants$WEIGHT_BL <- participants$BMI *
  (participants$HEIGHT_CM / 100)^2

participants$FPG_BL <- pmax(
  90,
  28.7 * participants$HBA1C_BL - 46.7 +
    rnorm(n_total, 0, 25)
)

# Randomization strata
participants$STRATUM <- ifelse(
  participants$HBA1C_BL < 9,
  "HbA1c <9%",
  "HbA1c >=9%"
)

# Stratified permuted-block randomization
randomize_stratum <- function(n, block_sizes = c(4, 6)) {
  allocation <- character(0)
  
  while (length(allocation) < n) {
    block_n <- sample(block_sizes, 1)
    block <- sample(rep(c("GLY-301", "Placebo"),
                        each = block_n / 2))
    allocation <- c(allocation, block)
  }
  
  allocation[seq_len(n)]
}

participants$TRT <- NA_character_

for (s in unique(participants$STRATUM)) {
  idx <- which(participants$STRATUM == s)
  participants$TRT[idx] <- randomize_stratum(length(idx))
}

# Check allocation
print(table(participants$TRT))
print(table(participants$STRATUM, participants$TRT))

# Validate baseline data
stopifnot(
  nrow(participants) == 478,
  !anyDuplicated(participants$USUBJID),
  all(participants$AGE >= 18 & participants$AGE <= 75),
  all(participants$HBA1C_BL >= 7.5 &
        participants$HBA1C_BL <= 10.5),
  all(participants$BMI >= 20 & participants$BMI <= 45),
  all(!is.na(participants$TRT))
)

# Save baseline dataset
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

write.csv(
  participants,
  "data/raw/participants_baseline.csv",
  row.names = FALSE
)

cat("\nStep 1 completed successfully.\n")

# ============================================================
# Step 2: Simulate longitudinal efficacy measurements
# ============================================================

visits <- c(0, 4, 8, 12, 18, 26)

# Hypothetical mean HbA1c changes by visit
gly_effect <- c(0, -0.35, -0.60, -0.80, -1.00, -1.10)
pbo_effect <- c(0, -0.15, -0.30, -0.40, -0.50, -0.60)

# Participant-specific response variation
participants$RESPONSE_HBA1C <- rnorm(n_total, 0, 1.25)
participants$RESPONSE_FPG <- rnorm(n_total, 0, 18)
participants$RESPONSE_WEIGHT <- rnorm(n_total, 0, 1.5)

longitudinal <- do.call(
  rbind,
  lapply(visits, function(w) {
    
    dat <- participants
    
    idx <- match(w, visits)
    progress <- w / 26
    
    mean_hba1c_change <- ifelse(
      dat$TRT == "GLY-301",
      gly_effect[idx],
      pbo_effect[idx]
    )
    
    # HbA1c: baseline plus treatment trajectory
    dat$HBA1C <- if (w == 0) {
      dat$HBA1C_BL
    } else {
      dat$HBA1C_BL +
        mean_hba1c_change +
        progress * dat$RESPONSE_HBA1C +
        rnorm(n_total, 0, 0.25)
    }
    
    # FPG: correlated with the HbA1c response
    dat$FPG <- if (w == 0) {
      dat$FPG_BL
    } else {
      pmax(
        50,
        dat$FPG_BL +
          28.7 * (dat$HBA1C - dat$HBA1C_BL) +
          progress * dat$RESPONSE_FPG +
          rnorm(n_total, 0, 12)
      )
    }
    
    # Weight: no scheduled Week-4 measurement
    dat$WEIGHT <- if (w == 0) {
      dat$WEIGHT_BL
    } else if (w == 4) {
      rep(NA_real_, n_total)
    } else {
      weight_effect <- ifelse(
        dat$TRT == "GLY-301",
        -2.0 * progress,
        -0.5 * progress
      )
      
      dat$WEIGHT_BL +
        weight_effect +
        progress * dat$RESPONSE_WEIGHT +
        rnorm(n_total, 0, 0.4)
    }
    
    data.frame(
      USUBJID = dat$USUBJID,
      TRT = dat$TRT,
      WEEK = w,
      HBA1C = round(dat$HBA1C, 2),
      FPG = round(dat$FPG, 1),
      WEIGHT = round(dat$WEIGHT, 1)
    )
  })
)

# Validation checks
stopifnot(
  nrow(longitudinal) == 478 * 6,
  all(longitudinal$WEEK %in% visits),
  all(is.na(longitudinal$WEIGHT[longitudinal$WEEK == 4])),
  !anyNA(longitudinal$HBA1C),
  !anyNA(longitudinal$FPG)
)

# Descriptive HbA1c changes
baseline <- longitudinal[
  longitudinal$WEEK == 0,
  c("USUBJID", "HBA1C")
]
names(baseline)[2] <- "HBA1C_BASELINE"

longitudinal <- merge(
  longitudinal, baseline,
  by = "USUBJID",
  all.x = TRUE,
  sort = FALSE
)

longitudinal$HBA1C_CHANGE <-
  longitudinal$HBA1C - longitudinal$HBA1C_BASELINE

summary_hba1c <- aggregate(
  HBA1C_CHANGE ~ TRT + WEEK,
  data = longitudinal,
  FUN = mean
)

print(summary_hba1c)

write.csv(
  longitudinal,
  "data/raw/efficacy_longitudinal.csv",
  row.names = FALSE
)

cat("\nStep 2 completed successfully.\n")


# ============================================================
# Step 3: Rescue medication, discontinuation, missing data
# ============================================================

set.seed(20261106)

# Work with the complete Step 2 efficacy data
trial <- longitudinal[order(
  longitudinal$USUBJID, longitudinal$WEEK
), ]

ids <- participants$USUBJID
n <- length(ids)

# Participant-level events
events <- data.frame(
  USUBJID = ids,
  TRT = participants$TRT,
  RESCUE_WEEK = NA_real_,
  DISCONT_WEEK = NA_real_,
  LAST_DOSE_DAY = 183,
  SAFETY_FU_DAY = 213
)

# Simulate treatment discontinuation (~12%)
discontinue <- rbinom(n, 1, 0.12) == 1

for (i in which(discontinue)) {
  events$DISCONT_WEEK[i] <- sample(
    c(4, 8, 12, 18),
    size = 1,
    prob = c(0.15, 0.25, 0.35, 0.25)
  )
}

# Hypothetical rescue decisions based on prior FPG
# A high FPG result does not automatically cause rescue:
# confirmation/clinical assessment is represented by probability.

for (i in seq_len(n)) {
  id <- ids[i]
  rows <- which(trial$USUBJID == id)
  
  for (w in c(4, 8, 12, 18, 26)) {
    fpg <- trial$FPG[rows[trial$WEEK[rows] == w]]
    
    threshold <- if (w <= 8) {
      240
    } else if (w <= 16) {
      200
    } else {
      180
    }
    
    if (fpg > threshold && runif(1) < 0.65) {
      events$RESCUE_WEEK[i] <- w
      break
    }
  }
}

# Apply hypothetical rescue effect only to later visits
# Current-visit values are not retrospectively modified.
for (i in seq_len(n)) {
  rw <- events$RESCUE_WEEK[i]
  
  if (!is.na(rw)) {
    idx <- trial$USUBJID == ids[i] & trial$WEEK > rw
    
    trial$HBA1C[idx] <- trial$HBA1C[idx] - 0.25
    trial$FPG[idx] <- trial$FPG[idx] - 12
  }
}

# Treatment discontinuation dates and safety follow-up
for (i in which(!is.na(events$DISCONT_WEEK))) {
  events$LAST_DOSE_DAY[i] <-
    1 + 7 * events$DISCONT_WEEK[i]
  
  events$SAFETY_FU_DAY[i] <-
    events$LAST_DOSE_DAY[i] + 30
}

# Add participant event information to visit data
trial <- merge(
  trial,
  events[, c("USUBJID", "RESCUE_WEEK", "DISCONT_WEEK")],
  by = "USUBJID",
  all.x = TRUE,
  sort = FALSE
)

trial <- trial[order(trial$USUBJID, trial$WEEK), ]

trial$POST_RESCUE <- with(
  trial,
  !is.na(RESCUE_WEEK) & WEEK > RESCUE_WEEK
)

trial$POST_DISCONT <- with(
  trial,
  !is.na(DISCONT_WEEK) & WEEK > DISCONT_WEEK
)

# Save complete simulated values for simulation validation.
# These are latent simulation values, not analysis observations.
write.csv(
  trial,
  "data/raw/efficacy_complete_simulated.csv",
  row.names = FALSE
)

# Generate approximately 20% missing Week-26 HbA1c
# Missingness depends on observed baseline and event history.

baseline_info <- participants[
  match(ids, participants$USUBJID), ]

risk_score <- 0.4 * (
  baseline_info$HBA1C_BL - 8.7
) +
  0.8 * !is.na(events$DISCONT_WEEK) +
  0.4 * !is.na(events$RESCUE_WEEK)

weights <- exp(risk_score - max(risk_score))

# Exactly 96 of 478 missing for this simulation
missing_ids <- sample(
  ids,
  size = round(0.20 * n),
  replace = FALSE,
  prob = weights
)

trial$HBA1C_MISSING <- FALSE

week26_missing <- trial$WEEK == 26 &
  trial$USUBJID %in% missing_ids

trial$HBA1C[week26_missing] <- NA_real_
trial$HBA1C_MISSING[week26_missing] <- TRUE

# Additional intermediate-visit missing HbA1c (~5%)
intermediate <- which(trial$WEEK %in% c(4, 8, 12, 18))

intermediate_missing <- sample(
  intermediate,
  size = round(0.05 * length(intermediate))
)

trial$HBA1C[intermediate_missing] <- NA_real_
trial$HBA1C_MISSING[intermediate_missing] <- TRUE

# Apply the same missing-visit pattern to FPG and weight
# for scheduled measurements.
trial$FPG[trial$HBA1C_MISSING] <- NA_real_

weight_missing <- trial$HBA1C_MISSING &
  trial$WEEK %in% c(8, 12, 18, 26)

trial$WEIGHT[weight_missing] <- NA_real_

# Recalculate observed change from baseline
trial$HBA1C_CHANGE <-
  trial$HBA1C - trial$HBA1C_BASELINE

# Save event and observed efficacy datasets
write.csv(
  events,
  "data/raw/participant_events.csv",
  row.names = FALSE
)

write.csv(
  trial,
  "data/raw/efficacy_observed.csv",
  row.names = FALSE
)

# Validation
stopifnot(
  nrow(trial) == 2868,
  length(unique(trial$USUBJID)) == 478,
  sum(is.na(trial$HBA1C[trial$WEEK == 26])) == 96,
  all(!is.na(trial$HBA1C[trial$WEEK == 0])),
  all(is.na(trial$WEIGHT[trial$WEEK == 4])),
  all(events$SAFETY_FU_DAY ==
        events$LAST_DOSE_DAY + 30)
)

cat("\nStep 3 completed successfully.\n")

cat("\nParticipants initiating rescue:",
    sum(!is.na(events$RESCUE_WEEK)), "\n")

cat("Participants discontinuing treatment:",
    sum(!is.na(events$DISCONT_WEEK)), "\n")

cat("Missing Week-26 HbA1c:",
    sum(is.na(trial$HBA1C[trial$WEEK == 26])),
    "of 478\n")

cat("Week-26 missing percentage:",
    round(100 * mean(
      is.na(trial$HBA1C[trial$WEEK == 26])
    ), 1), "%\n")


# ============================================================
# Step 4: Treatment exposure and safety simulation
# ============================================================

set.seed(20261107)

# ------------------------------------------------------------
# 4A. Treatment exposure and adherence
# ------------------------------------------------------------

exposure <- events[, c(
  "USUBJID", "TRT", "LAST_DOSE_DAY", "SAFETY_FU_DAY"
)]

exposure$FIRST_DOSE_DAY <- 1L

exposure$DURATION_DAYS <-
  exposure$LAST_DOSE_DAY - exposure$FIRST_DOSE_DAY + 1L

# Simulate adherence percentages
exposure$ADHERENCE_PCT <- round(
  pmin(pmax(rnorm(n, 94, 8), 55), 110), 1
)

exposure$TABLETS_PRESCRIBED <- exposure$DURATION_DAYS

exposure$TABLETS_TAKEN <- round(
  exposure$TABLETS_PRESCRIBED *
    exposure$ADHERENCE_PCT / 100
)

# Derive adherence from tablet counts for consistency
exposure$ADHERENCE_PCT <- round(
  100 * exposure$TABLETS_TAKEN /
    exposure$TABLETS_PRESCRIBED, 1
)

exposure$ADHERENCE_CATEGORY <- ifelse(
  exposure$ADHERENCE_PCT < 80, "<80%",
  ifelse(exposure$ADHERENCE_PCT <= 100,
         "80-100%", ">100%")
)

exposure$SAFFL <- "Y"

# ------------------------------------------------------------
# 4B. Treatment-emergent adverse events
# ------------------------------------------------------------

# Hypothetical event probabilities for educational simulation
ae_terms <- c(
  "Nausea",
  "Diarrhea",
  "Headache",
  "Dizziness",
  "Nasopharyngitis",
  "Urinary tract infection"
)

ae_soc <- c(
  "Gastrointestinal disorders",
  "Gastrointestinal disorders",
  "Nervous system disorders",
  "Nervous system disorders",
  "Infections and infestations",
  "Infections and infestations"
)

ae <- list()
ae_counter <- 0L

for (i in seq_len(n)) {
  
  trt <- events$TRT[i]
  
  # Probability of at least one AE
  p_any <- if (trt == "GLY-301") 0.46 else 0.40
  
  if (runif(1) < p_any) {
    
    n_ae <- sample(
      1:3, size = 1,
      prob = c(0.75, 0.20, 0.05)
    )
    
    for (j in seq_len(n_ae)) {
      
      ae_counter <- ae_counter + 1L
      
      term_idx <- sample(seq_along(ae_terms), 1)
      
      # AE onset within the TEAE observation window
      onset_day <- sample(
        seq.int(
          exposure$FIRST_DOSE_DAY[i],
          exposure$SAFETY_FU_DAY[i]
        ), 1
      )
      
      severity <- sample(
        c("MILD", "MODERATE", "SEVERE"),
        1, prob = c(0.65, 0.30, 0.05)
      )
      
      serious <- runif(1) < 0.035
      
      if (serious) {
        severity <- sample(
          c("MODERATE", "SEVERE"),
          1, prob = c(0.35, 0.65)
        )
      }
      
      ae_duration <- sample(1:21, 1)
      
      ae[[ae_counter]] <- data.frame(
        USUBJID = ids[i],
        AESEQ = j,
        AETERM = ae_terms[term_idx],
        AESOC = ae_soc[term_idx],
        AESTDY = onset_day,
        AEENDY = onset_day + ae_duration,
        AESEV = severity,
        AESER = ifelse(serious, "Y", "N"),
        AEREL = sample(
          c("RELATED", "NOT RELATED"),
          1, prob = c(0.35, 0.65)
        ),
        AEACN = sample(
          c("NONE", "DOSE INTERRUPTED",
            "DRUG WITHDRAWN"),
          1, prob = c(0.88, 0.09, 0.03)
        ),
        stringsAsFactors = FALSE
      )
    }
  }
}

adverse_events <- if (length(ae) > 0) {
  do.call(rbind, ae)
} else {
  data.frame()
}

# Derive TEAE using actual participant exposure window
if (nrow(adverse_events) > 0) {
  
  adverse_events <- merge(
    adverse_events,
    exposure[, c(
      "USUBJID", "FIRST_DOSE_DAY", "SAFETY_FU_DAY"
    )],
    by = "USUBJID",
    all.x = TRUE,
    sort = FALSE
  )
  
  adverse_events$TRTEMFL <- ifelse(
    adverse_events$AESTDY >=
      adverse_events$FIRST_DOSE_DAY &
      adverse_events$AESTDY <=
      adverse_events$SAFETY_FU_DAY,
    "Y", "N"
  )
  
  adverse_events$FIRST_DOSE_DAY <- NULL
  adverse_events$SAFETY_FU_DAY <- NULL
}

# ------------------------------------------------------------
# 4C. Hypoglycemic episodes
# ------------------------------------------------------------

hypo <- list()
hypo_counter <- 0L

for (i in seq_len(n)) {
  
  p_hypo <- if (
    events$TRT[i] == "GLY-301"
  ) 0.12 else 0.08
  
  if (runif(1) < p_hypo) {
    
    n_ep <- sample(
      1:3, 1, prob = c(0.80, 0.15, 0.05)
    )
    
    for (j in seq_len(n_ep)) {
      
      hypo_counter <- hypo_counter + 1L
      
      level <- sample(
        c("LEVEL 1", "LEVEL 2", "LEVEL 3"),
        1, prob = c(0.70, 0.25, 0.05)
      )
      
      glucose <- switch(
        level,
        "LEVEL 1" = runif(1, 54, 69.9),
        "LEVEL 2" = runif(1, 35, 53.9),
        "LEVEL 3" = runif(1, 35, 90)
      )
      
      hypo[[hypo_counter]] <- data.frame(
        USUBJID = ids[i],
        HYPOSEQ = j,
        HYPOSTDY = sample(
          seq.int(1, events$SAFETY_FU_DAY[i]), 1
        ),
        HYPOLEVEL = level,
        GLUCOSE_MGDL = round(glucose, 1),
        EXTERNAL_ASSISTANCE = ifelse(
          level == "LEVEL 3", "Y", "N"
        ),
        stringsAsFactors = FALSE
      )
    }
  }
}

hypoglycemia <- if (length(hypo) > 0) {
  do.call(rbind, hypo)
} else {
  data.frame()
}

# ------------------------------------------------------------
# 4D. Safety follow-up contacts
# ------------------------------------------------------------

safety_followup <- data.frame(
  USUBJID = ids,
  LAST_DOSE_DAY = events$LAST_DOSE_DAY,
  PLANNED_FU_DAY = events$SAFETY_FU_DAY
)

# Actual follow-up within +/- 3 days
safety_followup$ACTUAL_FU_DAY <-
  safety_followup$PLANNED_FU_DAY +
  sample(-3:3, n, replace = TRUE)

safety_followup$CONTACT_COMPLETED <-
  ifelse(runif(n) < 0.96, "Y", "N")

safety_followup$ACTUAL_FU_DAY[
  safety_followup$CONTACT_COMPLETED == "N"
] <- NA_integer_

# ------------------------------------------------------------
# 4E. Save safety datasets
# ------------------------------------------------------------

write.csv(
  exposure,
  "data/raw/treatment_exposure.csv",
  row.names = FALSE
)

write.csv(
  adverse_events,
  "data/raw/adverse_events.csv",
  row.names = FALSE
)

write.csv(
  hypoglycemia,
  "data/raw/hypoglycemia.csv",
  row.names = FALSE
)

write.csv(
  safety_followup,
  "data/raw/safety_followup.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 4F. Validation checks
# ------------------------------------------------------------

stopifnot(
  nrow(exposure) == 478,
  all(exposure$DURATION_DAYS > 0),
  all(exposure$SAFFL == "Y"),
  all(exposure$SAFETY_FU_DAY ==
        exposure$LAST_DOSE_DAY + 30),
  all(exposure$ADHERENCE_PCT > 0),
  nrow(safety_followup) == 478
)

if (nrow(adverse_events) > 0) {
  stopifnot(
    all(adverse_events$TRTEMFL == "Y"),
    all(adverse_events$AESER %in% c("Y", "N"))
  )
}

if (nrow(hypoglycemia) > 0) {
  stopifnot(
    all(
      hypoglycemia$EXTERNAL_ASSISTANCE[
        hypoglycemia$HYPOLEVEL == "LEVEL 3"
      ] == "Y"
    )
  )
}

cat("\nStep 4 completed successfully.\n")
cat("Total adverse events:", nrow(adverse_events), "\n")
cat("Participants with AEs:",
    length(unique(adverse_events$USUBJID)), "\n")
cat("Serious adverse events:",
    sum(adverse_events$AESER == "Y"), "\n")
cat("Hypoglycemic episodes:",
    nrow(hypoglycemia), "\n")
cat("Completed safety follow-up contacts:",
    sum(safety_followup$CONTACT_COMPLETED == "Y"),
    "\n")


# ============================================================
# Step 5: Data quality and consistency review
# ============================================================

cat("\n========== DAY 7 QUALITY REVIEW ==========\n")

# 1. Rescue medication by treatment
cat("\n1. RESCUE MEDICATION BY TREATMENT\n")

rescue_table <- table(
  Treatment = events$TRT,
  Rescue = ifelse(
    is.na(events$RESCUE_WEEK), "No", "Yes"
  )
)

print(rescue_table)
print(round(prop.table(rescue_table, 1) * 100, 1))

# 2. FPG distribution at baseline
cat("\n2. BASELINE FPG DISTRIBUTION\n")

print(
  aggregate(
    FPG_BL ~ TRT,
    data = participants,
    FUN = function(x) {
      c(
        Mean = mean(x),
        Median = median(x),
        Q1 = unname(quantile(x, 0.25)),
        Q3 = unname(quantile(x, 0.75))
      )
    }
  )
)

# 3. AE-related treatment withdrawals
cat("\n3. AE-RELATED DRUG WITHDRAWALS\n")

withdrawal_ae <- adverse_events[
  adverse_events$AEACN == "DRUG WITHDRAWN",
]

withdrawal_ids <- unique(withdrawal_ae$USUBJID)

cat("Participants with drug-withdrawal AEs:",
    length(withdrawal_ids), "\n")

cat("Of those, without recorded discontinuation:",
    sum(
      is.na(events$DISCONT_WEEK[
        match(withdrawal_ids, events$USUBJID)
      ])
    ), "\n")

# 4. Week-26 missingness by treatment
cat("\n4. WEEK-26 MISSINGNESS BY TREATMENT\n")

week26 <- trial[trial$WEEK == 26, ]

missing_table <- table(
  Treatment = week26$TRT,
  Missing = is.na(week26$HBA1C)
)

print(missing_table)
print(round(prop.table(missing_table, 1) * 100, 1))

# 5. Week-26 missingness by discontinuation
cat("\n5. MISSINGNESS BY DISCONTINUATION\n")

print(
  table(
    Discontinued = !is.na(week26$DISCONT_WEEK),
    Missing = is.na(week26$HBA1C)
  )
)

# 6. Check treatment-policy follow-up
cat("\n6. OBSERVED WEEK-26 VALUES AFTER DISCONTINUATION\n")

discontinued_w26 <- week26[
  !is.na(week26$DISCONT_WEEK),
]

cat("Discontinued participants:",
    nrow(discontinued_w26), "\n")

cat("With observed Week-26 HbA1c:",
    sum(!is.na(discontinued_w26$HBA1C)), "\n")

cat("\n========== REVIEW COMPLETE ==========\n")


# ============================================================
# Step 6: Reconcile simulated AE actions with discontinuation
# ============================================================

# Identify AEs labeled as causing drug withdrawal
withdrawal_rows <- which(
  adverse_events$AEACN == "DRUG WITHDRAWN"
)

# Find the corresponding participant discontinuation dates
discont_week <- events$DISCONT_WEEK[
  match(
    adverse_events$USUBJID[withdrawal_rows],
    events$USUBJID
  )
]

# A withdrawal action without discontinuation is inconsistent
inconsistent_rows <- withdrawal_rows[is.na(discont_week)]

# Recode these fictional events as dose interruptions
adverse_events$AEACN[inconsistent_rows] <- "DOSE INTERRUPTED"

# Validate that every remaining drug withdrawal is associated
# with a documented treatment discontinuation
remaining_ids <- unique(
  adverse_events$USUBJID[
    adverse_events$AEACN == "DRUG WITHDRAWN"
  ]
)

stopifnot(
  all(
    !is.na(
      events$DISCONT_WEEK[
        match(remaining_ids, events$USUBJID)
      ]
    )
  )
)

# Overwrite the preliminary AE file with reconciled data
write.csv(
  adverse_events,
  "data/raw/adverse_events.csv",
  row.names = FALSE
)

cat("\nStep 6 completed successfully.\n")
cat("AE records reconciled:",
    length(inconsistent_rows), "\n")
cat("Unmatched drug withdrawals remaining:",
    sum(
      adverse_events$AEACN == "DRUG WITHDRAWN"
    ), "\n")

