

/* DIA-GLY3
   Derive MI responder endpoint
   Reuses existing 50 imputations */

/* Obtain actual observed Week-26 HbA1c */
data work.resp_observed;
    set work.adeff(
        keep=USUBJID AVISITN HBA1C
    );

    where AVISITN = 26;

    length HBA1C_OBS 8;

    if strip(HBA1C) not in ("", "NA") then
        HBA1C_OBS = input(strip(HBA1C), best32.);
    else HBA1C_OBS = .;

    keep USUBJID HBA1C_OBS;
run;

/* Sort data for matching by participant */
proc sort data=work.mi_completed
          out=work.mi_completed_sorted;
    by USUBJID;
run;

proc sort data=work.resp_observed;
    by USUBJID;
run;

/* Reconstruct Week-26 HbA1c and responder */
data work.mi_responders;
    merge work.mi_completed_sorted(in=a)
          work.resp_observed(in=b);

    by USUBJID;

    if a;

    HBA1C_MI = HBA1C_BL + CHANGE;

    /* Preserve actual observed values */
    if not missing(HBA1C_OBS) then
        HBA1C_MI = HBA1C_OBS;

    RESP7_MI = (HBA1C_MI < 7);

    /* Check observed response preservation */
    if not missing(HBA1C_OBS) then
        OBS_MISMATCH =
          (RESP7_MI ne (HBA1C_OBS < 7));
    else OBS_MISMATCH = 0;
run;

/* Validate all 50 completed datasets */
title "MI Responder Integrity Validation";

proc sql;
    select count(*) as Total_Records,
           count(distinct _Imputation_)
               as Number_of_Imputations,
           sum(missing(HBA1C_MI))
               as Missing_HbA1c,
           sum(missing(RESP7_MI))
               as Missing_Responders,
           sum(OBS_MISMATCH)
               as Observed_Mismatches
    from work.mi_responders;
quit;

/* Inspect responder counts in first imputation */
title "Responder Counts - Imputation 1";

proc freq data=work.mi_responders;
    where _Imputation_ = 1;
    tables TRT01P*RESP7_MI / norow nocol;
run;

title;



/* DIA-GLY3
   MI Logistic Regression */

/* Sort by imputation number */
proc sort data=work.mi_responders
          out=work.mi_responders_sorted;
    by _Imputation_;
run;

/* Fit logistic regression for each imputation */
ods output ParameterEstimates=
    work.responder_logit_estimates;

proc logistic data=work.mi_responders_sorted
              descending;
    by _Imputation_;

    model RESP7_MI =
        TRT_N HBA1C_BL STRATUM_N;
run;

/* Verify model estimates */
title "Logistic Regression Coefficients";

proc sql;
    select Variable,
           count(*) as Number_of_Estimates,
           mean(Estimate) as Mean_Estimate,
           mean(StdErr) as Mean_SE
    from work.responder_logit_estimates
    group by Variable;
quit;

title;



/* DIA-GLY3
    Pool responder logistic models */

/* Extract treatment coefficients */
data work.responder_trt_estimates;
    set work.responder_logit_estimates;

    where Variable = "TRT_N";

    LOG_OR = Estimate;
    LOG_OR_SE = StdErr;

    keep _Imputation_ LOG_OR LOG_OR_SE;
run;

/* Confirm 50 treatment estimates */
title "Responder Treatment Estimates";

proc sql;
    select count(*) as Number_of_Estimates,
           mean(LOG_OR) as Mean_Log_OR,
           mean(LOG_OR_SE) as Mean_SE
    from work.responder_trt_estimates;
quit;

/* Pool estimates using Rubin's rules */
ods output ParameterEstimates=
    work.responder_pooled;

title "HbA1c Responder - Rubin Pooling";

proc mianalyze data=work.responder_trt_estimates;
    modeleffects LOG_OR;
    stderr LOG_OR_SE;
run;

/* Convert pooled log odds ratio to odds ratio */
data work.responder_final;
    set work.responder_pooled;

    Odds_Ratio = exp(Estimate);
    OR_Lower = exp(LCLMean);
    OR_Upper = exp(UCLMean);

    keep Estimate StdErr Odds_Ratio
         OR_Lower OR_Upper Probt;
run;

title "Final Adjusted Responder Odds Ratio";

proc print data=work.responder_final noobs;
run;

title;
