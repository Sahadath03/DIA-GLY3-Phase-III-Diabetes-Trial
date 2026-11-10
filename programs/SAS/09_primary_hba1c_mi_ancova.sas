
/* DIA-GLY3
   09 - Primary HbA1c MI ANCOVA
   Prepare imputation input */

/* Use the validated Week-26 dataset */

data work.mi_input;
    set work.hba1c_primary;

    /* Numeric randomization stratum */
    STRATUM_N = (STRATUM = "HbA1c >=9%");

    /* Numeric treatment indicator */
    TRT_N = (TRT01P = "GLY-301");

    /* Baseline HbA1c and change */
    HBA1C_BL = BASE;
    CHANGE = CHG;

    keep USUBJID TRT01P TRT_N
         STRATUM_N HBA1C_BL CHANGE;
run;

/* Check preparation before imputation */

title "Primary MI Input - Treatment and Stratum";

proc freq data=work.mi_input;
    tables TRT01P*STRATUM_N / missing;
run;

title "Primary MI Input - Missing Outcomes";

proc means data=work.mi_input n nmiss;
    class TRT01P;
    var HBA1C_BL CHANGE;
run;

title;



/* =====================================================
    Treatment-specific multiple imputation
   PMM with 50 imputations and 5 donors
   ===================================================== */

/* Sort by treatment for separate imputation */
proc sort data=work.mi_input
          out=work.mi_input_sorted;
    by TRT01P;
run;

/* Treatment-specific PMM */
proc mi data=work.mi_input_sorted
        out=work.mi_completed
        nimpute=50
        seed=2026;
    by TRT01P;

    class STRATUM_N;

    var HBA1C_BL STRATUM_N CHANGE;

    monotone regpmm(CHANGE =
                    HBA1C_BL STRATUM_N / k=5);
run;

/* Verify the number of completed datasets */
title "Multiple Imputation - Dataset Counts";

proc sql;
    select TRT01P,
           count(distinct _Imputation_)
               as Number_of_Imputations,
           count(*) as Total_Records,
           sum(missing(CHANGE))
               as Remaining_Missing
    from work.mi_completed
    group by TRT01P;
quit;

title;



/* Validate completed imputations */

/* Confirm that observed outcomes and
   original strata were preserved */

proc sql;
    create table work.mi_validation as
    select m.TRT01P,
           count(*) as Total_Records,
           sum(m.STRATUM_N ne o.STRATUM_N)
               as Stratum_Mismatches,
           sum(
               not missing(o.CHANGE) and
               abs(m.CHANGE-o.CHANGE)>1e-8
           ) as Observed_Changed,
           sum(missing(m.CHANGE))
               as Remaining_Missing
    from work.mi_completed as m
    inner join work.mi_input as o
      on m.USUBJID=o.USUBJID
    group by m.TRT01P;
quit;

title "MI Integrity Validation";
proc print data=work.mi_validation noobs;
run;
title;


/* =====================================================
   ANCOVA across 50 completed datasets
   ===================================================== */

/* Sort all records by imputation number */
proc sort data=work.mi_completed
          out=work.mi_for_ancova;
    by _Imputation_;
run;

/* Fit the original ANCOVA model */
ods output ParameterEstimates=work.ancova_estimates;

proc reg data=work.mi_for_ancova;
    by _Imputation_;

    model CHANGE =
          TRT_N
          HBA1C_BL
          STRATUM_N;

quit;

/* Check that each imputation produced
   one treatment coefficient */

title "ANCOVA Treatment Coefficients";

proc sql;
    select count(*) as Number_of_Models,
           mean(Estimate) as Mean_Treatment_Effect,
           min(Estimate) as Minimum_Effect,
           max(Estimate) as Maximum_Effect
    from work.ancova_estimates
    where Variable = "TRT_N";
quit;

title;



/* =====================================================
   Pool ANCOVA estimates
   Rubin's rules
   ===================================================== */

/* Extract the 50 treatment coefficients */
data work.trt_estimates;
    set work.ancova_estimates;

    where Variable = "TRT_N";

    TRT_EFFECT = Estimate;
    TRT_SE = StdErr;

    keep _Imputation_ TRT_EFFECT TRT_SE;
run;

/* Confirm that 50 coefficients exist */
title "Treatment Estimates Available for Pooling";

proc sql;
    select count(*) as Number_of_Estimates,
           mean(TRT_EFFECT) as Mean_Effect,
           mean(TRT_SE) as Mean_Standard_Error
    from work.trt_estimates;
quit;

/* Pool the treatment effects */
title "Primary HbA1c ANCOVA - Rubin Pooling";

proc mianalyze data=work.trt_estimates;
    modeleffects TRT_EFFECT;
    stderr TRT_SE;
run;

title;
