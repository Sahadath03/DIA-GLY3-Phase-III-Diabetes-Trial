
/* ============================================================
   DIA-GLY3
   Step 42C: FPG Treatment-Specific Multiple Imputation
   50 imputations, PMM with 5 donors
   ============================================================ */

/* Prepare analysis variables */
data work.fpg_mi_input;
    set work.fpg_analysis;

    keep USUBJID TRT01P TRT_N STRATUM_N BASE CHG;
    rename BASE = FPG_BL
           CHG  = CHANGE;
run;

/* Sort participants by treatment */
proc sort data=work.fpg_mi_input
          out=work.fpg_mi_sorted;
    by TRT01P;
run;

/* Perform MI separately within each treatment group */
ods select none;

proc mi data=work.fpg_mi_sorted
        out=work.fpg_mi_completed
        nimpute=50
        seed=2028
        noprint;

    by TRT01P;

    class STRATUM_N;

    fcs regpmm(CHANGE = FPG_BL STRATUM_N / k=5);

    var FPG_BL STRATUM_N CHANGE;
run;

ods select all;

/* Verify completed datasets */
proc sql;
    title "FPG Multiple Imputation Validation";

    select
        TRT01P,
        count(distinct _Imputation_) as Number_of_Imputations,
        count(*) as Total_Records,
        sum(missing(CHANGE)) as Remaining_Missing
    from work.fpg_mi_completed
    group by TRT01P;
quit;

title;



/* ============================================================
   DIA-GLY3
   Step 42D: FPG ANCOVA + Rubin's Rules
   ============================================================ */

/* Fit ANCOVA separately for each imputed dataset */
proc sort data=work.fpg_mi_completed
          out=work.fpg_mi_for_ancova;
    by _Imputation_;
run;

ods select none;
ods output ParameterEstimates=work.fpg_ancova_mi_estimates;

proc glm data=work.fpg_mi_for_ancova;
    by _Imputation_;
    model CHANGE = TRT_N FPG_BL STRATUM_N
        / solution;
run;
quit;

ods select all;

/* Retain treatment estimates and standard errors */
data work.fpg_trt_estimates;
    set work.fpg_ancova_mi_estimates;

    where Parameter = "TRT_N";

    TRT_EFFECT = Estimate;
    TRT_SE = StdErr;

    keep _Imputation_ TRT_EFFECT TRT_SE;
run;

/* Verify 50 estimates */
proc sql;
    title "FPG Treatment Estimates Available for Pooling";

    select
        count(*) as Number_of_Estimates,
        mean(TRT_EFFECT) as Mean_Effect,
        mean(TRT_SE) as Mean_Standard_Error
    from work.fpg_trt_estimates;
quit;

/* Pool estimates using Rubin's rules */
ods output ParameterEstimates=work.fpg_pooled_results;

proc mianalyze data=work.fpg_trt_estimates;
    modeleffects TRT_EFFECT;
    stderr TRT_SE;

    title "FPG Week-26 ANCOVA - Rubin Pooling";
run;

/* Display final pooled results */
proc print data=work.fpg_pooled_results noobs;
    title "Final FPG Adjusted Treatment Difference";
run;

title;
