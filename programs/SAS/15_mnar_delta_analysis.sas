
/* DIA-GLY3: MNAR Delta Sensitivity Analysis */
/* Prespecified SAP scenarios: 0 to 1.0 */

data work.mnar_scenarios;
    set work.mnar_base;

    do DELTA = 1.00 to 1.50 by 0.01;

        CHANGE_MNAR = CHANGE;

        if TRT01P = "GLY-301"
           and ORIG_MISSING = 1 then
            CHANGE_MNAR = CHANGE + DELTA;

        output;
    end;
run;

/* Validate number of records per scenario */

title "MNAR Delta Scenario Validation";

proc sql;
    select DELTA,
           count(*) as Total_Records,
           sum(
               TRT01P = "GLY-301"
               and ORIG_MISSING = 1
           ) as Adjusted_Records
    from work.mnar_scenarios
    group by DELTA;
quit;

/* Fit ANCOVA separately for every imputation
   and every delta scenario */

proc sort data=work.mnar_scenarios;
    by DELTA _Imputation_;
run;

ods select none;
ods output ParameterEstimates=work.mnar_estimates;

proc glm data=work.mnar_scenarios;
    by DELTA _Imputation_;
    model CHANGE_MNAR =
        TRT_N HBA1C_BL STRATUM_N
        / solution;
quit;

ods select all;

/* Retain GLY-301 minus Placebo coefficients */

data work.mnar_trt_estimates;
    set work.mnar_estimates;
    where Parameter = "TRT_N";

    TRT_EFFECT = Estimate;
    TRT_SE = StdErr;

    keep DELTA _Imputation_ TRT_EFFECT TRT_SE;
run;

/* Pool treatment effects using Rubin's rules */

proc sort data=work.mnar_trt_estimates;
    by DELTA;
run;

ods output ParameterEstimates=work.mnar_pooled;

proc mianalyze data=work.mnar_trt_estimates;
    by DELTA;
    modeleffects TRT_EFFECT;
    stderr TRT_SE;
run;

/* Display pooled results */

title "MNAR Sensitivity - Pooled Treatment Effects";

proc print data=work.mnar_pooled noobs;
run;

title;
