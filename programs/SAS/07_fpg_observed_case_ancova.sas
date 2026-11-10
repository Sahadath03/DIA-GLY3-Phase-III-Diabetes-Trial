
/* DIA-GLY3
   Step 41: FPG Week-26 Analysis Dataset
   Independent SAS validation
*/

title "DIA-GLY3: FPG Analysis Preparation";

/* Extract Week-26 FPG from ADLB */
proc sql;
    create table work.fpg_analysis as
    select
        a.USUBJID,
        a.TRT01P,
        a.BASE,
        a.AVAL,
        a.CHG,
        b.STRATUM
    from work.adlb as a
    left join work.adsl as b
        on a.USUBJID = b.USUBJID
    where a.PARAMCD = "FPG"
      and a.AVISITN = 26;
quit;

/* Numeric covariates for ANCOVA */
data work.fpg_analysis;
    set work.fpg_analysis;

    TRT_N = (TRT01P = "GLY-301");
    STRATUM_N = (STRATUM = "HbA1c >=9%");
run;

/* Confirm population and missingness */
proc sql;
    title "FPG Week-26 Population Validation";

    select
        TRT01P,
        count(*) as Total_Subjects,
        sum(not missing(CHG)) as Observed,
        sum(missing(CHG)) as Missing
    from work.fpg_analysis
    group by TRT01P;
quit;

/* Descriptive statistics */
proc means data=work.fpg_analysis
           n nmiss mean std;
    class TRT01P;
    var BASE AVAL CHG;
    title "FPG Week-26 Descriptive Statistics";
run;

title;



/* ===================================================
   Step 42: Week-26 FPG ANCOVA
   Observed-case supportive analysis
   =================================================== */

/* Verify stratum coding before fitting */
proc freq data=work.fpg_analysis;
    tables STRATUM*STRATUM_N / missing;
    title "FPG Stratum Coding Validation";
run;

/* Fit ANCOVA using observed Week-26 changes */
ods output ParameterEstimates=work.fpg_ancova_estimates;

proc glm data=work.fpg_analysis;
    where not missing(CHG);
    model CHG = TRT_N BASE STRATUM_N
        / solution clparm;
    title "FPG Week-26 ANCOVA";
run;
quit;

/* Extract treatment effect */
proc print data=work.fpg_ancova_estimates noobs;
    where Parameter = "TRT_N";
    title "FPG Adjusted Treatment Difference";
run;

title;
