
/* ============================================================
   DIA-GLY3
   Body-Weight Analysis
   Week-26 Observed-Case ANCOVA
   ============================================================ */

/* Create analysis dataset */
proc sql;
    create table work.weight_analysis as
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
    where a.PARAMCD = "WEIGHT"
      and a.AVISITN = 26;
quit;

/* Prepare model variables */
data work.weight_analysis;
    set work.weight_analysis;

    TRT_N = (TRT01P = "GLY-301");
    STRATUM_N = (STRATUM = "HbA1c >=9%");
run;

/* Validate analysis population */
proc sql;
    title "Week-26 Weight Analysis Population";

    select
        TRT01P,
        count(*) as Total_Subjects,
        sum(not missing(CHG)) as Observed,
        sum(missing(CHG)) as Missing
    from work.weight_analysis
    group by TRT01P;
quit;

/* Observed-case descriptive statistics */
proc means data=work.weight_analysis
           n nmiss mean std;
    class TRT01P;
    var BASE AVAL CHG;
    title "Week-26 Body-Weight Descriptive Statistics";
run;

/* Fit adjusted ANCOVA */
ods output ParameterEstimates=work.weight_ancova_estimates;

proc glm data=work.weight_analysis;
    where not missing(CHG);
    model CHG = TRT_N BASE STRATUM_N
        / solution clparm;
    title "Week-26 Body-Weight ANCOVA";
run;
quit;

/* Display treatment effect */
proc print data=work.weight_ancova_estimates noobs;
    where Parameter = "TRT_N";
    title "Adjusted Body-Weight Treatment Difference";
run;

title;
