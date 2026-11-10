
/* DIA-GLY3
   06 - Derived Variable and Data Type QC
   Step 19: Inspect ADLB variables */

title "ADLB Variable Structure";

proc contents data=work.adlb varnum;
run;

title;



/* DIA-GLY3
   Step 20 - Change from baseline QC */

title "ADLB Change-from-Baseline Validation";

proc sql;
    select PARAMCD,
           count(*) as Total_Records,
           sum(not missing(AVAL) and
               not missing(BASE) and
               not missing(CHG))
               as Complete_Records,
           sum(not missing(AVAL) and
               not missing(BASE) and
               missing(CHG))
               as Missing_Change,
           sum(not missing(AVAL) and
               not missing(BASE) and
               not missing(CHG) and
               abs(CHG - (AVAL - BASE)) > 1e-6)
               as Calculation_Mismatches
    from work.adlb
    group by PARAMCD
    order by PARAMCD;
quit;

title;
