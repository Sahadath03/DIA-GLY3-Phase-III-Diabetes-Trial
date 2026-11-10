
/* DIA-GLY3
   04 - ADLB parameter and missingness QC */

/* Parameter and visit structure */
title "ADLB Parameter and Visit Structure";

proc freq data=work.adlb;
    tables PARAMCD*AVISITN / missing;
run;

/* Missingness by parameter and visit */
title "ADLB Missingness Summary";

proc sql;
    select PARAMCD,
           AVISITN,
           count(*) as Total_Records,
           sum(not missing(AVAL)) as Observed,
           sum(missing(AVAL)) as Missing,
           calculated Missing /
               calculated Total_Records * 100
               as Missing_Pct format=6.2
    from work.adlb
    group by PARAMCD, AVISITN
    order by PARAMCD, AVISITN;
quit;

/* Week-26 weight by treatment */
title "Week-26 Weight Missingness by Treatment";

proc sql;
    select TRT01P,
           count(*) as Total,
           sum(not missing(AVAL)) as Observed,
           sum(missing(AVAL)) as Missing
    from work.adlb
    where upcase(strip(PARAMCD)) = "WEIGHT"
      and AVISITN = 26
    group by TRT01P;
quit;

title;


/* Confirm ADEFF Week-4 missing-value encoding */
title "ADEFF Week-4 Weight Source QC";

proc freq data=work.adeff;
    where AVISITN = 4;
    tables WEIGHT / missing;
run;

title;
