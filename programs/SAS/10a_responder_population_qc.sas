
/* DIA-GLY3
   10 - HbA1c Responder Analysis
   Inspect responder endpoint */

/* Check responder variable and coding */
title "ADEFF Week-26 Responder Distribution";

proc freq data=work.adeff;
    where AVISITN = 26;
    tables TRT01P*RESP7 / missing;
run;

/* Confirm one Week-26 record per participant */
title "Responder Analysis Population";

proc sql;
    select count(*) as Total_Records,
           count(distinct USUBJID) as Unique_Subjects
    from work.adeff
    where AVISITN = 26;
quit;

title;
