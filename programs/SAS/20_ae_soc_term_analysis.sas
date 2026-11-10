
title "DIA-GLY3: Adverse Events by SOC and Preferred Term";

/* 1. Validate input */
proc sql;
    select count(*) as Total_AE_Events,
           count(distinct USUBJID) as Participants_With_AEs
    from work.adae;
quit;

/* 2. Count events and unique participants */
proc sql;
    create table work.ae_soc_term_summary as
    select TRT01A,
           AESOC,
           AETERM,
           count(*) as AE_Events,
           count(distinct USUBJID) as Participants_With_AEs
    from work.adae
    where TRTEMFL = "Y"
    group by TRT01A, AESOC, AETERM;
quit;

/* 3. Display summary */
proc sort data=work.ae_soc_term_summary;
    by AESOC AETERM TRT01A;
run;

proc print data=work.ae_soc_term_summary noobs;
    var AESOC AETERM TRT01A
        AE_Events Participants_With_AEs;
run;

title;
