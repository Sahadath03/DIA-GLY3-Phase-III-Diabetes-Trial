
/* =====================================================
   DIA-GLY3 — SAS Safety Analysis
    Overall TEAE Summary
   ===================================================== */

/* 1. Safety population by treatment */
title "Safety Population by Treatment";

proc sql;
    select TRT01P,
           count(distinct USUBJID) as Safety_N
    from work.adsl
    where SAFFL = "Y"
    group by TRT01P;
quit;

/* 2. Overall treatment-emergent adverse events */
title "Overall TEAE Summary";

proc sql;
    select a.TRT01A as Treatment,
           count(*) as TEAE_Events,
           count(distinct a.USUBJID) as Subjects_With_TEAEs
    from work.adae as a
    inner join work.adsl as b
        on a.USUBJID = b.USUBJID
    where b.SAFFL = "Y"
      and a.TRTEMFL = "Y"
    group by a.TRT01A;
quit;

/* 3. TEAE flag distribution */
title "Treatment-Emergent AE Flag Validation";

proc freq data=work.adae;
    tables TRTEMFL / missing;
run;

title;



/* =====================================================
    Serious Treatment-Emergent Adverse Events
   ===================================================== */

title "Serious TEAE Summary by Treatment";

proc sql;
    select TRT01A as Treatment,
           count(*) as Serious_AE_Events,
           count(distinct USUBJID)
               as Subjects_With_Serious_AEs
    from work.adae
    where TRTEMFL = "Y"
      and AESER = "Y"
    group by TRT01A;
quit;

title "Serious AE Flag Distribution";

proc freq data=work.adae;
    tables AESER / missing;
run;

title;



/* =====================================================
    Hypoglycemia Safety Analysis
   ===================================================== */

/* 1. Identify hypoglycemia event terminology */

title "Hypoglycemia Event Terms";

proc freq data=work.adae;
    tables AETERM / missing;
    where upcase(AETERM) contains "HYPOGLYC";
run;

/* 2. Hypoglycemia events and affected participants */

title "Hypoglycemia Summary by Treatment";

proc sql;
    select TRT01A as Treatment,
           count(*) as Hypoglycemia_Events,
           count(distinct USUBJID)
               as Subjects_With_Hypoglycemia
    from work.adae
    where TRTEMFL = "Y"
      and upcase(AETERM) contains "HYPOGLYC"
    group by TRT01A;
quit;

title;



/* Inspect adverse-event terms */

title "All Adverse Event Terms";

proc freq data=work.adae;
    tables AETERM / missing;
run;

title;
