
/* DIA-GLY3: Import hypoglycemia event data */

proc import
    datafile="/home/u64646130/DIA-GLY3-SAS/hypoglycemia.csv"
    out=work.hypo
    dbms=csv
    replace;
    guessingrows=max;
run;

title "Hypoglycemia Dataset Structure";

proc contents data=work.hypo;
run;

title "Hypoglycemia Event Count";

proc sql;
    select count(*) as Total_Events,
           count(distinct USUBJID) as Unique_Subjects
    from work.hypo;
quit;

title;



/* =====================================================
    Hypoglycemia by Treatment
   ===================================================== */

/* Add treatment assignment from ADSL */

proc sql;
    create table work.hypo_analysis as
    select h.*,
           a.TRT01A
    from work.hypo as h
    inner join work.adsl as a
        on h.USUBJID = a.USUBJID
    where a.SAFFL = "Y";
quit;

/* Count events and unique participants */

title "Hypoglycemia Events by Treatment";

proc sql;
    select TRT01A as Treatment,
           count(*) as Total_Events,
           count(distinct USUBJID)
               as Participants_With_Hypoglycemia
    from work.hypo_analysis
    group by TRT01A;
quit;

/* Hypoglycemia severity classification */

title "Hypoglycemia Levels by Treatment";

proc freq data=work.hypo_analysis;
    tables TRT01A * HYPOLEVEL / missing;
run;

/* External assistance */

title "External Assistance by Treatment";

proc freq data=work.hypo_analysis;
    tables TRT01A * EXTERNAL_ASSISTANCE / missing;
run;

title;



/* =====================================================
   Recurrent Hypoglycemia
   ===================================================== */

/* Count events per participant */
proc sql;
    create table work.hypo_subject_counts as
    select TRT01A,
           USUBJID,
           count(*) as Event_Count
    from work.hypo_analysis
    group by TRT01A, USUBJID;
quit;

/* Summarize recurrence by treatment */
title "Recurrent Hypoglycemia by Treatment";

proc sql;
    select TRT01A as Treatment,
           count(*) as Participants_With_Hypoglycemia,
           sum(Event_Count >= 2)
               as Participants_With_2plus_Events,
           sum(Event_Count) as Total_Events
    from work.hypo_subject_counts
    group by TRT01A;
quit;

title;
