
/* DIA-GLY3
   07 - Primary Efficacy Data QC
   Step 22: Week-26 HbA1c population */

title "Week-26 HbA1c Analysis Population";

proc sql;
    select TRT01P,
           count(*) as Total_Subjects,
           sum(not missing(AVAL)) as Observed,
           sum(missing(AVAL)) as Missing,
           sum(not missing(CHG)) as Complete_Changes
    from work.adlb
    where PARAMCD = "HBA1C"
      and AVISITN = 26
    group by TRT01P;
quit;

title "Primary Endpoint Records and Unique Subjects";

proc sql;
    select count(*) as Total_Records,
           count(distinct USUBJID) as Unique_Subjects
    from work.adlb
    where PARAMCD = "HBA1C"
      and AVISITN = 26;
quit;

title;
