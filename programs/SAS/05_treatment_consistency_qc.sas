
/* DIA-GLY3
   05 - Treatment Consistency QC */

proc sql;
    title "Treatment Consistency Across ADaM Datasets";

    select "ADEFF" as Dataset,
           count(*) as Total_Records,
           sum(a.TRT01P ne b.TRT01P)
               as Treatment_Mismatches
    from work.adeff as a
    inner join work.adsl as b
        on a.USUBJID = b.USUBJID

    union all

    select "ADLB",
           count(*),
           sum(a.TRT01P ne b.TRT01P)
    from work.adlb as a
    inner join work.adsl as b
        on a.USUBJID = b.USUBJID

    union all

    select "ADAE",
           count(*),
           sum(a.TRT01A ne b.TRT01A)
    from work.adae as a
    inner join work.adsl as b
        on a.USUBJID = b.USUBJID;
quit;

title;

