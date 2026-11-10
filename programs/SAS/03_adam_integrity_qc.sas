
/* DIA-GLY3
   03 - ADaM dataset integrity QC */

/* Record counts and unique participants */
proc sql;
    title "ADaM Dataset Integrity Summary";

    select "ADSL" as Dataset,
           count(*) as Records,
           count(distinct USUBJID) as Subjects
    from work.adsl

    union all

    select "ADEFF",
           count(*),
           count(distinct USUBJID)
    from work.adeff

    union all

    select "ADLB",
           count(*),
           count(distinct USUBJID)
    from work.adlb

    union all

    select "ADAE",
           count(*),
           count(distinct USUBJID)
    from work.adae;
quit;

title;

/* Identify analysis records without a matching ADSL subject */
proc sql;
    title "Unmatched Subject Records";

    select "ADEFF" as Dataset,
           count(*) as Unmatched_Records
    from work.adeff as a
    where not exists
        (select 1 from work.adsl as b
         where a.USUBJID=b.USUBJID)

    union all

    select "ADLB",
           count(*)
    from work.adlb as a
    where not exists
        (select 1 from work.adsl as b
         where a.USUBJID=b.USUBJID)

    union all

    select "ADAE",
           count(*)
    from work.adae as a
    where not exists
        (select 1 from work.adsl as b
         where a.USUBJID=b.USUBJID);
quit;

title;
