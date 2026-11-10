
title "DIA-GLY3: Final SAS Dataset Integrity Audit";

proc sql;
    select "ADSL" as Dataset length=8,
           count(*) as Records,
           count(distinct USUBJID) as Unique_Subjects
    from work.adsl

    union all

    select "ADEFF", count(*),
           count(distinct USUBJID)
    from work.adeff

    union all

    select "ADLB", count(*),
           count(distinct USUBJID)
    from work.adlb

    union all

    select "ADAE", count(*),
           count(distinct USUBJID)
    from work.adae;
quit;

title;
