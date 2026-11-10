
title "DIA-GLY3: Serious Adverse Event QC";

proc sql;
    select TRT01A,
           count(*) as Serious_AE_Events,
           count(distinct USUBJID)
               as Participants_With_Serious_AEs
    from work.adae
    where upcase(strip(AESER)) = "Y"
    group by TRT01A;
quit;

title;
