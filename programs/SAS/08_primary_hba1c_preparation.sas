
/* DIA-GLY3
   08 - Primary HbA1c MI ANCOVA
    Prepare analysis dataset */

/* Extract Week-26 HbA1c records and
   attach the randomization stratum */

proc sql;
    create table work.hba1c_primary as
    select a.USUBJID,
           a.TRT01P,
           b.STRATUM,
           a.AVAL,
           a.BASE,
           a.CHG
    from work.adlb as a
    inner join work.adsl as b
        on a.USUBJID = b.USUBJID
    where a.PARAMCD = "HBA1C"
      and a.AVISITN = 26;
quit;

/* Verify analysis dataset */
title "Primary HbA1c Analysis Dataset";

proc contents data=work.hba1c_primary varnum;
run;

title "Treatment and Stratum Distribution";

proc freq data=work.hba1c_primary;
    tables TRT01P*STRATUM / missing;
run;

title "Primary HbA1c Analysis Data Summary";

proc means data=work.hba1c_primary
           n nmiss mean std;
    class TRT01P;
    var AVAL BASE CHG;
run;

title;




/*Longitudinal HbA1c availability */

/* Convert character HbA1c values to numeric.
   The text "NA" becomes a SAS missing value. */

data work.adeff_hba1c_qc;
    set work.adeff;

    if strip(upcase(HBA1C)) in ("NA", "")
        then HBA1C_NUM = .;
    else HBA1C_NUM = input(strip(HBA1C), best32.);
run;

/* Count observed and missing measurements */

title "ADEFF HbA1c Data by Visit";

proc sql;
    select AVISITN,
           count(*) as Total_Records,
           sum(not missing(HBA1C_NUM)) as Observed,
           sum(missing(HBA1C_NUM)) as Missing
    from work.adeff_hba1c_qc
    group by AVISITN
    order by AVISITN;
quit;

title;

