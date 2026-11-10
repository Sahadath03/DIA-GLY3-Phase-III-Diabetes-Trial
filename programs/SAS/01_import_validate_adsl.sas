
/* DIA-GLY3: SAS validation
   Step 4 - Import ADSL */

filename adslfile
  "~/DIA-GLY3-SAS/adsl.csv";

proc import datafile=adslfile
    out=work.adsl
    dbms=csv
    replace;
    guessingrows=max;
    getnames=yes;
run;

/* Check the imported dataset structure */
proc contents data=work.adsl;
run;

/* Check total number of records */
proc sql;
    select count(*) as Total_Records,
           count(distinct USUBJID) as Unique_Subjects
    from work.adsl;
quit;

/* Check treatment allocation */
proc freq data=work.adsl;
    tables TRT01P / missing;
run;
