
/* DIA-GLY3
   02 - Import remaining ADaM datasets */

%macro import_adam(name);

    filename csvfile
        "~/DIA-GLY3-SAS/&name..csv";

    proc import datafile=csvfile
        out=work.&name
        dbms=csv
        replace;
        guessingrows=max;
        getnames=yes;
    run;

    title "Dataset structure: &name";

    proc contents data=work.&name;
    run;

    proc sql;
        select "&name" as Dataset,
               count(*) as Total_Records,
               count(distinct USUBJID)
                   as Unique_Subjects
        from work.&name;
    quit;

    title;

    filename csvfile clear;

%mend;

%import_adam(adeff);
%import_adam(adlb);
%import_adam(adae);
