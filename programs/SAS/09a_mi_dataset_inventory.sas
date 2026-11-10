
/* DIA-GLY3
   Step 35 - Inspect saved MI datasets
   No imputation or modeling performed */

title "Available SAS Work Datasets";

proc sql;
    select memname as Dataset_Name,
           nobs as Number_of_Records,
           nvar as Number_of_Variables
    from dictionary.tables
    where libname = "WORK"
      and (
          upcase(memname) contains "IMP"
          or upcase(memname) contains "MI_"
          or upcase(memname) contains "ANCOVA"
      )
    order by memname;
quit;

title;
