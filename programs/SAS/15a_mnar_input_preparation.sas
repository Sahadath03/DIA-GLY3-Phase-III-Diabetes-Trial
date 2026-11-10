
/* =====================================================
   DIA-GLY3
   Step 45B - MNAR Input Preparation
   Reuse primary MI datasets
   ===================================================== */

/* 1. Identify originally missing Week-26 outcomes */

proc sql;
    create table work.mnar_original as
    select a.USUBJID,
           a.TRT01P,
           a.CHANGE as ORIGINAL_CHANGE
    from work.mi_input as a;
quit;

/* 2. Attach original missingness to completed MI data */

proc sql;
    create table work.mnar_base as
    select a.*,
           case
               when missing(b.ORIGINAL_CHANGE)
               then 1
               else 0
           end as ORIG_MISSING
    from work.mi_completed as a
    left join work.mnar_original as b
        on a.USUBJID = b.USUBJID;
quit;

/* 3. Check original missingness by treatment */

title "MNAR Original Missingness by Treatment";

proc freq data=work.mnar_base;
    where _Imputation_ = 1;
    tables TRT01P*ORIG_MISSING /
        missing norow nocol nopercent;
run;

/* 4. Check completed imputations */

title "MNAR Input Validation";

proc sql;
    select count(*) as Total_Records,
           count(distinct _Imputation_)
               as Number_of_Imputations,
           sum(missing(CHANGE))
               as Remaining_Missing
    from work.mnar_base;
quit;

title;
