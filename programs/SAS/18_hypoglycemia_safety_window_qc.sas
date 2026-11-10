/* Import hypoglycemia dataset */

proc import
    datafile="/home/u64646130/DIA-GLY3-SAS/hypoglycemia.csv"
    out=work.hypoglycemia
    dbms=csv
    replace;
    guessingrows=max;
run;

/* Verify import */

proc sql;
    select count(*) as Total_Events
    from work.hypoglycemia;
quit;

/* DIA-GLY3 —  Hypoglycemia Safety Window QC */

proc sql;
    create table work.hypo_window_qc as
    select
        h.USUBJID,
        h.HYPOSEQ,
        h.HYPOSTDY,
        a.FIRST_DOSE_DAY,
        a.LAST_DOSE_DAY,
        a.SAFETY_FU_DAY,
        case
            when h.HYPOSTDY >= a.FIRST_DOSE_DAY
             and h.HYPOSTDY <= a.SAFETY_FU_DAY
            then 1
            else 0
        end as WITHIN_SAFETY_WINDOW
    from work.hypoglycemia as h
    left join work.adsl as a
        on h.USUBJID = a.USUBJID;
quit;

title "Hypoglycemia Safety Follow-up Validation";

proc sql;
    select
        count(*) as Total_Events,
        sum(WITHIN_SAFETY_WINDOW = 1) as Within_Safety_Window,
        sum(WITHIN_SAFETY_WINDOW = 0) as Outside_Safety_Window,
        sum(missing(FIRST_DOSE_DAY)) as Missing_Dose_Information
    from work.hypo_window_qc;
quit;

title;