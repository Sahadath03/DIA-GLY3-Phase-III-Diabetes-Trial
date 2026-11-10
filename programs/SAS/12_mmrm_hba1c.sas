
/* =====================================================
   DIA-GLY3
   HbA1c MMRM
   Independent SAS reproduction of R model
   ===================================================== */

/* 1. Prepare observed post-baseline HbA1c records */

data work.mmrm_hba1c;
    set work.adeff;

    where AVISITN in (4, 8, 12, 18, 26);

    /* Convert character outcome to numeric */
    CHANGE = input(strip(CHG_HBA1C), ?? best32.);

    /* Numeric treatment coding */
    TRT_N = (TRT01P = "GLY-301");

    /* Randomization stratum */
    STRATUM_N = (HBA1C_BL >= 9);

    /* Only observed outcomes */
    if missing(CHANGE) then delete;

    keep USUBJID AVISITN TRT_N
         HBA1C_BL STRATUM_N CHANGE;
run;

/* 2. Validate analysis population */

title "MMRM Analysis Population";

proc sql;
    select count(*) as Observed_Records,
           count(distinct USUBJID) as Unique_Subjects
    from work.mmrm_hba1c;
quit;

title "MMRM Observed Records by Visit";

proc freq data=work.mmrm_hba1c;
    tables AVISITN / missing;
run;

/* 3. Fit MMRM with unstructured covariance */

ods output
    SolutionF=work.mmrm_fixed_effects
    CovParms=work.mmrm_covariance
    LSMeans=work.mmrm_lsmeans
    Diffs=work.mmrm_differences;

title "HbA1c MMRM - Unstructured Covariance";

proc mixed data=work.mmrm_hba1c
           method=reml;

    class USUBJID AVISITN TRT_N;

    model CHANGE =
        TRT_N
        AVISITN
        TRT_N*AVISITN
        HBA1C_BL
        HBA1C_BL*AVISITN
        STRATUM_N
        / solution ddfm=kr;

    repeated AVISITN /
        subject=USUBJID
        type=un;

    lsmeans TRT_N*AVISITN /
        diff cl;

run;

/* 4. Display captured model outputs */

title "MMRM Fixed Effects";
proc print data=work.mmrm_fixed_effects noobs;
run;

title "MMRM Treatment-by-Visit Differences";
proc print data=work.mmrm_differences noobs;
run;

title;
