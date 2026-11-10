/* DIA-GLY3 — Step 49: Adverse Event Severity QC */

title "Adverse Event Severity Distribution";

proc freq data=work.adae;
    tables AESEV / missing;
run;

title "Adverse Event Severity by Treatment";

proc freq data=work.adae;
    tables TRT01A*AESEV / missing;
run;

title;

/* Step 49B: Maximum AE Severity per Participant */

data work.ae_severity_ranked;
    set work.adae;

    select (upcase(strip(AESEV)));
        when ("MILD") SEV_RANK = 1;
        when ("MODERATE") SEV_RANK = 2;
        when ("SEVERE") SEV_RANK = 3;
        otherwise SEV_RANK = .;
    end;
run;

proc sql;
    create table work.ae_max_severity as
    select
        USUBJID,
        TRT01A,
        max(SEV_RANK) as MAX_SEV_RANK
    from work.ae_severity_ranked
    group by USUBJID, TRT01A;
quit;

proc format;
    value severityfmt
        1 = "Mild"
        2 = "Moderate"
        3 = "Severe";
run;

title "Maximum AE Severity per Participant by Treatment";

proc freq data=work.ae_max_severity;
    tables TRT01A*MAX_SEV_RANK / missing;
    format MAX_SEV_RANK severityfmt.;
run;

title "Maximum AE Severity - Unique Participant Count";

proc sql;
    select
        TRT01A,
        count(*) as Participants_With_AEs
    from work.ae_max_severity
    group by TRT01A;
quit;

title;