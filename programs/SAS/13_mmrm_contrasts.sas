
/* DIA-GLY3
   Extract visit-specific MMRM treatment differences
   Contrast: GLY-301 minus Placebo
*/

data work.mmrm_final_contrasts;
    set work.mmrm_differences;

    /* Same visit, Placebo (0) minus GLY-301 (1) */
    if AVISITN = _AVISITN
       and TRT_N = 0
       and _TRT_N = 1;

    Week = AVISITN;

    /* Reverse contrast to GLY-301 minus Placebo */
    Treatment_Difference = -Estimate;
    SE = StdErr;
    CI_Lower = -Upper;
    CI_Upper = -Lower;
    P_Value = Probt;

    keep Week Treatment_Difference SE
         DF CI_Lower CI_Upper P_Value;
run;

proc sort data=work.mmrm_final_contrasts;
    by Week;
run;

title "DIA-GLY3: Final MMRM Treatment Differences";
title2 "GLY-301 minus Placebo";

proc print data=work.mmrm_final_contrasts noobs;
    format Treatment_Difference SE
           CI_Lower CI_Upper 8.4
           P_Value pvalue6.4;
run;

title;
title2;
