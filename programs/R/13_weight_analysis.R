# DIA-GLY3: observed-case supportive body-weight ANCOVA
# IMPORTANT: This is an observed-case analysis, not a MAR multiple-imputation analysis.
adsl <- read.csv("data/adam/adsl.csv")
adlb <- read.csv("data/adam/adlb.csv")
w <- subset(adlb,PARAMCD=="WEIGHT" & AVISITN==26)
a <- merge(adsl[,c("USUBJID","TRT01P","FASFL","WEIGHT_BL","HBA1C_BL")],w[,c("USUBJID","AVAL")],by="USUBJID")
a <- subset(a,FASFL=="Y" & !is.na(AVAL))
a$TRT <- factor(a$TRT01P,levels=c("Placebo","GLY-301"))
a$STRATUM <- factor(ifelse(a$HBA1C_BL<9,"<9%",">=9%"))
a$CHANGE <- a$AVAL-a$WEIGHT_BL
model <- lm(CHANGE ~ TRT + WEIGHT_BL + STRATUM,data=a)
term <- "TRTGLY-301"
ci <- confint(model,term)
res <- data.frame(Endpoint="Body weight change at Week 26 (observed cases)",Contrast="GLY-301 minus Placebo",N_Observed=nrow(a),Estimate=coef(model)[term],SE=summary(model)$coefficients[term,"Std. Error"],CI_Lower=ci[1],CI_Upper=ci[2],P_Value=summary(model)$coefficients[term,"Pr(>|t|)"])
summary_arm <- function(d,arm) data.frame(Treatment=arm,N=nrow(d),Baseline_Mean=mean(d$WEIGHT_BL),Baseline_SD=sd(d$WEIGHT_BL),Week26_Mean=mean(d$AVAL),Week26_SD=sd(d$AVAL),Change_Mean=mean(d$CHANGE),Change_SD=sd(d$CHANGE))
desc <- rbind(summary_arm(subset(a,TRT=="Placebo"),"Placebo"),summary_arm(subset(a,TRT=="GLY-301"),"GLY-301"))
write.csv(res,"outputs/tables/T14_2_4_weight_ancova.csv",row.names=FALSE)
write.csv(desc,"outputs/tables/T14_2_4_weight_descriptives.csv",row.names=FALSE)
saveRDS(model,"outputs/diagnostics/weight_ancova_model.rds")
print(desc);print(res)
