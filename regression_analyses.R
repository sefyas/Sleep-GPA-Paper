setwd("~/Dropbox/Life@CMU/FINAL PAPER/Publications/Sleep-GPA Paper")
reg_df <- read.csv(file = 'regression_df.csv')

cohort_feature <- character(20)
coeff_vals <- double(20)
p_vals <- double(20)
nrow_vals <- double(20)
i <- 0
for (phase in c('lac1','lac2', 'uw1', 'uw2', 'nh')){
  for (feature in c('TST', 'midpoint_sleep', 'bedtime_mssd', 'bedtime_mssd_no_outliers')){
    i <- i+1
    df = subset(reg_df, cohort  %in% c(phase)) 
    
    feature_name <- feature
    if (feature == 'bedtime_mssd_no_outliers'){
      feature = 'bedtime_mssd'
      df <- subset(df, bedtime_mssd < mean(bedtime_mssd) + 3*sd(bedtime_mssd) 
                   & bedtime_mssd > mean(bedtime_mssd) - 3*sd(bedtime_mssd))
    } 
    
    
    code <- paste0("model <- lm(term_gpa ~ ",feature," + cum_gpa, data=df)")
    eval(parse(text=code))
    pval <- coef(summary(model))[, "Pr(>|t|)"][feature][[1]]
    coeff <- coef(summary(model))[, "Estimate"][feature][[1]]
    
    cohort_feature[i] <- paste0(phase,'_',feature_name)
    p_vals[i] <- pval
    coeff_vals[i] <- coeff
    nrow_vals[i] <- nrow(df)
  }
}
regressions <- cbind(cohort_feature, coeff_vals, p_vals, nrow_vals)


