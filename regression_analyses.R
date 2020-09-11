library("rstudioapi")   

# set working directory to current
setwd(dirname(getActiveDocumentContext()$path)) 

# read feature file for regressions
reg_df <- read.csv(file = 'regression_df.csv')

cohort_feature <- character(20) # name of feature
coeff_vals <- double(20) # regression coefficients
p_vals <- double(20) # regression p-vals
nrow_vals <- double(20) # number of participants
i <- 0 # index for vectors

for (phase in c('lac1','lac2', 'uw1', 'uw2', 'nh')){
  for (feature in c('TST', 'midpoint_sleep', 'bedtime_mssd', 'bedtime_mssd_no_outliers')){
    i <- i+1
    df = subset(reg_df, cohort  %in% c(phase)) 
    
    feature_name <- feature
    
    # when computing bedtime_mssd without outliers
    if (feature == 'bedtime_mssd_no_outliers'){
      feature = 'bedtime_mssd'
      df <- subset(df, bedtime_mssd < mean(bedtime_mssd) + 3*sd(bedtime_mssd) 
                   & bedtime_mssd > mean(bedtime_mssd) - 3*sd(bedtime_mssd))
    } 
    
    # run regression, controlling for baseline GPA
    code <- paste0("model <- lm(term_gpa ~ ",feature," + cum_gpa, data=df)")
    eval(parse(text=code))
    
    # extract pval and coeff from regression
    pval <- coef(summary(model))[, "Pr(>|t|)"][feature][[1]]
    coeff <- coef(summary(model))[, "Estimate"][feature][[1]]
    
    # fill in table
    cohort_feature[i] <- paste0(phase,'_',feature_name)
    p_vals[i] <- pval
    coeff_vals[i] <- coeff
    nrow_vals[i] <- nrow(df)
  }
}

# all the regression results
regressions <- cbind(cohort_feature, coeff_vals, p_vals, nrow_vals)


