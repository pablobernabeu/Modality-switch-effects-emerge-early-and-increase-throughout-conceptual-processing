setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Trial-by-trial export files')

# Analysis of the role of modality exclusivity


install.packages('afex')
library(afex)


EEG = readRDS('EEG.rds')

EEG.window3 = EEG[EEG$time >= 350 & EEG$time <= 550, c('microvolts','participant','target_item','condition',
  'electrode','time','CONTEXT_mean_exc','TARGET_prop_exc')]

rm(EEG)

EEG.window3 = data.frame(EEG.window3)

# Mean-center and scale IVs
EEG.window3$CONTEXT_mean_exc = scale(EEG.window3$CONTEXT_mean_exc)
EEG.window3$TARGET_prop_exc = scale(EEG.window3$TARGET_prop_exc)

EEG.window3$time = as.factor(EEG.window3$time)
EEG.window3$target_item = as.factor(EEG.window3$target_item)
EEG.window3$time = factor(EEG.window3$time)

sum(is.na(EEG.window3))  # No NAs


# Within Haptic-to-visual condition

m.h2v = mixed(microvolts ~
  (1| electrode) + (1| time) + (1| participant) + (1| target_item) +
  CONTEXT_mean_exc * TARGET_prop_exc,
  EEG.window3[EEG.window3$condition=='haptic2visual',],
  control = lmerControl(optCtrl=list(maxfun=10000000)), method='S')

warnings)
anova(m.h2v)
summary(m.h2v)


# Within Auditory-to-visual condition

m.a2v = mixed(microvolts ~
  (1| electrode) + (1| time) + (1| participant) + (1| target_item) +
  CONTEXT_mean_exc * TARGET_prop_exc,
  EEG.window3[EEG.window3$condition=='auditory2visual',],
  control = lmerControl(optCtrl=list(maxfun=10000000)), method='S')

warnings()
anova(m.a2v)
summary(m.a2v)



# Save general LME and t-value summary results for each Switch condition subset

# Auditory-to-visual

Switch_condition = 'Auditory-to-visual'
Variable = rownames(coefficients(summary(m.a2v)))	# NA = intercept, only present in t-value summary
num_DF = c(NA, anova(m.a2v)['num Df'][[1]])		# NA = intercept, only present in t-value summary
den_DF = c(NA, anova(m.a2v)['den Df'][[1]])		# NA = intercept, only present in t-value summary
F = c(NA, anova(m.a2v)['F'][[1]])				# NA = intercept, only present in t-value summary
F_p = c(NA, anova(m.a2v)['Pr(>F)'][[1]])			# NA = intercept, only present in t-value summary
Estimate = as.vector(coefficients(summary(m.a2v))[1:length(rownames(coefficients(summary(m.a2v)))), 'Estimate'])
SE = as.vector(coefficients(summary(m.a2v))[1:length(rownames(coefficients(summary(m.a2v)))), 'Std. Error'])
Lower_CI = Estimate - 1.96 * SE
Upper_CI = Estimate + 1.96 * SE
t = as.vector(coefficients(summary(m.a2v))[1:length(rownames(coefficients(summary(m.a2v)))), 't value'])
df = as.vector(coefficients(summary(m.a2v))[1:length(rownames(coefficients(summary(m.a2v)))), 'df'])
p = as.vector(coefficients(summary(m.a2v))[1:length(rownames(coefficients(summary(m.a2v)))), 'Pr(>|t|)'])

results_a2v = data.frame(Switch_condition, Variable, num_DF, den_DF, F, F_p, Estimate, SE, Lower_CI, Upper_CI, t, df, p)


# Haptic-to-visual

Switch_condition = 'Haptic-to-visual'
Variable = rownames(coefficients(summary(m.h2v)))	# NA = intercept, only present in t-value summary
num_DF = c(NA, anova(m.h2v)['num Df'][[1]])		# NA = intercept, only present in t-value summary
den_DF = c(NA, anova(m.h2v)['den Df'][[1]])		# NA = intercept, only present in t-value summary
F = c(NA, anova(m.h2v)['F'][[1]])				# NA = intercept, only present in t-value summary
F_p = c(NA, anova(m.h2v)['Pr(>F)'][[1]])			# NA = intercept, only present in t-value summary
Estimate = as.vector(coefficients(summary(m.h2v))[1:length(rownames(coefficients(summary(m.h2v)))), 'Estimate'])
SE = as.vector(coefficients(summary(m.h2v))[1:length(rownames(coefficients(summary(m.h2v)))), 'Std. Error'])
Lower_CI = Estimate - 1.96 * SE
Upper_CI = Estimate + 1.96 * SE
t = as.vector(coefficients(summary(m.h2v))[1:length(rownames(coefficients(summary(m.h2v)))), 't value'])
df = as.vector(coefficients(summary(m.h2v))[1:length(rownames(coefficients(summary(m.h2v)))), 'df'])
p = as.vector(coefficients(summary(m.h2v))[1:length(rownames(coefficients(summary(m.h2v)))), 'Pr(>|t|)'])

results_h2v = data.frame(Switch_condition, Variable, num_DF, den_DF, F, F_p, Estimate, SE, Lower_CI, Upper_CI, t, df, p)

results = rbind(results_a2v, results_h2v)

write.csv(results, 'Results on the role of modality exclusivity.csv', row.names=FALSE)





