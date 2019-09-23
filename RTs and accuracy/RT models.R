# MULTIVEL LME MODELING:

install.packages("car")
install.packages("pastecs")
install.packages('gmodels')
install.packages("compute.es") 
install.packages('trimr')
install.packages('lme4')
install.packages('ltm')
install.packages('psych')
install.packages('dae')
install.packages('SuppDists')
install.packages('effects')
install.packages('plyr')
install.packages('multcomp')
install.packages('lattice')
install.packages('ggplot2')
install.packages('doBy')
install.packages('eeptools')
install.packages('sjPlot')
install.packages("stargazer")
install.packages("Hmisc")
install.packages("merTools")
install.packages("MuMIn")

library(ltm)
library(lattice)
library(psych)
library(gmodels)
library(pastecs)
library(plyr)
library(SuppDists)
library(car)
library(compute.es)
library(effects)
library(multcomp)
library(Hmisc)
library(ggplot2)
library(dae)
library(trimr)
library(lme4)
library(doBy)
library(eeptools)
library(sjPlot)
library(merTools)
library(MuMIn)
library(stargazer)


targetbeh_responded_OUT = readRDS('targetbeh_responded_OUT.rds')


# Preparations:
# Mean-center and scale, as built-in in R
targetbeh_responded_OUT$s_conc_letters <- scale(targetbeh_responded_OUT$conc_letters)
targetbeh_responded_OUT$s_prop_letters <- scale(targetbeh_responded_OUT$prop_letters)
targetbeh_responded_OUT$s_LSA_distance <- scale(targetbeh_responded_OUT$LSA_distance)
targetbeh_responded_OUT$s_prop_lg10CD <- scale(targetbeh_responded_OUT$prop_lg10CD)
targetbeh_responded_OUT$s_conc_lg10CD <- scale(targetbeh_responded_OUT$conc_lg10CD)
targetbeh_responded_OUT$s_conc_exc <- scale(targetbeh_responded_OUT$conc_exc)
targetbeh_responded_OUT$s_prop_exc <- scale(targetbeh_responded_OUT$prop_exc)
targetbeh_responded_OUT$s_trial <- scale(targetbeh_responded_OUT$trial)
targetbeh_responded_OUT$s_prop_orthneigh <- scale(targetbeh_responded_OUT$prop_orthneigh)
targetbeh_responded_OUT$s_conc_orthneigh <- scale(targetbeh_responded_OUT$conc_orthneigh)
targetbeh_responded_OUT$s_Age_months <- scale(targetbeh_responded_OUT$Age_months)
str(targetbeh_responded_OUT)

# The standardization above does not consider experimental cells. Below is code 
# that does consider it, with specific standardization for each RT.based_Groups and each 
# Condition. It is finally not applied, however, because one cannot assume those 
# effects--and indeed, when thus transformed, the variables lose the effects they 
# do present when in the more standard form (regardless of experimental cells). 
# Still, the code is maintained below, should anyone wish to use it.

# GM_conc_letters <- aggregate(targetbeh_responded_OUT$conc_letters, 
# list(targetbeh_responded_OUT$Condition, targetbeh_responded_OUT$RT.based_Groups), 
# FUN = mean, data=targetbeh_responded_OUT)
# names(GM_conc_letters)<- c('Condition', 'RT.based_Groups','GM_conc_letters')
# 
# GM_prop_letters <- aggregate(targetbeh_responded_OUT$prop_letters, 
# list(targetbeh_responded_OUT$Condition, targetbeh_responded_OUT$RT.based_Groups), 
# FUN = mean, data=targetbeh_responded_OUT)
# names(GM_prop_letters)<- c('Condition', 'RT.based_Groups','GM_prop_letters')
# 
# GM_LSA_distance <- aggregate(targetbeh_responded_OUT$LSA_distance, 
# list(targetbeh_responded_OUT$Condition, targetbeh_responded_OUT$RT.based_Groups), 
# FUN = mean, data=targetbeh_responded_OUT)
# names(GM_LSA_distance)<- c('Condition', 'RT.based_Groups','GM_LSA_distance')
# 
# GM_prop_lg10CD <- aggregate(targetbeh_responded_OUT$prop_lg10CD, 
# list(targetbeh_responded_OUT$Condition, targetbeh_responded_OUT$RT.based_Groups), 
# FUN = mean, data=targetbeh_responded_OUT)
# names(GM_prop_lg10CD)<- c('Condition', 'RT.based_Groups','GM_prop_lg10CD')
# 
# GM_conc_lg10CD <- aggregate(targetbeh_responded_OUT$conc_lg10CD, 
# list(targetbeh_responded_OUT$Condition, targetbeh_responded_OUT$RT.based_Groups), 
# FUN = mean, data=targetbeh_responded_OUT)
# names(GM_conc_lg10CD)<- c('Condition', 'RT.based_Groups','GM_conc_lg10CD')
# 
# GM_conc_exc <- aggregate(targetbeh_responded_OUT$conc_exc, 
# list(targetbeh_responded_OUT$Condition, targetbeh_responded_OUT$RT.based_Groups), 
# FUN = mean, data=targetbeh_responded_OUT)
# names(GM_conc_exc)<- c('Condition', 'RT.based_Groups','GM_conc_exc')
# 
# GM_prop_exc <- aggregate(targetbeh_responded_OUT$prop_exc, 
# list(targetbeh_responded_OUT$Condition, targetbeh_responded_OUT$RT.based_Groups), 
# FUN = mean, data=targetbeh_responded_OUT)
# names(GM_prop_exc)<- c('Condition', 'RT.based_Groups','GM_prop_exc')
# 
# GM_trial <- aggregate(targetbeh_responded_OUT$trial, 
# list(targetbeh_responded_OUT$Condition, targetbeh_responded_OUT$RT.based_Groups), 
# FUN = mean, data=targetbeh_responded_OUT)
# names(GM_trial)<- c('Condition', 'RT.based_Groups','GM_trial')
# 
# GM_prop_orthneigh <- aggregate(targetbeh_responded_OUT$prop_orthneigh, 
# list(targetbeh_responded_OUT$Condition, targetbeh_responded_OUT$RT.based_Groups), 
# FUN = mean, data=targetbeh_responded_OUT)
# names(GM_prop_orthneigh)<- c('Condition', 'RT.based_Groups','GM_prop_orthneigh')
# 
# GM_conc_orthneigh <- aggregate(targetbeh_responded_OUT$conc_orthneigh, 
# list(targetbeh_responded_OUT$Condition, targetbeh_responded_OUT$RT.based_Groups), 
# FUN = mean, data=targetbeh_responded_OUT)
# names(GM_conc_orthneigh)<- c('Condition', 'RT.based_Groups','GM_conc_orthneigh')
# 
# GM_Age_months <- aggregate(targetbeh_responded_OUT$Age_months, 
# list(targetbeh_responded_OUT$Condition, targetbeh_responded_OUT$RT.based_Groups), 
# FUN = mean, data=targetbeh_responded_OUT)
# names(GM_Age_months)<- c('Condition', 'RT.based_Groups','GM_Age_months')
# 
# Add to data set:
# targetbeh_responded_OUT2 <- merge(targetbeh_responded_OUT, GM_conc_letters,
#  by = c('Condition', 'RT.based_Groups'))
# unique(targetbeh_responded_OUT2$GM_conc_letters)  # One mean per cell (inc 0-group)
# 
# targetbeh_responded_OUT3 <- merge(targetbeh_responded_OUT2, GM_prop_letters,
#  by = c('Condition', 'RT.based_Groups'))
# 
# targetbeh_responded_OUT4 <- merge(targetbeh_responded_OUT3, GM_LSA_distance,
#  by = c('Condition', 'RT.based_Groups'))
# 
# targetbeh_responded_OUT5 <- merge(targetbeh_responded_OUT4, GM_prop_lg10CD,
#  by = c('Condition', 'RT.based_Groups'))
# 
# targetbeh_responded_OUT6 <- merge(targetbeh_responded_OUT5, GM_conc_lg10CD,
#  by = c('Condition', 'RT.based_Groups'))
# 
# targetbeh_responded_OUT7 <- merge(targetbeh_responded_OUT6, GM_conc_exc,
#  by = c('Condition', 'RT.based_Groups'))
# 
# targetbeh_responded_OUT8 <- merge(targetbeh_responded_OUT7, GM_prop_exc,
#  by = c('Condition', 'RT.based_Groups'))
# 
# targetbeh_responded_OUT9 <- merge(targetbeh_responded_OUT8, GM_trial,
#  by = c('Condition', 'RT.based_Groups')) 
# 
# targetbeh_responded_OUT10 <- merge(targetbeh_responded_OUT9, GM_prop_orthneigh,
#  by = c('Condition', 'RT.based_Groups')) 
# 
# targetbeh_responded_OUT11 <- merge(targetbeh_responded_OUT10, GM_conc_orthneigh,
#  by = c('Condition', 'RT.based_Groups')) 
# 
# targetbeh_responded_OUT12 <- merge(targetbeh_responded_OUT11, GM_Age_months,
#  by = c('Condition', 'RT.based_Groups')) 


# Scaling = dividing by mean by SD
# save = summaryBy(conc_letters ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, 
# FUN=sd)
# targetbeh_responded_OUT <- merge(targetbeh_responded_OUT, save, by = c('Condition', 
# 'RT.based_Groups'))
# targetbeh_responded_OUT$GMCS_conc_letters = 
# targetbeh_responded_OUT$GM_conc_letters / targetbeh_responded_OUT$conc_letters.sd
# unique(targetbeh_responded_OUT$GMCS_conc_letters)
# 
# save = summaryBy(prop_letters ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, 
# FUN=sd)
# targetbeh_responded_OUT <- merge(targetbeh_responded_OUT, save, by = c('Condition', 
# 'RT.based_Groups'))
# targetbeh_responded_OUT$GMCS_prop_letters = 
# targetbeh_responded_OUT$GM_prop_letters / targetbeh_responded_OUT$prop_letters.sd
# 
# save = summaryBy(LSA_distance ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, 
# FUN=sd)
# targetbeh_responded_OUT <- merge(targetbeh_responded_OUT, save, by = c('Condition', 
# 'RT.based_Groups'))
# targetbeh_responded_OUT$GMCS_LSA_distance = 
# targetbeh_responded_OUT$GM_LSA_distance / targetbeh_responded_OUT$LSA_distance.sd
# 
# save = summaryBy(prop_lg10CD ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, 
# FUN=sd)
# targetbeh_responded_OUT <- merge(targetbeh_responded_OUT, save, by = c('Condition', 
# 'RT.based_Groups'))
# targetbeh_responded_OUT$GMCS_prop_lg10CD = 
# targetbeh_responded_OUT$GM_prop_lg10CD / targetbeh_responded_OUT$prop_lg10CD.sd
# 
# save = summaryBy(conc_lg10CD ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, 
# FUN=sd)
# targetbeh_responded_OUT <- merge(targetbeh_responded_OUT, save, by = c('Condition', 
# 'RT.based_Groups'))
# targetbeh_responded_OUT$GMCS_conc_lg10CD = 
# targetbeh_responded_OUT$GM_conc_lg10CD / targetbeh_responded_OUT$conc_lg10CD.sd
# 
# save = summaryBy(conc_exc ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, FUN=sd)
# targetbeh_responded_OUT <- merge(targetbeh_responded_OUT, save, by = c('Condition', 
# 'RT.based_Groups'))
# targetbeh_responded_OUT$GMCS_conc_exc = 
# targetbeh_responded_OUT$GM_conc_exc / targetbeh_responded_OUT$conc_exc.sd
# 
# save = summaryBy(prop_exc ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, FUN=sd)
# targetbeh_responded_OUT <- merge(targetbeh_responded_OUT, save, by = c('Condition', 
# 'RT.based_Groups'))
# targetbeh_responded_OUT$GMCS_prop_exc = 
# targetbeh_responded_OUT$GM_prop_exc / targetbeh_responded_OUT$prop_exc.sd
# 
# save = summaryBy(trial ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, FUN=sd)
# targetbeh_responded_OUT <- merge(targetbeh_responded_OUT, save, by = c('Condition', 
# 'RT.based_Groups'))
# targetbeh_responded_OUT$GMCS_trial = 
# targetbeh_responded_OUT$GM_trial / targetbeh_responded_OUT$trial.sd
# 
# save = summaryBy(prop_orthneigh ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, 
# FUN=sd)
# targetbeh_responded_OUT <- merge(targetbeh_responded_OUT, save, by = c('Condition', 
# 'RT.based_Groups'))
# targetbeh_responded_OUT$GMCS_prop_orthneigh = 
# targetbeh_responded_OUT$GM_prop_orthneigh / targetbeh_responded_OUT$prop_orthneigh.sd
# 
# save = summaryBy(conc_orthneigh ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, 
# FUN=sd)
# targetbeh_responded_OUT <- merge(targetbeh_responded_OUT, save, by = c('Condition', 
# 'RT.based_Groups'))
# targetbeh_responded_OUT$GMCS_conc_orthneigh = 
# targetbeh_responded_OUT$GM_conc_orthneigh / targetbeh_responded_OUT$conc_orthneigh.sd
# 
# save = summaryBy(Age_months ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, 
# FUN=sd)
# targetbeh_responded_OUT <- merge(targetbeh_responded_OUT, save, by = c('Condition', 
# 'RT.based_Groups'))
# targetbeh_responded_OUT$GMCS_Age_months = 
# targetbeh_responded_OUT$GM_Age_months / targetbeh_responded_OUT$Age_months.sd
# 

# Residuals of the multilevel models are slightly non-normal. Perhaps transform.
# Five different transformations will be compared with the original measure:
targetbeh_responded_OUT <- cbind(targetbeh_responded_OUT, 
targetbeh_responded_OUT$RT^2)
targetbeh_responded_OUT <- cbind(targetbeh_responded_OUT, 
sqrt(targetbeh_responded_OUT$RT))
targetbeh_responded_OUT <- cbind(targetbeh_responded_OUT, 
log(targetbeh_responded_OUT$RT +1))
targetbeh_responded_OUT <- cbind(targetbeh_responded_OUT, 
1/targetbeh_responded_OUT$RT)
targetbeh_responded_OUT <- cbind(targetbeh_responded_OUT, 
1/sqrt(targetbeh_responded_OUT$RT))

targetbeh_responded_OUT$sqRT <- targetbeh_responded_OUT$RT^2
targetbeh_responded_OUT$sqrtRT <- sqrt(targetbeh_responded_OUT$RT)
targetbeh_responded_OUT$logRT <- log(targetbeh_responded_OUT$RT +1)
targetbeh_responded_OUT$recRT <- 1/targetbeh_responded_OUT$RT
targetbeh_responded_OUT$recsqrtRT <- 1/sqrt(targetbeh_responded_OUT$RT)

# save
saveRDS(targetbeh_responded_OUT, 'targetbeh_responded_OUT.rds')

# read in
targetbeh_responded_OUT = readRDS('targetbeh_responded_OUT.rds')
head(targetbeh_responded_OUT)

# Check any improvements on the basis of the final model. Q-Q plot is used 
# because z-scores of skewness and kurtosis cannot be checked due to having
# over 5,000 observations. But Q-Q plot is nearly as straightforward. The
# ideal distribution presents a uniform, linear increase through the X and 
# Y axes--that is, a diagonal line on the plot.

m9 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| s_conc_lg10CD : Ptp) + s_trial + I(s_trial^2) + s_conc_letters,
	data = targetbeh_responded_OUT)
qqnorm(resid(m9))

m9 = lmer(sqRT ~ 1 + (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| s_conc_lg10CD : Ptp) + s_trial + I(s_trial^2) + s_conc_letters,
	data = targetbeh_responded_OUT)
qqnorm(resid(m9))

m9 = lmer(sqrtRT ~ 1 + (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| s_conc_lg10CD : Ptp) + s_trial + I(s_trial^2) + s_conc_letters,
	data = targetbeh_responded_OUT)
qqnorm(resid(m9))

m9 = lmer(logRT ~ 1 + (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| s_conc_lg10CD : Ptp) + s_trial + I(s_trial^2) + s_conc_letters,
	data = targetbeh_responded_OUT)
qqnorm(resid(m9))

m9 = lmer(recRT ~ 1 + (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| s_conc_lg10CD : Ptp) + s_trial + I(s_trial^2) + s_conc_letters,
	data = targetbeh_responded_OUT)
qqnorm(resid(m9))

m9 = lmer(recsqrtRT ~ 1 + (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| s_conc_lg10CD : Ptp) + s_trial + I(s_trial^2) + s_conc_letters,
	data = targetbeh_responded_OUT)
qqnorm(resid(m9))

# Conclusion on transformations check: Best with the raw RT. It'll be maintained.

# IN for the modeling...

# METHOD:
# These are random slope models with random intercepts and slopes for sub-
# jects. Both the RT and the EEG data are modeled with the most optimal 
# method as of today. The main characteristic is the maximal, data-driven 
# incorporation of effects, whereby the critical variables of interest are 
# analyzed on the basis of a null model. This null model contains all pos-
# sible random intercepts and slopes, including interactions, as well as all
# possible fixed effects and interactions, insofar as they add significantly 
# to that null model (Barr, Levy, Scheepers, & Tily, 2013: J Mem Lang). Both 
# random and fixed effects are analyzed one by one, stepwise, always keeping
# the degrees of freedom as similar as possible in the two models compared.
# The null hypothesis significance test is then the Chi-Square based on the 
# Likelihood Ratio Test. This method is optimal with the number of subjects 
# and items of this study (Luke, 2016: Behav Res).
# IMPORTANT: Models are named ad-hoc. Their numbers are NOT ordinal.


# Each model comparison will be commented. 
# First, specify random effects structure.

m0 = lmer(RT ~ 1 + (1 | Ptp),
	data = targetbeh_responded_OUT)
# random intercepts for participant

m0.1 = lmer(RT ~ 1 + (1 | Ptp) + (1 | Item),
	data = targetbeh_responded_OUT)
	# + (1 | Ptp : Item) = not accepted, too many groupings
anova(m0, m0.1)  # random intercepts Item: IN

m1 = lmer(RT ~ 1 + (1 | Ptp) + (1 | Item) + (1 | s_conc_letters), 
	data = targetbeh_responded_OUT)
anova(m0.1, m1)  # random ints for number letters concept word: IN

m2.0 = lmer(RT ~ 1+ (1 | Ptp) + (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp), 
	data = targetbeh_responded_OUT)
anova(m1, m2.0)  # random ints interact concept letters w/ Ptp: IN

m3 = lmer(RT ~ 1 + (1 | Ptp) + (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp) + (1| s_trial), 
	# + (1| Ptp : s_trial) = not accepted, too many groupings
	data = targetbeh_responded_OUT)
anova(m2.0, m3)  # random ints trial number: IN

m3.2 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters) 
	+ (1 | s_conc_letters : Ptp) + (1+ s_trial | Ptp), 
	data = targetbeh_responded_OUT)
anova(m3, m3.2)  # random slopes trial number per Ptp: IN

m4 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters) 
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| Accuracy), 
	data = targetbeh_responded_OUT)
anova(m3.2, m4)  # random ints Accuracy: out

m5 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters) 
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD), 
	data = targetbeh_responded_OUT)
anova(m3.2, m5)  # random ints word freq of concept: IN

m5.01 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters) 
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| s_conc_lg10CD : Ptp), 
	data = targetbeh_responded_OUT)
anova(m5, m5.01)  # random ints interact wordfreq concept x participant: out

m5.1 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters) 
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| s_conc_orthneigh), 
	data = targetbeh_responded_OUT)
anova(m5, m5.1) # random ints orthograph neighbourhood size concept: out

m5.2 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters) 
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| s_prop_orthneigh), 
	data = targetbeh_responded_OUT)
anova(m5, m5.2) # random ints orthograph neighbourhood size property: out

# To check LSA/semantic distance property-concept, subset to trials w/ scores,
# i.e., 79% of the trials.
mWith = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters) 
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD), 
	data = targetbeh_responded_OUT[!is.na(targetbeh_responded_OUT$s_LSA_distance),])

m5.22 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters) 
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| s_LSA_distance), 
	data = targetbeh_responded_OUT[!is.na(targetbeh_responded_OUT$s_LSA_distance),])
# random intercepts semantic distance property-concept: unidentifiable

#...Back to the full data
m5.3 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| solution),
	data = targetbeh_responded_OUT)
anova(m5, m5.3)  # random ints solution: out

m6 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters) 
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups),
	data = targetbeh_responded_OUT)
anova(m5, m6)   # random ints Group: IN

m7 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters) 
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + (1| Condition), 
	data = targetbeh_responded_OUT)
anova(m6, m7)  # Random ints Condition: OUT

m7.01 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + (1| s_Age_months),
	data = targetbeh_responded_OUT)
anova(m6, m7.01)  # random ints age: unidentifiable

m7.1 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + (1| Gender),
	data = targetbeh_responded_OUT)
anova(m6, m7.1)   # random ints gender: OUT

m7.11 = lmer(RT ~ 1 + (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + (1| Lefthanded),
	data = targetbeh_responded_OUT)
anova(m6, m7.11)   # random ints Handedness: OUT

########### Random effs structure set at m6. On to fixed effects:

m7.2 = lmer(RT ~ (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + s_trial,
	data = targetbeh_responded_OUT)
anova(m6, m7.2)  # trial number: IN

m8 = lmer(RT ~ (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + s_trial + I(s_trial^2),
	data = targetbeh_responded_OUT)
anova(m7.2, m8)  # quadratic trend for trial number: IN

m8.0 = lmer(RT ~ (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + s_trial + I(s_trial^2)+ I(s_trial^3),
	data = targetbeh_responded_OUT)
anova(m8, m8.0)  # cubic trend for trial number: IN

m8.01 = lmer(RT ~ (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + s_trial + I(s_trial^2)+ I(s_trial^3) + s_conc_lg10CD,
	data = targetbeh_responded_OUT)
anova(m8.0, m8.01)  # word frequency of the concept: out

m8.1 = lmer(RT ~ (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + s_trial + I(s_trial^2)+ I(s_trial^3) + s_Age_months,
	data = targetbeh_responded_OUT)
anova(m8.0, m8.1)  # age: out

m8.2 = lmer(RT ~ (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + s_trial + I(s_trial^2)+ I(s_trial^3) + Gender,
	data = targetbeh_responded_OUT)
anova(m8.0, m8.2)  # participant's gender: out

m8.3 = lmer(RT ~ (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + s_trial + I(s_trial^2)+ I(s_trial^3) + Lefthanded,
	data = targetbeh_responded_OUT)
anova(m8.0, m8.3)  # handedness: out

m9 = lmer(RT ~ (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + s_trial + I(s_trial^2)+ I(s_trial^3) + s_conc_letters,
	data = targetbeh_responded_OUT)
anova(m8.0, m9)  # number of letters of the concept: IN

########## Null model set: m8
# Data-driven null model: m8

# 1. ME Condition
mCondition = lmer(RT ~ (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + s_trial + I(s_trial^2)+ I(s_trial^3) + s_conc_letters 
	+ Condition,
	data = targetbeh_responded_OUT)
anova(m9, mCondition)  # Condition: out

# Group effects
mGroup = lmer(RT ~ (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + s_trial + I(s_trial^2)+ I(s_trial^3) + s_conc_letters
	+ RT.based_Groups,
	data = targetbeh_responded_OUT)
anova(m9, mGroup)  # Group: IN

# Interaction Condition*Group
mIntGroupCondition = lmer(RT ~ (1 | Item) + (1 | s_conc_letters)
	+ (1 | s_conc_letters : Ptp)+ (1+ s_trial | Ptp) + (1| s_conc_lg10CD)
	+ (1| RT.based_Groups) + s_trial + I(s_trial^2)+ I(s_trial^3) + s_conc_letters
	+ RT.based_Groups : Condition,
	data = targetbeh_responded_OUT)
# Rank deficient: model would want to have main eff of either factor, but
# those were tested out above already.


# MODEL SELECTED:
summary(mGroup)

# Diagnostics:
# Normal Q-Q plot of residuals:
qqnorm(resid(mGroup))
# Plot of Residuals vs. Fitted values:
plot(fitted(mGroup),resid(mGroup),xlab='Fitted Values', ylab='Residuals'); 
abline(h=0)
# random effs
plot(ranef(mGroup))
# RESULTS: non-normal residuals.
#

# Fit
1-var(residuals(mGroup))/(var(model.response(model.frame(mGroup)))) # Omega^2
r.squaredGLMM(mGroup)  # R2m: fixed effs.  R2c: fixed + random effs
RMSE.merMod(mGroup, scale = FALSE)   # Root MSE
plotFEsim(FEsim(m9))



#