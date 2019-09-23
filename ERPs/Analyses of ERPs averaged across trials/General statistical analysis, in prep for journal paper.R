
setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Analyses of ERPs averaged across trials')

install.packages('doBy')
install.packages('dae')
install.packages('MuMIn')
install.packages('merTools')
install.packages('foreign')
install.packages('Hmisc')
install.packages('phia')
install.packages('Matrix')   # Used by lme4
install.packages('lme4')
install.packages('dfoptim')  # Used by lme4::allFit for convergence tests
install.packages('afex')
install.packages('sjstats')
install.packages('ggeffects')
install.packages('plyr')

library(doBy)
library(dae)
library(MuMIn)
library(merTools)
library(foreign)
library(Hmisc)
library(phia)
library(Matrix)
library(lme4) 
library(dfoptim)
library(afex)
library(sjstats)
library(ggeffects)
library(plyr)



# R version
R.Version()

# Versions of the statistical packages
packageVersion('Matrix') # used by lme4
packageVersion('lme4')
packageVersion('afex')



# Read in dataset
EEG = readRDS('EEG.rds')




# Remove Participant '7' due to very poor EEG signal (out of 36 trials per condition,
# this data only kept 1, 2, and 4 respectively per condition after artefact rejection).

EEG = EEG[!EEG$participant=='7', ]


# Limit data set for analysis to include only electrodes within the Anterior and Posterior
# area subsets.

EEG = EEG[!is.na(EEG$location), ]





# Peak waveforms and latencies per time window and group

# Time Window 1

# Quick group

dat = EEG[as.numeric(as.character(EEG$time)) >= 160 & 
  as.numeric(as.character(EEG$time)) < 216 & 
  EEG$RT.based_Groups=='Quick', ]

dat[which.max(abs(dat$microvolts)),]

# Slow group

dat = EEG[as.numeric(as.character(EEG$time)) >= 160 & 
  as.numeric(as.character(EEG$time)) < 216 & 
  EEG$RT.based_Groups=='Slow', ]

dat[which.max(abs(dat$microvolts)),]


# Time Window 2

# Quick group

dat = EEG[as.numeric(as.character(EEG$time)) >= 270 & 
  as.numeric(as.character(EEG$time)) < 370 & 
  EEG$RT.based_Groups=='Quick', ]

dat[which.max(abs(dat$microvolts)),]

# Slow group

dat = EEG[as.numeric(as.character(EEG$time)) >= 270 & 
  as.numeric(as.character(EEG$time)) < 370 & 
  EEG$RT.based_Groups=='Slow', ]

dat[which.max(abs(dat$microvolts)),]


# Time Window 3

# Quick group

dat = EEG[as.numeric(as.character(EEG$time)) >= 350 & 
  as.numeric(as.character(EEG$time)) < 550 & 
  EEG$RT.based_Groups=='Quick', ]

dat[which.max(abs(dat$microvolts)),]

# Slow group

dat = EEG[as.numeric(as.character(EEG$time)) >= 350 & 
  as.numeric(as.character(EEG$time)) < 550 & 
  EEG$RT.based_Groups=='Slow', ]

dat[which.max(abs(dat$microvolts)),]


# Time Window 4

# Quick group

dat = EEG[as.numeric(as.character(EEG$time)) >= 500 & 
  as.numeric(as.character(EEG$time)) < 750 & 
  EEG$RT.based_Groups=='Quick', ]

dat[which.max(abs(dat$microvolts)),]

# Slow group

dat = EEG[as.numeric(as.character(EEG$time)) >= 500 & 
  as.numeric(as.character(EEG$time)) < 750 & 
  EEG$RT.based_Groups=='Slow', ]

dat[which.max(abs(dat$microvolts)),]



# Mean and SD of the Switch conditions overall from 0 ms to 800 ms

summaryBy(microvolts ~ condition, FUN=c(mean, sd), EEG[!is.na(EEG$location) & 
  !is.na(EEG$RT.based_Groups) & !is.na(as.numeric(EEG$time)>=0),])



# Mean and SD of the Switch conditions per time window, group, and brain area

# Time Window 1

summaryBy(microvolts ~ c(RT.based_Groups, location, condition), FUN=c(mean, sd),
  EEG[as.numeric(as.character(EEG$time)) >= 160 & 
  as.numeric(as.character(EEG$time)) < 216 & 
  !is.na(EEG$location) & !is.na(EEG$RT.based_Groups),])

# Time Window 2

summaryBy(microvolts ~ c(RT.based_Groups, location, condition), FUN=c(mean, sd),
  EEG[as.numeric(as.character(EEG$time)) >= 270 & 
  as.numeric(as.character(EEG$time)) < 370 & 
  !is.na(EEG$location) & !is.na(EEG$RT.based_Groups),])

# Time Window 3

summaryBy(microvolts ~ c(RT.based_Groups, location, condition), FUN=c(mean, sd),
  EEG[as.numeric(as.character(EEG$time)) >= 350 & 
  as.numeric(as.character(EEG$time)) < 550 & 
  !is.na(EEG$location) & !is.na(EEG$RT.based_Groups),])

# Time Window 4

summaryBy(microvolts ~ c(RT.based_Groups, location, condition), FUN=c(mean, sd),
  EEG[as.numeric(as.character(EEG$time)) >= 500 & 
  as.numeric(as.character(EEG$time)) < 750 & 
  !is.na(EEG$location) & !is.na(EEG$RT.based_Groups),])





# Mixed models' residuals (further below) are extremely non-normal.
# Perhaps transform DV. First check several methods.

# The transformation is checked on Time Window 3 because it contains
# the most relevant ERP component, N400. Models are fitted using
# lme4::lmer() because the functions don't work with afex-fitted
# models.

EEG.window3 = EEG[as.numeric(as.character(EEG$time)) >= 350 &
  as.numeric(as.character(EEG$time)) <= 548, ]

EEG.window3$time = factor(EEG.window3$time)
str(EEG.window3)

# Some transformations create non-values in cells that are originally
# negative or 0. This is the case of the square root and the logit. 
# Before performing each transformation, the minimum value, turned
# into positive, plus 0.1, is added to all cells.



# First, check residuals with original, untransformed variable

orig = mixed(microvolts ~
		(1| time) + (1| electrode) + (condition | participant) +
		RT.based_Groups * condition * location,
		EEG.window3,
		control = lmerControl(optCtrl=list(maxfun=10000000)), method='S')
		
warnings()
		
anova(orig)
summary(orig)


# Residuals diagnostics:
# Normal Q-Q plot of residuals:
qqnorm(resid(orig))
# Plot of Residuals vs. Fitted values:
plot(fitted(orig), resid(orig), xlab='Fitted Values', ylab='Residuals'); 
abline(h=0)


# Second, check residuals when squared transformation is applied on DV

EEG.window3$sqmicrovolts = EEG.window3$microvolts ^2

sq_transformed = lme4::lmer(sqmicrovolts ~
		(1| time) + (1| electrode) + (1| location : electrode) +
		(condition | participant) +
		RT.based_Groups * condition * location,
		EEG.window3,
		control = lmerControl(optCtrl=list(maxfun=10000000)))
      
# Residuals diagnostics:
# Normal Q-Q plot of residuals:
qqnorm(resid(sq_transformed))
# Plot of Residuals vs. Fitted values:
plot(fitted(sq_transformed), resid(sq_transformed), xlab='Fitted Values', ylab='Residuals'); 
abline(h=0)


# Third, check residuals when square root transformation is applied on DV

EEG.window3$sqrtmicrovolts = sqrt(EEG.window3$microvolts + abs(min(EEG.window3$microvolts)) +0.1)

sqrt_transformed = lme4::lmer(sqrtmicrovolts ~
		(1| time) + (1| electrode) + (1| location : electrode) +
		(condition | participant) +
		RT.based_Groups * condition * location,
		EEG.window3,
		control = lmerControl(optCtrl=list(maxfun=10000000)))
      
# Residuals diagnostics:
# Normal Q-Q plot of residuals:
qqnorm(resid(sqrt_transformed))
# Plot of Residuals vs. Fitted values:
plot(fitted(sqrt_transformed), resid(sqrt_transformed), xlab='Fitted Values', ylab='Residuals'); 
abline(h=0)


# Fourth, check residuals when log transformation is applied on DV

EEG.window3$logmicrovolts = log(EEG.window3$microvolts + abs(min(EEG.window3$microvolts)) +0.1)

log_transformed = lme4::lmer(logmicrovolts ~
		(1| time) + (1| electrode) + (1| location : electrode) +
		(condition | participant) +
		RT.based_Groups * condition * location,
		EEG.window3,
		control = lmerControl(optCtrl=list(maxfun=10000000)))
      
# Residuals diagnostics:
# Normal Q-Q plot of residuals:
qqnorm(resid(log_transformed))
# Plot of Residuals vs. Fitted values:
plot(fitted(log_transformed), resid(log_transformed), xlab='Fitted Values', ylab='Residuals'); 
abline(h=0)


# Fifth, check residuals when reciprocal transformation is applied on DV

EEG.window3$recmicrovolts = 1 / EEG.window3$microvolts

rec_transformed = lme4::lmer(recmicrovolts ~
		(1| time) + (1| electrode) + (1| location : electrode) +
		(condition | participant) +
		RT.based_Groups * condition * location,
		EEG.window3,
		control = lmerControl(optCtrl=list(maxfun=10000000)))
      
# Residuals diagnostics:
# Normal Q-Q plot of residuals:
qqnorm(resid(rec_transformed))
# Plot of Residuals vs. Fitted values:
plot(fitted(rec_transformed), resid(rec_transformed), xlab='Fitted Values', ylab='Residuals'); 
abline(h=0)


# Sixth, check residuals when reciprocal of the square root transformation is applied on DV

EEG.window3$recsqrtmicrovolts = 1 / sqrt(EEG.window3$microvolts + abs(min(EEG.window3$microvolts)) +0.1)

recsq_transformed = lme4::lmer(recsqrtmicrovolts ~
		(1| time) + (1| electrode) + (1| location : electrode) +
		(condition | participant) +
		RT.based_Groups * condition * location,
		EEG.window3,
		control = lmerControl(optCtrl=list(maxfun=10000000)))
      
# Residuals diagnostics:
# Normal Q-Q plot of residuals:
qqnorm(resid(recsq_transformed))
# Plot of Residuals vs. Fitted values:
plot(fitted(recsq_transformed), resid(recsq_transformed), xlab='Fitted Values', ylab='Residuals'); 
abline(h=0)


# CONCLUSION TRANSFORMATIONS: No transformation is applied because none of them 
# improve the residuals of the mixed models.


# Remove ad-hoc object
rm(EEG.window3)




# Modeling method

# Maximal converging models were sought, by starting with maximal model, and reducing the
# random effects structure gradually when required for model convergence (Singmann
# & Kellen, in press; Bates et al., 2015). As it is commonly the case, the random
# effects structure had to be reduced considerably to allow convergence.

# The reduction of random effects began with the higher order terms, followed by the
# lower order. An example of a higher order term in our data is the anterior/posterior
# brain areas ('location'), and its lower order term is the electrodes. In sum, seven
# models were necessary for all time windows to converge, as detailed below.
 
# The p-values were calculated via the Satterthwaite approximation for degrees of freedom.
# The Satterthwaite method controls Type I error better than the classic Likelihood Ratio 
# approach (Luke, 2017, Behavior Research Methods). 

# References

# Bates, D., Kliegl, R., Vasishth, S., & Baayen, H. (2015). Parsimonious mixed models.
# arXiv:1506.04967 [stat]. arXiv: 1506.04967.

# Luke, S. G. (2017). Evaluating significance in linear mixed-effects models in R. 
# Behavior Research Methods, 49: 1494. https://doi.org/10.3758/s13428-016-0809-y.

# Singmann, H., & Kellen, D. (in press). An Introduction to Mixed Models for Experimental
# Psychology. In D. H. Spieler & E. Schumacher (Eds.), New Methods in Neuroscience and
# Cognitive Psychology. Psychology Press.





# At the end, measurements of the residuals will be provided for each final model, along with 
# goodness-of-fit measurements.



# For each model, convergence is tested by comparing results from various optimizers. The developers 
# of package `lme4` describe this procedure as follows (see this by calling ?lme4::convergence in R):

# ‘try all available optimizers (e.g. several different implementations of BOBYQA and Nelder-Mead, 
# L-BFGS-B from optim, nlminb, ...) via the allFit function, see ‘5.’ in the examples. While 
# this will of course be slow for large fits, we consider it the gold standard; if all optimizers converge 
# to values that are practically equivalent, then we would consider the convergence warnings to be 
# false positives.’



# Model attempts in order, from larger random effects structures to lighter structures.



# TIME WINDOW 1

# # First model attempt:

# # Measure running time
# start_time <- Sys.time()

# attempt1 = lme4::lmer(microvolts ~
	# (1| time) + (1| electrode) + (condition | participant) + (location | participant) +
	# (RT.based_Groups | participant) + (RT.based_Groups : condition : location | participant) +
	# RT.based_Groups * condition * location,
	# EEG[as.numeric(as.character(EEG$time)) >= 160 & as.numeric(as.character(EEG$time)) <= 214, ])
	
# # Show running time
# end_time <- Sys.time()
# end_time - start_time

# warnings()   # Warnings only apply only if model above indicated any warnings.

# # Check convergence as described above:

# attempt1_allFit <- allFit(attempt1)
# summ <- summary(attempt1_allFit)
# summ$fixef               ## extract fixed effects
# summ$llik                ## log-likelihoods
# summ$sdcor               ## SDs and correlations
# summ$theta               ## Cholesky factors
# summ$which.OK            ## which fits worked

# #------------------------------------------------------------------


# # Second model attempt:

# # Measure running time
# start_time <- Sys.time()

# attempt2 = lme4::lmer(microvolts ~
	# (1| time) + (1| electrode) + (condition | participant) + (location | participant) +
	# (RT.based_Groups | participant) + (RT.based_Groups : condition : location | participant) +
	# RT.based_Groups * condition * location,
	# EEG[as.numeric(as.character(EEG$time)) >= 160 & as.numeric(as.character(EEG$time)) <= 214, ])
	
# # Show running time
# end_time <- Sys.time()
# end_time - start_time

# warnings()   # Warnings only apply only if model above indicated any warnings.

# # Check convergence as described above:

# attempt2_allFit <- allFit(attempt2)
# summ <- summary(attempt2_allFit)
# summ$fixef               ## extract fixed effects
# summ$llik                ## log-likelihoods
# summ$sdcor               ## SDs and correlations
# summ$theta               ## Cholesky factors
# summ$which.OK            ## which fits worked

# #------------------------------------------------------------------


# # Third model attempt:

# # Measure running time
# start_time <- Sys.time()

# attempt3 = lme4::lmer(microvolts ~
	# (1| time) + (1| electrode) + (location | participant) +
	# (RT.based_Groups | participant) + (condition | participant) +
	# RT.based_Groups * condition * location,
	# EEG[as.numeric(as.character(EEG$time)) >= 160 & as.numeric(as.character(EEG$time)) <= 214, ])
	
# # Show running time
# end_time <- Sys.time()
# end_time - start_time

# warnings()   # Warnings only apply only if model above indicated any warnings.

# # Check convergence as described above:

# attempt3_allFit <- allFit(attempt3)
# summ <- summary(attempt3_allFit)
# summ$fixef               ## extract fixed effects
# summ$llik                ## log-likelihoods
# summ$sdcor               ## SDs and correlations
# summ$theta               ## Cholesky factors
# summ$which.OK            ## which fits worked

#------------------------------------------------------------------


# Fourth model attempt:

# Measure running time
start_time <- Sys.time()

attempt4 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| location : electrode) +
	(1| RT.based_Groups : participant) + (condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 160 & as.numeric(as.character(EEG$time)) <= 214, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time

warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt4_allFit <- allFit(attempt4)
summ <- summary(attempt4_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked

#------------------------------------------------------------------


# Fifth model attempt:

# Measure running time
start_time <- Sys.time()

attempt5 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| location : electrode) +
	(condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 160 & as.numeric(as.character(EEG$time)) <= 214, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time

warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt5_allFit <- allFit(attempt5)
summ <- summary(attempt5_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked

#------------------------------------------------------------------


# Sixth model attempt:

# Measure running time
start_time <- Sys.time()

attempt6 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| RT.based_Groups : participant) +
	(condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 160 & as.numeric(as.character(EEG$time)) <= 214, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time

warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt6_allFit <- allFit(attempt6)
summ <- summary(attempt6_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked
 
#------------------------------------------------------------------


# Seventh model attempt:

# Measure running time
start_time <- Sys.time()

attempt7 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 160 & as.numeric(as.character(EEG$time)) <= 214, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time
	
warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt7_allFit <- allFit(attempt7)
summ <- summary(attempt7_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked







# TIME WINDOW 2

# # First model attempt:

# # Measure running time
# start_time <- Sys.time()

# attempt1 = lme4::lmer(microvolts ~
	# (1| time) + (1| electrode) + (condition | participant) + (location | participant) +
	# (RT.based_Groups | participant) + (RT.based_Groups : condition : location | participant) +
	# RT.based_Groups * condition * location,
	# EEG[as.numeric(as.character(EEG$time)) >= 270 & as.numeric(as.character(EEG$time)) <= 368, ])
	
# # Show running time
# end_time <- Sys.time()
# end_time - start_time

# warnings()   # Warnings only apply only if model above indicated any warnings.

# # Check convergence as described above:

# attempt1_allFit <- allFit(attempt1)
# summ <- summary(attempt1_allFit)
# summ$fixef               ## extract fixed effects
# summ$llik                ## log-likelihoods
# summ$sdcor               ## SDs and correlations
# summ$theta               ## Cholesky factors
# summ$which.OK            ## which fits worked

# #------------------------------------------------------------------


# # Second model attempt:

# # Measure running time
# start_time <- Sys.time()

# attempt2 = lme4::lmer(microvolts ~
	# (1| time) + (1| electrode) + (condition | participant) + (location | participant) +
	# (RT.based_Groups | participant) + (RT.based_Groups : condition : location | participant) +
	# RT.based_Groups * condition * location,
	# EEG[as.numeric(as.character(EEG$time)) >= 270 & as.numeric(as.character(EEG$time)) <= 368, ])
	
# # Show running time
# end_time <- Sys.time()
# end_time - start_time

# warnings()   # Warnings only apply only if model above indicated any warnings.

# # Check convergence as described above:

# attempt2_allFit <- allFit(attempt2)
# summ <- summary(attempt2_allFit)
# summ$fixef               ## extract fixed effects
# summ$llik                ## log-likelihoods
# summ$sdcor               ## SDs and correlations
# summ$theta               ## Cholesky factors
# summ$which.OK            ## which fits worked

# #------------------------------------------------------------------


# # Third model attempt:

# # Measure running time
# start_time <- Sys.time()

# attempt3 = lme4::lmer(microvolts ~
	# (1| time) + (1| electrode) + (location | participant) +
	# (RT.based_Groups | participant) + (condition | participant) +
	# RT.based_Groups * condition * location,
	# EEG[as.numeric(as.character(EEG$time)) >= 270 & as.numeric(as.character(EEG$time)) <= 368, ])
	
# # Show running time
# end_time <- Sys.time()
# end_time - start_time

# warnings()   # Warnings only apply only if model above indicated any warnings.

# # Check convergence as described above:

# attempt3_allFit <- allFit(attempt3)
# summ <- summary(attempt3_allFit)
# summ$fixef               ## extract fixed effects
# summ$llik                ## log-likelihoods
# summ$sdcor               ## SDs and correlations
# summ$theta               ## Cholesky factors
# summ$which.OK            ## which fits worked

#------------------------------------------------------------------


# Fourth model attempt:

# Measure running time
start_time <- Sys.time()

attempt4 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| location : electrode) +
	(1| RT.based_Groups : participant) + (condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 270 & as.numeric(as.character(EEG$time)) <= 368, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time

warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt4_allFit <- allFit(attempt4)
summ <- summary(attempt4_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked

#------------------------------------------------------------------


# Fifth model attempt:

# Measure running time
start_time <- Sys.time()

attempt5 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| location : electrode) +
	(condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 270 & as.numeric(as.character(EEG$time)) <= 368, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time

warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt5_allFit <- allFit(attempt5)
summ <- summary(attempt5_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked

#------------------------------------------------------------------


# Sixth model attempt:

# Measure running time
start_time <- Sys.time()

attempt6 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| RT.based_Groups : participant) +
	(condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 270 & as.numeric(as.character(EEG$time)) <= 368, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time

warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt6_allFit <- allFit(attempt6)
summ <- summary(attempt6_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked
 
#------------------------------------------------------------------


# Seventh model attempt:

# Measure running time
start_time <- Sys.time()

attempt7 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 270 & as.numeric(as.character(EEG$time)) <= 368, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time
	
warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt7_allFit <- allFit(attempt7)
summ <- summary(attempt7_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked







# TIME WINDOW 3

# # First model attempt:

# # Measure running time
# start_time <- Sys.time()

# attempt1 = lme4::lmer(microvolts ~
	# (1| time) + (1| electrode) + (condition | participant) + (location | participant) +
	# (RT.based_Groups | participant) + (RT.based_Groups : condition : location | participant) +
	# RT.based_Groups * condition * location,
	# EEG[as.numeric(as.character(EEG$time)) >= 350 & as.numeric(as.character(EEG$time)) <= 550, ])
	
# # Show running time
# end_time <- Sys.time()
# end_time - start_time

# warnings()   # Warnings only apply only if model above indicated any warnings.

# # Check convergence as described above:

# attempt1_allFit <- allFit(attempt1)
# summ <- summary(attempt1_allFit)
# summ$fixef               ## extract fixed effects
# summ$llik                ## log-likelihoods
# summ$sdcor               ## SDs and correlations
# summ$theta               ## Cholesky factors
# summ$which.OK            ## which fits worked

# #------------------------------------------------------------------


# # Second model attempt:

# # Measure running time
# start_time <- Sys.time()

# attempt2 = lme4::lmer(microvolts ~
	# (1| time) + (1| electrode) + (condition | participant) + (location | participant) +
	# (RT.based_Groups | participant) + (RT.based_Groups : condition : location | participant) +
	# RT.based_Groups * condition * location,
	# EEG[as.numeric(as.character(EEG$time)) >= 350 & as.numeric(as.character(EEG$time)) <= 550, ])
	
# # Show running time
# end_time <- Sys.time()
# end_time - start_time

# warnings()   # Warnings only apply only if model above indicated any warnings.

# # Check convergence as described above:

# attempt2_allFit <- allFit(attempt2)
# summ <- summary(attempt2_allFit)
# summ$fixef               ## extract fixed effects
# summ$llik                ## log-likelihoods
# summ$sdcor               ## SDs and correlations
# summ$theta               ## Cholesky factors
# summ$which.OK            ## which fits worked

# #------------------------------------------------------------------


# # Third model attempt:

# # Measure running time
# start_time <- Sys.time()

# attempt3 = lme4::lmer(microvolts ~
	# (1| time) + (1| electrode) + (location | participant) +
	# (RT.based_Groups | participant) + (condition | participant) +
	# RT.based_Groups * condition * location,
	# EEG[as.numeric(as.character(EEG$time)) >= 350 & as.numeric(as.character(EEG$time)) <= 550, ])
	
# # Show running time
# end_time <- Sys.time()
# end_time - start_time

# warnings()   # Warnings only apply only if model above indicated any warnings.

# # Check convergence as described above:

# attempt3_allFit <- allFit(attempt3)
# summ <- summary(attempt3_allFit)
# summ$fixef               ## extract fixed effects
# summ$llik                ## log-likelihoods
# summ$sdcor               ## SDs and correlations
# summ$theta               ## Cholesky factors
# summ$which.OK            ## which fits worked

#------------------------------------------------------------------


# Fourth model attempt:

# Measure running time
start_time <- Sys.time()

attempt4 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| location : electrode) +
	(1| RT.based_Groups : participant) + (condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 350 & as.numeric(as.character(EEG$time)) <= 550, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time

warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt4_allFit <- allFit(attempt4)
summ <- summary(attempt4_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked

#------------------------------------------------------------------


# Fifth model attempt:

# Measure running time
start_time <- Sys.time()

attempt5 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| location : electrode) +
	(condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 350 & as.numeric(as.character(EEG$time)) <= 550, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time

warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt5_allFit <- allFit(attempt5)
summ <- summary(attempt5_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked

#------------------------------------------------------------------


# Sixth model attempt:

# Measure running time
start_time <- Sys.time()

attempt6 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| RT.based_Groups : participant) +
	(condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 350 & as.numeric(as.character(EEG$time)) <= 550, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time

warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt6_allFit <- allFit(attempt6)
summ <- summary(attempt6_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked
 
#------------------------------------------------------------------


# Seventh model attempt:

# Measure running time
start_time <- Sys.time()

attempt7 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 350 & as.numeric(as.character(EEG$time)) <= 550, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time
	
warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt7_allFit <- allFit(attempt7)
summ <- summary(attempt7_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked







# TIME WINDOW 4

# # First model attempt:

# # Measure running time
# start_time <- Sys.time()

# attempt1 = lme4::lmer(microvolts ~
	# (1| time) + (1| electrode) + (condition | participant) + (location | participant) +
	# (RT.based_Groups | participant) + (RT.based_Groups : condition : location | participant) +
	# RT.based_Groups * condition * location,
	# EEG[as.numeric(as.character(EEG$time)) >= 500 & as.numeric(as.character(EEG$time)) <= 748, ])
	
# # Show running time
# end_time <- Sys.time()
# end_time - start_time

# warnings()   # Warnings only apply only if model above indicated any warnings.

# # Check convergence as described above:

# attempt1_allFit <- allFit(attempt1)
# summ <- summary(attempt1_allFit)
# summ$fixef               ## extract fixed effects
# summ$llik                ## log-likelihoods
# summ$sdcor               ## SDs and correlations
# summ$theta               ## Cholesky factors
# summ$which.OK            ## which fits worked

# #------------------------------------------------------------------


# # Second model attempt:

# # Measure running time
# start_time <- Sys.time()

# attempt2 = lme4::lmer(microvolts ~
	# (1| time) + (1| electrode) + (condition | participant) + (location | participant) +
	# (RT.based_Groups | participant) + (RT.based_Groups : condition : location | participant) +
	# RT.based_Groups * condition * location,
	# EEG[as.numeric(as.character(EEG$time)) >= 500 & as.numeric(as.character(EEG$time)) <= 748, ])
	
# # Show running time
# end_time <- Sys.time()
# end_time - start_time

# warnings()   # Warnings only apply only if model above indicated any warnings.

# # Check convergence as described above:

# attempt2_allFit <- allFit(attempt2)
# summ <- summary(attempt2_allFit)
# summ$fixef               ## extract fixed effects
# summ$llik                ## log-likelihoods
# summ$sdcor               ## SDs and correlations
# summ$theta               ## Cholesky factors
# summ$which.OK            ## which fits worked

# #------------------------------------------------------------------


# # Third model attempt:

# # Measure running time
# start_time <- Sys.time()

# attempt3 = lme4::lmer(microvolts ~
	# (1| time) + (1| electrode) + (location | participant) +
	# (RT.based_Groups | participant) + (condition | participant) +
	# RT.based_Groups * condition * location,
	# EEG[as.numeric(as.character(EEG$time)) >= 500 & as.numeric(as.character(EEG$time)) <= 748, ])
	
# # Show running time
# end_time <- Sys.time()
# end_time - start_time

# warnings()   # Warnings only apply only if model above indicated any warnings.

# # Check convergence as described above:

# attempt3_allFit <- allFit(attempt3)
# summ <- summary(attempt3_allFit)
# summ$fixef               ## extract fixed effects
# summ$llik                ## log-likelihoods
# summ$sdcor               ## SDs and correlations
# summ$theta               ## Cholesky factors
# summ$which.OK            ## which fits worked

#------------------------------------------------------------------


# Fourth model attempt:

# Measure running time
start_time <- Sys.time()

attempt4 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| location : electrode) +
	(1| RT.based_Groups : participant) + (condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 500 & as.numeric(as.character(EEG$time)) <= 768, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time

warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt4_allFit <- allFit(attempt4)
summ <- summary(attempt4_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked

#------------------------------------------------------------------


# Fifth model attempt:

# Measure running time
start_time <- Sys.time()

attempt5 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| location : electrode) +
	(condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 500 & as.numeric(as.character(EEG$time)) <= 748, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time

warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt5_allFit <- allFit(attempt5)
summ <- summary(attempt5_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked

#------------------------------------------------------------------


# Sixth model attempt:

# Measure running time
start_time <- Sys.time()

attempt6 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| RT.based_Groups : participant) +
	(condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 500 & as.numeric(as.character(EEG$time)) <= 748, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time

warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt6_allFit <- allFit(attempt6)
summ <- summary(attempt6_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked
 
#------------------------------------------------------------------


# Seventh model attempt:

# Measure running time
start_time <- Sys.time()

attempt7 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (condition | participant) +
	RT.based_Groups * condition * location,
	EEG[as.numeric(as.character(EEG$time)) >= 500 & as.numeric(as.character(EEG$time)) <= 748, ])
	
# Show running time
end_time <- Sys.time()
end_time - start_time
	
warnings()   # Warnings only apply only if model above indicated any warnings.

# Check convergence as described above:

attempt7_allFit <- allFit(attempt7)
summ <- summary(attempt7_allFit)
summ$fixef               ## extract fixed effects
summ$llik                ## log-likelihoods
summ$sdcor               ## SDs and correlations
summ$theta               ## Cholesky factors
summ$which.OK            ## which fits worked




















# Save the resulting models

# Time Window 1




# Time Window 2




# Time Window 3




# Time Window 4











# Residuals and goodness-of-fit measures for each time window. Models are fitted using
# lme4::lmer() because the functions don't work with afex::mixed-fitted models.


# Time Window 1

lmer_m.w1 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| RT.based_Groups : participant) +
	(condition | participant) +
	RT.based_Groups * condition * location,
	data = EEG[as.numeric(as.character(EEG$time)) >= 160 & 
	as.numeric(as.character(EEG$time)) < 216,],
	control = lmerControl(optCtrl=list(maxfun=10000000)))

saveRDS(lmer_m.w1, 'lmer-fitted model Time Window 1.rds')
lmer_m.w1 = readRDS('lmer-fitted model Time Window 1.rds')

# Residuals diagnostics:
# Normal Q-Q plot of residuals:
qqnorm(resid(lmer_m.w1))
# Plot of Residuals vs. Fitted values:
plot(fitted(lmer_m.w1), resid(lmer_m.w1), xlab='Fitted Values', ylab='Residuals'); 
abline(h=0)
outliers(lmer_m.w1)
heteroskedastic(lmer_m.w1)
autocorrelation(lmer_m.w1)
normality(lmer_m.w1)
multicollin(lmer_m.w1)

# Goodness-of-fit
1-var(residuals(lmer_m.w1))/(var(model.response(model.frame(lmer_m.w1)))) # Omega^2
r.squaredGLMM(lmer_m.w1)  # R2m: fixed effs.  R2c: fixed + random effs
r2(lmer_m.w1)
RMSE.merMod(lmer_m.w1, scale = FALSE)   # Root MSE
# Below, Bayesian simulation of fixed effects
#png(file="Time window 4, posterior distributions of fixed effects.png", units="in", 
#  width=6, height=6, res=500)
plotFEsim(FEsim(lmer_m.w1))
#dev.off()


# Time Window 2

lmer_m.w2 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (condition | participant) +
	RT.based_Groups * condition * location,
	data = EEG[as.numeric(as.character(EEG$time)) >= 270 & 
	as.numeric(as.character(EEG$time)) < 370,],
	control = lmerControl(optCtrl=list(maxfun=10000000)))

saveRDS(lmer_m.w2, 'lmer-fitted model Time Window 2.rds')
lmer_m.w2 = readRDS('lmer-fitted model Time Window 2.rds')

# Residuals diagnostics:
# Normal Q-Q plot of residuals:
qqnorm(resid(lmer_m.w2))
# Plot of Residuals vs. Fitted values:
plot(fitted(lmer_m.w2), resid(lmer_m.w2), xlab='Fitted Values', ylab='Residuals'); 
abline(h=0)
outliers(lmer_m.w2)
heteroskedastic(lmer_m.w2)
autocorrelation(lmer_m.w2)
normality(lmer_m.w2)
multicollin(lmer_m.w2)

# Goodness-of-fit
1-var(residuals(lmer_m.w2))/(var(model.response(model.frame(lmer_m.w2)))) # Omega^2
r.squaredGLMM(lmer_m.w2)  # R2m: fixed effs.  R2c: fixed + random effs
r2(lmer_m.w2)
RMSE.merMod(lmer_m.w2, scale = FALSE)   # Root MSE
# Below, Bayesian simulation of fixed effects
#png(file="Time window 4, posterior distributions of fixed effects.png", units="in", 
#  width=6, height=6, res=500)
plotFEsim(FEsim(lmer_m.w2))
#dev.off()


# Time Window 3

lmer_m.w3 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| location : electrode) +
	(condition | participant) +
	RT.based_Groups * condition * location,
	data = EEG[as.numeric(as.character(EEG$time)) >= 350 & 
	as.numeric(as.character(EEG$time)) < 550,],
	control = lmerControl(optCtrl=list(maxfun=10000000)))

saveRDS(lmer_m.w3, 'lmer-fitted model Time Window 3.rds')
lmer_m.w3 = readRDS('lmer-fitted model Time Window 3.rds')

# Residuals diagnostics:
# Normal Q-Q plot of residuals:
qqnorm(resid(lmer_m.w3))
# Plot of Residuals vs. Fitted values:
plot(fitted(lmer_m.w3), resid(lmer_m.w3), xlab='Fitted Values', ylab='Residuals'); 
abline(h=0)
outliers(lmer_m.w3)
heteroskedastic(lmer_m.w3)
autocorrelation(lmer_m.w3)
normality(lmer_m.w3)
multicollin(lmer_m.w3)

# Goodness-of-fit
1-var(residuals(lmer_m.w3))/(var(model.response(model.frame(lmer_m.w3)))) # Omega^2
r.squaredGLMM(lmer_m.w3)  # R2m: fixed effs.  R2c: fixed + random effs
r2(lmer_m.w3)
RMSE.merMod(lmer_m.w3, scale = FALSE)   # Root MSE
# Below, Bayesian simulation of fixed effects
#png(file="Time window 4, posterior distributions of fixed effects.png", units="in", 
#  width=6, height=6, res=500)
plotFEsim(FEsim(lmer_m.w3))
#dev.off()

# Plot marginal effects
mydf = ggpredict(lmer_m.w3, c('RT.based_Groups', 'condition', 'location'))
plot(mydf)
mydf$x = as.numeric(mydf$x)
ggplot(mydf, aes(x = x, y = predicted, colour = facet)) +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~group)


# Time Window 4

lmer_m.w4 = lme4::lmer(microvolts ~
	(1| time) + (1| electrode) + (1| RT.based_Groups : participant) +
	(condition | participant) +
	RT.based_Groups * condition * location,
	data = EEG[as.numeric(as.character(EEG$time)) >= 500 & 
	as.numeric(as.character(EEG$time)) < 750,],
	control = lmerControl(optCtrl=list(maxfun=10000000)))

saveRDS(lmer_m.w4, 'lmer-fitted model Time Window 4.rds')
lmer_m.w4 = readRDS('lmer-fitted model Time Window 4.rds')

# Residuals diagnostics:
# Normal Q-Q plot of residuals:
qqnorm(resid(lmer_m.w4))
# Plot of Residuals vs. Fitted values:
plot(fitted(lmer_m.w4), resid(lmer_m.w4), xlab='Fitted Values', ylab='Residuals'); 
abline(h=0)
outliers(lmer_m.w4)
heteroskedastic(lmer_m.w4)
autocorrelation(lmer_m.w4)
normality(lmer_m.w4)
multicollin(lmer_m.w4)

# Goodness-of-fit
1-var(residuals(lmer_m.w4))/(var(model.response(model.frame(lmer_m.w4)))) # Omega^2
r.squaredGLMM(lmer_m.w4)  # R2m: fixed effs.  R2c: fixed + random effs
r2(lmer_m.w4)
RMSE.merMod(lmer_m.w4, scale = FALSE)   # Root MSE
# Below, Bayesian simulation of fixed effects
#png(file="Time window 4, posterior distributions of fixed effects.png", units="in", 
#  width=6, height=6, res=500)
plotFEsim(FEsim(lmer_m.w4))
#dev.off()






# Save results from all time windows: general LME and t-value summary results

# Time Window 1

tw1 = readRDS('LME model Time Window 1.rds')

Time.window = 'Window 1 (160-216 ms)'
Statistic = 'F'
Variable = rownames(anova(tw1))
F = anova(tw1)['F'][[1]]
num_df = anova(tw1)['num Df'][[1]]
den_df = anova(tw1)['den Df'][[1]]
F_p = anova(tw1)['Pr(>F)'][[1]]
results_tw1_F = data.frame(Time.window, Statistic, Variable, num_df, den_df, F, F_p)

Statistic = 't'
Variable = rownames(coefficients(summary(tw1)))

Estimate = as.vector(coefficients(summary(tw1))[1:length(rownames(coefficients(summary(tw1)))), 
  'Estimate'])

SE = as.vector(coefficients(summary(tw1))[1:length(rownames(coefficients(summary(tw1)))), 
  'Std. Error'])

Lower_CI = Estimate - 1.96 * SE
Upper_CI = Estimate + 1.96 * SE

t = as.vector(coefficients(summary(tw1))[1:length(rownames(coefficients(summary(tw1)))), 
  't value'])

df = as.vector(coefficients(summary(tw1))[1:length(rownames(coefficients(summary(tw1)))), 
  'df'])

t_p = as.vector(coefficients(summary(tw1))[1:length(rownames(coefficients(summary(tw1)))), 
  'Pr(>|t|)'])

results_tw1_t = data.frame(Time.window, Statistic, Variable, Estimate, SE, 
  Lower_CI, Upper_CI, t, df, t_p)

results_tw1 = rbind.fill(results_tw1_F, results_tw1_t)




# Time Window 2

tw2 = readRDS('LME model Time Window 2.rds')

Time.window = 'Window 2 (270-370 ms)'
Statistic = 'F'
Variable = rownames(anova(tw2))
F = anova(tw2)['F'][[1]]
num_df = anova(tw2)['num Df'][[1]]
den_df = anova(tw2)['den Df'][[1]]
F_p = anova(tw2)['Pr(>F)'][[1]]
results_tw2_F = data.frame(Time.window, Statistic, Variable, num_df, den_df, F, F_p)

Statistic = 't'
Variable = rownames(coefficients(summary(tw2)))

Estimate = as.vector(coefficients(summary(tw2))[1:length(rownames(coefficients(summary(tw2)))), 
  'Estimate'])

SE = as.vector(coefficients(summary(tw2))[1:length(rownames(coefficients(summary(tw2)))), 
  'Std. Error'])

Lower_CI = Estimate - 1.96 * SE
Upper_CI = Estimate + 1.96 * SE

t = as.vector(coefficients(summary(tw2))[1:length(rownames(coefficients(summary(tw2)))), 
  't value'])

df = as.vector(coefficients(summary(tw2))[1:length(rownames(coefficients(summary(tw2)))), 
  'df'])

t_p = as.vector(coefficients(summary(tw2))[1:length(rownames(coefficients(summary(tw2)))), 
  'Pr(>|t|)'])

results_tw2_t = data.frame(Time.window, Statistic, Variable, Estimate, SE, 
  Lower_CI, Upper_CI, t, df, t_p)

results_tw2 = rbind.fill(results_tw2_F, results_tw2_t)




# Time Window 3

tw3 = readRDS('LME model Time Window 3.rds')

Time.window = 'Window 3 (350-550 ms)'
Statistic = 'F'
Variable = rownames(anova(tw3))
F = anova(tw3)['F'][[1]]
num_df = anova(tw3)['num Df'][[1]]
den_df = anova(tw3)['den Df'][[1]]
F_p = anova(tw3)['Pr(>F)'][[1]]
results_tw3_F = data.frame(Time.window, Statistic, Variable, num_df, den_df, F, F_p)

Statistic = 't'
Variable = rownames(coefficients(summary(tw3)))

Estimate = as.vector(coefficients(summary(tw3))[1:length(rownames(coefficients(summary(tw3)))), 
  'Estimate'])

SE = as.vector(coefficients(summary(tw3))[1:length(rownames(coefficients(summary(tw3)))), 
  'Std. Error'])

Lower_CI = Estimate - 1.96 * SE
Upper_CI = Estimate + 1.96 * SE

t = as.vector(coefficients(summary(tw3))[1:length(rownames(coefficients(summary(tw3)))), 
  't value'])

df = as.vector(coefficients(summary(tw3))[1:length(rownames(coefficients(summary(tw3)))), 
  'df'])

t_p = as.vector(coefficients(summary(tw3))[1:length(rownames(coefficients(summary(tw3)))), 
  'Pr(>|t|)'])

results_tw3_t = data.frame(Time.window, Statistic, Variable, Estimate, SE, 
  Lower_CI, Upper_CI, t, df, t_p)

results_tw3 = rbind.fill(results_tw3_F, results_tw3_t)




# Time Window 4

tw4 = readRDS('LME model Time Window 4.rds')

Time.window = 'Window 4 (500-750 ms)'
Statistic = 'F'
Variable = rownames(anova(tw4))
F = anova(tw4)['F'][[1]]
num_df = anova(tw4)['num Df'][[1]]
den_df = anova(tw4)['den Df'][[1]]
F_p = anova(tw4)['Pr(>F)'][[1]]
results_tw4_F = data.frame(Time.window, Statistic, Variable, num_df, den_df, F, F_p)

Statistic = 't'
Variable = rownames(coefficients(summary(tw4)))

Estimate = as.vector(coefficients(summary(tw4))[1:length(rownames(coefficients(summary(tw4)))), 
  'Estimate'])

SE = as.vector(coefficients(summary(tw4))[1:length(rownames(coefficients(summary(tw4)))), 
  'Std. Error'])

Lower_CI = Estimate - 1.96 * SE
Upper_CI = Estimate + 1.96 * SE

t = as.vector(coefficients(summary(tw4))[1:length(rownames(coefficients(summary(tw4)))), 
  't value'])

df = as.vector(coefficients(summary(tw4))[1:length(rownames(coefficients(summary(tw4)))), 
  'df'])

t_p = as.vector(coefficients(summary(tw4))[1:length(rownames(coefficients(summary(tw4)))), 
  'Pr(>|t|)'])

results_tw4_t = data.frame(Time.window, Statistic, Variable, Estimate, SE, 
  Lower_CI, Upper_CI, t, df, t_p)

results_tw4 = rbind.fill(results_tw4_F, results_tw4_t)


# Join results from all time windows in one table
results = rbind(results_tw1, results_tw2, results_tw3, results_tw4)



# Adjust p values for multiple comparisons via Holm-Bonferroni method (Holm, 1979).
# Thus, the lowest p-value in the four time windows is multiplied by 4, the next p-value 
# is multiplied by 3, the next by 2, and the highest p-value is left as it is. In this 
# stepwise correction, if and when a non-significant p-value is reached, all of the next
# p-values become non-significant.
# Holm, S. (1979). A simple sequentially rejective multiple test procedure. Scandinavian 
# Journal of Statistics, 6, 65-70. http://www.jstor.org/stable/4615733.

results$'Holm-Bonferroni-corrected_F_p' = NA


# First, for the F-based p-values, on each variable in turn

n_var = 1	# Variable whose p-values will be adjusted

results[results$Statistic=='F' &
results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_F_p'] =

  p.adjust(results[results$Statistic=='F' &
  results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 'F_p'],
  method = 'holm')



n_var = 2	# Variable whose p-values will be adjusted

results[results$Statistic=='F' &
results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 
'Holm-Bonferroni-corrected_F_p'] =

  p.adjust(results[results$Statistic=='F' &
  results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 'F_p'],
  method = 'holm')



n_var = 3	# Variable whose p-values will be adjusted

results[results$Statistic=='F' &
results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 
'Holm-Bonferroni-corrected_F_p'] =

  p.adjust(results[results$Statistic=='F' &
  results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 'F_p'],
  method = 'holm')



n_var = 4	# Variable whose p-values will be adjusted

results[results$Statistic=='F' &
results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 
'Holm-Bonferroni-corrected_F_p'] =

  p.adjust(results[results$Statistic=='F' &
  results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 'F_p'],
  method = 'holm')



n_var = 5	# Variable whose p-values will be adjusted

results[results$Statistic=='F' &
results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 
'Holm-Bonferroni-corrected_F_p'] =

  p.adjust(results[results$Statistic=='F' &
  results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 'F_p'],
  method = 'holm')



n_var = 6	# Variable whose p-values will be adjusted

results[results$Statistic=='F' &
results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 
'Holm-Bonferroni-corrected_F_p'] =

  p.adjust(results[results$Statistic=='F' &
  results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 'F_p'],
  method = 'holm')



n_var = 7	# Variable whose p-values will be adjusted

results[results$Statistic=='F' &
results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 
'Holm-Bonferroni-corrected_F_p'] =

  p.adjust(results[results$Statistic=='F' &
  results$Variable==unique(results[results$Statistic=='F', 'Variable'])[n_var], 'F_p'],
  method = 'holm')





# Same procedure for t-based p-values, on each variable in turn

n_var = 1	# Variable whose t-values will be adjusted

results[results$Statistic=='t' &
results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_t_p'] =

  p.adjust(results[results$Statistic=='t' &
  results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var], 't_p'],
  method = 'holm')



n_var = 2	# Variable whose t-values will be adjusted

results[results$Statistic=='t' &
results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_t_p'] =

  p.adjust(results[results$Statistic=='t' &
  results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var], 't_p'],
  method = 'holm')



n_var = 3	# Variable whose t-values will be adjusted

results[results$Statistic=='t' &
results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_t_p'] =

  p.adjust(results[results$Statistic=='t' &
  results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var], 't_p'],
  method = 'holm')


n_var = 4	# Variable whose t-values will be adjusted

results[results$Statistic=='t' &
results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_t_p'] =

  p.adjust(results[results$Statistic=='t' &
  results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var], 't_p'],
  method = 'holm')


n_var = 5	# Variable whose t-values will be adjusted

results[results$Statistic=='t' &
results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_t_p'] =

  p.adjust(results[results$Statistic=='t' &
  results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var], 't_p'],
  method = 'holm')


n_var = 6	# Variable whose t-values will be adjusted

results[results$Statistic=='t' &
results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_t_p'] =

  p.adjust(results[results$Statistic=='t' &
  results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var], 't_p'],
  method = 'holm')


n_var = 7	# Variable whose t-values will be adjusted

results[results$Statistic=='t' &
results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_t_p'] =

  p.adjust(results[results$Statistic=='t' &
  results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var], 't_p'],
  method = 'holm')


n_var = 8	# Variable whose t-values will be adjusted

results[results$Statistic=='t' &
results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_t_p'] =

  p.adjust(results[results$Statistic=='t' &
  results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var], 't_p'],
  method = 'holm')


n_var = 9	# Variable whose t-values will be adjusted

results[results$Statistic=='t' &
results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_t_p'] =

  p.adjust(results[results$Statistic=='t' &
  results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var], 't_p'],
  method = 'holm')


n_var = 10	# Variable whose t-values will be adjusted

results[results$Statistic=='t' &
results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_t_p'] =

  p.adjust(results[results$Statistic=='t' &
  results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var], 't_p'],
  method = 'holm')


n_var = 11	# Variable whose t-values will be adjusted

results[results$Statistic=='t' &
results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_t_p'] =

  p.adjust(results[results$Statistic=='t' &
  results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var], 't_p'],
  method = 'holm')


n_var = 12	# Variable whose t-values will be adjusted

results[results$Statistic=='t' &
results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var],
'Holm-Bonferroni-corrected_t_p'] =

  p.adjust(results[results$Statistic=='t' &
  results$Variable==unique(results[results$Statistic=='t', 'Variable'])[n_var], 't_p'],
  method = 'holm')


# Save

results$F_p = results$'Holm-Bonferroni-corrected_F_p'
results$t_p = results$'Holm-Bonferroni-corrected_t_p'

results = results[1:(length(results)-2)]

names(results)[names(results) == 'F_p'] = 'Holm-Bonferroni-corrected_F_p'
names(results)[names(results) == 't_p'] = 'Holm-Bonferroni-corrected_t_p'



# Add p-value asterisks

# For F-based p-values

results$F_p.asterisks = NA
results$F_p.asterisks = as.character(results$F_p.asterisks)

results$F_p.asterisks = ifelse(results$'Holm-Bonferroni-corrected_F_p' < .001, '***', NA)

results$F_p.asterisks = 
	ifelse(results$'Holm-Bonferroni-corrected_F_p' > .001 & 
	results$'Holm-Bonferroni-corrected_F_p' < .01, '**',
	results$F_p.asterisks)

results$F_p.asterisks = 
	ifelse(results$'Holm-Bonferroni-corrected_F_p' > .01 & 
	results$'Holm-Bonferroni-corrected_F_p' < .05, '*',
	results$F_p.asterisks)


# For t-based p-values

results$t_p.asterisks = NA
results$t_p.asterisks = as.character(results$t_p.asterisks)

results$t_p.asterisks = ifelse(results$'Holm-Bonferroni-corrected_t_p' < .001, '***', NA)

results$t_p.asterisks = 
	ifelse(results$'Holm-Bonferroni-corrected_t_p' > .001 & 
	results$'Holm-Bonferroni-corrected_t_p' < .01, '**',
	results$t_p.asterisks)

results$t_p.asterisks = 
	ifelse(results$'Holm-Bonferroni-corrected_t_p' > .01 & 
	results$'Holm-Bonferroni-corrected_t_p' < .05, '*',
	results$t_p.asterisks)


# Order columns
str(results)
results = results[, c(1:3, 6, 4:5, 7, 15, 8:14, 16)]

# Save
write.csv(results, 'General LME results, all windows.csv', na='', row.names=FALSE)







########################################

# LMEs following up on the interactions of CMS ('condition') with Groups and electrode areas
# are performed in another script on the current folder.

########################################






