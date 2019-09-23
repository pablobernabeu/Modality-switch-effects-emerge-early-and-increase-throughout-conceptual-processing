
setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Export Files')

install.packages('TOSTER')
install.packages('ggplot2')
install.packages('purrr')
install.packages('lattice')
library(TOSTER)
library(purrr)
library(ggplot2)
library(lattice)



# In this script, the Modality Switch effect is further tested by means of Equivalence testing
# (Lakens, Scheel, & Isager, 2018). This serves especially for double-testing the effect within
# those time windows in which the effect came out as non-significant, in the standard statistics.
# This procedure requires aggregating over repeated measures such as electrodes, unlike in mixed
# models, where random effects are used to account for those measurements. As a result, the 
# statistical power is different (Brysbaert & Stevens, 2018).

# References
# Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing for psychological research: 
# A tutorial. Advances in Methods and Practices in Psychological Science, 2515245918770963.



# The Smallest Effect Size of interest (SESOI) is set as the effect size that was actually as in 
# in each window. 



# Read in dataset
EEG = readRDS('EEG.rds')



# TOST via TOSTpaired()


# TOST equivalence test for Modality Switch effect in Time Window 1

# Since this is going to be a t-test, aggregate data to remove repeated measures within electrodes etc.

EEG_aggreg = aggregate(microvolts ~ condition * participant, 
                       EEG[EEG$timewindow=='Window 1' & !is.na(EEG$RT.based_Groups),], FUN=mean)
str(EEG_aggreg)


# Compute average of both switch conditions and save variables

mean_switch_conditions =
  c(EEG_aggreg[EEG_aggreg$condition=='auditory2visual', 'microvolts'] +
      EEG_aggreg[EEG_aggreg$condition=='haptic2visual', 'microvolts'])    / 2

visual2visual = EEG_aggreg[EEG_aggreg$condition=='visual2visual', 'microvolts']

dat = data.frame(mean_switch_conditions, visual2visual)
str(dat)


# Calculate Cohen's dz, which will be used as smallest effect size of interest.
# Formula as in Lakens (2017) (https://osf.io/qzjaj/).
SD1 = sd(dat$visual2visual)
SD2 = sd(dat$mean_switch_conditions)
corr = cor(dat$visual2visual,	dat$mean_switch_conditions)
SD_diff = sqrt(SD1^2 + SD2^2 - 2 * corr * SD1 * SD2)
Cohen_dz = mean(dat$visual2visual - dat$mean_switch_conditions) / SD_diff
Cohen_dz 


# TOST equivalence test

TimeWindow.1 = TOSTpaired(n = 46, m1 = mean(dat$visual2visual), m2 = mean(dat$mean_switch_conditions),
	sd1 = sd(dat$visual2visual), sd2 = sd(dat$mean_switch_conditions), r12 = cor(dat$visual2visual, 
      dat$mean_switch_conditions), low_eqbound_dz = -Cohen_dz, high_eqbound_dz = Cohen_dz, alpha = 0.05)




# TOST equivalence test for Modality Switch effect in Time Window 2

# Since this is going to be a t-test, aggregate data to remove repeated measures within electrodes etc.

EEG_aggreg = aggregate(microvolts ~ condition * participant, 
                       EEG[EEG$timewindow=='Window 2' & !is.na(EEG$RT.based_Groups),], FUN=mean)
str(EEG_aggreg)


# Compute average of both switch conditions and save variables

mean_switch_conditions =
  c(EEG_aggreg[EEG_aggreg$condition=='auditory2visual', 'microvolts'] +
      EEG_aggreg[EEG_aggreg$condition=='haptic2visual', 'microvolts'])    / 2

visual2visual = EEG_aggreg[EEG_aggreg$condition=='visual2visual', 'microvolts']

dat = data.frame(mean_switch_conditions, visual2visual)
str(dat)


# Calculate Cohen's dz, which will be used as smallest effect size of interest.
# Formula as in Lakens (2017) (https://osf.io/qzjaj/).
SD1 = sd(dat$visual2visual)
SD2 = sd(dat$mean_switch_conditions)
corr = cor(dat$visual2visual,	dat$mean_switch_conditions)
SD_diff = sqrt(SD1^2 + SD2^2 - 2 * corr * SD1 * SD2)
Cohen_dz = mean(dat$visual2visual - dat$mean_switch_conditions) / SD_diff
Cohen_dz 


# TOST equivalence test

TimeWindow.2 = TOSTpaired(n = 46, m1 = mean(dat$visual2visual), m2 = mean(dat$mean_switch_conditions),
	sd1 = sd(dat$visual2visual), sd2 = sd(dat$mean_switch_conditions), r12 = cor(dat$visual2visual, 
      dat$mean_switch_conditions), low_eqbound_dz = -Cohen_dz, high_eqbound_dz = Cohen_dz, alpha = 0.05)




# TOST equivalence test for Modality Switch effect in Time Window 3

# Since this is going to be a t-test, aggregate data to remove repeated measures within electrodes etc.

EEG_aggreg = aggregate(microvolts ~ condition * participant, 
                       EEG[EEG$timewindow=='Window 3' & !is.na(EEG$RT.based_Groups),], FUN=mean)
str(EEG_aggreg)


# Compute average of both switch conditions and save variables

mean_switch_conditions =
  c(EEG_aggreg[EEG_aggreg$condition=='auditory2visual', 'microvolts'] +
      EEG_aggreg[EEG_aggreg$condition=='haptic2visual', 'microvolts'])    / 2

visual2visual = EEG_aggreg[EEG_aggreg$condition=='visual2visual', 'microvolts']

dat = data.frame(mean_switch_conditions, visual2visual)
str(dat)


# Calculate Cohen's dz, which will be used as smallest effect size of interest.
# Formula as in Lakens (2017) (https://osf.io/qzjaj/).
SD1 = sd(dat$visual2visual)
SD2 = sd(dat$mean_switch_conditions)
corr = cor(dat$visual2visual,	dat$mean_switch_conditions)
SD_diff = sqrt(SD1^2 + SD2^2 - 2 * corr * SD1 * SD2)
Cohen_dz = mean(dat$visual2visual - dat$mean_switch_conditions) / SD_diff
Cohen_dz 


# TOST equivalence test

TimeWindow.3 = TOSTpaired(n = 46, m1 = mean(dat$visual2visual), m2 = mean(dat$mean_switch_conditions),
	sd1 = sd(dat$visual2visual), sd2 = sd(dat$mean_switch_conditions), r12 = cor(dat$visual2visual,
      dat$mean_switch_conditions), low_eqbound_dz = -Cohen_dz, high_eqbound_dz = Cohen_dz, alpha = 0.05)





# TOST equivalence test for Modality Switch effect in Time Window 4

# Since this is going to be a t-test, aggregate data to remove repeated measures within electrodes etc.

EEG_aggreg = aggregate(microvolts ~ condition * participant, 
                       EEG[EEG$timewindow=='Window 4' & !is.na(EEG$RT.based_Groups),], FUN=mean)
str(EEG_aggreg)


# Compute average of both switch conditions and save variables

mean_switch_conditions =
  c(EEG_aggreg[EEG_aggreg$condition=='auditory2visual', 'microvolts'] +
      EEG_aggreg[EEG_aggreg$condition=='haptic2visual', 'microvolts'])    / 2

visual2visual = EEG_aggreg[EEG_aggreg$condition=='visual2visual', 'microvolts']

dat = data.frame(mean_switch_conditions, visual2visual)
str(dat)


# Calculate Cohen's dz, which will be used as smallest effect size of interest.
# Formula as in Lakens (2017) (https://osf.io/qzjaj/).
SD1 = sd(dat$visual2visual)
SD2 = sd(dat$mean_switch_conditions)
corr = cor(dat$visual2visual,	dat$mean_switch_conditions)
SD_diff = sqrt(SD1^2 + SD2^2 - 2 * corr * SD1 * SD2)
Cohen_dz = mean(dat$visual2visual - dat$mean_switch_conditions) / SD_diff
Cohen_dz 


# TOST equivalence test

TimeWindow.4 = TOSTpaired(n = 46, m1 = mean(dat$visual2visual), m2 = mean(dat$mean_switch_conditions),
	sd1 = sd(dat$visual2visual), sd2 = sd(dat$mean_switch_conditions), 
      r12 = cor(dat$visual2visual,	dat$mean_switch_conditions), low_eqbound_dz = -Cohen_dz, 
      high_eqbound_dz = Cohen_dz, alpha = 0.05)



TimeWindow.1 = data.frame(TimeWindow.1)
TimeWindow.2 = data.frame(TimeWindow.2)
TimeWindow.3 = data.frame(TimeWindow.3)
TimeWindow.4 = data.frame(TimeWindow.4)

TimeWindow.1$time.window = 'Window 1'
TimeWindow.1 = TimeWindow.1[c(16, 1:15)]

TimeWindow.2$time.window = 'Window 2'
TimeWindow.2 = TimeWindow.2[c(16, 1:15)]

TimeWindow.3$time.window = 'Window 3'
TimeWindow.3 = TimeWindow.3[c(16, 1:15)]

TimeWindow.4$time.window = 'Window 4'
TimeWindow.4 = TimeWindow.4[c(16, 1:15)]

equivalence.test.results = rbind(TimeWindow.1, TimeWindow.2, TimeWindow.3, TimeWindow.4)

write.csv(equivalence.test.results, 'Switch effect TOST Equivalence test results.csv', row.names=FALSE)
