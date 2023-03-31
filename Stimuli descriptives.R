setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment')

# Descriptives of the stimuli. 

library(doBy)
library(pastecs)

stimuli = read.csv('Stimulicsv.csv')
str(stimuli)

# First confirm design-matched variables, length and frequency
# of target words per condition.

# Number of letters
summaryBy(prop_letters ~ Condition, FUN=stat.desc, data=stimuli[stimuli$position=='target',])
# mean in every condition = 7.08

# Word frequency: Log-10 Contextual Diversity measure from 
# the SUBTLEX-NL corpus (Keuleers, Brysbaert, & New, 2010)
summaryBy(prop_lg10CD ~ Condition, FUN=stat.desc, data=stimuli[stimuli$position=='target',])
# mean in every condition = 1.92


# Measure how imageable the stimuli were.
# After word length and frequency, which are minutely matched across 
# conditions, the most relevant variable may be the general perceptual 
# strength, equivalent to the highest modality score for a word (i.e., 
# that of the dominant modality).
# Note that this index outperforms concreteness in predicting reading
# measures, as shown in: 
# Connell, L., & Lynott, D. (2012). Strength of perceptual experience 
# predicts word processing performance better than concreteness or 
# imageability. Cognition, 125, 452-465. 


# General perceptual strength (scores from 0 to 5)
stat.desc(stimuli$mean_perceptualstrength)
# RESULT: 3.33. That's above half of the maximum score, so above average as it were.

# and per condition:
summaryBy(mean_perceptualstrength ~ Condition, FUN=stat.desc, data=stimuli[stimuli$position=='target',])


# Now, modality exclusivity (scores from 0 to 1)
stat.desc(stimuli$mean_exclusivity)
# RESULT: .34. That's below average.

# Exclusivity per condition:
summaryBy(mean_exclusivity ~ Condition, FUN=stat.desc, data=stimuli[stimuli$position=='target',])

# Exclusivity per dominant modality:
summaryBy(mean_exclusivity ~ modality, FUN=stat.desc, data=stimuli)


#*************************
# General concreteness* (scores from 1 to 5; retrieved from http://crr.ugent.be/archives/1602)
# *NOTE THAT SOME SCORES ARE MISSING as they were absent from the corpus, so the more reliable indices are above.
#**************************

stat.desc(stimuli$mean_concreteness)
# RESULT: 3.02. That's essentially on the average point.

# and per condition:
summaryBy(mean_concreteness ~ Condition, FUN=stat.desc, data=stimuli[stimuli$position=='target',])

# RESULTS: The overall imageability of the stimuli is slightly above average according 
# to the perceptual strength index, whereas it is below average based on modality 
# exclusivity. All three measures are highly similar across conditions.


# OTHER VARIABLES
# Figures for the continuous IVS for each of the three critical conditions

summary = data.frame(summaryBy(prop_letters +conc_letters +prop_lg10CD +conc_lg10CD 
	+ prop_orthneigh +conc_orthneigh +prop_exclusivity 
	+ conc_exclusivity +LSA_distance ~ Condition,
	stimuli[stimuli$position=='target',], FUN=list(mean, sd), na.rm=TRUE) )

summary[,-1] = round(summary[,-1], 2)

summary


# Last, true and false trials per condition
summary(stimuli[stimuli$Condition=='visual2visual', c('solution')])
summary(stimuli[stimuli$Condition=='haptic2visual', c('solution')])
summary(stimuli[stimuli$Condition=='auditory2visual', c('solution')])

# and for target items only:
summary(stimuli[stimuli$position=='target' & stimuli$Condition=='visual2visual', 'solution'])
summary(stimuli[stimuli$position=='target' & stimuli$Condition=='haptic2visual', 'solution'])
summary(stimuli[stimuli$position=='target' & stimuli$Condition=='auditory2visual', 'solution'])
