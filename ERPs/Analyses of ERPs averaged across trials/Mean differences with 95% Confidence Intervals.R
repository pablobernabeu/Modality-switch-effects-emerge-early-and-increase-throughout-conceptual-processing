
setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Export Files')

# This script contains: 	Two functions for mean differences with confidence intervals, tables with such mean differences,
#					and finally basic descriptive tables. Data is duly aggregated at participant level (n=23, n=23).

install.packages('metafor')
install.packages('doBy')
install.packages('magrittr')
install.packages('knitr')
install.packages('Rmisc')
install.packages('tidyr')
library(metafor)
library(doBy)
library(magrittr)
library(knitr)
library(Rmisc)
library(tidyr)

# Read in key objects
EEG.window1 = readRDS('EEG.window1.rds')
EEG.window2 = readRDS('EEG.window2.rds')
EEG.window3 = readRDS('EEG.window3.rds')
EEG.window4 = readRDS('EEG.window4.rds')

# Aggregating data to meet assumption of independence
EEG.window1 = aggregate(EEG.window1$microvolts, list(EEG.window1$RT.based_Groups, EEG.window1$location,
			EEG.window1$condition, EEG.window1$participant), data=EEG.window1, FUN=mean)
EEG.window2 = aggregate(EEG.window2$microvolts, list(EEG.window2$RT.based_Groups, EEG.window2$location,
			EEG.window2$condition, EEG.window2$participant), data=EEG.window2, FUN=mean)
EEG.window3 = aggregate(EEG.window3$microvolts, list(EEG.window3$RT.based_Groups, EEG.window3$location,
			EEG.window3$condition, EEG.window3$participant), data=EEG.window3, FUN=mean)
EEG.window4 = aggregate(EEG.window4$microvolts, list(EEG.window4$RT.based_Groups, EEG.window4$location,
			EEG.window4$condition, EEG.window4$participant), data=EEG.window4, FUN=mean)
str(EEG.window1)
str(EEG.window2)
str(EEG.window3)
str(EEG.window4)	# 276 observations = 46 participants * 2 brain areas * 3 conditions

names(EEG.window1)[1] = 'RT.based_Groups'
names(EEG.window1)[2] = 'location'
names(EEG.window1)[3] = 'condition'
names(EEG.window1)[4] = 'participant'
names(EEG.window1)[5] = 'microvolts'
names(EEG.window2)[1] = 'RT.based_Groups'
names(EEG.window2)[2] = 'location'
names(EEG.window2)[3] = 'condition'
names(EEG.window2)[4] = 'participant'
names(EEG.window2)[5] = 'microvolts'
names(EEG.window3)[1] = 'RT.based_Groups'
names(EEG.window3)[2] = 'location'
names(EEG.window3)[3] = 'condition'
names(EEG.window3)[4] = 'participant'
names(EEG.window3)[5] = 'microvolts'
names(EEG.window4)[1] = 'RT.based_Groups'
names(EEG.window4)[2] = 'location'
names(EEG.window4)[3] = 'condition'
names(EEG.window4)[4] = 'participant'
names(EEG.window4)[5] = 'microvolts'

# Rename conditions presentable
EEG.window1$condition <- gsub('visual2visual', 'Visual_to_visual', EEG.window1$condition)
EEG.window1$condition <- gsub('haptic2visual', 'Haptic_to_visual', EEG.window1$condition)
EEG.window1$condition <- gsub('auditory2visual', 'Auditory_to_visual', EEG.window1$condition)
EEG.window2$condition <- gsub('visual2visual', 'Visual_to_visual', EEG.window2$condition)
EEG.window2$condition <- gsub('haptic2visual', 'Haptic_to_visual', EEG.window2$condition)
EEG.window2$condition <- gsub('auditory2visual', 'Auditory_to_visual', EEG.window2$condition)
EEG.window3$condition <- gsub('visual2visual', 'Visual_to_visual', EEG.window3$condition)
EEG.window3$condition <- gsub('haptic2visual', 'Haptic_to_visual', EEG.window3$condition)
EEG.window3$condition <- gsub('auditory2visual', 'Auditory_to_visual', EEG.window3$condition)
EEG.window4$condition <- gsub('visual2visual', 'Visual_to_visual', EEG.window4$condition)
EEG.window4$condition <- gsub('haptic2visual', 'Haptic_to_visual', EEG.window4$condition)
EEG.window4$condition <- gsub('auditory2visual', 'Auditory_to_visual', EEG.window4$condition)


# 95% Confidence Intervals for absolute switch effect (i.e., visual_to_visual condition v. both switch conditions)

mean_difference_generalswitch = function(timewindow, group, brain_area){

# No need for quotes in function input
timewindow = substitute(timewindow)
timewindow = as.character(timewindow)
group = substitute(group)
group = as.character(group)
brain_area = substitute(brain_area)
brain_area = as.character(brain_area)

title = paste0('No-switch v. both switch conditions, ', timewindow, ', ', group, ', ', brain_area)

if(timewindow == 1){
# No-switch condition (aka baseline)
a = EEG.window1[EEG.window1$RT.based_Groups==group &
		EEG.window1$location==brain_area &
		EEG.window1$condition=='Visual_to_visual', 'microvolts']

# Both switch conditions averaged
b = EEG.window1[EEG.window1$RT.based_Groups==group &
		EEG.window1$location==brain_area &
		EEG.window1$condition=='Auditory_to_visual', 'microvolts'] +
	EEG.window1[EEG.window1$RT.based_Groups==group &
		EEG.window1$location==brain_area &
		EEG.window1$condition=='Haptic_to_visual', 'microvolts'] / 2  # average switch conditions
}

if(timewindow == 2){
# No-switch condition (aka baseline)
a = EEG.window2[EEG.window2$RT.based_Groups==group &
		EEG.window2$location==brain_area &
		EEG.window2$condition=='Visual_to_visual', 'microvolts']

# Both switch conditions averaged
b = EEG.window2[EEG.window2$RT.based_Groups==group &
		EEG.window2$location==brain_area &
		EEG.window2$condition=='Auditory_to_visual', 'microvolts'] +
	EEG.window2[EEG.window2$RT.based_Groups==group &
		EEG.window2$location==brain_area &
		EEG.window2$condition=='Haptic_to_visual', 'microvolts'] / 2  # average switch conditions
}

if(timewindow == 3){
# No-switch condition (aka baseline)
a = EEG.window3[EEG.window3$RT.based_Groups==group &
		EEG.window3$location==brain_area &
		EEG.window3$condition=='Visual_to_visual', 'microvolts']

# Both switch conditions averaged
b = EEG.window3[EEG.window3$RT.based_Groups==group &
		EEG.window3$location==brain_area &
		EEG.window3$condition=='Auditory_to_visual', 'microvolts'] +
	EEG.window3[EEG.window3$RT.based_Groups==group &
		EEG.window3$location==brain_area &
		EEG.window3$condition=='Auditory_to_visual', 'microvolts'] / 2  # average switch conditions
}

if(timewindow == 4){
# No-switch condition (aka baseline)
a = EEG.window4[EEG.window4$RT.based_Groups==group &
		EEG.window4$location==brain_area &
		EEG.window4$condition=='Visual_to_visual', 'microvolts']

# Both switch conditions averaged
b = EEG.window4[EEG.window4$RT.based_Groups==group &
		EEG.window4$location==brain_area &
		EEG.window4$condition=='Auditory_to_visual', 'microvolts'] +
	EEG.window4[EEG.window4$RT.based_Groups==group &
		EEG.window4$location==brain_area &
		EEG.window4$condition=='Auditory_to_visual', 'microvolts'] / 2  # average switch conditions
}

M = mean(a-b)
M = round(M, digits=4)

confint = summary(escalc(measure="SMCC", m1i=mean(a), sd1i=sd(a), m2i=mean(b), sd2i=sd(b), ni=length(a), ri=cor(a,b)))
names(confint)[5] = 'L 95% CI'
names(confint)[6] = 'U 95% CI'
confint = confint[c(5,6)]   # Leaving only CIs output
confint = round(confint, digits=4)

contains0 = function(x){
		if(x[1]<0 & x[2]>0){'Non-sig.'}
		else{'Sig.'}
}

Result = contains0(confint)
confint$Result = Result
names(confint)[3] = 'CI result'

return(noquote(unlist(list('Conditions compared, time window, group, brain area' = title, 'Diff. µV' = M, confint))))
}

# example
mean_difference_generalswitch(1, Quick, posterior)


#############################################################################



# 95% Confidence Intervals for difference of any two conditions

mean_difference = function(timewindow, group, brain_area, cond1, cond2){

# No need for quotes in function input
timewindow = substitute(timewindow)
timewindow = as.character(timewindow)
group = substitute(group)
group = as.character(group)
brain_area = substitute(brain_area)
brain_area = as.character(brain_area)
cond1 = substitute(cond1)
cond1 = as.character(cond1)
cond2 = substitute(cond2)
cond2 = as.character(cond2)

title = paste0(cond1, ' v. ', cond2, ', ', timewindow, ', ', group, ', ', brain_area)

if(timewindow == 1){
# No-switch condition (aka baseline)
a = EEG.window1[EEG.window1$RT.based_Groups==group &
		EEG.window1$location==brain_area &
		EEG.window1$condition==cond1, 'microvolts']

# Both switch conditions averaged
b = EEG.window1[EEG.window1$RT.based_Groups==group &
		EEG.window1$location==brain_area &
		EEG.window1$condition==cond2, 'microvolts']
}

if(timewindow == 2){
# No-switch condition (aka baseline)
a = EEG.window2[EEG.window2$RT.based_Groups==group &
		EEG.window2$location==brain_area &
		EEG.window2$condition==cond1, 'microvolts']

# Both switch conditions averaged
b = EEG.window2[EEG.window2$RT.based_Groups==group &
		EEG.window2$location==brain_area &
		EEG.window2$condition==cond2, 'microvolts']
}

if(timewindow == 3){
# No-switch condition (aka baseline)
a = EEG.window3[EEG.window3$RT.based_Groups==group &
		EEG.window3$location==brain_area &
		EEG.window3$condition==cond1, 'microvolts']

# Both switch conditions averaged
b = EEG.window3[EEG.window3$RT.based_Groups==group &
		EEG.window3$location==brain_area &
		EEG.window3$condition==cond2, 'microvolts']
}

if(timewindow == 4){
# No-switch condition (aka baseline)
a = EEG.window4[EEG.window4$RT.based_Groups==group &
		EEG.window4$location==brain_area &
		EEG.window4$condition==cond1, 'microvolts']

# Both switch conditions averaged
b = EEG.window4[EEG.window4$RT.based_Groups==group &
		EEG.window4$location==brain_area &
		EEG.window4$condition==cond2, 'microvolts']
}

M = mean(a-b)
M = round(M, digits=4)

confint = summary(escalc(measure="SMCC", m1i=mean(a), sd1i=sd(a), m2i=mean(b), sd2i=sd(b), ni=length(a), ri=cor(a,b)))
names(confint)[5] = 'L 95% CI'
names(confint)[6] = 'U 95% CI'
confint = confint[c(5,6)]   # Leaving only CIs output
confint = round(confint, digits=4)

contains0 = function(x){
		if(x[1]<0 & x[2]>0){'Non-sig.'}
		else{'Sig.'}
}

Result = contains0(confint)
confint$Result = Result
names(confint)[3] = 'CI result'

return(noquote(unlist(list('Conditions compared, time window, group, brain area' = title, 'Diff. µV' = M, confint))))
}

# example
mean_difference(4, Slow, posterior, Haptic_to_visual, Visual_to_visual)


###################################################################################



# USE

# GENERAL DIFFERENCE: No-switch condition v. the average of both switch conditions

as.data.frame(rbind(
# Time window 1, Quick group, anterior brain area
mean_difference_generalswitch(1, Quick, anterior),

# Time window 1, Quick group, posterior brain area
mean_difference_generalswitch(1, Quick, posterior),

# Time window 1, Slow group, anterior brain area
mean_difference_generalswitch(1, Slow, anterior),

# Time window 1, Slow group, posterior brain area
mean_difference_generalswitch(1, Slow, posterior),


# Time window 2, Quick group, anterior brain area
mean_difference_generalswitch(2, Quick, anterior),

# Time window 2, Quick group, posterior brain area
mean_difference_generalswitch(2, Quick, posterior),

# Time window 2, Slow group, anterior brain area
mean_difference_generalswitch(2, Slow, anterior),

# Time window 2, Slow group, posterior brain area
mean_difference_generalswitch(2, Slow, posterior),


# Time window 3, Quick group, anterior brain area
mean_difference_generalswitch(3, Quick, anterior),

# Time window 3, Quick group, posterior brain area
mean_difference_generalswitch(3, Quick, posterior),

# Time window 3, Slow group, anterior brain area
mean_difference_generalswitch(3, Slow, anterior),

# Time window 3, Slow group, posterior brain area
mean_difference_generalswitch(3, Slow, posterior),


# Time window 4, Quick group, anterior brain area
mean_difference_generalswitch(4, Quick, anterior),

# Time window 4, Quick group, posterior brain area
mean_difference_generalswitch(4, Quick, posterior),

# Time window 4, Slow group, anterior brain area
mean_difference_generalswitch(4, Slow, anterior),

# Time window 4, Slow group, posterior brain area
mean_difference_generalswitch(4, Slow, posterior),


##################################################################



# SPECIFIC DIFFERENCE: Visual_to_visual condition v. Auditory_to_visual condition 
# (i.e., the slightly stronger switch)

# Time window 1, Quick group, anterior brain area
mean_difference(1, Quick, anterior, Visual_to_visual, Auditory_to_visual),

# Time window 1, Quick group, posterior brain area
mean_difference(1, Quick, posterior, Visual_to_visual, Auditory_to_visual),

# Time window 1, Slow group, anterior brain area
mean_difference(1, Slow, anterior, Visual_to_visual, Auditory_to_visual),

# Time window 1, Slow group, posterior brain area
mean_difference(1, Slow, posterior, Visual_to_visual, Auditory_to_visual),


# Time window 2, Quick group, anterior brain area
mean_difference(2, Quick, anterior, Visual_to_visual, Auditory_to_visual),

# Time window 2, Quick group, posterior brain area
mean_difference(2, Quick, posterior, Visual_to_visual, Auditory_to_visual),

# Time window 2, Slow group, anterior brain area
mean_difference(2, Slow, anterior, Visual_to_visual, Auditory_to_visual),

# Time window 2, Slow group, posterior brain area
mean_difference(2, Slow, posterior, Visual_to_visual, Auditory_to_visual),


# Time window 3, Quick group, anterior brain area
mean_difference(3, Quick, anterior, Visual_to_visual, Auditory_to_visual),

# Time window 3, Quick group, posterior brain area
mean_difference(3, Quick, posterior, Visual_to_visual, Auditory_to_visual),

# Time window 3, Slow group, anterior brain area
mean_difference(3, Slow, anterior, Visual_to_visual, Auditory_to_visual),

# Time window 3, Slow group, posterior brain area
mean_difference(3, Slow, posterior, Visual_to_visual, Auditory_to_visual),


# Time window 4, Quick group, anterior brain area
mean_difference(4, Quick, anterior, Visual_to_visual, Auditory_to_visual),

# Time window 4, Quick group, posterior brain area
mean_difference(4, Quick, posterior, Visual_to_visual, Auditory_to_visual),

# Time window 4, Slow group, anterior brain area
mean_difference(4, Slow, anterior, Visual_to_visual, Auditory_to_visual),

# Time window 4, Slow group, posterior brain area
mean_difference(4, Slow, posterior, Visual_to_visual, Auditory_to_visual),

################################################################################



# SPECIFIC DIFFERENCE: Visual_to_visual condition v. Haptic_to_visual condition 
# (i.e., the slightly weaker switch)

# Time window 1, Quick group, anterior brain area
mean_difference(1, Quick, anterior, Visual_to_visual, Haptic_to_visual),

# Time window 1, Quick group, posterior brain area
mean_difference(1, Quick, posterior, Visual_to_visual, Haptic_to_visual),

# Time window 1, Slow group, anterior brain area
mean_difference(1, Slow, anterior, Visual_to_visual, Haptic_to_visual),

# Time window 1, Slow group, posterior brain area
mean_difference(1, Slow, posterior, Visual_to_visual, Haptic_to_visual),


# Time window 2, Quick group, anterior brain area
mean_difference(2, Quick, anterior, Visual_to_visual, Haptic_to_visual),

# Time window 2, Quick group, posterior brain area
mean_difference(2, Quick, posterior, Visual_to_visual, Haptic_to_visual),

# Time window 2, Slow group, anterior brain area
mean_difference(2, Slow, anterior, Visual_to_visual, Haptic_to_visual),

# Time window 2, Slow group, posterior brain area
mean_difference(2, Slow, posterior, Visual_to_visual, Haptic_to_visual),


# Time window 3, Quick group, anterior brain area
mean_difference(3, Quick, anterior, Visual_to_visual, Haptic_to_visual),

# Time window 3, Quick group, posterior brain area
mean_difference(3, Quick, posterior, Visual_to_visual, Haptic_to_visual),

# Time window 3, Slow group, anterior brain area
mean_difference(3, Slow, anterior, Visual_to_visual, Haptic_to_visual),

# Time window 3, Slow group, posterior brain area
mean_difference(3, Slow, posterior, Visual_to_visual, Haptic_to_visual),


# Time window 4, Quick group, anterior brain area
mean_difference(4, Quick, anterior, Visual_to_visual, Haptic_to_visual),

# Time window 4, Quick group, posterior brain area
mean_difference(4, Quick, posterior, Visual_to_visual, Haptic_to_visual),

# Time window 4, Slow group, anterior brain area
mean_difference(4, Slow, anterior, Visual_to_visual, Haptic_to_visual),

# Time window 4, Slow group, posterior brain area
mean_difference(4, Slow, posterior, Visual_to_visual, Haptic_to_visual),

#######################################################################



# SPECIFIC DIFFERENCE: Auditory_to_visual condition v. Haptic_to_visual condition
# (i.e, the two switch conditions)

# Time window 1, Quick group, anterior brain area
mean_difference(1, Quick, anterior, Auditory_to_visual, Haptic_to_visual),

# Time window 1, Quick group, posterior brain area
mean_difference(1, Quick, posterior, Auditory_to_visual, Haptic_to_visual),

# Time window 1, Slow group, anterior brain area
mean_difference(1, Slow, anterior, Auditory_to_visual, Haptic_to_visual),

# Time window 1, Slow group, posterior brain area
mean_difference(1, Slow, posterior, Auditory_to_visual, Haptic_to_visual),


# Time window 2, Quick group, anterior brain area
mean_difference(2, Quick, anterior, Auditory_to_visual, Haptic_to_visual),

# Time window 2, Quick group, posterior brain area
mean_difference(2, Quick, posterior, Auditory_to_visual, Haptic_to_visual),

# Time window 2, Slow group, anterior brain area
mean_difference(2, Slow, anterior, Auditory_to_visual, Haptic_to_visual),

# Time window 2, Slow group, posterior brain area
mean_difference(2, Slow, posterior, Auditory_to_visual, Haptic_to_visual),


# Time window 3, Quick group, anterior brain area
mean_difference(3, Quick, anterior, Auditory_to_visual, Haptic_to_visual),

# Time window 3, Quick group, posterior brain area
mean_difference(3, Quick, posterior, Auditory_to_visual, Haptic_to_visual),

# Time window 3, Slow group, anterior brain area
mean_difference(3, Slow, anterior, Auditory_to_visual, Haptic_to_visual),

# Time window 3, Slow group, posterior brain area
mean_difference(3, Slow, posterior, Auditory_to_visual, Haptic_to_visual),


# Time window 4, Quick group, anterior brain area
mean_difference(4, Quick, anterior, Auditory_to_visual, Haptic_to_visual),

# Time window 4, Quick group, posterior brain area
mean_difference(4, Quick, posterior, Auditory_to_visual, Haptic_to_visual),

# Time window 4, Slow group, anterior brain area
mean_difference(4, Slow, anterior, Auditory_to_visual, Haptic_to_visual),

# Time window 4, Slow group, posterior brain area
mean_difference(4, Slow, posterior, Auditory_to_visual, Haptic_to_visual)
))	%>% kable()	# makes table

##############################################################################



# Basic descriptive tables for each data section

# WINDOW 1
saver = merge(summaryBy(microvolts ~ c(RT.based_Groups, location, condition), FUN=sd, EEG.window1),
		summarySE(data=EEG.window1, measurevar="microvolts", 
		groupvars=c('RT.based_Groups','location','condition'),    
		conf.interval = 0.95)[c(1,2,3,4)])
# +1 merge
saver = merge(saver, group.CI(microvolts ~ RT.based_Groups+location+condition, EEG.window1, ci=0.95))

# tidying up
names(saver)[1] = 'Group'
names(saver)[2] = 'Brain area'
names(saver)[3] = 'Switch condition'
names(saver)[4] = 'SD'
names(saver)[5] = 'n'
names(saver)[6] = 'U95%CI'
names(saver)[7] = 'Mean µV'
names(saver)[8] = 'L95%CI'
saver[4] = round(saver[4],digits=4)
saver[5] = round(saver[5],digits=4)
saver[6] = round(saver[6],digits=4)
saver[7] = round(saver[7],digits=4)
saver[8] = round(saver[8],digits=4)
saver$'Switch condition' = gsub('_', '-', saver$'Switch condition')
saver$Window = 1	# Specify time window
saver1 = saver[c('Window', 'Group', 'Brain area', 'Switch condition', 'n', 'Mean µV', 'SD', 'L95%CI', 'U95%CI')] # order

# WINDOW 2
saver = merge(summaryBy(microvolts ~ c(RT.based_Groups, location, condition), FUN=sd, EEG.window2),
		summarySE(data=EEG.window2, measurevar="microvolts", 
		groupvars=c('RT.based_Groups','location','condition'),    
		conf.interval = 0.95)[c(1,2,3,4)])
# +1 merge
saver = merge(saver, group.CI(microvolts ~ RT.based_Groups+location+condition, EEG.window2, ci=0.95))

# tidying up
names(saver)[1] = 'Group'
names(saver)[2] = 'Brain area'
names(saver)[3] = 'Switch condition'
names(saver)[4] = 'SD'
names(saver)[5] = 'n'
names(saver)[6] = 'U95%CI'
names(saver)[7] = 'Mean µV'
names(saver)[8] = 'L95%CI'
saver[4] = round(saver[4],digits=4)
saver[5] = round(saver[5],digits=4)
saver[6] = round(saver[6],digits=4)
saver[7] = round(saver[7],digits=4)
saver[8] = round(saver[8],digits=4)
saver$'Switch condition' = gsub('_', '-', saver$'Switch condition')
saver$Window = 3	# Specify time window
saver2 = saver[c('Window', 'Group', 'Brain area', 'Switch condition', 'n', 'Mean µV', 'SD', 'L95%CI', 'U95%CI')] # order

# WINDOW 3
saver = merge(summaryBy(microvolts ~ c(RT.based_Groups, location, condition), FUN=sd, EEG.window3),
		summarySE(data=EEG.window3, measurevar="microvolts", 
		groupvars=c('RT.based_Groups','location','condition'),    
		conf.interval = 0.95)[c(1,2,3,4)])
# +1 merge
saver = merge(saver, group.CI(microvolts ~ RT.based_Groups+location+condition, EEG.window3, ci=0.95))

# tidying up
names(saver)[1] = 'Group'
names(saver)[2] = 'Brain area'
names(saver)[3] = 'Switch condition'
names(saver)[4] = 'SD'
names(saver)[5] = 'n'
names(saver)[6] = 'U95%CI'
names(saver)[7] = 'Mean µV'
names(saver)[8] = 'L95%CI'
saver[4] = round(saver[4],digits=4)
saver[5] = round(saver[5],digits=4)
saver[6] = round(saver[6],digits=4)
saver[7] = round(saver[7],digits=4)
saver[8] = round(saver[8],digits=4)
saver$'Switch condition' = gsub('_', '-', saver$'Switch condition')
saver$Window = 3	# Specify time window
saver3 = saver[c('Window', 'Group', 'Brain area', 'Switch condition', 'n', 'Mean µV', 'SD', 'L95%CI', 'U95%CI')] # order

# WINDOW 4
saver = merge(summaryBy(microvolts ~ c(RT.based_Groups, location, condition), FUN=sd, EEG.window4),
		summarySE(data=EEG.window4, measurevar="microvolts", 
		groupvars=c('RT.based_Groups','location','condition'),    
		conf.interval = 0.95)[c(1,2,3,4)])
# +1 merge
saver = merge(saver, group.CI(microvolts ~ RT.based_Groups+location+condition, EEG.window4, ci=0.95))

# tidying up
names(saver)[1] = 'Group'
names(saver)[2] = 'Brain area'
names(saver)[3] = 'Switch condition'
names(saver)[4] = 'SD'
names(saver)[5] = 'n'
names(saver)[6] = 'U95%CI'
names(saver)[7] = 'Mean µV'
names(saver)[8] = 'L95%CI'
saver[4] = round(saver[4],digits=4)
saver[5] = round(saver[5],digits=4)
saver[6] = round(saver[6],digits=4)
saver[7] = round(saver[7],digits=4)
saver[8] = round(saver[8],digits=4)
saver$'Switch condition' = gsub('_', '-', saver$'Switch condition')
saver$Window = 4	# Specify time window
saver4 = saver[c('Window', 'Group', 'Brain area', 'Switch condition', 'n', 'Mean µV', 'SD', 'L95%CI', 'U95%CI')] # order

as.data.frame(rbind(saver1, saver2, saver3, saver4))	%>% kable()	# makes table

