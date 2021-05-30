
setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/RTs and accuracy')

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
library(pastecs)


beh <- read.csv('beh.csv')
str(beh)

# There's a bizarre bug with NA participants. Solve:
beh = beh[!is.na(beh$Ptp),]

# Reorder Condition levels
beh$Condition <- factor(beh$Condition, levels = c('visual2visual', 'haptic2visual', 'auditory2visual'))

# Two factor variables will be used as continuous for particular analyses:
beh$trial = as.numeric(beh$Trial)
beh$accuracy = ifelse(beh$Accuracy == 'rightanswer', '1', NA)
beh$accuracy = ifelse(beh$Accuracy == 'wronganswer', '0', beh$accuracy)
beh$accuracy = as.numeric(beh$accuracy)


# Check mean accuracy per participant. Numbers 1 and 0 for right and wrong
# respectively. Therefore, the mean is equal to an accuracy percentage.

by(beh$accuracy, beh$Ptp, FUN = mean, na.rm=T)

accuracy.rate = rep(NA, length(levels(as.factor(beh$Ptp))) * length(levels(as.factor(beh$Condition))))

for(iPtp in levels(as.factor(beh$Ptp))) {
   for(iCond in levels(as.factor(beh$Condition))) {

beh[beh$Ptp==iPtp & beh$Condition==iCond, 'accuracy.rate'] = 
   by(beh$accuracy, list(beh$Ptp, beh$Condition), FUN=mean, na.rm=T)[[iPtp,iCond]][1]

} }


range(unique(beh$accuracy.rate))
mean(unique(beh$accuracy.rate))
sd(unique(beh$accuracy.rate))

summaryBy(accuracy.rate ~ Condition, data=beh, FUN=range, na.rm=T)
summaryBy(accuracy.rate ~ Condition, data=beh, FUN=mean, na.rm=T)
summaryBy(accuracy.rate ~ Condition, data=beh, FUN=sd, na.rm=T)

# See these stats again with RT.based_Groups further below




summary(beh$accuracy, data= beh)
ord = summaryBy(accuracy ~ Ptp,
data= beh, FUN = mean, na.rm=T) # Mean 62% / min 37% / max 71% / median 62%
range(ord$accuracy.mean)  # 50% accuracy seems reasonable outlier cut-off
median(ord$accuracy.mean) # (cf. cut-off 70% in Lynott & Connell 09, where
				  # relationships were more clearly true/false).
     				  # The point is to reduce risk of participants who
				  # wouldn't pay enough attention to the experiment.
				  # That would affect the critical RT & ERP measures.
				  # With 50% cut-off, ONE PARTICIPANT (nr 33) FALLS:



########   IMPORTANT   ########

# Participant 33 will be removed from both RT and ERP data. #
beh = beh[!beh$Ptp=='33',]

#################################




# Check accuracy again without the removed participant:

range(unique(beh$accuracy.rate))
mean(unique(beh$accuracy.rate))
sd(unique(beh$accuracy.rate))



summary(beh$accuracy, data= beh)
SD(beh$accuracy)
ord = summaryBy(accuracy ~ Ptp, data= beh, FUN = mean, na.rm=T)
range(ord$accuracy.mean)
median(ord$accuracy.mean)   # mean 63% / min 54% / max 71% / median 63%




# Is the accuracy significantly different from 50%? (See further below for each group, Quick and Slow)

str(beh$accuracy)
beh_aggregated = aggregate(accuracy ~ Ptp, beh, FUN=mean)
mean(beh_aggregated$accuracy, na.rm=TRUE)

result <- t.test(beh_aggregated$accuracy, mu = .5)

result	# p < 2.2e-16




# See figures excluding the participant who was removed from ERP data

summary(beh[!beh$Ptp=='7',]$accuracy, data= beh[!beh$Ptp=='7',])
SD(beh[!beh$Ptp=='7',]$accuracy)
ord = summaryBy(accuracy ~ Ptp, data= beh[!beh$Ptp=='7',], FUN = mean, na.rm=T)
range(ord$accuracy.mean)
median(ord$accuracy.mean)   # mean 63%


# Accuracy for the original Quick, Self-paced and Null groups

by(beh$accuracy, beh$Group, FUN = describe)




# Compare proportion correct/incorrect through trials = Any improvement?

by(beh$accuracy, beh$Trial, FUN = mean) 	# No learning through trials
rcor.test(beh[, c('accuracy', 'Trial')], use = 'complete.obs')

# No improvement over trials



# With another method

by(beh$trial, beh$accuracy, FUN = describe)

beh$trial.section = NA
beh$trial.section = ifelse(as.numeric(beh$trial)<=108, 'first.half', NA)
beh$trial.section = ifelse(as.numeric(beh$trial)>=109, 'second.half', beh$trial.section)
beh$trial.section = as.factor(beh$trial.section)

by(beh$accuracy, beh$trial.section, FUN = describe)

# Same result: no improvement over trials



# t-test for significance
t_test <- t.test(Trial ~ accuracy, data = beh, var.equal = FALSE)
t_test
# RES: participants' accuracy is about the same in earlier and later trials




# Mean RT per participant, specifically from target, responded trials only:
by(beh[beh$Position=='target' & !beh$Accuracy=='','RT'], beh[beh$Position=='target' &
 !beh$Accuracy=='','Ptp'], FUN = mean)
# This measure is used in the analysis of ERPs.

# Further descriptives: RANGE of Mean RT per participant per group:
RTpp = summaryBy(RT ~ list(Ptp, Group), beh, FUN=summary)
range(RTpp[RTpp$Group=='Quick', 'RT.Mean'])
range(RTpp[RTpp$Group=='Selfpaced', 'RT.Mean'])
plot(RT.Mean ~ Group, data=RTpp, ylab='RT', main='Mean RT per participant')

# Jitter plot:
ggplot(beh,
  aes(x = Group, y = RT)) +
  geom_jitter() +
  labs(
    x = "Group",
    y = "RT")


# Most general: mean RT per group
by(beh$RT, beh$Group, FUN = mean)
str(beh)
# check significance. averaging across trials first:
beh0 = aggregate(beh$RT, list(beh$Condition,beh$Ptp, beh$Group), data=beh, FUN=mean)
str(beh0)
t_test <- t.test(x ~ Group.3, data = beh0, 
var.equal = F)
t_test
#    #    #    #    #
# IMPORTANT: as in the pretest, Quick group turns out hardly any faster, albeit
# significant. It will prove insufficient in the sharper mixed effects model. 

# CONCLUSION: Group manipulation doesn't seem to have worked. Too close 
# groups, and too much variation among the Self-paced. As participants 
# are well used to experiments, the Self-paced may have been fallen back 
# on the more common requirement for speeded responses, or simply might 
# have wanted to finish the experiment as soon as possible. In order to
# solve this, create two participant groups based on each participant's 
# mean RT, as computed above based on target trials that were responded.

# Add each participant's mean RT
beh['MRT'] = NA
beh$MRT = as.numeric(beh$MRT)
beh$MRT = ifelse(beh$Ptp == '1', 753.0556, NA)
beh$MRT = ifelse(beh$Ptp == '2', 768.8889, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '3', 714.75, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '4', 784.3519, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '5', 1031.639, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '6', 835.1296, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '7', 791.0648, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '8', 591.4167, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '9', 1282.704, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '10', 583.5278, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '11', 872.3458, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '12', 557.5556, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '13', 572.4444, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '14', 738.2963, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '15', 755.4722, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '16', 871.2222, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '17', 514.1667, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '18', 515.9815, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '19', 1954.12, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '20', 582.5278, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '21', 737.5185, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '22', 963.5093, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '23', 1138.787, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '24', 791.5926, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '26', 375.463, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '27', 495.7493, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '28', 670.0741, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '29', 811.4057, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '30', 725.4766, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '31', 480.5185, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '32', 571.451, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '34', 727.537, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '35', 898.4167, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '36', 436.0833, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '37', 1206.509, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '38', 634.3056, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '39', 1034.185, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '40', 345.1759, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '41', 868.8426, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '42', 760.9444, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '44', 430.3925, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '45', 969.3611, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '46', 680.0185, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '47', 685.5926, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '48', 565.9815, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '49', 651.6389, beh$MRT)
beh$MRT = ifelse(beh$Ptp == '50', 692.787, beh$MRT)

# Compute two RT-based groups based on each participant's mean RT
transitory <- beh[, c('Ptp','MRT')]
transitory$RT.based_Groups <- as.numeric(cut2(transitory$MRT, g=2))
keeps <- c('Ptp','RT.based_Groups')
transitory = transitory[ , names(transitory) %in% keeps]
transitory = aggregate(RT.based_Groups ~ Ptp, transitory, mean, na.action = na.pass)
beh.test = merge(beh,transitory,by='Ptp', all=T)
beh = beh.test
beh$RT.based_Groups <- as.factor(beh$RT.based_Groups)
beh$RT.based_Groups <- revalue(beh$RT.based_Groups, c("1"="Quick", "2"="Slow"))

# double-check number of participants in each group
unique(beh[beh$RT.based_Groups=='Quick',]$Ptp)	# 24
unique(beh[beh$RT.based_Groups=='Slow',]$Ptp)	# 23

# ! Note that these figures are slightly different in the EEG data because, therein,
# participant '7' was removed (poor EEG signal).

# Mean RT per group, excluding the participant removed from ERP data
summaryBy(MRT ~ RT.based_Groups, data=beh[!beh$Ptp=='7',], FUN=c(mean, SD))






# Check accuracy again with these RT-based groups

summaryBy(accuracy.rate, data=aggregate(accuracy.rate~Ptp,data=beh,FUN=mean), FUN=range, na.rm=T)
summaryBy(accuracy.rate, data=beh, FUN=mean, na.rm=T)
summaryBy(accuracy.rate, data=beh, FUN=sd, na.rm=T)


summaryBy(accuracy.rate ~ c(RT.based_Groups, Condition), data=beh, FUN=range, na.rm=T)
summaryBy(accuracy.rate ~ c(RT.based_Groups, Condition), data=beh, FUN=mean, na.rm=T)
summaryBy(accuracy.rate ~ c(RT.based_Groups, Condition), data=beh, FUN=sd, na.rm=T)


# Plot

mu = ddply(beh, "RT.based_Groups", summarise, grp.mean=mean(accuracy.rate))

RT.based_Groups = c('Quick', 'Slow')

group_mean =
c( summaryBy(accuracy.rate ~ RT.based_Groups, data = beh, FUN = mean, na.rm=T)[[1]][1],
   summaryBy(accuracy.rate ~ RT.based_Groups, data = beh, FUN = mean, na.rm=T)[[2]][1] )



unique(factor(beh[beh$RT.based_Groups=='Slow', 'MRT']))
unique(beh[beh$RT.based_Groups=='Slow', 'MRT'])

mu = data.frame(RT.based_Groups, group_mean)

ggplot(beh, aes(x=accuracy.rate)) +
  geom_histogram(binwidth=.01, fill="white", position="dodge") +
  geom_vline(data=mu, aes(xintercept=RT.based_Groups, color=RT.based_Groups),
  linetype="dashed") + theme(legend.position="top")





# Quick group

beh_Quick_aggregated = aggregate(accuracy ~ Ptp, beh[beh$RT.based_Groups=='Quick',], FUN=mean)
mean(beh_Quick_aggregated$accuracy, na.rm=TRUE)

result <- t.test(beh_Quick_aggregated$accuracy, mu = .5)

result	# p = 1.123e-13


# Slow group

beh_Slow_aggregated = aggregate(accuracy ~ Ptp, beh[beh$RT.based_Groups=='Slow',], FUN=mean)
mean(beh_Slow_aggregated$accuracy, na.rm=TRUE)

result <- t.test(beh_Slow_aggregated$accuracy, mu = .5)

result	# p = 5.957e-15


# Accuracy in both Quick and Slow groups is significantly different higher than 50%.



# Accuracy per group excluding the participant removed from ERP data
by(beh[!beh$Ptp=='7',]$accuracy, beh[!beh$Ptp=='7',]$RT.based_Groups, FUN = describe)

# Accuracy per group and switch condition, excluding the participant removed from ERP data
by(beh[!beh$Ptp=='7',]$accuracy, list(beh[!beh$Ptp=='7',]$RT.based_Groups, beh[!beh$Ptp=='7',]$Condition), FUN = describe)





# Add participants' age in months
beh['Age_months'] = NA
beh$Age_months = as.numeric(beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '1', 274.5172, NA)
beh$Age_months = ifelse(beh$Ptp == '2', 221.2581, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '3', 250.6839, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '4', 226.3226, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '5', 290.6172, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '6', 242.8505, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '7', 241.6172, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '8', 261.3871, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '9', 297.9505, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '10', 373.4839, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '11', 243.7505, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '12', 240.9172, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '13', 225.3871, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '14', 295.0323, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '15', 237.4516, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '16', 236.4516, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '17', 235.2258, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '18', 280.0323, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '19', 290.4194, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '20', 236.6172, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '21', 265.4505, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '22', 236.7505, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '23', 292.8172, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '24', 239.3226, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '26', 244.4839, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '27', 284.3226, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '28', 288.6505, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '29', 300.4839, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '30', 225.0645, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '31', 276.5172, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '32', 304.0323, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '34', 237.9172, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '35', 256.7505, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '36', 269.7505, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '37', 285.3548, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '38', 305.6505, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '39', 259.4839, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '40', 261.6505, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '41', 245.6172, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '42', 256.3548, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '44', 272.4194, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '45', 253.0645, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '46', 238.3226, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '47', 276.8172, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '48', 253.7505, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '49', 343.2903, beh$Age_months)
beh$Age_months = ifelse(beh$Ptp == '50', 363.7505, beh$Age_months)

# General age range in years, excluding the participant removed from ERP data
summary(beh[!beh$Ptp=='7',]$Age_months/12, FUN=range)

# General age mean in years, excluding the participant removed from ERP data
summary(beh[!beh$Ptp=='7',]$Age_months/12, FUN=mean)

# Age figures per group, excluding the participant removed from ERP data
# Age range in each group in years
summaryBy(Age_months/12 ~ RT.based_Groups, beh[!beh$Ptp=='7',], FUN=range)

# Average age in each group in years, excluding the participant removed from ERP data
summaryBy(Age_months/12 ~ RT.based_Groups, beh[!beh$Ptp=='7',], FUN=mean) 	

# Distribution above and below age average in each group, excluding the participant removed from ERP data
str(unique(factor(beh[!beh$Ptp=='7',][c(beh[!beh$Ptp=='7',]$Age_months < mean(beh[!beh$Ptp=='7',]$Age_months) & 
beh[!beh$Ptp=='7',]$RT.based_Groups=='Quick'), 'Ptp']))) 		
str(unique(factor(beh[!beh$Ptp=='7',][c(beh[!beh$Ptp=='7',]$Age_months > mean(beh[!beh$Ptp=='7',]$Age_months) & 
beh[!beh$Ptp=='7',]$RT.based_Groups=='Quick'), 'Ptp']))) 		
str(unique(factor(beh[!beh$Ptp=='7',][c(beh[!beh$Ptp=='7',]$Age_months < mean(beh[!beh$Ptp=='7',]$Age_months) & 
beh[!beh$Ptp=='7',]$RT.based_Groups=='Slow'), 'Ptp']))) 	
str(unique(factor(beh[!beh$Ptp=='7',][c(beh[!beh$Ptp=='7',]$Age_months > mean(beh[!beh$Ptp=='7',]$Age_months) & 
beh[!beh$Ptp=='7',]$RT.based_Groups=='Slow'), 'Ptp'])))

# Age will be controlled for in the statistical models.


# Participant gender and handedness
table(beh$RT.based_Groups, beh$Gender)/216
table(beh$RT.based_Groups, beh$Lefthanded)/216

############################################################




# TARGET, RESPONDED TRIALS ONLY will be analyzed hereon

targetbeh_responded <- beh[c(beh$Position == 'target' & !beh$Accuracy==''),]
str(targetbeh_responded)

# Ensure Participant and Trial vars as factors

targetbeh_responded$Ptp <- as.factor(targetbeh_responded$Ptp)
targetbeh_responded$Trial <- as.factor(targetbeh_responded$Trial)

# Remove outlier trials, i.e., any falling more than three standard 
# deviations away from a participant's mean for a given condition.

str(targetbeh_responded)
targetbeh_responded_OUT = targetbeh_responded[!c(targetbeh_responded$Ptp=='1' & targetbeh_responded$Condition=='auditory2visual' & targetbeh_responded$RT > c(mean(targetbeh_responded[targetbeh_responded$Ptp=='1' & targetbeh_responded$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded[targetbeh_responded$Ptp=='1' & targetbeh_responded$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='1' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='1' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='1' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='1' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='1' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='1' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='2' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='2' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='2' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='2' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='2' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='2' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='2' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='2' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='2' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='3' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='3' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='3' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='3' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='3' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='3' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='3' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='3' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='3' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='4' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='4' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='4' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='4' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='4' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='4' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='4' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='4' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='4' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='5' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='5' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='5' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='5' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='5' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='5' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='5' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='5' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='5' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='6' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='6' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='6' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='6' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='6' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='6' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='6' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='6' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='6' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='7' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='7' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='7' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='7' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='7' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='7' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='7' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='7' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='7' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='8' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='8' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='8' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='8' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='8' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='8' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='8' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='8' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='8' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='9' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='9' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='9' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='9' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='9' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='9' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='9' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='9' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='9' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='10' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='10' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='10' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='10' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='10' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='10' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='10' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='10' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='10' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='11' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='11' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='11' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='11' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='11' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='11' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='11' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='11' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='11' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='12' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='12' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='12' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='12' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='12' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='12' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='12' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='12' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='12' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='13' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='13' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='13' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='13' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='13' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='13' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='13' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='13' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='13' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='14' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='14' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='14' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='14' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='14' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='14' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='14' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='14' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='14' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='15' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='15' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='15' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='15' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='15' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='15' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='15' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='15' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='15' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='16' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='16' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='16' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='16' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='16' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='16' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='16' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='16' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='16' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='17' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='17' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='17' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='17' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='17' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='17' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='17' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='17' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='17' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='18' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='18' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='18' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='18' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='18' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='18' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='18' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='18' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='18' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='19' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='19' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='19' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='19' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='19' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='19' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='19' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='19' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='19' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='20' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='20' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='20' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='20' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='20' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='20' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='20' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='20' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='20' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='21' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='21' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='21' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='21' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='21' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='21' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='21' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='21' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='21' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='22' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='22' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='22' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='22' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='22' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='22' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='22' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='22' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='22' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='23' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='23' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='23' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='23' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='23' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='23' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='23' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='23' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='23' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='24' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='24' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='24' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='24' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='24' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='24' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='24' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='24' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='24' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='26' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='26' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='26' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='26' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='26' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='26' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='26' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='26' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='26' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='27' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='27' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='27' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='27' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='27' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='27' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='27' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='27' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='27' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='28' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='28' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='28' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='28' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='28' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='28' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='28' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='28' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='28' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='29' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='29' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='29' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='29' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='29' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='29' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='29' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='29' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='29' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='30' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='30' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='30' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='30' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='30' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='30' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='30' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='30' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='30' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='31' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='31' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='31' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='31' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='31' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='31' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='31' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='31' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='31' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='32' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='32' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='32' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='32' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='32' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='32' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='32' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='32' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='32' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='34' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='34' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='34' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='34' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='34' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='34' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='34' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='34' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='34' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='35' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='35' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='35' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='35' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='35' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='35' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='35' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='35' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='35' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='36' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='36' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='36' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='36' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='36' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='36' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='36' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='36' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='36' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='37' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='37' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='37' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='37' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='37' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='37' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='37' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='37' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='37' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='38' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='38' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='38' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='38' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='38' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='38' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='38' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='38' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='38' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='39' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='39' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='39' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='39' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='39' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='39' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='39' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='39' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='39' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='40' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='40' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='40' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='40' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='40' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='40' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='40' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='40' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='40' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='41' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='41' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='41' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='41' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='41' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='41' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='41' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='41' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='41' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='42' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='42' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='42' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='42' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='42' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='42' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='42' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='42' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='42' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='44' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='44' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='44' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='44' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='44' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='44' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='44' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='44' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='44' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='45' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='45' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='45' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='45' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='45' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='45' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='45' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='45' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='45' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='46' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='46' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='46' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='46' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='46' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='46' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='46' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='46' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='46' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='47' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='47' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='47' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='47' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='47' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='47' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='47' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='47' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='47' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='48' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='48' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='48' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='48' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='48' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='48' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='48' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='48' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='48' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='49' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='49' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='49' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='49' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='49' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='49' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='49' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='49' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='49' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='50' & targetbeh_responded_OUT$Condition=='auditory2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='50' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='50' & targetbeh_responded_OUT$Condition=='auditory2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='50' & targetbeh_responded_OUT$Condition=='haptic2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='50' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='50' & targetbeh_responded_OUT$Condition=='haptic2visual', 'RT']))),]
targetbeh_responded_OUT = targetbeh_responded_OUT[!c(targetbeh_responded_OUT$Ptp=='50' & targetbeh_responded_OUT$Condition=='visual2visual' & targetbeh_responded_OUT$RT > c(mean(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='50' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']) + 3*sd(targetbeh_responded_OUT[targetbeh_responded_OUT$Ptp=='50' & targetbeh_responded_OUT$Condition=='visual2visual', 'RT']))),]

# Percentage of outlier trials dropped
((nrow(targetbeh_responded) - nrow(targetbeh_responded_OUT)) / nrow(targetbeh_responded)) * 100
# = 1.28%

# Check group RTs again:
ggplot(targetbeh_responded_OUT,
  aes(x = RT.based_Groups, y = RT)) +
  geom_jitter() +
  labs(
    x = "RT.based_Groups",
    y = "RT")

# Critical descriptives per condition
summaryBy(RT ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, FUN= stat.desc)

# Rename condition levels for plot below
targetbeh_responded_OUT$ConditionRe <- gsub('visual2visual', ' Visual to Visual', targetbeh_responded_OUT$Condition)
targetbeh_responded_OUT$ConditionRe <- gsub('haptic2visual', ' Haptic to Visual', targetbeh_responded_OUT$ConditionRe)
targetbeh_responded_OUT$ConditionRe <- gsub('auditory2visual', ' Auditory to Visual', targetbeh_responded_OUT$ConditionRe)

# Plot:
RTplot= ggplot(targetbeh_responded_OUT,
  aes(x = ConditionRe, y = RT)) +
  geom_jitter(aes(colour = RT.based_Groups, shape = RT.based_Groups)) +
  labs(
    x = "Context / Target trial",
    y = "RT",
    colour = "RT-based groups",
    shape = "RT-based groups"  )
png(file="RT plot.png", units="in", width=8, height=6, res=500)
plot(RTplot)
dev.off()

# Accuracy per condition
summaryBy(accuracy ~ Condition, targetbeh_responded_OUT, FUN= c(mean, sd))

# per group and condition
summaryBy(accuracy ~ c(RT.based_Groups, Condition), targetbeh_responded_OUT, FUN= c(mean, sd))

# ANOVA
fit = aov(accuracy ~ RT.based_Groups * Ptp * trial * Condition, targetbeh_responded_OUT)
leveneTest(targetbeh_responded_OUT$accuracy, targetbeh_responded_OUT$Condition)
# Levene's test is significant, indicating heterogenous variances. However, Hartley's
# Fmax ratio attests for homogenous variances (cf. Fmax critical values for a
# sample size over 60). Therefore we can conclude homogenous variances (see
# Field, Miles, & Field, 2012).
plot(fit)
summary(fit)
drop1(fit,~.,test="F")  # drop1 is a similar alternative to classical ANOVA
# Sig differences in accuracy across conditions but it won't be in the LME model below


# SEMANTIC DISTANCE BETWEEN THE PROPERTY AND THE CONCEPT IN EACH TRIAL
# One of the main enhancements of the present experiment is the cancellation of
# the confound of semantic distance. This distance regards the relation between
# the property and the concept in each trial. By time-locking ERPs to the first
# word of target trials, that simply doesn't exist. However, for the RTs, this
# still matters. We gathered the semantic (LSA) distance from a recent corpus
# published by Mandera, Keuleers, and Brysbaert (2017). This will allow us to
# control for this variable in the statistical models. Yet, in addition, we
# tested any statistical differences across conditions. Note that this is only
# informative at an exploratory level. Mixed models will tell.

by(targetbeh_responded_OUT$LSA_distance, targetbeh_responded_OUT$Condition, 
FUN = summary)
# ANOVA
fit <- aov(LSA_distance ~ Condition * Item * Ptp, data = targetbeh_responded_OUT)
leveneTest(targetbeh_responded_OUT$LSA_distance, targetbeh_responded_OUT$Condition)
# Levene's test is significant, indicating heterogenous variances. However, Hartley's
# Fmax ratio attests for homogenous variances (cf. Fmax critical values for a
# sample size over 60). Therefore we can conclude homogenous variances (see
# Field, Miles, & Field, 2012).
plot(fit)
summary(fit)
drop1(fit,~.,test="F")  # drop1 is a similar alternative to classical ANOVA

# RES: Sig differences in semantic distance across conditions. Mixed models will control for it.


# Now similarly, check modality exclusivity across conditions. Same cautions!

by(targetbeh_responded_OUT$mean_exc, targetbeh_responded_OUT$Condition, FUN = summary)

# Properties+Concepts excl
fit <- aov(mean_exc ~ Condition * Item * Ptp, data=targetbeh_responded_OUT)
leveneTest(targetbeh_responded_OUT$mean_exc, targetbeh_responded_OUT$Condition)
# Levene's test is not significant. Good: homogenous variances.
plot(fit)
summary(fit)
drop1(fit,~.,test="F")    # drop1 is a similar alternative to classical ANOVA
# RES: Sig differences in mean excl across conditions. Mixed models will control for it.

###########################################################################

# Drop inexistent levels from factors
targetbeh_responded_OUT$Gender = factor(targetbeh_responded_OUT$Gender)
targetbeh_responded_OUT$A_V_OK = factor(targetbeh_responded_OUT$A_V_OK)
targetbeh_responded_OUT$Lefthanded = factor(targetbeh_responded_OUT$Lefthanded)
targetbeh_responded_OUT$Condition = factor(targetbeh_responded_OUT$Condition)


# save
saveRDS(targetbeh_responded_OUT, 'targetbeh_responded_OUT.rds')

# read in
targetbeh_responded_OUT = readRDS('targetbeh_responded_OUT.rds')
str(targetbeh_responded_OUT)


#################################################################################





# Question: Is each participant using one system or two systems with different speeds?
# Frequency distribution of RTs per participant (histograms). Thank you to 
# Alexis Perez-Bellido for suggesting this analysis.

plot = ggplot(targetbeh_responded_OUT, aes(x=RT)) + geom_histogram(bins = 15) + facet_wrap(~Ptp, nrow=8) + theme_bw()

png(file="RT frequencies per participant.png", units="in", width=14, height=8, res=500)
plot(plot)
dev.off()

# Figure does not suggest two processing systems per participant, as the distributions are unimodal.
