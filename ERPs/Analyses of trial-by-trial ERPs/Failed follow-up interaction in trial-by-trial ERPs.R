
# Follow-up analysis on the interaction of Modality Switch and RT Group. 

# The hypothesis, based on Louwerse and Connell (2011, Cognitive Science), predicts that in 
# the Quick group, the auditory-to-visual switch would be harder than the haptic-to-visual 
# switch, whereas this difference would be less notable in the Slow group. This interaction 
# is used as a window into a symbolic and an embodied conceptual processing system. Now, to
# probe this from a different angle, two different indices will be calculated and compared.
# On the one hand, a 'modality-switch weight' will be calculated by measuring the difference 
# in modality between the property word in the context trial and that in the target trial. 
# On the other hand, a 'linguistic-switch weight' will be calculated by measuring the 
# co-occurrence frequency of the property word in the context trial and that in the target 
# trial. The hypothesis is that the modality-switch weight will better predict ERPs in the
# Slow group than in the Quick group, whereas the linguistic-switch weight will better predict 
# ERPs in the Quick group than in the Slow group.

# Eventually, the analysis fails, as neither predictor--corpus co-occurrence or modality--fits
# the data well enough, yielding very incoherent effects.


setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Trial-by-trial export files')


install.packages('gtools')
install.packages('tabplot')
install.packages('plyr')
install.packages('ggplot2')
install.packages('pastecs')
install.packages('reshape')
install.packages('dplyr')
install.packages('tidyr')
install.packages('knitr')
install.packages('ez')
install.packages('lmerTest')
install.packages('ggthemes')
install.packages('magrittr')
install.packages('lsr')
install.packages('beepr')
install.packages('rasterImage')
install.packages('eeptools')
install.packages('MuMIn')
install.packages('psych')
install.packages('car')
install.packages('lme4')
install.packages('stringi')
install.packages('scales')
install.packages("merTools")
install.packages("lsmeans")
install.packages("devtools")
install.packages("data.table")
install.packages('ggalt')
install.packages("ltm")
install.packages("ggpubr")
library(devtools)
install_github("kassambara/easyGgplot2")

library(foreign)
library(easyGgplot2)
library(merTools)
library('gtools')
library('tabplot')
library('plyr')
library('ggplot2')
library('Hmisc')
library('pastecs')
library('reshape')
library('grid')
library('tidyr')
library('knitr')
library('lmerTest')
library('dplyr')
library('lsr')
library("ggthemes")
library("magrittr")
library('doBy')
library(Hmisc)
library(lme4)
library(stats)
library(QuantPsyc)
library(psych)
library(car)
library(beepr)
library(dae)
library(png)
library(rasterImage)
library(eeptools)
library(MuMIn)
library(stringi)
library(data.table)
library(lsmeans)
library(ggalt)
library(purrr)
library(ltm)
library(grid)
library(gtable)
library(ggpubr)


EEG = readRDS('EEG.rds')


# Compute 'modality-switch weight' based on the averaged subtractions of the modality strengths 
# of the context properties and the target properties

names(EEG)

# Any NAs in the modality ratings?
summary(is.na(EEG$TARGET_property_Auditory)) # None. Good

# first stage
saved = summaryBy(
			(TARGET_property_Auditory - CONTEXT_property_Auditory) +  	# These are 3 independent means;
			(TARGET_property_Haptic - CONTEXT_property_Haptic) +		# plus sign (+) is not for adding.
			(TARGET_property_Visual - CONTEXT_property_Visual)
		~ participant * CONTEXT_property * TARGET_property, data = EEG, FUN = mean )

nrow(saved) # good; it's the number of unique combinations passed in the 'import' script

# second stage
names(saved)
saved$modality_switch_weight = (saved[,4] + saved[,5] + saved[,6]) / 3
length(unique(saved$modality_switch_weight)) # 1583 unique values
head(saved)

summary(saved$modality_switch_weight)

# finally, bind to main dataset
names(EEG)
EEG = merge(EEG, saved[,c(1,2,3,7)], by=c('participant','CONTEXT_property',
'TARGET_property'))

length(unique(EEG$modality_switch_weight)) # good: 1583 too

# Order columns
names(EEG)
EEG = EEG[,c(1,5,4,6:16,2,17,3,18,19:25)]

# SAVE
saveRDS(EEG, 'EEG.rds', compress='xz')

# Read back in
EEG = readRDS('EEG.rds')




# Create the same time windows as in the averaged-trials analysis and aggregate over time points and electrodes

aggreg_EEG.window1 = aggregate(microvolts ~ participant * RT.based_Groups * CONTEXT_property *
				TARGET_property * condition * location * log_cooccurrence_propertytoproperty * 
				modality_switch_weight * target_trial, EEG[EEG$time>=160 & EEG$time<=216,], mean)

aggreg_EEG.window2 = aggregate(microvolts ~ participant * RT.based_Groups * CONTEXT_property *
				TARGET_property * condition * location * log_cooccurrence_propertytoproperty * 
				modality_switch_weight * target_trial, EEG[EEG$time>=270 & EEG$time<=370,], mean)

aggreg_EEG.window3 = aggregate(microvolts ~ participant * RT.based_Groups * CONTEXT_property *
				TARGET_property * condition * location * log_cooccurrence_propertytoproperty * 
				modality_switch_weight * target_trial, EEG[EEG$time>=350 & EEG$time<=550,], mean)

aggreg_EEG.window4 = aggregate(microvolts ~ participant * RT.based_Groups * CONTEXT_property *
				TARGET_property * condition * location * log_cooccurrence_propertytoproperty * 
				modality_switch_weight * target_trial, EEG[EEG$time>=500 & EEG$time<=750,], mean)

# Important check
unique(EEG[EEG$time>=500 & EEG$time<=750,'time']) # good

# free up space
rm(EEG)

# Bind all 
aggreg_EEG.window1$timewindow = 'Window 1'
aggreg_EEG.window2$timewindow = 'Window 2'
aggreg_EEG.window3$timewindow = 'Window 3'
aggreg_EEG.window4$timewindow = 'Window 4'
aggreg_EEG = rbind(aggreg_EEG.window1,aggreg_EEG.window2,aggreg_EEG.window3,aggreg_EEG.window4)
names(aggreg_EEG)
aggreg_EEG = aggreg_EEG[,c(11,1:10)]  # place timewindow column first

head(aggreg_EEG) # all right

# free up space
rm(aggreg_EEG.window1, aggreg_EEG.window2, aggreg_EEG.window3, aggreg_EEG.window4)

# SAVE
saveRDS(aggreg_EEG, 'Aggregated EEG.rds', compress='xz')

# Read back in
aggreg_EEG = readRDS('Aggregated EEG.rds')



# Compute mean-centered and scaled versions of the independent variables

aggreg_EEG$s_modality_switch_weight = scale(aggreg_EEG$modality_switch_weight)
aggreg_EEG$s_log_cooccurrence_propertytoproperty = scale(aggreg_EEG$log_cooccurrence_propertytoproperty)



# REGRESSIONS
names(aggreg_EEG)
head(aggreg_EEG)
TimeWindow = 'Window 1'
Group = 'Quick'
BrainArea = 'anterior'

fit <- lm(microvolts ~ log_cooccurrence_propertytoproperty + modality_switch_weight,
		data = aggreg_EEG[aggreg_EEG$timewindow==TimeWindow & aggreg_EEG$RT.based_Groups==Group & 
			aggreg_EEG$location==BrainArea,])

# Residuals diagnostics:
# Normal Q-Q plot of residuals:
qqnorm(resid(fit))
# Plot of Residuals vs. Fitted values:
plot(fited(fit),resid(fit),xlab='Fitted Values', ylab='Residuals');
abline(h=0)

# Residuals distribution: non-normal. Log-transform DV and re-run regression
psych::describe(aggreg_EEG[aggreg_EEG$timewindow=='Window 3' & 
			aggreg_EEG$RT.based_Groups==Group &
			aggreg_EEG$location==BrainArea,]$microvolts) # kurtose

aggreg_EEG$log_microvolts = log(55.84 +   # raise all values above zero
			aggreg_EEG[aggreg_EEG$timewindow=='Window 3' & 
			aggreg_EEG$RT.based_Groups==Group &
			aggreg_EEG$location==BrainArea,]$microvolts )

fit <- lm(log_microvolts ~ log_cooccurrence_propertytoproperty + modality_switch_weight,
			data = aggreg_EEG[aggreg_EEG[aggreg_EEG$timewindow=='Window 3' & 
				aggreg_EEG$RT.based_Groups==Group & 
				aggreg_EEG$location==BrainArea,])

anova(fit)
# check residuals again
# Normal Q-Q plot of residuals:
qqnorm(resid(fit))
# Plot of Residuals vs. Fitted values:
plot(fited(fit),resid(fit),xlab='Fitted Values', ylab='Residuals'); 
abline(h=0)

# DV transformation not useful:
# Logarithmic transformation of the dependent variable only worsened residuals in the previous test,
# and the same happened when tried in several tests below. However, the assumption of normality of
# residuals is not so strictly relevant here because these regression analyses are framed in a very
# specific comparison between the Quick group and the Slow group. Therefore, no further 
# transformations are attempted, and the assumption is acknowledged as violated.


# Back to untransformed IVs
fit <- lm(microvolts ~ log_cooccurrence_propertytoproperty + modality_switch_weight,
		data = aggreg_EEG[aggreg_EEG$timewindow=='Window 3' & aggreg_EEG$RT.based_Groups==Group & 
			aggreg_EEG$location==BrainArea,])


# Check multicollinearity:
# Preferably, largest VIF (pref. < 10), mean VIF (pref. around 1), and tolerance (pref. > 0.2)

# VIF
vif(fit)

# Mean VIF
mean(vif(fit))

# Tolerance
1/vif(fit)

# Step AIC
step_fit_AIC <- stepAIC(fit, direction="both")


# Step F
step_fit_F <- stepAIC(fit, direction="both", test="F")
str(step_fit_F)[[4,6]]
step_fit_F[['Sum of Sq']]

# Summary
summary(fit)
lm.beta(fit)


######################################################################################



# Function for computing and saving all regressions
# Start by creating dataframe

scaling = NA
timewindow = NA
group = NA
brainarea = NA
IV = NA
estimate = NA
sum_of_squares = NA
modality.corpus_SS_difference = NA
std.error = NA
t.value = NA
p.value = NA
fit_Omega.squared = NA
results_table = data.frame(scaling,timewindow,group,brainarea,IV,estimate,sum_of_squares,
					modality.corpus_SS_difference,std.error,t.value,p.value,fit_Omega.squared)

names(results_table)[1] = 'State.of.IVs'
names(results_table)[2] = 'Time.window'
names(results_table)[3] = 'Group'
names(results_table)[4] = 'Brain.area'
names(results_table)[5] = 'IV'
names(results_table)[6] = 'Estimate'
names(results_table)[7] = 'Sum.of.Squares'
names(results_table)[8] = 'Modality.Corpus.SS.difference'
names(results_table)[9] = 'SE'
names(results_table)[10] = 't.value'
names(results_table)[11] = 'p.value'
names(results_table)[12] = 'Fit.Omega.Squared'
names(results_table)

unique(results_table$Estimate)
str(results_table)
names(aggreg_EEG)

for(TimeWindow in c('Window 1', 'Window 2', 'Window 3', 'Window 4')) {
	for(Group in c('Quick', 'Slow')) {
		for(BrainArea in c('anterior', 'posterior')) {
			for(IV_scaling in c('not scaled', 'scaled')) {

if(IV_scaling=='not scaled') {

	fit <- lm(microvolts ~ log_cooccurrence_propertytoproperty + modality_switch_weight,
		data = aggreg_EEG[aggreg_EEG$timewindow==TimeWindow & 
			aggreg_EEG$RT.based_Groups==Group & aggreg_EEG$location==BrainArea,])

	results_table <<- rbind(results_table,
			c(IV_scaling, TimeWindow, Group, BrainArea, 'Modality',
				summary(fit)$coefficients['modality_switch_weight','Estimate'],
				anova(fit)['modality_switch_weight','Sum Sq'],
				anova(fit)['modality_switch_weight','Sum Sq'] -
				anova(fit)['log_cooccurrence_propertytoproperty','Sum Sq'],
				summary(fit)$coefficients['modality_switch_weight','Std. Error'],
				summary(fit)$coefficients['modality_switch_weight','t value'],
				summary(fit)$coefficients['modality_switch_weight','Pr(>|t|)'],
				summary(fit)$coefficients['modality_switch_weight','Estimate'] /
				summary(fit)$coefficients['log_cooccurrence_propertytoproperty','Estimate'],
				1-var(residuals(fit))/(var(model.response(model.frame(fit)))) ),
			c(IV_scaling, TimeWindow, Group, BrainArea, 'Corpus',
				summary(fit)$coefficients['log_cooccurrence_propertytoproperty','Estimate'],
				anova(fit)['log_cooccurrence_propertytoproperty','Sum Sq'],
				anova(fit)['modality_switch_weight','Sum Sq'] -
				anova(fit)['log_cooccurrence_propertytoproperty','Sum Sq'],	
				summary(fit)$coefficients['log_cooccurrence_propertytoproperty','Std. Error'],
				summary(fit)$coefficients['log_cooccurrence_propertytoproperty','t value'],
				summary(fit)$coefficients['log_cooccurrence_propertytoproperty','Pr(>|t|)'],
				summary(fit)$coefficients['modality_switch_weight','Estimate'] /
				summary(fit)$coefficients['log_cooccurrence_propertytoproperty','Estimate'],
				1-var(residuals(fit))/(var(model.response(model.frame(fit)))) ) )
}

else{	fit <- lm(microvolts ~ s_log_cooccurrence_propertytoproperty + s_modality_switch_weight,
		data = aggreg_EEG[aggreg_EEG$timewindow==TimeWindow & 
			aggreg_EEG$RT.based_Groups==Group & aggreg_EEG$location==BrainArea,])

	results_table <<- rbind(results_table,
			c(IV_scaling, TimeWindow, Group, BrainArea, 'Modality',
				summary(fit)$coefficients['s_modality_switch_weight','Estimate'],
				anova(fit)['s_modality_switch_weight','Sum Sq'],
				anova(fit)['s_modality_switch_weight','Sum Sq'] -
				anova(fit)['s_log_cooccurrence_propertytoproperty','Sum Sq'],
				summary(fit)$coefficients['s_modality_switch_weight','Std. Error'],
				summary(fit)$coefficients['s_modality_switch_weight','t value'],
				summary(fit)$coefficients['s_modality_switch_weight','Pr(>|t|)'],
				summary(fit)$coefficients['s_modality_switch_weight','Estimate'] /
				summary(fit)$coefficients['s_log_cooccurrence_propertytoproperty','Estimate'],
				1-var(residuals(fit))/(var(model.response(model.frame(fit)))) ),
			c(IV_scaling, TimeWindow, Group, BrainArea, 'Corpus',
				summary(fit)$coefficients['s_log_cooccurrence_propertytoproperty','Estimate'],
				anova(fit)['s_log_cooccurrence_propertytoproperty','Sum Sq'],
				anova(fit)['s_modality_switch_weight','Sum Sq'] -
				anova(fit)['s_log_cooccurrence_propertytoproperty','Sum Sq'],
				summary(fit)$coefficients['s_log_cooccurrence_propertytoproperty','Std. Error'],
				summary(fit)$coefficients['s_log_cooccurrence_propertytoproperty','t value'],
				summary(fit)$coefficients['s_log_cooccurrence_propertytoproperty','Pr(>|t|)'],
				summary(fit)$coefficients['s_modality_switch_weight','Estimate'] /
				summary(fit)$coefficients['s_log_cooccurrence_propertytoproperty','Estimate'],
				1-var(residuals(fit))/(var(model.response(model.frame(fit)))) ) )
}}}}}

# Remove initial NA row and reset row numbers
results_table = results_table[!is.na(results_table['Estimate']),]  # remove NA cells from creation of object
rownames(results_table) = seq(length=nrow(results_table))

length(unique(results_table$Estimate)) # good: = nrow(results_table)



# Now check goodness of fit of the results based on the scaled IVs and the unscaled ones
str(results_table)
results_table[,6] = as.numeric(results_table[,6])
results_table[,7] = as.numeric(results_table[,7])
results_table[,8] = as.numeric(results_table[,8])
results_table[,9] = as.numeric(results_table[,9])
results_table[,10] = as.numeric(results_table[,10])
results_table[,11] = as.numeric(results_table[,11])
results_table[,12] = as.numeric(results_table[,12])
summaryBy(Fit.Omega.Squared ~ State.of.IVs, results_table, FUN=c(mean,sd))

# Result is clear: regression fit is better with unscaled IVs:

# State.of.IVs Fit.Omega.Squared.mean Fit.Omega.Squared.sd
#   not scaled             -3.9477103            13.476732
#       scaled             -0.8027413             2.740406

# So unscaled will be used. Remove scaled results from table too.
results_table = results_table[!results_table$State.of.IVs=='scaled',]
rownames(results_table) = seq(length=nrow(results_table))

# With the plots in mind, add word 'group' to group factors
results_table$Group = as.factor(results_table$Group)
results_table$Group <- gsub('Quick', 'Quick group', results_table$Group)
results_table$Group <- gsub('Slow', 'Slow group', results_table$Group)


# Save results in folder
write.csv(results_table, 'Regression results.csv', row.names=FALSE)

# Read the results back in
results_table = read.csv('Regression results.csv')



# NUMERICAL SUMMARY
summaryBy(Sum.of.Squares ~ Time.window * Group * Brain.area * IV, 
	data=results_table, FUN=mean)

############################################################################





# Manual testing
timewindow='Window 1'
brainarea = 'posterior'

# Add margin to factor names
results_table$IV = as.factor(results_table$IV)
results_table$IV = gsub('Modality', ' Modality', results_table$IV)
results_table$IV = gsub('Corpus', ' Corpus', results_table$IV)


# PLOTS

plot_list = list()
for (timewindow in c('Window 1','Window 2','Window 3','Window 4')) {
   for (brainarea in c('anterior','posterior')) {

if(timewindow=='Window 1' & brainarea=='anterior') {

p = ggplot(results_table[results_table$Time.window==timewindow &
		results_table$Brain.area==brainarea,], aes(Group, Sum.of.Squares, colour=IV, fill=IV)) +
  xlab("RT Group") + ylab("Relevance of IV  (Sum of Squares)") +
  ggtitle(paste0(timewindow, ', ', brainarea, ' area')) +
  geom_bar(width=0.85, stat = "summary", fun.y = "mean", position = position_dodge(width = .9)) +
  geom_jitter(aes(y=Sum.of.Squares, colour=IV), color='black', position = position_dodge(width=.89)) +
  theme(axis.text.x = element_text(size = 12, face="bold"), legend.title = element_text(face='bold'),
	axis.title.y = element_text(size=12, face="bold"), legend.title.align=0.5,
	legend.text = element_text(size=12), legend.position = c(0.2, 0.8),
	axis.title.x=element_blank(), legend.background = element_rect(fill="white",
	size=0.1, linetype="solid"), panel.background = element_rect(fill = '#EEEEEE'),
	plot.title = element_text(size=13, face='bold',hjust = 0.5)) +
  scale_x_discrete(expand = c(.03,0)) + scale_y_continuous(limits=c(0, 270))
}


if(timewindow=='Window 1' & brainarea=='posterior') {

p = ggplot(results_table[results_table$Time.window==timewindow &
		results_table$Brain.area==brainarea,], aes(Group, Sum.of.Squares, colour=IV, fill=IV)) +
  xlab("RT Group") + ylab("                          ") +
  ggtitle(paste0(timewindow, ', ', brainarea, ' area')) +
  geom_bar(width=0.85, stat = "summary", fun.y = "mean", position = position_dodge(width = .9)) +
  geom_jitter(aes(y=Sum.of.Squares, colour=IV), color='black', position = position_dodge(width=.89)) +
  theme(axis.text.x = element_text(size = 12, face="bold"), legend.title = element_text(face='bold'),
	axis.title.y = element_text(size=12, face="bold"), legend.title.align=0.5,
	legend.text = element_text(size=12), legend.position = c(0.2, 0.8), 
	axis.title.x=element_blank(), legend.background = element_rect(fill="white",
	size=0.1, linetype="solid"), panel.background = element_rect(fill = '#EEEEEE'),
	plot.title = element_text(size=13, face='bold',hjust = 0.5)) +
  scale_y_continuous(limits=c(0, 270), labels=c('','','','')) +
  scale_x_discrete(expand = c(.03,0)) 
}


if(timewindow=='Window 2' & brainarea=='anterior') {

p = ggplot(results_table[results_table$Time.window==timewindow &
		results_table$Brain.area==brainarea,], aes(Group, Sum.of.Squares, colour=IV, fill=IV)) +
  xlab("RT Group") + ylab("Relevance of IV  (Sum of Squares)") +
  ggtitle(paste0(timewindow, ', ', brainarea, ' area')) +
  geom_bar(width=0.85, stat = "summary", fun.y = "mean", position = position_dodge(width = .9)) +
  geom_jitter(aes(y=Sum.of.Squares, colour=IV), color='black', position = position_dodge(width=.89)) +
  theme(axis.text.x = element_text(size = 12, face="bold"), legend.title = element_text(face='bold'),
	axis.title.y = element_text(size=12, face="bold"), legend.title.align=0.5,
	legend.text = element_text(size=12), legend.position = c(0.2, 0.8), 
	axis.title.x=element_blank(), legend.background = element_rect(fill="white",
	size=0.1, linetype="solid"), panel.background = element_rect(fill = '#EEEEEE'),
	plot.title = element_text(size=13, face='bold',hjust = 0.5)) +
  scale_x_discrete(expand = c(.03,0)) + scale_y_continuous(limits=c(0, 270))
}


if(timewindow=='Window 2' & brainarea=='posterior') {

p = ggplot(results_table[results_table$Time.window==timewindow &
		results_table$Brain.area==brainarea,], aes(Group, Sum.of.Squares, colour=IV, fill=IV)) +
  xlab("RT Group") + ylab("                          ") +
  ggtitle(paste0(timewindow, ', ', brainarea, ' area')) +
  geom_bar(width=0.85, stat = "summary", fun.y = "mean", position = position_dodge(width = .9)) +
  geom_jitter(aes(y=Sum.of.Squares, colour=IV), color='black', position = position_dodge(width=.89)) +
  theme(axis.text.x = element_text(size = 12, face="bold"), legend.title = element_text(face='bold'),
	axis.title.y = element_text(size=12, face="bold"), legend.title.align=0.5,
	legend.text = element_text(size=12), legend.position = c(0.2, 0.8), 
	axis.title.x=element_blank(), legend.background = element_rect(fill="white",
	size=0.1, linetype="solid"), panel.background = element_rect(fill = '#EEEEEE'),
	plot.title = element_text(size=13, face='bold',hjust = 0.5)) +
  scale_y_continuous(limits=c(0, 270), labels=c('','','','')) +
  scale_x_discrete(expand = c(.03,0)) 
}


if(timewindow=='Window 3' & brainarea=='anterior') {

p = ggplot(results_table[results_table$Time.window==timewindow &
		results_table$Brain.area==brainarea,], aes(Group, Sum.of.Squares, colour=IV, fill=IV)) +
  xlab("RT Group") + ylab("Relevance of IV  (Sum of Squares)") +
  ggtitle(paste0(timewindow, ', ', brainarea, ' area')) +
  geom_bar(width=0.85, stat = "summary", fun.y = "mean", position = position_dodge(width = .9)) +
  geom_jitter(aes(y=Sum.of.Squares, colour=IV), color='black', position = position_dodge(width=.89)) +
  theme(axis.text.x = element_text(size = 12, face="bold"), legend.title = element_text(face='bold'),
	axis.title.y = element_text(size=12, face="bold"), legend.title.align=0.5,
	legend.text = element_text(size=12), legend.position = c(0.2, 0.8), 
	axis.title.x=element_blank(), legend.background = element_rect(fill="white",
	size=0.1, linetype="solid"), panel.background = element_rect(fill = '#EEEEEE'),
	plot.title = element_text(size=13, face='bold',hjust = 0.5)) +
  scale_x_discrete(expand = c(.03,0)) + scale_y_continuous(limits=c(0, 270))
}


if(timewindow=='Window 3' & brainarea=='posterior') {

p = ggplot(results_table[results_table$Time.window==timewindow &
		results_table$Brain.area==brainarea,], aes(Group, Sum.of.Squares, colour=IV, fill=IV)) +
  xlab("RT Group") + ylab("                          ") +
  ggtitle(paste0(timewindow, ', ', brainarea, ' area')) +
  geom_bar(width=0.85, stat = "summary", fun.y = "mean", position = position_dodge(width = .9)) +
  geom_jitter(aes(y=Sum.of.Squares, colour=IV), color='black', position = position_dodge(width=.89)) +
  theme(axis.text.x = element_text(size = 12, face="bold"), legend.title = element_text(face='bold'),
	axis.title.y = element_text(size=12, face="bold"), legend.title.align=0.5,
	legend.text = element_text(size=12), legend.position = c(0.2, 0.8), 
	axis.title.x=element_blank(), legend.background = element_rect(fill="white",
	size=0.1, linetype="solid"), panel.background = element_rect(fill = '#EEEEEE'),
	plot.title = element_text(size=13, face='bold',hjust = 0.5)) +
  scale_y_continuous(limits=c(0, 270), labels=c('','','','')) +
  scale_x_discrete(expand = c(.03,0)) 
}


if(timewindow=='Window 4' & brainarea=='anterior') {

p = ggplot(results_table[results_table$Time.window==timewindow &
		results_table$Brain.area==brainarea,], aes(Group, Sum.of.Squares, colour=IV, fill=IV)) +
  xlab("RT Group") + ylab("Relevance of IV  (Sum of Squares)") +
  ggtitle(paste0(timewindow, ', ', brainarea, ' area')) +
  geom_bar(width=0.85, stat = "summary", fun.y = "mean", position = position_dodge(width = .9)) +
  geom_jitter(aes(y=Sum.of.Squares, colour=IV), color='black', position = position_dodge(width=.89)) +
  theme(axis.text.x = element_text(size = 12, face="bold"), legend.title = element_text(face='bold'),
	axis.title.y = element_text(size=12, face="bold"), legend.title.align=0.5,
	legend.text = element_text(size=12), legend.position = c(0.2, 0.8), 
	axis.title.x=element_blank(), legend.background = element_rect(fill="white",
	size=0.1, linetype="solid"), panel.background = element_rect(fill = '#EEEEEE'),
	plot.title = element_text(size=13, face='bold',hjust = 0.5)) +
  scale_x_discrete(expand = c(.03,0)) + scale_y_continuous(limits=c(0, 270))
}


if(timewindow=='Window 4' & brainarea=='posterior') {

p = ggplot(results_table[results_table$Time.window==timewindow &
		results_table$Brain.area==brainarea,], aes(Group, Sum.of.Squares, colour=IV, fill=IV)) +
  xlab("RT Group") + ylab("                          ") +
  ggtitle(paste0(timewindow, ', ', brainarea, ' area')) +
  geom_bar(width=0.85, stat = "summary", fun.y = "mean", position = position_dodge(width = .9)) +
  geom_jitter(aes(y=Sum.of.Squares, colour=IV), color='black', position = position_dodge(width=.89)) +
  theme(axis.text.x = element_text(size = 12, face="bold"), legend.title = element_text(face='bold'),
	axis.title.y = element_text(size=12, face="bold"), legend.title.align=0.5,
	legend.text = element_text(size=12), legend.position = c(0.2, 0.8),
	axis.title.x=element_blank(), legend.background = element_rect(fill="white",
	size=0.1, linetype="solid"), panel.background = element_rect(fill = '#EEEEEE'),
	plot.title = element_text(size=13, face='bold',hjust = 0.5)) +
  scale_y_continuous(limits=c(0, 270), labels=c('','','','')) +
  scale_x_discrete(expand = c(.03,0))
}

    plot_list[[paste0(timewindow, ', ', brainarea, ' area')]] = p
} }

# Unlist plots
win1_ant = plot_list[['Window 1, anterior area']]
win1_pos = plot_list[['Window 1, posterior area']]
win2_ant = plot_list[['Window 2, anterior area']]
win2_pos = plot_list[['Window 2, posterior area']]
win3_ant = plot_list[['Window 3, anterior area']]
win3_pos = plot_list[['Window 3, posterior area']]
win4_ant = plot_list[['Window 4, anterior area']]
win4_pos = plot_list[['Window 4, posterior area']]

# Plot combined
png(file="Plot failed follow-up interaction in trial-by-trial ERPs.png", units="in", width=7, height=16, res=1800)
ggplot2.multiplot(win4_ant, win4_pos, win3_ant, win3_pos, win2_ant, win2_pos, win1_ant, win1_pos, cols = 2)
dev.off()

#########################################################################################################
