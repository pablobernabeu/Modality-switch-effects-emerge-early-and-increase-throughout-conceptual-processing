
setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Analyses of ERPs averaged across trials')

install.packages('phia')
install.packages('magrittr')
install.packages('stats')
install.packages('foreign')
install.packages('afex')
install.packages('eegkit')
install.packages('signal')
install.packages('stringr')
install.packages('ggplot2')
install.packages('grid')
install.packages('lmerTest')
install.packages('sjstats')
install.packages('ggeffects')
install.packages('doBy')
install.packages('dae')
install.packages('MuMIn')
install.packages('lattice')
install.packages('dplyr')
install.packages('plyr')
install.packages('emmeans')

library(phia)
library(magrittr)
library(stats)
library(foreign)
library(afex)
library(eegkit)
library(signal)
library(stringr)
library(ggplot2)
library(grid)
library(lmerTest)
library(sjstats)
library(ggeffects)
library(doBy)
library(dae)
library(MuMIn)
library(lattice)
library(dplyr)
library(plyr)
library(emmeans)




# These follow-ups are performed with three goals. The first one is to get insight into the
# interactions of CMS with Groups and electrode areas, as analyzed in the main models in the
# corresponding script in the current folder). The second goal is to check specifically the
# interaction between CMS and Groups, which is key to this study. The hypothesis for the
# latter, based on Louwerse and Connell (2011), is that a larger CMS effect would hold in
# the Slow group compared to the Quick group, because the former has greater resources and
# can better engage perceptual simulation. The third goal is to look into the CMS effect in
# the different groups and in the brain areas related to language or to vision.


# Read in dataset
EEG = readRDS('EEG.rds')



# Define language and vision electrodes
language = c('C44', 'C50', 'C51') 		# (in 10/20 montage: FC5, T7, F7)
vision = c('C54', 'C22', 'C55', 'C23')	# (in 10/20 montage: O1, O2, P7, P8)

# N.B. The areas were initially defined as below, but the above definition is more accurate.
# language = c('C49', 'C50', 'C56', 'C51') # (in 10/20 montage: TP7, T7, ~T7, FT7)
# vision = c('C54', 'C47', 'C22', 'C15')	 # (in 10/20 montage: O1, PO3, O2, PO4)

EEG$languageVSvision = NA
EEG$languageVSvision = ifelse(EEG$electrode %in% language, 'language', NA)
EEG$languageVSvision = ifelse(EEG$electrode %in% vision, 'vision', EEG$languageVSvision)


EEG$condition = as.factor(EEG$condition)
EEG$location = as.factor(EEG$location)
EEG$languageVSvision = as.factor(EEG$languageVSvision)


# Save new variable
#saveRDS(EEG, 'EEG.rds', compress='xz')





# read in final, maximal, converging models
m.w1 = readRDS('LME model Time Window 1.rds')
m.w2 = readRDS('LME model Time Window 2.rds')
m.w3 = readRDS('LME model Time Window 3.rds')
m.w4 = readRDS('LME model Time Window 4.rds')




# Two-way interaction in each time window: Switch condition by Brain area (anterior/posterior)

# Time Window 1

# Plot

agg_p <- EEG[as.numeric(as.character(EEG$time)) > 160 & as.numeric(as.character(EEG$time)) < 214 & 
  !is.na(EEG$RT.based_Groups) & !is.na(EEG$location),] %>%
  group_by(condition, location, participant) %>% summarise(mean = mean(microvolts)) %>% ungroup()

agg_p = agg_p[!is.na(agg_p$mean),]
agg_p$participant = factor(agg_p$participant)

xyplot(mean ~ condition|location, agg_p[agg_p$location=='anterior',],
	jitter.x = TRUE, pch = 20, alpha = 0.5,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) +
bwplot(mean ~ condition|location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

xyplot(mean ~ condition|location, agg_p[agg_p$location=='posterior',],
	jitter.x = TRUE, pch = 20, alpha = 0.5,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) +
bwplot(mean ~ condition|location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

# Marginal means of the model
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 10000000)
emm_i1 <- emmeans(m.w1, "condition", by = "location")	# Estimates

# Modality Switch contrast: visual-to-visual versus the other conditions combined
contrast.w1 = summary(update(contrast(emm_i1, list(Switch_conditions_minus_nonswitch_condition =
		cbind(c(1,1,-2))), adjust = 'none'), by = NULL))
# p-value adjustment is applied at the end for all 8 tests


# Time Window 2

# Plot 

agg_p <- EEG[as.numeric(as.character(EEG$time)) > 270 & as.numeric(as.character(EEG$time)) < 368 & 
  !is.na(EEG$RT.based_Groups) & !is.na(EEG$location),] %>%
  group_by(condition, location, participant) %>%
  summarise(mean = mean(microvolts)) %>%
  ungroup()

agg_p = agg_p[!is.na(agg_p$mean),]
agg_p$participant = factor(agg_p$participant)

xyplot(mean ~ condition|location, agg_p[agg_p$location=='anterior',], 
	jitter.x = TRUE, pch = 20, alpha = 0.5, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) + 
bwplot(mean ~ condition|location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

xyplot(mean ~ condition|location, agg_p[agg_p$location=='posterior',], 
	jitter.x = TRUE, pch = 20, alpha = 0.5, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) + 
bwplot(mean ~ condition|location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

# Marginal means of the model
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 10000000)
emm_i1 <- emmeans(m.w2, "condition", by = "location")	# Estimates

# Modality Switch contrast: visual-to-visual versus the other conditions combined
contrast.w2 = summary(update(contrast(emm_i1, list(Switch_conditions_minus_nonswitch_condition =
		cbind(c(1,1,-2))), adjust = 'none'), by = NULL))
# p-value adjustment is applied at the end for all 8 tests


# Time Window 3

# Plot 

agg_p <- EEG[as.numeric(as.character(EEG$time)) > 350 & as.numeric(as.character(EEG$time)) < 548 & 
  !is.na(EEG$RT.based_Groups) & !is.na(EEG$location),] %>%
  group_by(condition, location, participant) %>%
  summarise(mean = mean(microvolts)) %>%
  ungroup()

agg_p = agg_p[!is.na(agg_p$mean),]
agg_p$participant = factor(agg_p$participant)

xyplot(mean ~ condition|location, agg_p[agg_p$location=='anterior',],
	jitter.x = TRUE, pch = 20, alpha = 0.5, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) +
bwplot(mean ~ condition|location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

xyplot(mean ~ condition|location, agg_p[agg_p$location=='posterior',],
	jitter.x = TRUE, pch = 20, alpha = 0.5, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) + 
bwplot(mean ~ condition|location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

# Marginal means of the model
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 10000000)
emm_i1 <- emmeans(m.w3, "condition", by = "location")	# Estimates

# Modality Switch contrast: visual-to-visual versus the other conditions combined
contrast.w3 = summary(update(contrast(emm_i1, list(Switch_conditions_minus_nonswitch_condition =
		cbind(c(1,1,-2))), adjust = 'none'), by = NULL))
# p-value adjustment is applied at the end for all 8 tests


# Time Window 4

# Plot 

agg_p <- EEG[as.numeric(as.character(EEG$time)) > 500 & as.numeric(as.character(EEG$time)) < 748 & 
  !is.na(EEG$RT.based_Groups) & !is.na(EEG$location),] %>%
  group_by(condition, location, participant) %>%
  summarise(mean = mean(microvolts)) %>%
  ungroup()

agg_p = agg_p[!is.na(agg_p$mean),]
agg_p$participant = factor(agg_p$participant)

xyplot(mean ~ condition|location, agg_p[agg_p$location=='anterior',],
	jitter.x = TRUE, pch = 20, alpha = 0.5,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) +
bwplot(mean ~ condition|location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

xyplot(mean ~ condition|location, agg_p[agg_p$location=='posterior',],
	jitter.x = TRUE, pch = 20, alpha = 0.5, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) + 
bwplot(mean ~ condition|location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

# Marginal means of the model
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 10000000)
emm_i1 <- emmeans(m.w4, "condition", by = "location")	# Estimates

# Modality Switch contrast: visual-to-visual versus the other conditions combined
contrast.w4 = summary(update(contrast(emm_i1, list(Switch_conditions_minus_nonswitch_condition =
		cbind(c(1,1,-2))), adjust = 'none'), by = NULL))
# p-value adjustment is applied at the end for all 8 tests

# Gather results from all windows
contrast.w1$Time.window = 'Window 1'
contrast.w2$Time.window = 'Window 2'
contrast.w3$Time.window = 'Window 3'
contrast.w4$Time.window = 'Window 4'
save.contrasts = rbind(contrast.w1, contrast.w2, contrast.w3, contrast.w4)
save.contrasts = data.frame(save.contrasts)

# Adjust p values for all 8 tests
save.contrasts$p.value = p.adjust(save.contrasts$p.value, method = 'holm')

# Add asterisks about p values

save.contrasts$p.asterisks = NA
save.contrasts$p.asterisks = as.character(save.contrasts$p.asterisks)

save.contrasts$p.asterisks = ifelse(save.contrasts$p.value < .001, '***', NA)

save.contrasts$p.asterisks = 
	ifelse(save.contrasts$p.value > .001 & save.contrasts$p.value < .01, '**',
	save.contrasts$p.asterisks)

save.contrasts$p.asterisks = 
	ifelse(save.contrasts$p.value > .01 & save.contrasts$p.value < .05, '*',
	save.contrasts$p.asterisks)

# Add confidence intervals
save.contrasts$Lower_CI = NA
save.contrasts$Upper_CI = NA
save.contrasts$Lower_CI = as.numeric(save.contrasts$Lower_CI)
save.contrasts$Upper_CI = as.numeric(save.contrasts$Upper_CI)
save.contrasts$Lower_CI = save.contrasts$estimate - 1.96 * save.contrasts$SE
save.contrasts$Upper_CI = save.contrasts$estimate + 1.96 * save.contrasts$SE

# Rename
colnames(save.contrasts)[colnames(save.contrasts)=='contrast'] = 'Contrast'
colnames(save.contrasts)[colnames(save.contrasts)=='timewindow'] = 'Time.window'
colnames(save.contrasts)[colnames(save.contrasts)=='contrast'] = 'Contrast'
colnames(save.contrasts)[colnames(save.contrasts)=='location'] = 'Brain.area'
colnames(save.contrasts)[colnames(save.contrasts)=='estimate'] = 'Estimate'
colnames(save.contrasts)[colnames(save.contrasts)=='t.ratio'] = 't'
colnames(save.contrasts)[colnames(save.contrasts)=='df'] = 'df'
colnames(save.contrasts)[colnames(save.contrasts)=='p.value'] = 'Holm-Bonferroni-corrected_p'

# Order
save.contrasts = save.contrasts[,c(8, 2, 1, 3:4, 10:11, 6, 5, 7, 9)]

# Save
write.csv(save.contrasts, 
  'Switch effect by time window and anterior vs posterior brain areas.csv', row.names=FALSE)



#################################################################







# Three-way interaction in each time window: Switch condition by group by Brain area (anterior/posterior)


# Time Window 1

# Plot

agg_p <- EEG[as.numeric(as.character(EEG$time)) > 160 & as.numeric(as.character(EEG$time)) < 214 & 
  !is.na(EEG$RT.based_Groups) & !is.na(EEG$location),] %>%
  group_by(condition, RT.based_Groups, location, participant) %>%
  summarise(mean = mean(microvolts)) %>%
  ungroup()

agg_p = agg_p[!is.na(agg_p$mean),]
agg_p$participant = factor(agg_p$participant)

xyplot(mean ~ condition|RT.based_Groups+location, agg_p[agg_p$location=='anterior',],
	jitter.x = TRUE, pch = 20, alpha = 0.5,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) +
bwplot(mean ~ condition|RT.based_Groups+location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

xyplot(mean ~ condition|RT.based_Groups+location, agg_p[agg_p$location=='posterior',],
	jitter.x = TRUE, pch = 20, alpha = 0.5,
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) +
bwplot(mean ~ condition|RT.based_Groups+location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

# Marginal means of the model
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 10000000)
emm_i1 <- emmeans(m.w1, "condition", by = c("RT.based_Groups", "location"))	# Estimates

# Modality Switch contrast: visual-to-visual versus the other conditions combined
contrast.w1 = summary(update(contrast(emm_i1, list(Switch_conditions_minus_nonswitch_condition =
		cbind(c(1,1,-2))), adjust = 'none'), by = NULL))
# p-value adjustment is applied at the end for all 16 tests


# Time Window 2

# Plot 

agg_p <- EEG[as.numeric(as.character(EEG$time)) > 270 & as.numeric(as.character(EEG$time)) < 368 & 
  !is.na(EEG$RT.based_Groups) & !is.na(EEG$location),] %>%
  group_by(condition, RT.based_Groups, location, participant) %>%
  summarise(mean = mean(microvolts)) %>%
  ungroup()

agg_p = agg_p[!is.na(agg_p$mean),]
agg_p$participant = factor(agg_p$participant)

xyplot(mean ~ condition|RT.based_Groups+location, agg_p[agg_p$location=='anterior',], 
	jitter.x = TRUE, pch = 20, alpha = 0.5, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) + 
bwplot(mean ~ condition|RT.based_Groups+location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

xyplot(mean ~ condition|RT.based_Groups+location, agg_p[agg_p$location=='posterior',], 
	jitter.x = TRUE, pch = 20, alpha = 0.5, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) + 
bwplot(mean ~ condition|RT.based_Groups+location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

# Marginal means of the model
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 10000000)
emm_i1 <- emmeans(m.w2, "condition", by = c("RT.based_Groups", "location"))	# Estimates

# Modality Switch contrast: visual-to-visual versus the other conditions combined
contrast.w2 = summary(update(contrast(emm_i1, list(Switch_conditions_minus_nonswitch_condition =
		cbind(c(1,1,-2))), adjust = 'none'), by = NULL))
# p-value adjustment is applied at the end for all 16 tests


# Time Window 3

# Plot 

agg_p <- EEG[as.numeric(as.character(EEG$time)) > 350 & as.numeric(as.character(EEG$time)) < 548 & 
  !is.na(EEG$RT.based_Groups) & !is.na(EEG$location),] %>%
  group_by(condition, RT.based_Groups, location, participant) %>%
  summarise(mean = mean(microvolts)) %>%
  ungroup()

agg_p = agg_p[!is.na(agg_p$mean),]
agg_p$participant = factor(agg_p$participant)

xyplot(mean ~ condition|RT.based_Groups+location, agg_p[agg_p$location=='anterior',], 
	jitter.x = TRUE, pch = 20, alpha = 0.5, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) + 
bwplot(mean ~ condition|RT.based_Groups+location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

xyplot(mean ~ condition|RT.based_Groups+location, agg_p[agg_p$location=='posterior',], 
	jitter.x = TRUE, pch = 20, alpha = 0.5, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) + 
bwplot(mean ~ condition|RT.based_Groups+location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

# Marginal means of the model
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 10000000)
emm_i1 <- emmeans(m.w3, "condition", by = c("RT.based_Groups", "location"))	# Estimates

# Modality Switch contrast: visual-to-visual versus the other conditions combined
contrast.w3 = summary(update(contrast(emm_i1, list(Switch_conditions_minus_nonswitch_condition =
		cbind(c(1,1,-2))), adjust = 'none'), by = NULL))
# p-value adjustment is applied at the end for all 16 tests


# Time Window 4

# Plot 

agg_p <- EEG[as.numeric(as.character(EEG$time)) > 500 & as.numeric(as.character(EEG$time)) < 748 & 
  !is.na(EEG$RT.based_Groups) & !is.na(EEG$location),] %>%
  group_by(condition, RT.based_Groups, location, participant) %>%
  summarise(mean = mean(microvolts)) %>%
  ungroup()

agg_p = agg_p[!is.na(agg_p$mean),]
agg_p$participant = factor(agg_p$participant)

xyplot(mean ~ condition|RT.based_Groups+location, agg_p[agg_p$location=='anterior',], 
	jitter.x = TRUE, pch = 20, alpha = 0.5, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) + 
bwplot(mean ~ condition|RT.based_Groups+location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

xyplot(mean ~ condition|RT.based_Groups+location, agg_p[agg_p$location=='posterior',], 
	jitter.x = TRUE, pch = 20, alpha = 0.5, 
       panel = function(x, y, ...) {
         panel.xyplot(x, y, ...)
         tmp <- aggregate(y, by = list(x), mean)
         panel.points(tmp$x, tmp$y, pch = 13, cex = 1.5)
       }) + 
bwplot(mean ~ condition|RT.based_Groups+location, agg_p, pch="|", do.out = FALSE)    
# This line alone can draw the plot, if necessary

# Marginal means of the model
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 10000000)
emm_i1 <- emmeans(m.w4, "condition", by = c("RT.based_Groups", "location"))	# Estimates

# Modality Switch contrast: visual-to-visual versus the other conditions combined
contrast.w4 = summary(update(contrast(emm_i1, list(Switch_conditions_minus_nonswitch_condition =
		cbind(c(1,1,-2))), adjust = 'none'), by = NULL))
# p-value adjustment is applied at the end for all 16 tests

# Gather results from all windows
contrast.w1$Time.window = 'Window 1'
contrast.w2$Time.window = 'Window 2'
contrast.w3$Time.window = 'Window 3'
contrast.w4$Time.window = 'Window 4'
save.contrasts = rbind(contrast.w1, contrast.w2, contrast.w3, contrast.w4)
save.contrasts = data.frame(save.contrasts)

# Adjust p values for all 16 tests
save.contrasts$p.value = p.adjust(save.contrasts$p.value, method = 'holm')

# Add asterisks about p values

save.contrasts$p.asterisks = NA
save.contrasts$p.asterisks = as.character(save.contrasts$p.asterisks)

save.contrasts$p.asterisks = ifelse(save.contrasts$p.value < .001, '***', NA)

save.contrasts$p.asterisks = 
	ifelse(save.contrasts$p.value > .001 & save.contrasts$p.value < .01, '**',
	save.contrasts$p.asterisks)

save.contrasts$p.asterisks = 
	ifelse(save.contrasts$p.value > .01 & save.contrasts$p.value < .05, '*',
	save.contrasts$p.asterisks)

# Add confidence intervals
save.contrasts$Lower_CI = NA
save.contrasts$Upper_CI = NA
save.contrasts$Lower_CI = as.numeric(save.contrasts$Lower_CI)
save.contrasts$Upper_CI = as.numeric(save.contrasts$Upper_CI)
save.contrasts$Lower_CI = save.contrasts$estimate - 1.96 * save.contrasts$SE
save.contrasts$Upper_CI = save.contrasts$estimate + 1.96 * save.contrasts$SE

# Rename
colnames(save.contrasts)[colnames(save.contrasts)=='contrast'] = 'Contrast'
colnames(save.contrasts)[colnames(save.contrasts)=='timewindow'] = 'Time.window'
colnames(save.contrasts)[colnames(save.contrasts)=='RT.based_Groups'] = 'Group'
colnames(save.contrasts)[colnames(save.contrasts)=='location'] = 'Brain.area'
colnames(save.contrasts)[colnames(save.contrasts)=='estimate'] = 'Estimate'
colnames(save.contrasts)[colnames(save.contrasts)=='t.ratio'] = 't'
colnames(save.contrasts)[colnames(save.contrasts)=='df'] = 'df'
colnames(save.contrasts)[colnames(save.contrasts)=='p.value'] = 'Holm-Bonferroni-corrected_p'


# Order
save.contrasts = save.contrasts[,c(9, 1:5, 11, 12, 7, 6, 8, 10)]

# Save
write.csv(save.contrasts, 
'Switch effect by group, time window, and anterior vs posterior brain areas.csv', row.names=FALSE)

#######################################################################################################
#######################################################################################################





# CMS effect tested in each group and language/vision electrode area over time windows

m = mixed(microvolts ~
	(1| time) + (1| electrode) + (condition | participant) +
	RT.based_Groups * condition * languageVSvision * timewindow,
	EEG[which(!is.na(EEG$RT.based_Groups) &
	!is.na(EEG$languageVSvision) & !is.na(EEG$timewindow)),],
	control = lmerControl(optCtrl=list(maxfun=10000000)), method='S')

warnings()
anova(m)
summary(m)


# Save results

Statistic = 'F'
Variable = rownames(anova(m))
F = anova(m)['F'][[1]]
num_df = anova(m)['num Df'][[1]]
den_df = anova(m)['den Df'][[1]]
F_p = anova(m)['Pr(>F)'][[1]]
results_F = data.frame(Statistic, Variable, num_df, den_df, F, F_p)

Statistic = 't'
Variable = rownames(coefficients(summary(m)))
Estimate = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'Estimate'])
SE = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'Std. Error'])
Lower_CI = Estimate - 1.96 * SE
Upper_CI = Estimate + 1.96 * SE
t = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 't value'])
df = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'df'])
t_p = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'Pr(>|t|)'])

results_t = data.frame(Statistic, Variable, Estimate, SE, Lower_CI, Upper_CI, t, df, t_p)

results = rbind.fill(results_F, results_t)


# Add p-value asterisks

# For F-based p-values

results$F_p.asterisks = NA
results$F_p.asterisks = as.character(results$F_p.asterisks)

results$F_p.asterisks = ifelse(results$'F_p' < .001, '***', NA)

results$F_p.asterisks = 
	ifelse(results$'F_p' > .001 & results$'F_p' < .01, '**',
	results$F_p.asterisks)

results$F_p.asterisks = 
	ifelse(results$'F_p' > .01 & results$'F_p' < .05, '*',
	results$F_p.asterisks)


# For t-based p-values

results$t_p.asterisks = NA
results$t_p.asterisks = as.character(results$t_p.asterisks)

results$t_p.asterisks = ifelse(results$'t_p' < .001, '***', NA)

results$t_p.asterisks = 
	ifelse(results$'t_p' > .001 & results$'t_p' < .01, '**',
	results$t_p.asterisks)

results$t_p.asterisks = 
	ifelse(results$'t_p' > .01 & results$'t_p' < .05, '*',
	results$t_p.asterisks)


# Order columns
str(results)
results = results[, c(1, 2, 5, 3, 4, 6, 14, 7:13, 15)]

write.csv(results, 
  'LME results on the Switch effect by group, time window, and language vs vision brain areas.csv', 
  na='', row.names=FALSE)




# Run and save factor level contrasts

# Contrasts: Switch condition by time window, group and language vs vision brain areas
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 10000000)
emm_i1 <- emmeans(m, "condition", by = c('timewindow', 'RT.based_Groups', "languageVSvision"))	
# Estimates

# Modality switch contrast: visual-to-visual versus the other conditions combined
save.contrasts = update(contrast(emm_i1, list(Switch_conditions_minus_nonswitch_condition =
	cbind(c(1,1,-2)))), by = NULL, adjust = "holm")	# Significance
save.contrasts = summary(save.contrasts)

# Rename
colnames(save.contrasts)[colnames(save.contrasts)=='contrast'] = 'Contrast'
colnames(save.contrasts)[colnames(save.contrasts)=='timewindow'] = 'Time.window'
colnames(save.contrasts)[colnames(save.contrasts)=='contrast'] = 'Contrast'
colnames(save.contrasts)[colnames(save.contrasts)=='RT.based_Groups'] = 'Group'
colnames(save.contrasts)[colnames(save.contrasts)=='languageVSvision'] = 'Brain.area'
colnames(save.contrasts)[colnames(save.contrasts)=='estimate'] = 'Estimate'
colnames(save.contrasts)[colnames(save.contrasts)=='t.ratio'] = 't'
colnames(save.contrasts)[colnames(save.contrasts)=='df'] = 'df'
colnames(save.contrasts)[colnames(save.contrasts)=='p.value'] = 'Holm-Bonferroni-corrected_p'

# Add confidence intervals
save.contrasts$Lower_CI = save.contrasts$Estimate - 1.96 * save.contrasts$SE
save.contrasts$Upper_CI = save.contrasts$Estimate + 1.96 * save.contrasts$SE

# Order
save.contrasts = save.contrasts[,c(1:6, 10, 11, 8, 7, 9)]

# Save
write.csv(save.contrasts, 
  'Switch effect by group, time window, and language vs vision brain areas.csv', 
  row.names=FALSE)

#############################################################################################################
#############################################################################################################





# Analysis similar to Louwerse and Hutchinson (2012), Figure 3. Linguistic-perceptual bias over time.

EEG = readRDS('EEG.rds')

# Since the LME with the original data turns out rank deficient--i.e. a sparsity problem--, 
# downsample timescale to 125 Hz

# Downsample. First, rename every three time points as previous one:
EEG$time2 <- EEG$time
EEG$time2 <- as.integer(as.character(EEG$time2))
EEG$time2 = ifelse(EEG$time2=='-198', '-200', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-196', '-200', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-194', '-200', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-190', '-192', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-188', '-192', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-186', '-192', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-182', '-184', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-180', '-184', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-178', '-184', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-174', '-176', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-172', '-176', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-170', '-176', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-166', '-168', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-164', '-168', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-162', '-168', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-158', '-160', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-156', '-160', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-154', '-160', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-150', '-152', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-148', '-152', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-146', '-152', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-142', '-144', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-140', '-144', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-138', '-144', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-134', '-136', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-132', '-136', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-130', '-136', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-126', '-128', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-124', '-128', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-122', '-128', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-118', '-120', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-116', '-120', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-114', '-120', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-110', '-112', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-108', '-112', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-106', '-112', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-102', '-104', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-100', '-104', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-98', '-104', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-94', '-96', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-92', '-96', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-90', '-96', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-86', '-88', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-84', '-88', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-82', '-88', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-78', '-80', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-76', '-80', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-74', '-80', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-70', '-72', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-68', '-72', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-66', '-72', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-62', '-64', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-60', '-64', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-58', '-64', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-54', '-56', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-52', '-56', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-50', '-56', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-46', '-48', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-44', '-48', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-42', '-48', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-38', '-40', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-36', '-40', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-34', '-40', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-30', '-32', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-28', '-32', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-26', '-32', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-22', '-24', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-20', '-24', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-18', '-24', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-14', '-16', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-12', '-16', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-10', '-16', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-6', '-8', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-4', '-8', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='-2', '-8', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='2', '0', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='4', '0', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='6', '0', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='10', '8', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='12', '8', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='14', '8', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='18', '16', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='20', '16', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='22', '16', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='26', '24', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='28', '24', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='30', '24', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='34', '32', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='36', '32', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='38', '32', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='42', '40', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='44', '40', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='46', '40', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='50', '48', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='52', '48', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='54', '48', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='58', '56', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='60', '56', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='62', '56', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='66', '64', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='68', '64', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='70', '64', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='74', '72', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='76', '72', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='78', '72', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='82', '80', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='84', '80', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='86', '80', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='90', '88', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='92', '88', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='94', '88', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='98', '96', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='100', '96', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='102', '96', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='106', '104', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='108', '104', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='110', '104', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='114', '112', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='116', '112', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='118', '112', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='122', '120', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='124', '120', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='126', '120', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='130', '128', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='132', '128', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='134', '128', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='138', '136', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='140', '136', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='142', '136', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='146', '144', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='148', '144', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='150', '144', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='154', '152', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='156', '152', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='158', '152', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='162', '160', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='164', '160', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='166', '160', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='170', '168', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='172', '168', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='174', '168', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='178', '176', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='180', '176', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='182', '176', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='186', '184', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='188', '184', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='190', '184', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='194', '192', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='196', '192', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='198', '192', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='202', '200', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='204', '200', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='206', '200', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='210', '208', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='212', '208', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='214', '208', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='218', '216', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='220', '216', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='222', '216', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='226', '224', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='228', '224', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='230', '224', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='234', '232', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='236', '232', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='238', '232', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='242', '240', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='244', '240', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='246', '240', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='250', '248', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='252', '248', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='254', '248', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='258', '256', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='260', '256', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='262', '256', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='266', '264', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='268', '264', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='270', '264', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='274', '272', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='276', '272', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='278', '272', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='282', '280', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='284', '280', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='286', '280', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='290', '288', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='292', '288', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='294', '288', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='298', '296', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='300', '296', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='302', '296', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='306', '304', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='308', '304', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='310', '304', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='314', '312', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='316', '312', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='318', '312', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='322', '320', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='324', '320', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='326', '320', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='330', '328', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='332', '328', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='334', '328', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='338', '336', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='340', '336', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='342', '336', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='346', '344', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='348', '344', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='350', '344', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='354', '352', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='356', '352', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='358', '352', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='362', '360', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='364', '360', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='366', '360', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='370', '368', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='372', '368', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='374', '368', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='378', '376', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='380', '376', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='382', '376', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='386', '384', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='388', '384', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='390', '384', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='394', '392', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='396', '392', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='398', '392', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='402', '400', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='404', '400', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='406', '400', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='410', '408', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='412', '408', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='414', '408', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='418', '416', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='420', '416', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='422', '416', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='426', '424', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='428', '424', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='430', '424', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='434', '432', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='436', '432', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='438', '432', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='442', '440', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='444', '440', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='446', '440', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='450', '448', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='452', '448', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='454', '448', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='458', '456', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='460', '456', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='462', '456', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='466', '464', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='468', '464', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='470', '464', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='474', '472', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='476', '472', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='478', '472', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='482', '480', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='484', '480', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='486', '480', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='490', '488', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='492', '488', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='494', '488', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='498', '496', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='500', '496', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='502', '496', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='506', '504', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='508', '504', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='510', '504', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='514', '512', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='516', '512', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='518', '512', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='522', '520', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='524', '520', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='526', '520', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='530', '528', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='532', '528', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='534', '528', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='538', '536', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='540', '536', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='542', '536', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='546', '544', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='548', '544', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='550', '544', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='554', '552', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='556', '552', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='558', '552', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='562', '560', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='564', '560', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='566', '560', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='570', '568', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='572', '568', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='574', '568', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='578', '576', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='580', '576', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='582', '576', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='586', '584', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='588', '584', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='590', '584', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='594', '592', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='596', '592', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='598', '592', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='602', '600', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='604', '600', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='606', '600', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='610', '608', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='612', '608', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='614', '608', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='618', '616', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='620', '616', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='622', '616', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='626', '624', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='628', '624', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='630', '624', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='634', '632', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='636', '632', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='638', '632', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='642', '640', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='644', '640', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='646', '640', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='650', '648', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='652', '648', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='654', '648', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='658', '656', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='660', '656', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='662', '656', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='666', '664', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='668', '664', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='670', '664', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='674', '672', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='676', '672', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='678', '672', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='682', '680', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='684', '680', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='686', '680', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='690', '688', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='692', '688', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='694', '688', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='698', '696', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='700', '696', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='702', '696', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='706', '704', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='708', '704', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='710', '704', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='714', '712', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='716', '712', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='718', '712', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='722', '720', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='724', '720', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='726', '720', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='730', '728', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='732', '728', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='734', '728', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='738', '736', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='740', '736', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='742', '736', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='746', '744', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='748', '744', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='750', '744', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='754', '752', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='756', '752', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='758', '752', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='762', '760', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='764', '760', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='766', '760', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='770', '768', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='772', '768', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='774', '768', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='778', '776', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='780', '776', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='782', '776', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='786', '784', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='788', '784', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='790', '784', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='794', '792', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='796', '792', EEG$time2)
EEG$time2 = ifelse(EEG$time2=='798', '792', EEG$time2)
EEG$time2 <- as.factor(EEG$time2)

EEG$time = EEG$time2

# Aggregate data to collapse electrodes, keeping only language/vision brain areas, 
# and to collapse original 500 Hz time scale, creating with a 125 Hz frequency.

EEG.125Hz = aggregate(microvolts ~ time*condition*participant*
  RT.based_Groups*languageVSvision, EEG, mean)
str(EEG.125Hz)

# Dummy-code language and vision electrodes as 1 and 2

EEG.125Hz$dummy.languageVSvision = EEG.125Hz$languageVSvision
EEG.125Hz$dummy.languageVSvision = ifelse(EEG.125Hz$languageVSvision == 'language', 1, 
  EEG.125Hz$dummy.languageVSvision)
EEG.125Hz$dummy.languageVSvision = ifelse(EEG.125Hz$languageVSvision == 'vision', 2, 
  EEG.125Hz$dummy.languageVSvision)
EEG.125Hz$dummy.languageVSvision = as.numeric(EEG.125Hz$dummy.languageVSvision)

# Create 20 time bins over the 800 ms time course (excluding baseline period, from -200 ms to 0 ms)

EEG.125Hz$time.bins = NA
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 0 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 38, '1', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 40 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 78, '2', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 80 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 118, '3', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 120 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 158, '4', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 160 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 198, '5', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 200 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 238, '6', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 240 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 278, '7', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 280 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 318, '8', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 320 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 358, '9', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 360 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 398, '10', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 400 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 438, '11', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 440 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 478, '12', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 480 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 518, '13', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 520 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 558, '14', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 560 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 598, '15', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 600 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 638, '16', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 640 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 678, '17', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 680 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 718, '18', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 720 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 758, '19', EEG.125Hz$time.bins)
  
EEG.125Hz$time.bins = ifelse(as.numeric(as.character(EEG.125Hz$time)) >= 760 & 
  as.numeric(as.character(EEG.125Hz$time)) <= 798, '20', EEG.125Hz$time.bins)

EEG.125Hz$time.bins = as.factor(EEG.125Hz$time.bins)
str(EEG.125Hz$time.bins)

# Consolidate data by excluding baseline period, poor EEG, and electrodes other than 
# language and vision areas
EEG.125Hz = EEG.125Hz[which(as.numeric(as.character(EEG.125Hz$time.bins)) > 0 &	
		# above, exclude baseline period, i.e. before 0 ms
		!is.na(EEG.125Hz$RT.based_Groups) &				
		# above, exclude participant 7, with poor EEG
		!is.na(EEG.125Hz$dummy.languageVSvision)), ]

EEG.125Hz$time = factor(EEG.125Hz$time)
EEG.125Hz$participant = factor(EEG.125Hz$participant)

head(EEG.125Hz)
str(EEG.125Hz)






# The regression is performed on three different sections of the data. This is done 
# because the results are rather different from Louwerse and Hutchinson (2012), so 
# it was worth double-checking. The general interpretation is these results, though,
# must consider the fact that Louwerse and Hutchinson's tasks and data are different 
# from ours.


# Analysis over time bins (cf. Louwerse & Hutchinson, 2012: Tables 2 and 3 and 
# Figure 3), computed across Switch conditions


# Create Switch effect subtraction: non-switch minus both switch conditions combined
# Method: The switch effect subtraction is computed into the Visual-to-visual 
# condition, then that is renamed as 'Switch.effect', and finally the data set is 
# subsetted to keep only the Switch.effect condition


# Remove Switch factor, leaving average of the three conditions

EEG = EEG.125Hz
EEG = aggregate(microvolts ~ time.bins*time*participant*RT.based_Groups*dummy.languageVSvision, EEG, mean)


# Omnibus test for all period (cf. pages 5 and 6 of Louwerse & Hutchinson, 2012)

m = mixed(microvolts ~
	(dummy.languageVSvision : RT.based_Groups || participant) +	 
	# Above, estimation of correlation among random effects disabled, to facilitate convergence
	dummy.languageVSvision : RT.based_Groups + time,
	data=EEG, method='S', expand_re=TRUE)

warnings()
anova(m)
summary(m)


# Regression over time bins
# afex() w/ Satterthwaite, as default (Kenward Roger's alternative doesn't allow convergence)

m = mixed(microvolts ~
	(RT.based_Groups : dummy.languageVSvision : time.bins || participant) +
	RT.based_Groups : dummy.languageVSvision : time.bins + time,
	data=EEG, control = lmerControl(optCtrl=list(maxfun=10000000)), method='S', expand_re=TRUE)

# save results

Group = gsub('^.*RT.based_Groups\\s*|\\s*:dummy.*$', '', rownames(coefficients(summary(m))))
Time.bin = str_extract(rownames(coefficients(summary(m))), "\\-*\\d+\\.*\\d*")
Estimate = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'Estimate'])
SE = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'Std. Error'])
Lower.CI = Estimate - 1.96 * SE
Upper.CI = Estimate + 1.96 * SE
t = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 't value'])
df = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'df'])
p = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'Pr(>|t|)'])

coeffs = data.frame(Group, Time.bin, Estimate, SE, Lower.CI, Upper.CI, t, df, p)

coeffs = coeffs[coeffs$Group=='(Intercept)' | coeffs$Group=='Slow' | coeffs$Group=='Quick',]

# Save table

coeffs$Time.bin = as.integer(as.character(coeffs$Time.bin))
coeffs$Group = as.character(coeffs$Group)
coeffs[coeffs$Group=='(Intercept)', 'Group'] = 'Intercept'
coeffs = coeffs[order(as.integer(as.character(coeffs$Time.bin))),]
coeffs = coeffs[order(coeffs$Group),]
coeffs[is.na(coeffs$Time.bin), 'Time.bin'] = 'Intercept'
coeffs = coeffs[,c(2,1,3:9)]

# Add asterisks about p values
coeffs$p.asterisks = NA
coeffs$p.asterisks = ifelse(coeffs$p < .001, '***', NA)
coeffs$p.asterisks = ifelse(coeffs$p > .001 & coeffs$p < .01, '**', coeffs$p.asterisks)
coeffs$p.asterisks = ifelse(coeffs$p > .01 & coeffs$p < .05, '*', coeffs$p.asterisks)

coeffs[3:8] = round(coeffs[3:8],2)
coeffs[9] = round(coeffs[9],3)

coeffs$Estimate.and.SE = paste0(coeffs$Estimate, " (", coeffs$SE, ")")
coeffs$t.and.df = paste0(coeffs$t, " (", coeffs$df, ")")
coeffs$t.and.df = str_trim(str_replace(coeffs$t.and.df, 'NA', ''))
coeffs = coeffs[,c('Time.bin','Group','Estimate.and.SE','Lower.CI','Upper.CI','t.and.df','p.asterisks')]
rownames(coeffs) <- NULL

# SAVE
write.csv(coeffs, 
  'Switch effect by group and language vs vision brain areas over time bins, computed across Switch conditions.csv', 
   na='', row.names=FALSE)

# See this plot as 'Switch effect by group and language vs vision brain areas over time bins (cf.
# Louwerse & Hutchinson, 2012, Figure 3A), computed across Switch conditions' in Plots script.

#######################









# Analysis over time bins (cf. Louwerse & Hutchinson, 2012: Tables 2 and 3 and Figure 3), 
# computed on Switch effect subtraction


# Create Switch effect subtraction: non-switch minus both switch conditions combined Method:
# The switch effect subtraction is computed into the Visual-to-visual condition, then that 
# is renamed as 'Switch.effect', and finally the data set is subsetted to keep only the 
# Switch.effect condition

EEG = EEG.125Hz

EEG[EEG$condition=='visual2visual', 'microvolts'] =
	EEG[EEG$condition=='visual2visual', 'microvolts'] -
	(( EEG[EEG$condition=='auditory2visual', 'microvolts'] + 
	EEG[EEG$condition=='haptic2visual', 'microvolts']) / 2 )

EEG$condition = revalue(EEG$condition, c("visual2visual" = "Switch.effect"))

EEG = EEG[EEG$condition=='Switch.effect',]



# Omnibus test for all period (cf. pages 5 and 6 of Louwerse & Hutchinson, 2012)

m = mixed(microvolts ~
	(dummy.languageVSvision : RT.based_Groups || participant) +	 
	# Above, estimation of correlation among random effects disabled, to facilitate convergence
	dummy.languageVSvision : RT.based_Groups + time,
	data=EEG, method='S', expand_re=TRUE)

warnings()
anova(m)
summary(m)



# Regression over time bins
# afex() w/ Satterthwaite, as default (Kenward Roger's alternative doesn't allow convergence)

m = mixed(microvolts ~
	(RT.based_Groups : dummy.languageVSvision : time.bins || participant) +
	RT.based_Groups : dummy.languageVSvision : time.bins + time,
	data=EEG, control = lmerControl(optCtrl=list(maxfun=10000000)), method='S', expand_re=TRUE)

# save results

Group = gsub('^.*RT.based_Groups\\s*|\\s*:dummy.*$', '', rownames(coefficients(summary(m))))
Time.bin = str_extract(rownames(coefficients(summary(m))), "\\-*\\d+\\.*\\d*")
Estimate = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'Estimate'])
SE = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'Std. Error'])
Lower.CI = Estimate - 1.96 * SE
Upper.CI = Estimate + 1.96 * SE
t = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 't value'])
df = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'df'])
p = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'Pr(>|t|)'])

coeffs = data.frame(Group, Time.bin, Estimate, SE, Lower.CI, Upper.CI, t, df, p)

coeffs = coeffs[coeffs$Group=='(Intercept)' | coeffs$Group=='Slow' | coeffs$Group=='Quick',]

# Save table

coeffs$Time.bin = as.integer(as.character(coeffs$Time.bin))
coeffs$Group = as.character(coeffs$Group)
coeffs[coeffs$Group=='(Intercept)', 'Group'] = 'Intercept'
coeffs = coeffs[order(as.integer(as.character(coeffs$Time.bin))),]
coeffs = coeffs[order(coeffs$Group),]
coeffs[is.na(coeffs$Time.bin), 'Time.bin'] = 'Intercept'
coeffs = coeffs[,c(2,1,3:9)]

# Add asterisks about p values
coeffs$p.asterisks = NA
coeffs$p.asterisks = ifelse(coeffs$p < .001, '***', NA)
coeffs$p.asterisks = ifelse(coeffs$p > .001 & coeffs$p < .01, '**', coeffs$p.asterisks)
coeffs$p.asterisks = ifelse(coeffs$p > .01 & coeffs$p < .05, '*', coeffs$p.asterisks)

coeffs[3:8] = round(coeffs[3:8],2)
coeffs[9] = round(coeffs[9],3)

coeffs$Estimate.and.SE = paste0(coeffs$Estimate, " (", coeffs$SE, ")")
coeffs$t.and.df = paste0(coeffs$t, " (", coeffs$df, ")")
coeffs$t.and.df = str_trim(str_replace(coeffs$t.and.df, 'NA', ''))
coeffs = coeffs[,c('Time.bin','Group','Estimate.and.SE','Lower.CI','Upper.CI','t.and.df','p.asterisks')]
rownames(coeffs) <- NULL

# SAVE
write.csv(coeffs, 
  'Switch effect by group and language vs vision brain areas over time bins, computed on Switch effect subtraction.csv', 
  na='', row.names=FALSE)

# See this plot as 'Switch effect by group and language vs vision brain areas over time bins
# (cf Louwerse & Hutchinson, 2012, Figure 3A), computed on Switch effect subtraction' in Plots script.

#######################




# Analysis over time bins (cf. Louwerse & Hutchinson, 2012: Tables 2 and 3 and Figure 3), 
# computed on Visual-to-visual condition

EEG = EEG.125Hz

EEG = EEG[EEG$condition=='visual2visual',]



# Omnibus test for all period (cf. pages 5 and 6 of Louwerse & Hutchinson, 2012)

m = mixed(microvolts ~
	(dummy.languageVSvision : RT.based_Groups || participant) +	 
	# above, estimation of correlation among random effects disabled, to facilitate convergence
	dummy.languageVSvision : RT.based_Groups + time,
	data=EEG, method='S', expand_re=TRUE)

warnings()
anova(m)
summary(m)


# Regression over time bins
# afex() w/ Satterthwaite, as default (Kenward Roger's alternative doesn't allow convergence)

m = mixed(microvolts ~
	(RT.based_Groups : dummy.languageVSvision : time.bins || participant) +
	RT.based_Groups : dummy.languageVSvision : time.bins + time,
	data=EEG, control = lmerControl(optCtrl=list(maxfun=10000000)), method='S', expand_re=TRUE)

# save results

Group = gsub('^.*RT.based_Groups\\s*|\\s*:dummy.*$', '', rownames(coefficients(summary(m))))
Time.bin = str_extract(rownames(coefficients(summary(m))), "\\-*\\d+\\.*\\d*")
Estimate = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'Estimate'])
SE = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'Std. Error'])
Lower.CI = Estimate - 1.96 * SE
Upper.CI = Estimate + 1.96 * SE
t = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 't value'])
df = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'df'])
p = as.vector(coefficients(summary(m))[1:length(rownames(coefficients(summary(m)))), 'Pr(>|t|)'])

coeffs = data.frame(Group, Time.bin, Estimate, SE, Lower.CI, Upper.CI, t, df, p)

coeffs = coeffs[coeffs$Group=='(Intercept)' | coeffs$Group=='Slow' | coeffs$Group=='Quick',]

# Save table

coeffs$Time.bin = as.integer(as.character(coeffs$Time.bin))
coeffs$Group = as.character(coeffs$Group)
coeffs[coeffs$Group=='(Intercept)', 'Group'] = 'Intercept'
coeffs = coeffs[order(as.integer(as.character(coeffs$Time.bin))),]
coeffs = coeffs[order(coeffs$Group),]
coeffs[is.na(coeffs$Time.bin), 'Time.bin'] = 'Intercept'
coeffs = coeffs[,c(2,1,3:9)]

# Add asterisks about p values
coeffs$p.asterisks = NA
coeffs$p.asterisks = ifelse(coeffs$p < .001, '***', NA)
coeffs$p.asterisks = ifelse(coeffs$p > .001 & coeffs$p < .01, '**', coeffs$p.asterisks)
coeffs$p.asterisks = ifelse(coeffs$p > .01 & coeffs$p < .05, '*', coeffs$p.asterisks)

coeffs[3:8] = round(coeffs[3:8],2)
coeffs[9] = round(coeffs[9],3)

coeffs$Estimate.and.SE = paste0(coeffs$Estimate, " (", coeffs$SE, ")")
coeffs$t.and.df = paste0(coeffs$t, " (", coeffs$df, ")")
coeffs$t.and.df = str_trim(str_replace(coeffs$t.and.df, 'NA', ''))
coeffs = coeffs[,c('Time.bin','Group','Estimate.and.SE','Lower.CI','Upper.CI','t.and.df','p.asterisks')]
rownames(coeffs) <- NULL

# SAVE
write.csv(coeffs, 
  'Switch effect by group and language vs vision brain areas over time bins, computed on Visual-to-visual condition.csv', 
  na='', row.names=FALSE)

# See this plot as 'Switch effect by group and language vs vision brain areas over time bins
# (cf Louwerse & Hutchinson, 2012, Figure 3A), computed on Visual-to-visual condition' in Plots script.

#######################



# Since there might be differences across Switch conditions influencing the analysis above in the background,
# a plot has been made to test that. It's in the Plots script, titled 'Waveforms by group, switch
# condition and language vs vision brain area over time bins'. It shows that the different Switch conditions
# cannot influence the plot right above in any considerable way, as the difference across Switch conditions 
# does not entail differences in the polarity of the waveforms.
