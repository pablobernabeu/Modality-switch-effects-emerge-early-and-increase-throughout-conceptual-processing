setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Export Files')

# IMPORTING CONDITION-AVERAGED DATA

# ACKNOWLEDGMENT: This code is partly based on code by Gwilym Lockwood:
# https://osf.io/mu3nw/

install.packages('gtools')
install.packages('tabplot')
install.packages('plyr')
install.packages('ggplot2')
install.packages('pastecs')
install.packages('reshape')
install.packages('dplyr')
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
library(devtools)
install_github("kassambara/easyGgplot2")
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

# NOTE ON VALID DATA: Once preprocessed and averaged per participant 
# and CMS condition in Brain Vision Analyzer 2, the data were exported, 
# and then imported into R. From then, participants 7 and 33 were removed: 
# participant 7 due to too noisy ERPs, and participant 33 due to 
# presenting a response accuracy < 50% (see behavioural analyses). 
# Beyond that, outlier trials were not sought because it's not so 
# standard a practice with EEG, as the randomization of the trials 
# and the large amount of the data (consider all time points and 
# electrodes) tends to minimize the influence of those cases.

# NOTE ON NAMING: The files below, which were exported from Brain Vision 
# Analyzer, had been named on the basis of previous research, specifically 
# alluding to the conceptual system that would presumably pick up the
# switch manipulation (Louwerse & Connell, 2011). Yet henceforth they'll 
# be renamed based on the modalities in each switch proper.


visual2visualfiles = as.vector(list.files(pattern=
"*TotalMatch.txt") ) 
str(visual2visualfiles)
head(visual2visualfiles)
tail(visual2visualfiles)

haptic2visualfiles = as.vector(list.files(pattern=
"*EmbodiedMismatch.txt"))
str(haptic2visualfiles)
head(haptic2visualfiles)
tail(haptic2visualfiles)

auditory2visualfiles = as.vector(list.files(pattern=
"*TotalMismatch.txt"))
str(auditory2visualfiles)
head(auditory2visualfiles)
tail(auditory2visualfiles)

# Construct rawdata object (structured as a 'list' of results)
visual2visuallist = lapply(1:length(visual2visualfiles),function(x) {
read.table(visual2visualfiles[x], header=FALSE) } )

haptic2visuallist = lapply(1:length(haptic2visualfiles),function(x) {
read.table(haptic2visualfiles[x], header=FALSE) } )

auditory2visuallist = lapply(1:length(auditory2visualfiles),function(x) {
read.table(auditory2visualfiles[x], header=FALSE) } )

visual2visualdata = ldply(visual2visuallist, data.frame)
haptic2visualdata = ldply(haptic2visuallist, data.frame)
auditory2visualdata = ldply(auditory2visuallist, data.frame)

# Sort out column names
seq = seq(-200, 798, 2)	# In milliseconds: baseline period, trial
					# period, measurement frequency (= 500 Hz)

names(visual2visualdata) = c('electrode', seq)
names(haptic2visualdata) = c('electrode', seq)
names(auditory2visualdata) = c('electrode', seq)

# Now put it in long format where microvolts is a variable/identifier 
# and shove them together...

visual2visual_melted = melt(visual2visualdata, id.vars="electrode")
visual2visual_melted = visual2visual_melted %>%
  mutate(condition = "visual2visual")
names(visual2visual_melted) = c('electrode', 'time', 'microvolts',
'condition')

haptic2visual_melted = melt(haptic2visualdata, id.vars="electrode")
haptic2visual_melted = haptic2visual_melted %>%
  mutate(condition = "haptic2visual")
names(haptic2visual_melted) = c('electrode', 'time', 'microvolts',
'condition')

auditory2visual_melted = melt(auditory2visualdata, id.vars="electrode")
auditory2visual_melted = auditory2visual_melted %>%
  mutate(condition = "auditory2visual")
names(auditory2visual_melted) = c('electrode', 'time', 'microvolts',
'condition')

# This works fine and creates two really long melted dataframes with data for 
# each electrode and condition. BUT!!! no participant data yet. Couldn't just 
# put in names of files because the switch to long format doesn't like that

# first, create a list where the participant name occurs according 
# to the number of electrodes in that participant's file
# For  Cond 1
participants = sub("\\).*", "", sub(".*\\(", "", visual2visualfiles))
head(participants)
fulllist1 = mapply(function(x,y) rep(x, y), participants, 59)
fulllist1 = unlist(fulllist1)     # remove all factors, print long list
fulllist1 = rep(fulllist1, 500)   # correct for individual time point

# For Cond 2
participants = sub("\\).*", "", sub(".*\\(", "", haptic2visualfiles))
head(participants)
fulllist2 = mapply(function(x,y) rep(x, y), participants, 59)
fulllist2 = unlist(fulllist2)
fulllist2 = rep(fulllist2, 500)

# For Cond 3
participants = sub("\\).*", "", sub(".*\\(", "", auditory2visualfiles))
head(participants)
fulllist3 = mapply(function(x,y) rep(x, y), participants, 59)
fulllist3 = unlist(fulllist3)
fulllist3 = rep(fulllist3, 500)

# second, create a column in the melted dataframes saying which participant 
# is at each data point
visual2visual_melted = visual2visual_melted %>%
  mutate(participant = fulllist1)
haptic2visual_melted = haptic2visual_melted %>%
  mutate(participant = fulllist2)
auditory2visual_melted = auditory2visual_melted %>%
  mutate(participant = fulllist3)

head(visual2visual_melted)
tail(visual2visual_melted)
head(haptic2visual_melted)
tail(haptic2visual_melted)
head(auditory2visual_melted)
tail(auditory2visual_melted)

# now combine it all into one dataframe

EEG = rbind(visual2visual_melted,
haptic2visual_melted, auditory2visual_melted)

str(EEG)
head(EEG)
tail(EEG)
str(EEG$time)

# by now, participants' name is their file. so rename via 
# 'regular expression'

EEG$participant[grep("1_", EEG$participant)] = "1"
EEG$participant[grep("2_", EEG$participant) ] = "2"
EEG$participant[grep("3_", EEG$participant) ] = "3"
EEG$participant[grep("4_", EEG$participant) ] = "4"
EEG$participant[grep("5_", EEG$participant) ] = "5"
EEG$participant[grep("6_", EEG$participant) ] = "6"
EEG$participant[grep("7_", EEG$participant) ] = "7"
EEG$participant[grep("8_", EEG$participant) ] = "8"
EEG$participant[grep("9_", EEG$participant) ] = "9"
EEG$participant[grep("10_", EEG$participant) ] = "10"
EEG$participant[grep("11_", EEG$participant) ] = "11"
EEG$participant[grep("12_", EEG$participant) ] = "12"
EEG$participant[grep("13_", EEG$participant) ] = "13"
EEG$participant[grep("14_", EEG$participant) ] = "14"
EEG$participant[grep("15_", EEG$participant) ] = "15"
EEG$participant[grep("16_", EEG$participant) ] = "16"
EEG$participant[grep("17_", EEG$participant) ] = "17"
EEG$participant[grep("18_", EEG$participant) ] = "18"
EEG$participant[grep("19_", EEG$participant) ] = "19"
EEG$participant[grep("20_", EEG$participant) ] = "20"
EEG$participant[grep("21_", EEG$participant) ] = "21"
EEG$participant[grep("22_", EEG$participant) ] = "22"
EEG$participant[grep("23_", EEG$participant) ] = "23"
EEG$participant[grep("24_", EEG$participant) ] = "24"
EEG$participant[grep("26_", EEG$participant) ] = "26"
EEG$participant[grep("27_", EEG$participant) ] = "27"
EEG$participant[grep("28_", EEG$participant) ] = "28"
EEG$participant[grep("29_", EEG$participant) ] = "29"
EEG$participant[grep("30_", EEG$participant) ] = "30"
EEG$participant[grep("31_", EEG$participant) ] = "31"
EEG$participant[grep("32_", EEG$participant) ] = "32"
EEG$participant[grep("33_", EEG$participant) ] = "33"
EEG$participant[grep("34_", EEG$participant) ] = "34"
EEG$participant[grep("35_", EEG$participant) ] = "35"
EEG$participant[grep("36_", EEG$participant) ] = "36"
EEG$participant[grep("37_", EEG$participant) ] = "37"
EEG$participant[grep("38_", EEG$participant) ] = "38"
EEG$participant[grep("39_", EEG$participant) ] = "39"
EEG$participant[grep("40_", EEG$participant) ] = "40"
EEG$participant[grep("41_", EEG$participant) ] = "41"
EEG$participant[grep("42_", EEG$participant) ] = "42"
EEG$participant[grep("44_", EEG$participant) ] = "44"
EEG$participant[grep("45_", EEG$participant) ] = "45"
EEG$participant[grep("46_", EEG$participant) ] = "46"
EEG$participant[grep("47_", EEG$participant) ] = "47"
EEG$participant[grep("48_", EEG$participant) ] = "48"
EEG$participant[grep("49_", EEG$participant) ] = "49"
EEG$participant[grep("50_", EEG$participant) ] = "50"

# Take out participant 33, the one with accuracy below 50%
EEG = EEG[!EEG$participant=='33',]

# check number of observations
table(EEG$participant)

# now include Group variable
EEG['group'] = NA
EEG$group = as.factor(EEG$group)
EEG$participant = as.factor(EEG$participant)

EEG$group = ifelse(EEG$participant == '1', 'Quick', NA)
EEG$group = ifelse(EEG$participant == '2', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '3', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '4', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '5', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '6', 'null', EEG$group)
EEG$group = ifelse(EEG$participant == '7', 'null', EEG$group)
EEG$group = ifelse(EEG$participant == '8', 'null', EEG$group)
EEG$group = ifelse(EEG$participant == '9', 'null', EEG$group)
EEG$group = ifelse(EEG$participant == '10', 'null', EEG$group)
EEG$group = ifelse(EEG$participant == '11', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '12', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '13', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '26', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '27', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '28', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '29', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '30', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '31', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '32', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '40', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '41', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '44', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '45', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '49', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '50', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '14', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '15', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '16', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '17', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '18', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '19', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '20', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '21', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '22', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '23', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '24', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '34', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '35', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '36', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '37', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '38', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '39', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '42', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '46', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '47', 'Selfpaced', EEG$group)
EEG$group = ifelse(EEG$participant == '48', 'Selfpaced', EEG$group)


# Add participant mean RT. This is each participant's mean RT across all 
# trials. It is used as a scaled variant of the Group factor. However, 
# in a deep sense, MRT is not necessarily better because because it
# lacks control. It's a post-hoc measure, whereas Group is an experi-
# mental manipulation (even in spite of its weak influence).
# The code retrieving MRT is in the 'behavioural data' script. 

EEG['MRT'] = NA
EEG$MRT = as.numeric(EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '1', 753.0556, NA)
EEG$MRT = ifelse(EEG$participant == '2', 768.8889, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '3', 714.75, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '4', 784.3519, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '5', 1031.639, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '6', 835.1296, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '7', 791.0648, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '8', 591.4167, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '9', 1282.704, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '10', 583.5278, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '11', 872.3458, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '12', 557.5556, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '13', 572.4444, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '14', 738.2963, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '15', 755.4722, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '16', 871.2222, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '17', 514.1667, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '18', 515.9815, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '19', 1954.12, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '20', 582.5278, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '21', 737.5185, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '22', 963.5093, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '23', 1138.787, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '24', 791.5926, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '26', 375.463, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '27', 495.7493, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '28', 670.0741, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '29', 811.4057, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '30', 725.4766, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '31', 480.5185, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '32', 571.451, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '34', 727.537, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '35', 898.4167, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '36', 436.0833, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '37', 1206.509, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '38', 634.3056, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '39', 1034.185, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '40', 345.1759, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '41', 868.8426, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '42', 760.9444, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '44', 430.3925, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '45', 969.3611, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '46', 680.0185, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '47', 685.5926, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '48', 565.9815, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '49', 651.6389, EEG$MRT)
EEG$MRT = ifelse(EEG$participant == '50', 692.787, EEG$MRT)


# Create two participant groups based on mean RT from above. 
# Participant '7' is not included in this calculation because this
# participant will be removed from statistical analyses due to EEG.
transitory = EEG[!EEG$participant=='7', c('participant','MRT')]
transitory$RT.based_Groups = as.numeric(cut2(transitory$MRT, g=2))
keeps = c('participant','RT.based_Groups')
transitory = transitory[ , names(transitory) %in% keeps]
transitory = aggregate(RT.based_Groups ~ participant, transitory, mean, na.action = na.pass)
EEG.test = merge(EEG,transitory,by='participant', all=T)
EEG = EEG.test
EEG$RT.based_Groups = as.factor(EEG$RT.based_Groups)
EEG$RT.based_Groups = revalue(EEG$RT.based_Groups, c("1"="Quick", "2"="Slow"))

# double-check number of ptps in each group
unique(EEG[EEG$RT.based_Groups=='Quick',]$participant)# 23
unique(EEG[EEG$RT.based_Groups=='Slow',]$participant)	# 23

# Mean RT per group
summaryBy(MRT ~ RT.based_Groups, data=EEG, FUN=c(mean, SD))  # NA = ptp 7



# Add participants' age
EEG['Age_months'] = NA
EEG$Age_months = as.numeric(EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '1', 274.5172, NA)
EEG$Age_months = ifelse(EEG$participant == '2', 221.2581, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '3', 250.6839, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '4', 226.3226, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '5', 290.6172, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '6', 242.8505, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '7', 241.6172, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '8', 261.3871, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '9', 297.9505, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '10', 373.4839, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '11', 243.7505, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '12', 240.9172, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '13', 225.3871, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '14', 295.0323, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '15', 237.4516, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '16', 236.4516, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '17', 235.2258, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '18', 280.0323, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '19', 290.4194, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '20', 236.6172, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '21', 265.4505, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '22', 236.7505, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '23', 292.8172, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '24', 239.3226, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '26', 244.4839, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '27', 284.3226, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '28', 288.6505, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '29', 300.4839, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '30', 225.0645, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '31', 276.5172, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '32', 304.0323, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '34', 237.9172, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '35', 256.7505, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '36', 269.7505, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '37', 285.3548, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '38', 305.6505, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '39', 259.4839, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '40', 261.6505, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '41', 245.6172, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '42', 256.3548, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '44', 272.4194, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '45', 253.0645, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '46', 238.3226, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '47', 276.8172, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '48', 253.7505, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '49', 343.2903, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '50', 363.7505, EEG$Age_months)

# General age range in years
summary(EEG[!EEG$RT.based_Groups=='',]$Age_months/12, FUN=range)

# General age mean in years:
summary(EEG[!EEG$RT.based_Groups=='',]$Age_months/12, FUN=mean)

# Age figures per group
# Age range in each group in years
summaryBy(Age_months/12 ~ RT.based_Groups, EEG, FUN=range)

# Average age in each group in years:
summaryBy(Age_months/12 ~ RT.based_Groups, EEG, FUN=mean) 	

# Distribution above and below age average in each group
str(unique(factor(EEG[c(EEG$Age_months < mean(EEG$Age_months) & 
EEG$RT.based_Groups=='Quick'), 'participant']))) 		
str(unique(factor(EEG[c(EEG$Age_months > mean(EEG$Age_months) & 
EEG$RT.based_Groups=='Quick'), 'participant']))) 		
str(unique(factor(EEG[c(EEG$Age_months < mean(EEG$Age_months) & 
EEG$RT.based_Groups=='Slow'), 'participant']))) 	
str(unique(factor(EEG[c(EEG$Age_months > mean(EEG$Age_months) & 
EEG$RT.based_Groups=='Slow'), 'participant'])))

# Age will be controlled for in the statistical models.



# Now add participant gender
EEG$Gender = NA
EEG$Gender = ifelse(EEG$participant == '1', 'F', NA)
EEG$Gender = ifelse(EEG$participant == '2', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '3', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '4', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '5', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '6', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '7', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '8', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '9', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '10', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '11', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '12', 'M', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '13', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '14', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '15', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '16', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '17', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '18', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '19', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '20', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '21', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '22', 'M', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '23', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '24', 'M', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '26', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '27', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '28', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '29', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '30', 'M', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '31', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '32', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '33', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '34', 'M', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '35', 'M', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '36', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '37', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '38', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '39', 'M', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '40', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '41', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '42', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '44', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '45', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '46', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '47', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '48', 'F', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '49', 'M', EEG$Gender)
EEG$Gender = ifelse(EEG$participant == '50', 'M', EEG$Gender)
EEG$Gender = as.factor(EEG$Gender)

# Add handedness info: whether participant is lefthanded or not
EEG$Lefthanded = NA
EEG$Lefthanded = ifelse(EEG$participant == '1', 'N', NA)
EEG$Lefthanded = ifelse(EEG$participant == '2', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '3', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '4', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '5', 'Y', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '6', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '7', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '8', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '9', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '10', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '11', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '12', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '13', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '14', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '15', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '16', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '17', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '18', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '19', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '20', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '21', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '22', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '23', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '24', 'Y', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '26', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '27', 'Y', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '28', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '29', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '30', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '31', 'Y', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '32', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '33', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '34', 'Y', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '35', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '36', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '37', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '38', 'Y', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '39', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '40', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '41', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '42', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '44', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '45', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '46', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '47', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '48', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '49', 'N', EEG$Lefthanded)
EEG$Lefthanded = ifelse(EEG$participant == '50', 'N', EEG$Lefthanded)
EEG$Lefthanded = as.factor(EEG$Lefthanded)

# Check gender and handedness per group. There are this many observations per participant...
str(EEG[EEG$participant=='1',])
# and therefore:
table(EEG$RT.based_Groups, EEG$Gender)/88500
table(EEG$RT.based_Groups, EEG$Lefthanded)/88500


# Set up areas (this is based on the MPI Psycholinguistics's custom, equi-
# distant ActiCAP, but is quite analogous to famous 10-20 layout). See at:
# https://goo.gl/W0sBNr

unique(EEG$electrode)
midfront = c('C60', 'C59', 'C58', 'C31')
midback = c('C29', 'C28', 'C27', 'C26')
leftfront = c('C53', 'C57', 'C52', 'C46', 'C45', 'C39', 'C51',
 'C56', 'C44', 'C38', 'C34')
rightfront = c('C21', 'C14', 'C7', 'C2', 'C6', 'C13', 'C20',
 'C12', 'C19', 'C25', 'C24')
leftback = c('C33', 'C36', 'C42', 'C49', 'C35', 'C41', 'C48',
 'C55', 'C40', 'C47', 'C54')
rightback = c('C1', 'C4', 'C10', 'C17', 'C3', 'C9', 'C16',
'C23', 'C8', 'C15', 'C22')

# Add quadrant and location columns to EEG and to EEG.window
EEG = EEG %>%
  mutate (quadrant = ifelse(electrode %in% leftback, "leftback",
                        ifelse(electrode %in% rightback, "rightback",
                               ifelse(electrode %in% leftfront, "leftfront",
                             ifelse(electrode %in% rightfront, "rightfront",
                                                     NA  )))))
EEG = EEG %>%
  mutate (location = ifelse(electrode %in% leftback, "posterior",
                     ifelse(electrode %in% rightback, "posterior",
                     ifelse(electrode %in% leftfront, "anterior",
                     ifelse(electrode %in% rightfront, "anterior",
				ifelse(electrode %in% midfront, "anterior",
				ifelse(electrode %in% midback, "posterior",
                                         			NA  )))))))


# EEG data is massive, such that the models take hours. Downsample 
# to 125 Hz by averaging across every four measurements. Waveform 
# plots, however, will still take the 500 Hz data.

# Downsampled data was analysed for the Cogsci 2017 paper, whereas
# the original 500 Hz data was analysed for the subsequent journal
#paper.


# Downsample. First, rename every three time points as previous one:
EEG$time2 = EEG$time
EEG$time2 = as.integer(as.character(EEG$time2))
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

# Last, aggregate by mean. Conveniently, this builds on the 
# full original 500 Hz data while reducing computation. 
EEG.2 = aggregate(microvolts ~ time2 * condition * participant * electrode *
  location * group * MRT * Age_months * Lefthanded * Gender * RT.based_Groups,
  EEG, mean)
str(EEG.2)
# Successfully downsampled, with one averaged measurement every 8 ms.
EEG$time2 = NULL   # EEG remains as is for the waveform plots

# Cf. original and downsampled data:
summary(EEG$microvolts)
# Original: no missing cases. Range -44 to +42
summary(EEG.2$microvolts)
# Downsampled: no missing cases. Range -74 to +79

# Set Condition reference to visual2visual
EEG.2$condition = as.factor(EEG.2$condition)
EEG.2$condition = relevel(EEG.2$condition, ref="visual2visual")

# Mean-center and scale continuous IVs
EEG.2$s_MRT = scale(EEG.2$MRT)
EEG.2$s_Age_months = scale(EEG.2$Age_months)



# Create all four windows in steps of 2ms for time window you want to measure
# (2 ms because the measure was indeed made every 2 ms = 500 Hz). Windows are
# virtually identical to those in Hald et al. (2011: Front Psych). Only
# Window 4 was extended by 50 ms based on visual inspection of the waves and
# because the Late Positive Component is known to extend beyond 700 ms
# (Balass, Nelson, & Perfetti, 2010; Hasko, Groth, Bruder, Bartling, &
# Schulte-Körne, 2013).


# Window 1 (160-216 ms). This window stands between an earlier N2 and a
# classic N2.  Being so early, few hypotheses can be established. Yet,
# semantic effects have been found before (Amsel, 2011; Hauk, Coutout, 
# Holden, & Chen, 2012).

EEG.window1 = EEG.2[EEG.2$time %in% seq(160, 214, 2),]
# Sequence up to 214 because that time point extends right up to 216

EEG.window1$time = factor(EEG.window1$time)
EEG.window1$time2 = NULL
str(EEG.window1)
str(EEG.window1$location)
# filter out no-location electrodes
EEG.window1=EEG.window1[!is.na(EEG.window1$location),]
EEG.window1$group=factor(EEG.window1$group)
EEG.window1$participant=factor(EEG.window1$participant)
EEG.window1$location=factor(EEG.window1$location)
unique(EEG.window1$participant)
unique(EEG.window1$group)
unique(EEG.window1$location)
str(EEG.window1)

# Save
saveRDS(EEG.window1, 'EEG.window1.rds')


# Window 2 (270-370 ms). This window could capture a late N2 in anterior 
# regions, and a P3 in posterior regions. Both components have been 
# related to perceptual mismatch (Folstein & van Petten, 2008: 
# Psychophysiology).

EEG.window2 = EEG.2[EEG.2$time %in% seq(270, 368, 2),]
# Sequence up to 368 because that time point extends right up to 370

EEG.window2$time = factor(EEG.window2$time)
EEG.window2$time2 = NULL
str(EEG.window2)
str(EEG.window2$location)
# filter out no-location electrodes
EEG.window2=EEG.window2[!is.na(EEG.window2$location),]
EEG.window2$group=factor(EEG.window2$group)
EEG.window2$participant=factor(EEG.window2$participant)
EEG.window2$location=factor(EEG.window2$location)
unique(EEG.window2$participant)
unique(EEG.window2$group)
unique(EEG.window2$location)
str(EEG.window2)

# Save
saveRDS(EEG.window2, 'EEG.window2.rds')


# Window 3 (350-550 ms). This is the classic N400 window.

EEG.window3 = EEG.2[EEG.2$time %in% seq(350, 548, 2),]
# Sequence up to 548 because that time point extends right up to 550

EEG.window3 = EEG.2[EEG.2$time %in% window3,]
EEG.window3$time = factor(EEG.window3$time)
EEG.window3$time2 = NULL
str(EEG.window3)
str(EEG.window3$location)
# filter out no-location electrodes
EEG.window3=EEG.window3[!is.na(EEG.window3$location),]
EEG.window3$group=factor(EEG.window3$group)
EEG.window3$participant=factor(EEG.window3$participant)
EEG.window3$location=factor(EEG.window3$location)
unique(EEG.window3$participant)
unique(EEG.window3$group)
unique(EEG.window3$location)
str(EEG.window3)

# Save
saveRDS(EEG.window3, 'EEG.window3.rds')


# Window 4 (500-750 ms). This window could capture late positive components.

EEG.window4 = EEG.2[EEG.2$time %in% seq(500, 748, 2),]
# Sequence up to 748 because that time point extends right up to 750

EEG.window4 = EEG.2[EEG.2$time %in% window4,]
EEG.window4$time = factor(EEG.window4$time)
EEG.window4$time2 = NULL
str(EEG.window4)
str(EEG.window4$location)
# filter out no-location electrodes
EEG.window4=EEG.window4[!is.na(EEG.window4$location),]
EEG.window4$group=factor(EEG.window4$group)
EEG.window4$participant=factor(EEG.window4$participant)
EEG.window4$location=factor(EEG.window4$location)
unique(EEG.window4$participant)
unique(EEG.window4$group)
unique(EEG.window4$location)
str(EEG.window4)

# Save
saveRDS(EEG.window4, 'EEG.window4.rds')



########################################################################




# Remove participant '7' because of very poor EEG signal (out of 36 trials per condition,
# this data only kept 1, 2, and 4 respectively per condition after artefact rejection).
# Remove only from single windows, leaving general 'EEG' as is.

EEG.window1 = EEG.window1[!EEG.window1$participant=='7',]
EEG.window2 = EEG.window2[!EEG.window2$participant=='7',]
EEG.window3 = EEG.window3[!EEG.window3$participant=='7',]
EEG.window4 = EEG.window4[!EEG.window4$participant=='7',]
