
# Trial-by-trial ERPs

setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Trial-by-trial export files')

# IMPORTING TRIAL-BY-TRIAL DATA

# ACKNOWLEDGMENT: This code is partly based on code by Gwilym Lockwood: https://osf.io/mu3nw/

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
install.packages("data.table")
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


# NOTE ON VALID DATA: Once preprocessed in Brain Vision Analyzer 2, 
# the 108 target trials of each participant were exported, and then 
# imported into R. From then, participants 7 and 33 were removed: 
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

visual2visualfiles <- as.vector(list.files(pattern=
"*TotalMatch_baselinecorr_artif.txt") ) 
str(visual2visualfiles)
head(visual2visualfiles)
tail(visual2visualfiles)

haptic2visualfiles <- as.vector(list.files(pattern=
"*EmbodiedMismatch_baselinecorr_artif.txt") ) 
str(haptic2visualfiles)
head(haptic2visualfiles)
tail(haptic2visualfiles)

auditory2visualfiles <- as.vector(list.files(pattern=
"*TotalMismatch_baselinecorr_artif.txt") ) 
str(auditory2visualfiles)
head(auditory2visualfiles)
tail(auditory2visualfiles)

# Construct rawdata object (structured as a 'list' of results)
visual2visuallist <- lapply(1:length(visual2visualfiles),function(x) { 
read.table(visual2visualfiles[x], header=FALSE) } )

haptic2visuallist <- lapply(1:length(haptic2visualfiles),function(x) { 
read.table(haptic2visualfiles[x], header=FALSE) } )

auditory2visuallist <- lapply(1:length(auditory2visualfiles),function(x) { 
read.table(auditory2visualfiles[x], header=FALSE) } )

visual2visualdata <- ldply(visual2visuallist, data.frame)
haptic2visualdata <- ldply(haptic2visuallist, data.frame)
auditory2visualdata <- ldply(auditory2visuallist, data.frame)

# Sort out column names
seq <- seq(from=-200, to=798, by=2) # In milliseconds: baseline period, trial
      # period, measurement frequency (= 500 Hz)

finalseq = c(seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,
seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq,seq)        # x 36, the no. of trials

names(visual2visualdata) <- c('electrode', finalseq)
names(haptic2visualdata) <- c('electrode', finalseq)
names(auditory2visualdata) <- c('electrode', finalseq)
head(auditory2visualdata)   # correct
str(auditory2visualdata)


# Split trials apart, and re-bind as rows, in each condition separately:

# Visual to visual
trial_1 = visual2visualdata[,c(1, 2:501)]
trial_2 = visual2visualdata[,c(1, 502:1001)]
trial_3 = visual2visualdata[,c(1, 1002:1501)]
trial_4 = visual2visualdata[,c(1, 1502:2001)]
trial_5 = visual2visualdata[,c(1, 2002:2501)]
trial_6 = visual2visualdata[,c(1, 2502:3001)]
trial_7 = visual2visualdata[,c(1, 3002:3501)]
trial_8 = visual2visualdata[,c(1, 3502:4001)]
trial_9 = visual2visualdata[,c(1, 4002:4501)]
trial_10 = visual2visualdata[,c(1, 4502:5001)]
trial_11 = visual2visualdata[,c(1, 5002:5501)]
trial_12 = visual2visualdata[,c(1, 5502:6001)]
trial_13 = visual2visualdata[,c(1, 6002:6501)]
trial_14 = visual2visualdata[,c(1, 6502:7001)]
trial_15 = visual2visualdata[,c(1, 7002:7501)]
trial_16 = visual2visualdata[,c(1, 7502:8001)]
trial_17 = visual2visualdata[,c(1, 8002:8501)]
trial_18 = visual2visualdata[,c(1, 8502:9001)]
trial_19 = visual2visualdata[,c(1, 9002:9501)]
trial_20 = visual2visualdata[,c(1, 9502:10001)]
trial_21 = visual2visualdata[,c(1, 10002:10501)]
trial_22 = visual2visualdata[,c(1, 10502:11001)]
trial_23 = visual2visualdata[,c(1, 11002:11501)]
trial_24 = visual2visualdata[,c(1, 11502:12001)]
trial_25 = visual2visualdata[,c(1, 12002:12501)]
trial_26 = visual2visualdata[,c(1, 12502:13001)]
trial_27 = visual2visualdata[,c(1, 13002:13501)]
trial_28 = visual2visualdata[,c(1, 13502:14001)]
trial_29 = visual2visualdata[,c(1, 14002:14501)]
trial_30 = visual2visualdata[,c(1, 14502:15001)]
trial_31 = visual2visualdata[,c(1, 15002:15501)]
trial_32 = visual2visualdata[,c(1, 15502:16001)]
trial_33 = visual2visualdata[,c(1, 16002:16501)]
trial_34 = visual2visualdata[,c(1, 16502:17001)]
trial_35 = visual2visualdata[,c(1, 17002:17501)]
trial_36 = visual2visualdata[,c(1, 17502:18001)]

visual2visualdata <- rbind(trial_1,trial_2,trial_3,trial_4,trial_5,trial_6,trial_7,trial_8,trial_9,trial_10,
trial_11,trial_12,trial_13,trial_14,trial_15,trial_16,trial_17,trial_18,trial_19,trial_20,trial_21,
trial_22,trial_23,trial_24,trial_25,trial_26,trial_27,trial_28,trial_29,trial_30,trial_31,trial_32,
trial_33,trial_34,trial_35,trial_36)

# Create trial names
visual2visualdata$trial = rep(1:36, each=2832)
count(visual2visualdata$trial)
str(visual2visualdata)




# Haptic to visual
trial_1 = haptic2visualdata[,c(1, 2:501)]
trial_2 = haptic2visualdata[,c(1, 502:1001)]
trial_3 = haptic2visualdata[,c(1, 1002:1501)]
trial_4 = haptic2visualdata[,c(1, 1502:2001)]
trial_5 = haptic2visualdata[,c(1, 2002:2501)]
trial_6 = haptic2visualdata[,c(1, 2502:3001)]
trial_7 = haptic2visualdata[,c(1, 3002:3501)]
trial_8 = haptic2visualdata[,c(1, 3502:4001)]
trial_9 = haptic2visualdata[,c(1, 4002:4501)]
trial_10 = haptic2visualdata[,c(1, 4502:5001)]
trial_11 = haptic2visualdata[,c(1, 5002:5501)]
trial_12 = haptic2visualdata[,c(1, 5502:6001)]
trial_13 = haptic2visualdata[,c(1, 6002:6501)]
trial_14 = haptic2visualdata[,c(1, 6502:7001)]
trial_15 = haptic2visualdata[,c(1, 7002:7501)]
trial_16 = haptic2visualdata[,c(1, 7502:8001)]
trial_17 = haptic2visualdata[,c(1, 8002:8501)]
trial_18 = haptic2visualdata[,c(1, 8502:9001)]
trial_19 = haptic2visualdata[,c(1, 9002:9501)]
trial_20 = haptic2visualdata[,c(1, 9502:10001)]
trial_21 = haptic2visualdata[,c(1, 10002:10501)]
trial_22 = haptic2visualdata[,c(1, 10502:11001)]
trial_23 = haptic2visualdata[,c(1, 11002:11501)]
trial_24 = haptic2visualdata[,c(1, 11502:12001)]
trial_25 = haptic2visualdata[,c(1, 12002:12501)]
trial_26 = haptic2visualdata[,c(1, 12502:13001)]
trial_27 = haptic2visualdata[,c(1, 13002:13501)]
trial_28 = haptic2visualdata[,c(1, 13502:14001)]
trial_29 = haptic2visualdata[,c(1, 14002:14501)]
trial_30 = haptic2visualdata[,c(1, 14502:15001)]
trial_31 = haptic2visualdata[,c(1, 15002:15501)]
trial_32 = haptic2visualdata[,c(1, 15502:16001)]
trial_33 = haptic2visualdata[,c(1, 16002:16501)]
trial_34 = haptic2visualdata[,c(1, 16502:17001)]
trial_35 = haptic2visualdata[,c(1, 17002:17501)]
trial_36 = haptic2visualdata[,c(1, 17502:18001)]

haptic2visualdata <- rbind(trial_1,trial_2,trial_3,trial_4,trial_5,trial_6,trial_7,trial_8,trial_9,trial_10,
trial_11,trial_12,trial_13,trial_14,trial_15,trial_16,trial_17,trial_18,trial_19,trial_20,trial_21,
trial_22,trial_23,trial_24,trial_25,trial_26,trial_27,trial_28,trial_29,trial_30,trial_31,trial_32,
trial_33,trial_34,trial_35,trial_36)

# Create trial names
haptic2visualdata$trial = rep(1:36, each=2832)
count(haptic2visualdata$trial)
str(haptic2visualdata)




# Auditory to visual
trial_1 = auditory2visualdata[,c(1, 2:501)]
trial_2 = auditory2visualdata[,c(1, 502:1001)]
trial_3 = auditory2visualdata[,c(1, 1002:1501)]
trial_4 = auditory2visualdata[,c(1, 1502:2001)]
trial_5 = auditory2visualdata[,c(1, 2002:2501)]
trial_6 = auditory2visualdata[,c(1, 2502:3001)]
trial_7 = auditory2visualdata[,c(1, 3002:3501)]
trial_8 = auditory2visualdata[,c(1, 3502:4001)]
trial_9 = auditory2visualdata[,c(1, 4002:4501)]
trial_10 = auditory2visualdata[,c(1, 4502:5001)]
trial_11 = auditory2visualdata[,c(1, 5002:5501)]
trial_12 = auditory2visualdata[,c(1, 5502:6001)]
trial_13 = auditory2visualdata[,c(1, 6002:6501)]
trial_14 = auditory2visualdata[,c(1, 6502:7001)]
trial_15 = auditory2visualdata[,c(1, 7002:7501)]
trial_16 = auditory2visualdata[,c(1, 7502:8001)]
trial_17 = auditory2visualdata[,c(1, 8002:8501)]
trial_18 = auditory2visualdata[,c(1, 8502:9001)]
trial_19 = auditory2visualdata[,c(1, 9002:9501)]
trial_20 = auditory2visualdata[,c(1, 9502:10001)]
trial_21 = auditory2visualdata[,c(1, 10002:10501)]
trial_22 = auditory2visualdata[,c(1, 10502:11001)]
trial_23 = auditory2visualdata[,c(1, 11002:11501)]
trial_24 = auditory2visualdata[,c(1, 11502:12001)]
trial_25 = auditory2visualdata[,c(1, 12002:12501)]
trial_26 = auditory2visualdata[,c(1, 12502:13001)]
trial_27 = auditory2visualdata[,c(1, 13002:13501)]
trial_28 = auditory2visualdata[,c(1, 13502:14001)]
trial_29 = auditory2visualdata[,c(1, 14002:14501)]
trial_30 = auditory2visualdata[,c(1, 14502:15001)]
trial_31 = auditory2visualdata[,c(1, 15002:15501)]
trial_32 = auditory2visualdata[,c(1, 15502:16001)]
trial_33 = auditory2visualdata[,c(1, 16002:16501)]
trial_34 = auditory2visualdata[,c(1, 16502:17001)]
trial_35 = auditory2visualdata[,c(1, 17002:17501)]
trial_36 = auditory2visualdata[,c(1, 17502:18001)]

auditory2visualdata <- rbind(trial_1,trial_2,trial_3,trial_4,trial_5,trial_6,trial_7,trial_8,trial_9,trial_10,
trial_11,trial_12,trial_13,trial_14,trial_15,trial_16,trial_17,trial_18,trial_19,trial_20,trial_21,
trial_22,trial_23,trial_24,trial_25,trial_26,trial_27,trial_28,trial_29,trial_30,trial_31,trial_32,
trial_33,trial_34,trial_35,trial_36)

# Create trial names
auditory2visualdata$trial = rep(1:36, each=2832)
count(auditory2visualdata$trial)
str(auditory2visualdata)




# Now put it in long format where microvolts is a variable/identifier, and shove them together...

visual2visual_melted <- melt(visual2visualdata, id.vars=c('trial','electrode'))
visual2visual_melted <- visual2visual_melted %>%  mutate(condition = "visual2visual")
names(visual2visual_melted) <- c('trial','electrode', 'time', 'microvolts', 'condition')
head(visual2visual_melted)

haptic2visual_melted <- melt(haptic2visualdata, id.vars=c('trial','electrode'))
haptic2visual_melted <- haptic2visual_melted %>%  mutate(condition = "haptic2visual")
names(haptic2visual_melted) <- c('trial','electrode', 'time', 'microvolts', 'condition')
head(haptic2visual_melted)

auditory2visual_melted <- melt(auditory2visualdata, id.vars=c('trial','electrode'))
auditory2visual_melted <- auditory2visual_melted %>%  mutate(condition = "auditory2visual")
names(auditory2visual_melted) <- c('trial','electrode', 'time', 'microvolts', 'condition')
head(auditory2visual_melted)




# This works fine and creates two really long melted dataframes with data for 
# each electrode and condition. BUT!!! no participant data yet. Couldn't just 
# put in names of files because the switch to long format doesn't like that

# First, create a list where the participant name occurs according 
# to the number of electrodes in that participant's file
# For  Cond 1
participants <- sub("\\).*", "", sub(".*\\(", "", visual2visualfiles))
head(participants)
fulllist1 <- mapply(function(x,y) rep(x, y), participants, 59)
fulllist1 <- unlist(fulllist1)     # remove all factors, print long list
fulllist1 <- rep(fulllist1, 500)   # correct for individual time point

# For Cond 2
participants <- sub("\\).*", "", sub(".*\\(", "", haptic2visualfiles))
head(participants)
fulllist2 <- mapply(function(x,y) rep(x, y), participants, 59)
fulllist2 <- unlist(fulllist2)     
fulllist2 <- rep(fulllist2, 500)   

# For Cond 3
participants <- sub("\\).*", "", sub(".*\\(", "", auditory2visualfiles))
head(participants)
fulllist3 <- mapply(function(x,y) rep(x, y), participants, 59)
fulllist3 <- unlist(fulllist3)     
fulllist3 <- rep(fulllist3, 500)

# Second, create a column in the melted dataframes saying which participant 
# is at each data point
visual2visual_melted <- visual2visual_melted %>%  mutate(participant = fulllist1)
haptic2visual_melted <- haptic2visual_melted %>%  mutate(participant = fulllist2)
auditory2visual_melted <- auditory2visual_melted %>%  mutate(participant = fulllist3)

head(visual2visual_melted)
tail(visual2visual_melted)
head(haptic2visual_melted)
tail(haptic2visual_melted)
head(auditory2visual_melted)
tail(auditory2visual_melted)
str(visual2visual_melted$participant)
str(haptic2visual_melted$participant)
str(auditory2visual_melted$participant)


# By now, participants' name is their file. so rename

visual2visual_melted$participant[visual2visual_melted$participant == "1_TotalMatch_baselinecorr_artif.txt"] <- "1"
visual2visual_melted$participant[visual2visual_melted$participant == "2_TotalMatch_baselinecorr_artif.txt"] <- "2"
visual2visual_melted$participant[visual2visual_melted$participant == "3_TotalMatch_baselinecorr_artif.txt"] <- "3"
visual2visual_melted$participant[visual2visual_melted$participant == "4_TotalMatch_baselinecorr_artif.txt"] <- "4"
visual2visual_melted$participant[visual2visual_melted$participant == "5_TotalMatch_baselinecorr_artif.txt"] <- "5"
visual2visual_melted$participant[visual2visual_melted$participant == "6_TotalMatch_baselinecorr_artif.txt"] <- "6"
visual2visual_melted$participant[visual2visual_melted$participant == "7_TotalMatch_baselinecorr_artif.txt"] <- "7"
visual2visual_melted$participant[visual2visual_melted$participant == "8_TotalMatch_baselinecorr_artif.txt"] <- "8"
visual2visual_melted$participant[visual2visual_melted$participant == "9_TotalMatch_baselinecorr_artif.txt"] <- "9"
visual2visual_melted$participant[visual2visual_melted$participant == "10_TotalMatch_baselinecorr_artif.txt"] <- "10"
visual2visual_melted$participant[visual2visual_melted$participant == "11_TotalMatch_baselinecorr_artif.txt"] <- "11"
visual2visual_melted$participant[visual2visual_melted$participant == "12_TotalMatch_baselinecorr_artif.txt"] <- "12"
visual2visual_melted$participant[visual2visual_melted$participant == "13_TotalMatch_baselinecorr_artif.txt"] <- "13"
visual2visual_melted$participant[visual2visual_melted$participant == "14_TotalMatch_baselinecorr_artif.txt"] <- "14"
visual2visual_melted$participant[visual2visual_melted$participant == "15_TotalMatch_baselinecorr_artif.txt"] <- "15"
visual2visual_melted$participant[visual2visual_melted$participant == "16_TotalMatch_baselinecorr_artif.txt"] <- "16"
visual2visual_melted$participant[visual2visual_melted$participant == "17_TotalMatch_baselinecorr_artif.txt"] <- "17"
visual2visual_melted$participant[visual2visual_melted$participant == "18_TotalMatch_baselinecorr_artif.txt"] <- "18"
visual2visual_melted$participant[visual2visual_melted$participant == "19_TotalMatch_baselinecorr_artif.txt"] <- "19"
visual2visual_melted$participant[visual2visual_melted$participant == "20_TotalMatch_baselinecorr_artif.txt"] <- "20"
visual2visual_melted$participant[visual2visual_melted$participant == "21_TotalMatch_baselinecorr_artif.txt"] <- "21"
visual2visual_melted$participant[visual2visual_melted$participant == "22_TotalMatch_baselinecorr_artif.txt"] <- "22"
visual2visual_melted$participant[visual2visual_melted$participant == "23_TotalMatch_baselinecorr_artif.txt"] <- "23"
visual2visual_melted$participant[visual2visual_melted$participant == "24_TotalMatch_baselinecorr_artif.txt"] <- "24"
visual2visual_melted$participant[visual2visual_melted$participant == "26_TotalMatch_baselinecorr_artif.txt"] <- "26"
visual2visual_melted$participant[visual2visual_melted$participant == "27_TotalMatch_baselinecorr_artif.txt"] <- "27"
visual2visual_melted$participant[visual2visual_melted$participant == "28_TotalMatch_baselinecorr_artif.txt"] <- "28"
visual2visual_melted$participant[visual2visual_melted$participant == "29_TotalMatch_baselinecorr_artif.txt"] <- "29"
visual2visual_melted$participant[visual2visual_melted$participant == "30_TotalMatch_baselinecorr_artif.txt"] <- "30"
visual2visual_melted$participant[visual2visual_melted$participant == "31_TotalMatch_baselinecorr_artif.txt"] <- "31"
visual2visual_melted$participant[visual2visual_melted$participant == "32_TotalMatch_baselinecorr_artif.txt"] <- "32"
visual2visual_melted$participant[visual2visual_melted$participant == "33_TotalMatch_baselinecorr_artif.txt"] <- "33"
visual2visual_melted$participant[visual2visual_melted$participant == "34_TotalMatch_baselinecorr_artif.txt"] <- "34"
visual2visual_melted$participant[visual2visual_melted$participant == "35_TotalMatch_baselinecorr_artif.txt"] <- "35"
visual2visual_melted$participant[visual2visual_melted$participant == "36_TotalMatch_baselinecorr_artif.txt"] <- "36"
visual2visual_melted$participant[visual2visual_melted$participant == "37_TotalMatch_baselinecorr_artif.txt"] <- "37"
visual2visual_melted$participant[visual2visual_melted$participant == "38_TotalMatch_baselinecorr_artif.txt"] <- "38"
visual2visual_melted$participant[visual2visual_melted$participant == "39_TotalMatch_baselinecorr_artif.txt"] <- "39"
visual2visual_melted$participant[visual2visual_melted$participant == "40_TotalMatch_baselinecorr_artif.txt"] <- "40"
visual2visual_melted$participant[visual2visual_melted$participant == "41_TotalMatch_baselinecorr_artif.txt"] <- "41"
visual2visual_melted$participant[visual2visual_melted$participant == "42_TotalMatch_baselinecorr_artif.txt"] <- "42"
visual2visual_melted$participant[visual2visual_melted$participant == "44_TotalMatch_baselinecorr_artif.txt"] <- "44"
visual2visual_melted$participant[visual2visual_melted$participant == "45_TotalMatch_baselinecorr_artif.txt"] <- "45"
visual2visual_melted$participant[visual2visual_melted$participant == "46_TotalMatch_baselinecorr_artif.txt"] <- "46"
visual2visual_melted$participant[visual2visual_melted$participant == "47_TotalMatch_baselinecorr_artif.txt"] <- "47"
visual2visual_melted$participant[visual2visual_melted$participant == "48_TotalMatch_baselinecorr_artif.txt"] <- "48"
visual2visual_melted$participant[visual2visual_melted$participant == "49_TotalMatch_baselinecorr_artif.txt"] <- "49"
visual2visual_melted$participant[visual2visual_melted$participant == "50_TotalMatch_baselinecorr_artif.txt"] <- "50"


haptic2visual_melted$participant[haptic2visual_melted$participant == "1_EmbodiedMismatch_baselinecorr_artif.txt"] <- "1"
haptic2visual_melted$participant[haptic2visual_melted$participant == "2_EmbodiedMismatch_baselinecorr_artif.txt"] <- "2"
haptic2visual_melted$participant[haptic2visual_melted$participant == "3_EmbodiedMismatch_baselinecorr_artif.txt"] <- "3"
haptic2visual_melted$participant[haptic2visual_melted$participant == "4_EmbodiedMismatch_baselinecorr_artif.txt"] <- "4"
haptic2visual_melted$participant[haptic2visual_melted$participant == "5_EmbodiedMismatch_baselinecorr_artif.txt"] <- "5"
haptic2visual_melted$participant[haptic2visual_melted$participant == "6_EmbodiedMismatch_baselinecorr_artif.txt"] <- "6"
haptic2visual_melted$participant[haptic2visual_melted$participant == "7_EmbodiedMismatch_baselinecorr_artif.txt"] <- "7"
haptic2visual_melted$participant[haptic2visual_melted$participant == "8_EmbodiedMismatch_baselinecorr_artif.txt"] <- "8"
haptic2visual_melted$participant[haptic2visual_melted$participant == "9_EmbodiedMismatch_baselinecorr_artif.txt"] <- "9"
haptic2visual_melted$participant[haptic2visual_melted$participant == "10_EmbodiedMismatch_baselinecorr_artif.txt"] <- "10"
haptic2visual_melted$participant[haptic2visual_melted$participant == "11_EmbodiedMismatch_baselinecorr_artif.txt"] <- "11"
haptic2visual_melted$participant[haptic2visual_melted$participant == "12_EmbodiedMismatch_baselinecorr_artif.txt"] <- "12"
haptic2visual_melted$participant[haptic2visual_melted$participant == "13_EmbodiedMismatch_baselinecorr_artif.txt"] <- "13"
haptic2visual_melted$participant[haptic2visual_melted$participant == "14_EmbodiedMismatch_baselinecorr_artif.txt"] <- "14"
haptic2visual_melted$participant[haptic2visual_melted$participant == "15_EmbodiedMismatch_baselinecorr_artif.txt"] <- "15"
haptic2visual_melted$participant[haptic2visual_melted$participant == "16_EmbodiedMismatch_baselinecorr_artif.txt"] <- "16"
haptic2visual_melted$participant[haptic2visual_melted$participant == "17_EmbodiedMismatch_baselinecorr_artif.txt"] <- "17"
haptic2visual_melted$participant[haptic2visual_melted$participant == "18_EmbodiedMismatch_baselinecorr_artif.txt"] <- "18"
haptic2visual_melted$participant[haptic2visual_melted$participant == "19_EmbodiedMismatch_baselinecorr_artif.txt"] <- "19"
haptic2visual_melted$participant[haptic2visual_melted$participant == "20_EmbodiedMismatch_baselinecorr_artif.txt"] <- "20"
haptic2visual_melted$participant[haptic2visual_melted$participant == "21_EmbodiedMismatch_baselinecorr_artif.txt"] <- "21"
haptic2visual_melted$participant[haptic2visual_melted$participant == "22_EmbodiedMismatch_baselinecorr_artif.txt"] <- "22"
haptic2visual_melted$participant[haptic2visual_melted$participant == "23_EmbodiedMismatch_baselinecorr_artif.txt"] <- "23"
haptic2visual_melted$participant[haptic2visual_melted$participant == "24_EmbodiedMismatch_baselinecorr_artif.txt"] <- "24"
haptic2visual_melted$participant[haptic2visual_melted$participant == "26_EmbodiedMismatch_baselinecorr_artif.txt"] <- "26"
haptic2visual_melted$participant[haptic2visual_melted$participant == "27_EmbodiedMismatch_baselinecorr_artif.txt"] <- "27"
haptic2visual_melted$participant[haptic2visual_melted$participant == "28_EmbodiedMismatch_baselinecorr_artif.txt"] <- "28"
haptic2visual_melted$participant[haptic2visual_melted$participant == "29_EmbodiedMismatch_baselinecorr_artif.txt"] <- "29"
haptic2visual_melted$participant[haptic2visual_melted$participant == "30_EmbodiedMismatch_baselinecorr_artif.txt"] <- "30"
haptic2visual_melted$participant[haptic2visual_melted$participant == "31_EmbodiedMismatch_baselinecorr_artif.txt"] <- "31"
haptic2visual_melted$participant[haptic2visual_melted$participant == "32_EmbodiedMismatch_baselinecorr_artif.txt"] <- "32"
haptic2visual_melted$participant[haptic2visual_melted$participant == "33_EmbodiedMismatch_baselinecorr_artif.txt"] <- "33"
haptic2visual_melted$participant[haptic2visual_melted$participant == "34_EmbodiedMismatch_baselinecorr_artif.txt"] <- "34"
haptic2visual_melted$participant[haptic2visual_melted$participant == "35_EmbodiedMismatch_baselinecorr_artif.txt"] <- "35"
haptic2visual_melted$participant[haptic2visual_melted$participant == "36_EmbodiedMismatch_baselinecorr_artif.txt"] <- "36"
haptic2visual_melted$participant[haptic2visual_melted$participant == "37_EmbodiedMismatch_baselinecorr_artif.txt"] <- "37"
haptic2visual_melted$participant[haptic2visual_melted$participant == "38_EmbodiedMismatch_baselinecorr_artif.txt"] <- "38"
haptic2visual_melted$participant[haptic2visual_melted$participant == "39_EmbodiedMismatch_baselinecorr_artif.txt"] <- "39"
haptic2visual_melted$participant[haptic2visual_melted$participant == "40_EmbodiedMismatch_baselinecorr_artif.txt"] <- "40"
haptic2visual_melted$participant[haptic2visual_melted$participant == "41_EmbodiedMismatch_baselinecorr_artif.txt"] <- "41"
haptic2visual_melted$participant[haptic2visual_melted$participant == "42_EmbodiedMismatch_baselinecorr_artif.txt"] <- "42"
haptic2visual_melted$participant[haptic2visual_melted$participant == "44_EmbodiedMismatch_baselinecorr_artif.txt"] <- "44"
haptic2visual_melted$participant[haptic2visual_melted$participant == "45_EmbodiedMismatch_baselinecorr_artif.txt"] <- "45"
haptic2visual_melted$participant[haptic2visual_melted$participant == "46_EmbodiedMismatch_baselinecorr_artif.txt"] <- "46"
haptic2visual_melted$participant[haptic2visual_melted$participant == "47_EmbodiedMismatch_baselinecorr_artif.txt"] <- "47"
haptic2visual_melted$participant[haptic2visual_melted$participant == "48_EmbodiedMismatch_baselinecorr_artif.txt"] <- "48"
haptic2visual_melted$participant[haptic2visual_melted$participant == "49_EmbodiedMismatch_baselinecorr_artif.txt"] <- "49"
haptic2visual_melted$participant[haptic2visual_melted$participant == "50_EmbodiedMismatch_baselinecorr_artif.txt"] <- "50"


auditory2visual_melted$participant[auditory2visual_melted$participant == "1_TotalMismatch_baselinecorr_artif.txt"] <- "1"
auditory2visual_melted$participant[auditory2visual_melted$participant == "2_TotalMismatch_baselinecorr_artif.txt"] <- "2"
auditory2visual_melted$participant[auditory2visual_melted$participant == "3_TotalMismatch_baselinecorr_artif.txt"] <- "3"
auditory2visual_melted$participant[auditory2visual_melted$participant == "4_TotalMismatch_baselinecorr_artif.txt"] <- "4"
auditory2visual_melted$participant[auditory2visual_melted$participant == "5_TotalMismatch_baselinecorr_artif.txt"] <- "5"
auditory2visual_melted$participant[auditory2visual_melted$participant == "6_TotalMismatch_baselinecorr_artif.txt"] <- "6"
auditory2visual_melted$participant[auditory2visual_melted$participant == "7_TotalMismatch_baselinecorr_artif.txt"] <- "7"
auditory2visual_melted$participant[auditory2visual_melted$participant == "8_TotalMismatch_baselinecorr_artif.txt"] <- "8"
auditory2visual_melted$participant[auditory2visual_melted$participant == "9_TotalMismatch_baselinecorr_artif.txt"] <- "9"
auditory2visual_melted$participant[auditory2visual_melted$participant == "10_TotalMismatch_baselinecorr_artif.txt"] <- "10"
auditory2visual_melted$participant[auditory2visual_melted$participant == "11_TotalMismatch_baselinecorr_artif.txt"] <- "11"
auditory2visual_melted$participant[auditory2visual_melted$participant == "12_TotalMismatch_baselinecorr_artif.txt"] <- "12"
auditory2visual_melted$participant[auditory2visual_melted$participant == "13_TotalMismatch_baselinecorr_artif.txt"] <- "13"
auditory2visual_melted$participant[auditory2visual_melted$participant == "14_TotalMismatch_baselinecorr_artif.txt"] <- "14"
auditory2visual_melted$participant[auditory2visual_melted$participant == "15_TotalMismatch_baselinecorr_artif.txt"] <- "15"
auditory2visual_melted$participant[auditory2visual_melted$participant == "16_TotalMismatch_baselinecorr_artif.txt"] <- "16"
auditory2visual_melted$participant[auditory2visual_melted$participant == "17_TotalMismatch_baselinecorr_artif.txt"] <- "17"
auditory2visual_melted$participant[auditory2visual_melted$participant == "18_TotalMismatch_baselinecorr_artif.txt"] <- "18"
auditory2visual_melted$participant[auditory2visual_melted$participant == "19_TotalMismatch_baselinecorr_artif.txt"] <- "19"
auditory2visual_melted$participant[auditory2visual_melted$participant == "20_TotalMismatch_baselinecorr_artif.txt"] <- "20"
auditory2visual_melted$participant[auditory2visual_melted$participant == "21_TotalMismatch_baselinecorr_artif.txt"] <- "21"
auditory2visual_melted$participant[auditory2visual_melted$participant == "22_TotalMismatch_baselinecorr_artif.txt"] <- "22"
auditory2visual_melted$participant[auditory2visual_melted$participant == "23_TotalMismatch_baselinecorr_artif.txt"] <- "23"
auditory2visual_melted$participant[auditory2visual_melted$participant == "24_TotalMismatch_baselinecorr_artif.txt"] <- "24"
auditory2visual_melted$participant[auditory2visual_melted$participant == "26_TotalMismatch_baselinecorr_artif.txt"] <- "26"
auditory2visual_melted$participant[auditory2visual_melted$participant == "27_TotalMismatch_baselinecorr_artif.txt"] <- "27"
auditory2visual_melted$participant[auditory2visual_melted$participant == "28_TotalMismatch_baselinecorr_artif.txt"] <- "28"
auditory2visual_melted$participant[auditory2visual_melted$participant == "29_TotalMismatch_baselinecorr_artif.txt"] <- "29"
auditory2visual_melted$participant[auditory2visual_melted$participant == "30_TotalMismatch_baselinecorr_artif.txt"] <- "30"
auditory2visual_melted$participant[auditory2visual_melted$participant == "31_TotalMismatch_baselinecorr_artif.txt"] <- "31"
auditory2visual_melted$participant[auditory2visual_melted$participant == "32_TotalMismatch_baselinecorr_artif.txt"] <- "32"
auditory2visual_melted$participant[auditory2visual_melted$participant == "33_TotalMismatch_baselinecorr_artif.txt"] <- "33"
auditory2visual_melted$participant[auditory2visual_melted$participant == "34_TotalMismatch_baselinecorr_artif.txt"] <- "34"
auditory2visual_melted$participant[auditory2visual_melted$participant == "35_TotalMismatch_baselinecorr_artif.txt"] <- "35"
auditory2visual_melted$participant[auditory2visual_melted$participant == "36_TotalMismatch_baselinecorr_artif.txt"] <- "36"
auditory2visual_melted$participant[auditory2visual_melted$participant == "37_TotalMismatch_baselinecorr_artif.txt"] <- "37"
auditory2visual_melted$participant[auditory2visual_melted$participant == "38_TotalMismatch_baselinecorr_artif.txt"] <- "38"
auditory2visual_melted$participant[auditory2visual_melted$participant == "39_TotalMismatch_baselinecorr_artif.txt"] <- "39"
auditory2visual_melted$participant[auditory2visual_melted$participant == "40_TotalMismatch_baselinecorr_artif.txt"] <- "40"
auditory2visual_melted$participant[auditory2visual_melted$participant == "41_TotalMismatch_baselinecorr_artif.txt"] <- "41"
auditory2visual_melted$participant[auditory2visual_melted$participant == "42_TotalMismatch_baselinecorr_artif.txt"] <- "42"
auditory2visual_melted$participant[auditory2visual_melted$participant == "44_TotalMismatch_baselinecorr_artif.txt"] <- "44"
auditory2visual_melted$participant[auditory2visual_melted$participant == "45_TotalMismatch_baselinecorr_artif.txt"] <- "45"
auditory2visual_melted$participant[auditory2visual_melted$participant == "46_TotalMismatch_baselinecorr_artif.txt"] <- "46"
auditory2visual_melted$participant[auditory2visual_melted$participant == "47_TotalMismatch_baselinecorr_artif.txt"] <- "47"
auditory2visual_melted$participant[auditory2visual_melted$participant == "48_TotalMismatch_baselinecorr_artif.txt"] <- "48"
auditory2visual_melted$participant[auditory2visual_melted$participant == "49_TotalMismatch_baselinecorr_artif.txt"] <- "49"
auditory2visual_melted$participant[auditory2visual_melted$participant == "50_TotalMismatch_baselinecorr_artif.txt"] <- "50"
head(haptic2visual_melted)
unique(visual2visual_melted$participant)
unique(haptic2visual_melted$participant)
unique(auditory2visual_melted$participant)



# save
saveRDS(visual2visual_melted, 'visual2visual_melted transitory file.rds')
saveRDS(haptic2visual_melted, 'haptic2visual_melted transitory file.rds')
saveRDS(auditory2visual_melted, 'auditory2visual_melted transitory file.rds')

# Freeing up workspace
rm(list=setdiff(ls(), "visual2visual_melted"))


# There's way too much data. Downsample to 125 Hz by averaging across every four measurements. Conveniently, 
# this builds on the full original 500 Hz data while reducing computation. First, rename every three time 
# points as previous one:

# First for Visual-to-Visual condition

visual2visual_melted$time2 <- visual2visual_melted$time
visual2visual_melted$time2 <- as.integer(as.character(visual2visual_melted$time2))
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-198', '-200', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-196', '-200', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-194', '-200', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-190', '-192', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-188', '-192', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-186', '-192', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-182', '-184', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-180', '-184', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-178', '-184', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-174', '-176', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-172', '-176', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-170', '-176', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-166', '-168', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-164', '-168', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-162', '-168', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-158', '-160', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-156', '-160', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-154', '-160', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-150', '-152', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-148', '-152', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-146', '-152', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-142', '-144', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-140', '-144', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-138', '-144', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-134', '-136', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-132', '-136', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-130', '-136', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-126', '-128', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-124', '-128', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-122', '-128', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-118', '-120', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-116', '-120', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-114', '-120', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-110', '-112', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-108', '-112', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-106', '-112', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-102', '-104', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-100', '-104', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-98', '-104', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-94', '-96', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-92', '-96', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-90', '-96', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-86', '-88', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-84', '-88', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-82', '-88', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-78', '-80', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-76', '-80', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-74', '-80', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-70', '-72', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-68', '-72', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-66', '-72', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-62', '-64', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-60', '-64', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-58', '-64', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-54', '-56', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-52', '-56', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-50', '-56', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-46', '-48', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-44', '-48', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-42', '-48', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-38', '-40', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-36', '-40', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-34', '-40', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-30', '-32', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-28', '-32', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-26', '-32', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-22', '-24', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-20', '-24', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-18', '-24', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-14', '-16', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-12', '-16', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-10', '-16', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-6', '-8', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-4', '-8', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='-2', '-8', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='2', '0', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='4', '0', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='6', '0', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='10', '8', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='12', '8', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='14', '8', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='18', '16', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='20', '16', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='22', '16', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='26', '24', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='28', '24', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='30', '24', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='34', '32', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='36', '32', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='38', '32', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='42', '40', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='44', '40', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='46', '40', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='50', '48', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='52', '48', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='54', '48', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='58', '56', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='60', '56', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='62', '56', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='66', '64', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='68', '64', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='70', '64', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='74', '72', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='76', '72', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='78', '72', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='82', '80', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='84', '80', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='86', '80', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='90', '88', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='92', '88', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='94', '88', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='98', '96', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='100', '96', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='102', '96', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='106', '104', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='108', '104', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='110', '104', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='114', '112', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='116', '112', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='118', '112', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='122', '120', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='124', '120', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='126', '120', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='130', '128', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='132', '128', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='134', '128', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='138', '136', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='140', '136', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='142', '136', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='146', '144', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='148', '144', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='150', '144', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='154', '152', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='156', '152', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='158', '152', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='162', '160', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='164', '160', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='166', '160', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='170', '168', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='172', '168', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='174', '168', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='178', '176', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='180', '176', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='182', '176', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='186', '184', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='188', '184', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='190', '184', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='194', '192', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='196', '192', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='198', '192', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='202', '200', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='204', '200', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='206', '200', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='210', '208', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='212', '208', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='214', '208', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='218', '216', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='220', '216', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='222', '216', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='226', '224', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='228', '224', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='230', '224', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='234', '232', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='236', '232', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='238', '232', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='242', '240', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='244', '240', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='246', '240', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='250', '248', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='252', '248', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='254', '248', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='258', '256', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='260', '256', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='262', '256', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='266', '264', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='268', '264', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='270', '264', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='274', '272', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='276', '272', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='278', '272', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='282', '280', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='284', '280', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='286', '280', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='290', '288', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='292', '288', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='294', '288', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='298', '296', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='300', '296', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='302', '296', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='306', '304', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='308', '304', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='310', '304', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='314', '312', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='316', '312', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='318', '312', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='322', '320', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='324', '320', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='326', '320', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='330', '328', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='332', '328', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='334', '328', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='338', '336', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='340', '336', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='342', '336', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='346', '344', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='348', '344', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='350', '344', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='354', '352', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='356', '352', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='358', '352', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='362', '360', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='364', '360', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='366', '360', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='370', '368', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='372', '368', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='374', '368', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='378', '376', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='380', '376', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='382', '376', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='386', '384', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='388', '384', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='390', '384', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='394', '392', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='396', '392', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='398', '392', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='402', '400', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='404', '400', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='406', '400', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='410', '408', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='412', '408', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='414', '408', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='418', '416', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='420', '416', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='422', '416', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='426', '424', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='428', '424', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='430', '424', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='434', '432', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='436', '432', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='438', '432', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='442', '440', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='444', '440', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='446', '440', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='450', '448', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='452', '448', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='454', '448', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='458', '456', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='460', '456', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='462', '456', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='466', '464', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='468', '464', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='470', '464', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='474', '472', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='476', '472', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='478', '472', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='482', '480', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='484', '480', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='486', '480', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='490', '488', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='492', '488', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='494', '488', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='498', '496', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='500', '496', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='502', '496', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='506', '504', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='508', '504', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='510', '504', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='514', '512', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='516', '512', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='518', '512', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='522', '520', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='524', '520', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='526', '520', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='530', '528', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='532', '528', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='534', '528', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='538', '536', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='540', '536', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='542', '536', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='546', '544', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='548', '544', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='550', '544', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='554', '552', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='556', '552', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='558', '552', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='562', '560', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='564', '560', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='566', '560', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='570', '568', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='572', '568', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='574', '568', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='578', '576', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='580', '576', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='582', '576', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='586', '584', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='588', '584', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='590', '584', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='594', '592', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='596', '592', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='598', '592', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='602', '600', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='604', '600', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='606', '600', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='610', '608', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='612', '608', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='614', '608', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='618', '616', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='620', '616', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='622', '616', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='626', '624', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='628', '624', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='630', '624', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='634', '632', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='636', '632', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='638', '632', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='642', '640', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='644', '640', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='646', '640', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='650', '648', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='652', '648', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='654', '648', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='658', '656', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='660', '656', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='662', '656', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='666', '664', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='668', '664', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='670', '664', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='674', '672', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='676', '672', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='678', '672', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='682', '680', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='684', '680', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='686', '680', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='690', '688', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='692', '688', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='694', '688', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='698', '696', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='700', '696', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='702', '696', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='706', '704', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='708', '704', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='710', '704', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='714', '712', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='716', '712', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='718', '712', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='722', '720', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='724', '720', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='726', '720', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='730', '728', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='732', '728', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='734', '728', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='738', '736', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='740', '736', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='742', '736', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='746', '744', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='748', '744', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='750', '744', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='754', '752', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='756', '752', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='758', '752', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='762', '760', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='764', '760', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='766', '760', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='770', '768', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='772', '768', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='774', '768', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='778', '776', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='780', '776', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='782', '776', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='786', '784', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='788', '784', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='790', '784', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='794', '792', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='796', '792', visual2visual_melted$time2)
visual2visual_melted$time2 = ifelse(visual2visual_melted$time2=='798', '792', visual2visual_melted$time2)

# Last, aggregate by mean. Split trials in three coz this is too heavy
visual2visual_melted.subset1 = visual2visual_melted[visual2visual_melted$trial<=12,]
visual2visual_melted.subset2 = visual2visual_melted[visual2visual_melted$trial>12 & visual2visual_melted$trial<25,]
visual2visual_melted.subset3 = visual2visual_melted[visual2visual_melted$trial>=25,]

# Freeing up workspace
rm(list=setdiff(ls(), c("visual2visual_melted.subset1", "visual2visual_melted.subset2", 
"visual2visual_melted.subset3")))

# rename 'time2' variable to just 'time'
visual2visual_melted.subset1$time = visual2visual_melted.subset1$time2
visual2visual_melted.subset2$time = visual2visual_melted.subset2$time2
visual2visual_melted.subset3$time = visual2visual_melted.subset3$time2

visual2visual_melted.subset1.downsampled = aggregate(microvolts ~ time+participant+condition+electrode+trial, 
visual2visual_melted.subset1, mean)
visual2visual_melted.subset2.downsampled = aggregate(microvolts ~ time+participant+condition+electrode+trial, 
visual2visual_melted.subset2, mean)
visual2visual_melted.subset3.downsampled = aggregate(microvolts ~ time+participant+condition+electrode+trial, 
visual2visual_melted.subset3, mean)
unique(visual2visual_melted.subset1.downsampled$time)
# Successfully downsampled, with one averaged measurement every 8 ms.

visual2visual_melted.downsampled = rbind(visual2visual_melted.subset1.downsampled, 
visual2visual_melted.subset2.downsampled, visual2visual_melted.subset3.downsampled)
head(visual2visual_melted.downsampled)
str(visual2visual_melted.downsampled)

# Freeing up workspace
rm(list=setdiff(ls(), 'visual2visual_melted.downsampled'))



# Same for Haptic-to-Visual condition

haptic2visual_melted = readRDS('haptic2visual_melted transitory file.rds')

haptic2visual_melted$time2 <- haptic2visual_melted$time
haptic2visual_melted$time2 <- as.integer(as.character(haptic2visual_melted$time2))
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-198', '-200', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-196', '-200', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-194', '-200', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-190', '-192', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-188', '-192', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-186', '-192', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-182', '-184', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-180', '-184', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-178', '-184', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-174', '-176', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-172', '-176', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-170', '-176', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-166', '-168', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-164', '-168', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-162', '-168', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-158', '-160', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-156', '-160', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-154', '-160', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-150', '-152', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-148', '-152', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-146', '-152', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-142', '-144', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-140', '-144', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-138', '-144', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-134', '-136', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-132', '-136', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-130', '-136', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-126', '-128', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-124', '-128', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-122', '-128', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-118', '-120', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-116', '-120', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-114', '-120', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-110', '-112', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-108', '-112', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-106', '-112', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-102', '-104', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-100', '-104', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-98', '-104', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-94', '-96', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-92', '-96', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-90', '-96', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-86', '-88', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-84', '-88', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-82', '-88', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-78', '-80', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-76', '-80', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-74', '-80', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-70', '-72', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-68', '-72', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-66', '-72', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-62', '-64', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-60', '-64', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-58', '-64', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-54', '-56', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-52', '-56', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-50', '-56', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-46', '-48', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-44', '-48', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-42', '-48', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-38', '-40', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-36', '-40', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-34', '-40', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-30', '-32', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-28', '-32', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-26', '-32', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-22', '-24', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-20', '-24', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-18', '-24', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-14', '-16', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-12', '-16', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-10', '-16', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-6', '-8', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-4', '-8', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='-2', '-8', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='2', '0', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='4', '0', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='6', '0', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='10', '8', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='12', '8', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='14', '8', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='18', '16', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='20', '16', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='22', '16', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='26', '24', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='28', '24', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='30', '24', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='34', '32', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='36', '32', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='38', '32', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='42', '40', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='44', '40', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='46', '40', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='50', '48', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='52', '48', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='54', '48', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='58', '56', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='60', '56', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='62', '56', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='66', '64', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='68', '64', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='70', '64', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='74', '72', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='76', '72', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='78', '72', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='82', '80', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='84', '80', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='86', '80', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='90', '88', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='92', '88', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='94', '88', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='98', '96', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='100', '96', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='102', '96', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='106', '104', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='108', '104', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='110', '104', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='114', '112', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='116', '112', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='118', '112', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='122', '120', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='124', '120', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='126', '120', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='130', '128', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='132', '128', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='134', '128', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='138', '136', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='140', '136', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='142', '136', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='146', '144', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='148', '144', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='150', '144', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='154', '152', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='156', '152', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='158', '152', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='162', '160', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='164', '160', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='166', '160', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='170', '168', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='172', '168', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='174', '168', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='178', '176', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='180', '176', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='182', '176', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='186', '184', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='188', '184', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='190', '184', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='194', '192', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='196', '192', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='198', '192', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='202', '200', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='204', '200', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='206', '200', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='210', '208', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='212', '208', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='214', '208', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='218', '216', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='220', '216', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='222', '216', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='226', '224', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='228', '224', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='230', '224', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='234', '232', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='236', '232', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='238', '232', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='242', '240', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='244', '240', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='246', '240', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='250', '248', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='252', '248', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='254', '248', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='258', '256', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='260', '256', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='262', '256', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='266', '264', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='268', '264', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='270', '264', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='274', '272', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='276', '272', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='278', '272', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='282', '280', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='284', '280', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='286', '280', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='290', '288', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='292', '288', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='294', '288', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='298', '296', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='300', '296', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='302', '296', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='306', '304', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='308', '304', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='310', '304', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='314', '312', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='316', '312', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='318', '312', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='322', '320', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='324', '320', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='326', '320', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='330', '328', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='332', '328', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='334', '328', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='338', '336', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='340', '336', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='342', '336', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='346', '344', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='348', '344', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='350', '344', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='354', '352', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='356', '352', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='358', '352', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='362', '360', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='364', '360', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='366', '360', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='370', '368', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='372', '368', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='374', '368', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='378', '376', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='380', '376', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='382', '376', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='386', '384', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='388', '384', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='390', '384', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='394', '392', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='396', '392', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='398', '392', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='402', '400', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='404', '400', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='406', '400', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='410', '408', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='412', '408', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='414', '408', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='418', '416', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='420', '416', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='422', '416', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='426', '424', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='428', '424', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='430', '424', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='434', '432', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='436', '432', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='438', '432', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='442', '440', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='444', '440', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='446', '440', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='450', '448', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='452', '448', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='454', '448', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='458', '456', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='460', '456', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='462', '456', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='466', '464', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='468', '464', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='470', '464', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='474', '472', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='476', '472', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='478', '472', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='482', '480', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='484', '480', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='486', '480', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='490', '488', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='492', '488', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='494', '488', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='498', '496', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='500', '496', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='502', '496', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='506', '504', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='508', '504', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='510', '504', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='514', '512', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='516', '512', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='518', '512', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='522', '520', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='524', '520', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='526', '520', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='530', '528', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='532', '528', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='534', '528', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='538', '536', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='540', '536', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='542', '536', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='546', '544', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='548', '544', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='550', '544', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='554', '552', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='556', '552', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='558', '552', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='562', '560', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='564', '560', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='566', '560', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='570', '568', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='572', '568', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='574', '568', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='578', '576', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='580', '576', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='582', '576', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='586', '584', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='588', '584', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='590', '584', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='594', '592', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='596', '592', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='598', '592', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='602', '600', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='604', '600', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='606', '600', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='610', '608', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='612', '608', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='614', '608', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='618', '616', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='620', '616', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='622', '616', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='626', '624', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='628', '624', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='630', '624', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='634', '632', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='636', '632', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='638', '632', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='642', '640', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='644', '640', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='646', '640', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='650', '648', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='652', '648', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='654', '648', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='658', '656', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='660', '656', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='662', '656', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='666', '664', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='668', '664', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='670', '664', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='674', '672', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='676', '672', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='678', '672', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='682', '680', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='684', '680', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='686', '680', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='690', '688', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='692', '688', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='694', '688', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='698', '696', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='700', '696', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='702', '696', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='706', '704', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='708', '704', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='710', '704', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='714', '712', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='716', '712', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='718', '712', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='722', '720', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='724', '720', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='726', '720', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='730', '728', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='732', '728', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='734', '728', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='738', '736', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='740', '736', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='742', '736', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='746', '744', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='748', '744', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='750', '744', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='754', '752', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='756', '752', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='758', '752', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='762', '760', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='764', '760', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='766', '760', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='770', '768', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='772', '768', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='774', '768', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='778', '776', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='780', '776', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='782', '776', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='786', '784', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='788', '784', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='790', '784', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='794', '792', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='796', '792', haptic2visual_melted$time2)
haptic2visual_melted$time2 = ifelse(haptic2visual_melted$time2=='798', '792', haptic2visual_melted$time2)

# Last, aggregate by mean. Split trials in three coz this is too heavy
haptic2visual_melted.subset1 = haptic2visual_melted[haptic2visual_melted$trial<=12,]
haptic2visual_melted.subset2 = haptic2visual_melted[haptic2visual_melted$trial>12 & 
haptic2visual_melted$trial<25,]
haptic2visual_melted.subset3 = haptic2visual_melted[haptic2visual_melted$trial>=25,]

# Freeing up workspace
rm(list=setdiff(ls(), c("visual2visual_melted.downsampled", "haptic2visual_melted.subset1", 
"haptic2visual_melted.subset2", "haptic2visual_melted.subset3")))

# rename 'time2' variable to just 'time'
haptic2visual_melted.subset1$time = haptic2visual_melted.subset1$time2
haptic2visual_melted.subset2$time = haptic2visual_melted.subset2$time2
haptic2visual_melted.subset3$time = haptic2visual_melted.subset3$time2

haptic2visual_melted.subset1.downsampled = aggregate(microvolts ~ time+participant+condition+electrode+trial, 
haptic2visual_melted.subset1, mean)
haptic2visual_melted.subset2.downsampled = aggregate(microvolts ~ time+participant+condition+electrode+trial, 
haptic2visual_melted.subset2, mean)
haptic2visual_melted.subset3.downsampled = aggregate(microvolts ~ time+participant+condition+electrode+trial, 
haptic2visual_melted.subset3, mean)
unique(haptic2visual_melted.subset1.downsampled$time)
# Successfully downsampled, with one averaged measurement every 8 ms.

haptic2visual_melted.downsampled = rbind(haptic2visual_melted.subset1.downsampled, 
haptic2visual_melted.subset2.downsampled, haptic2visual_melted.subset3.downsampled)

# Freeing up workspace
ls()
rm(list=setdiff(ls(), c("visual2visual_melted.downsampled", "haptic2visual_melted.downsampled")))



# Same for Auditory-to-Visual condition

auditory2visual_melted = readRDS('auditory2visual_melted transitory file.rds')

auditory2visual_melted$time2 <- auditory2visual_melted$time
auditory2visual_melted$time2 <- as.integer(as.character(auditory2visual_melted$time2))
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-198', '-200', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-196', '-200', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-194', '-200', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-190', '-192', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-188', '-192', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-186', '-192', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-182', '-184', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-180', '-184', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-178', '-184', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-174', '-176', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-172', '-176', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-170', '-176', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-166', '-168', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-164', '-168', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-162', '-168', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-158', '-160', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-156', '-160', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-154', '-160', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-150', '-152', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-148', '-152', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-146', '-152', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-142', '-144', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-140', '-144', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-138', '-144', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-134', '-136', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-132', '-136', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-130', '-136', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-126', '-128', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-124', '-128', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-122', '-128', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-118', '-120', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-116', '-120', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-114', '-120', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-110', '-112', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-108', '-112', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-106', '-112', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-102', '-104', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-100', '-104', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-98', '-104', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-94', '-96', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-92', '-96', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-90', '-96', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-86', '-88', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-84', '-88', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-82', '-88', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-78', '-80', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-76', '-80', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-74', '-80', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-70', '-72', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-68', '-72', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-66', '-72', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-62', '-64', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-60', '-64', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-58', '-64', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-54', '-56', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-52', '-56', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-50', '-56', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-46', '-48', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-44', '-48', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-42', '-48', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-38', '-40', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-36', '-40', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-34', '-40', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-30', '-32', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-28', '-32', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-26', '-32', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-22', '-24', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-20', '-24', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-18', '-24', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-14', '-16', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-12', '-16', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-10', '-16', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-6', '-8', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-4', '-8', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='-2', '-8', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='2', '0', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='4', '0', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='6', '0', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='10', '8', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='12', '8', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='14', '8', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='18', '16', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='20', '16', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='22', '16', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='26', '24', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='28', '24', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='30', '24', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='34', '32', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='36', '32', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='38', '32', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='42', '40', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='44', '40', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='46', '40', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='50', '48', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='52', '48', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='54', '48', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='58', '56', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='60', '56', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='62', '56', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='66', '64', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='68', '64', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='70', '64', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='74', '72', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='76', '72', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='78', '72', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='82', '80', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='84', '80', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='86', '80', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='90', '88', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='92', '88', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='94', '88', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='98', '96', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='100', '96', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='102', '96', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='106', '104', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='108', '104', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='110', '104', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='114', '112', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='116', '112', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='118', '112', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='122', '120', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='124', '120', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='126', '120', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='130', '128', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='132', '128', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='134', '128', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='138', '136', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='140', '136', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='142', '136', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='146', '144', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='148', '144', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='150', '144', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='154', '152', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='156', '152', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='158', '152', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='162', '160', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='164', '160', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='166', '160', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='170', '168', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='172', '168', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='174', '168', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='178', '176', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='180', '176', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='182', '176', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='186', '184', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='188', '184', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='190', '184', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='194', '192', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='196', '192', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='198', '192', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='202', '200', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='204', '200', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='206', '200', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='210', '208', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='212', '208', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='214', '208', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='218', '216', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='220', '216', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='222', '216', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='226', '224', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='228', '224', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='230', '224', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='234', '232', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='236', '232', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='238', '232', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='242', '240', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='244', '240', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='246', '240', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='250', '248', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='252', '248', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='254', '248', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='258', '256', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='260', '256', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='262', '256', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='266', '264', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='268', '264', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='270', '264', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='274', '272', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='276', '272', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='278', '272', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='282', '280', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='284', '280', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='286', '280', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='290', '288', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='292', '288', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='294', '288', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='298', '296', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='300', '296', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='302', '296', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='306', '304', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='308', '304', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='310', '304', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='314', '312', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='316', '312', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='318', '312', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='322', '320', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='324', '320', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='326', '320', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='330', '328', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='332', '328', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='334', '328', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='338', '336', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='340', '336', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='342', '336', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='346', '344', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='348', '344', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='350', '344', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='354', '352', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='356', '352', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='358', '352', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='362', '360', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='364', '360', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='366', '360', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='370', '368', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='372', '368', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='374', '368', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='378', '376', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='380', '376', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='382', '376', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='386', '384', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='388', '384', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='390', '384', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='394', '392', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='396', '392', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='398', '392', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='402', '400', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='404', '400', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='406', '400', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='410', '408', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='412', '408', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='414', '408', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='418', '416', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='420', '416', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='422', '416', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='426', '424', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='428', '424', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='430', '424', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='434', '432', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='436', '432', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='438', '432', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='442', '440', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='444', '440', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='446', '440', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='450', '448', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='452', '448', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='454', '448', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='458', '456', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='460', '456', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='462', '456', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='466', '464', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='468', '464', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='470', '464', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='474', '472', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='476', '472', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='478', '472', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='482', '480', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='484', '480', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='486', '480', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='490', '488', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='492', '488', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='494', '488', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='498', '496', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='500', '496', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='502', '496', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='506', '504', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='508', '504', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='510', '504', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='514', '512', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='516', '512', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='518', '512', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='522', '520', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='524', '520', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='526', '520', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='530', '528', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='532', '528', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='534', '528', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='538', '536', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='540', '536', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='542', '536', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='546', '544', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='548', '544', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='550', '544', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='554', '552', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='556', '552', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='558', '552', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='562', '560', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='564', '560', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='566', '560', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='570', '568', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='572', '568', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='574', '568', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='578', '576', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='580', '576', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='582', '576', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='586', '584', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='588', '584', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='590', '584', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='594', '592', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='596', '592', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='598', '592', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='602', '600', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='604', '600', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='606', '600', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='610', '608', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='612', '608', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='614', '608', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='618', '616', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='620', '616', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='622', '616', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='626', '624', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='628', '624', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='630', '624', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='634', '632', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='636', '632', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='638', '632', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='642', '640', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='644', '640', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='646', '640', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='650', '648', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='652', '648', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='654', '648', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='658', '656', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='660', '656', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='662', '656', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='666', '664', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='668', '664', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='670', '664', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='674', '672', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='676', '672', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='678', '672', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='682', '680', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='684', '680', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='686', '680', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='690', '688', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='692', '688', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='694', '688', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='698', '696', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='700', '696', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='702', '696', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='706', '704', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='708', '704', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='710', '704', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='714', '712', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='716', '712', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='718', '712', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='722', '720', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='724', '720', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='726', '720', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='730', '728', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='732', '728', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='734', '728', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='738', '736', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='740', '736', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='742', '736', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='746', '744', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='748', '744', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='750', '744', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='754', '752', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='756', '752', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='758', '752', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='762', '760', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='764', '760', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='766', '760', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='770', '768', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='772', '768', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='774', '768', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='778', '776', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='780', '776', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='782', '776', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='786', '784', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='788', '784', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='790', '784', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='794', '792', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='796', '792', auditory2visual_melted$time2)
auditory2visual_melted$time2 = ifelse(auditory2visual_melted$time2=='798', '792', auditory2visual_melted$time2)

# Last, aggregate by mean. Split trials in three coz this is too heavy
auditory2visual_melted.subset1 = auditory2visual_melted[auditory2visual_melted$trial<=12,]
auditory2visual_melted.subset2 = auditory2visual_melted[auditory2visual_melted$trial>12 & 
auditory2visual_melted$trial<25,]
auditory2visual_melted.subset3 = auditory2visual_melted[auditory2visual_melted$trial>=25,]

# Freeing up workspace
rm(list=setdiff(ls(), c("visual2visual_melted.downsampled", "haptic2visual_melted.downsampled", 
  'auditory2visual_melted.subset1','auditory2visual_melted.subset2','auditory2visual_melted.subset3')))

# rename 'time2' variable to just 'time'
auditory2visual_melted.subset1$time = auditory2visual_melted.subset1$time2
auditory2visual_melted.subset2$time = auditory2visual_melted.subset2$time2
auditory2visual_melted.subset3$time = auditory2visual_melted.subset3$time2

auditory2visual_melted.subset1.downsampled = aggregate(microvolts ~ time+participant+condition+electrode+trial,
 auditory2visual_melted.subset1, mean)
auditory2visual_melted.subset2.downsampled = aggregate(microvolts ~ time+participant+condition+electrode+trial,
 auditory2visual_melted.subset2, mean)
auditory2visual_melted.subset3.downsampled = aggregate(microvolts ~ time+participant+condition+electrode+trial,
 auditory2visual_melted.subset3, mean)
unique(auditory2visual_melted.subset1.downsampled$time)
# Successfully downsampled, with one averaged measurement every 8 ms.

auditory2visual_melted.downsampled = rbind(auditory2visual_melted.subset1.downsampled, 
auditory2visual_melted.subset2.downsampled, auditory2visual_melted.subset3.downsampled)

rm(list=setdiff(ls(), c("visual2visual_melted.downsampled", "haptic2visual_melted.downsampled", 
'auditory2visual_melted.downsampled')))



# Take out participant 33--the one with accuracy below 50%--and participant 7, with too noisy EEG
visual2visual_melted.downsampled = visual2visual_melted.downsampled[!visual2visual_melted.downsampled$participant=='7'
 & !visual2visual_melted.downsampled$participant=='33',]
haptic2visual_melted.downsampled = haptic2visual_melted.downsampled[!haptic2visual_melted.downsampled$participant=='7'
 & !haptic2visual_melted.downsampled$participant=='33',]
auditory2visual_melted.downsampled = auditory2visual_melted.downsampled[!auditory2visual_melted.downsampled$participant=='7'
 & !auditory2visual_melted.downsampled$participant=='33',]
unique(visual2visual_melted.downsampled$participant)
unique(haptic2visual_melted.downsampled$participant)
unique(auditory2visual_melted.downsampled$participant)  # 46, good


# Now combine it all into one dataframe

EEG <- rbind(visual2visual_melted.downsampled, haptic2visual_melted.downsampled, auditory2visual_melted.downsampled)

str(EEG)  # 36,639,000 observations = 46 participants * 108 trials * 59 electrodes * 125 time points
head(EEG)
tail(EEG)

# Freeing up workspace
rm(list=setdiff(ls(), 'EEG'))

# Set Condition reference to visual2visual
EEG$condition = as.factor(EEG$condition)
EEG$condition <- relevel(EEG$condition, ref="visual2visual")


# Set 'time' variable in its right mode
EEG$time <- as.integer(as.character(EEG$time))
EEG$time = sort(EEG$time)
unique(EEG$time)


# Now include Group variable
EEG['group'] = NA
EEG$group = as.factor(EEG$group)
EEG$participant = as.factor(EEG$participant)
EEG$group = ifelse(EEG$participant == '1', 'Quick', NA)
EEG$group = ifelse(EEG$participant == '2', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '3', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '4', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '5', 'Quick', EEG$group)
EEG$group = ifelse(EEG$participant == '6', 'null', EEG$group)
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
summaryBy(group~participant, EEG, FUN=unique)


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
transitory <- EEG
transitory$RT.based_Groups <- as.numeric(cut2(transitory$MRT, g=2))
keeps <- c('participant','RT.based_Groups')
transitory = transitory[ , names(transitory) %in% keeps]
transitory = aggregate(RT.based_Groups ~ participant, transitory, mean, na.action = na.pass)

transitory  # These are the participants and the RT-based groups

# transfer

EEG['RT.based_Groups'] = NA
EEG$RT.based_Groups = as.factor(EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '1', '2', NA)
EEG$RT.based_Groups = ifelse(EEG$participant == '10', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '11', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '12', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '13', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '14', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '15', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '16', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '17', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '18', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '19', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '2', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '20', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '21', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '22', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '23', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '24', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '26', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '27', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '28', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '29', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '3', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '30', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '31', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '32', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '34', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '35', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '36', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '37', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '38', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '39', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '4', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '40', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '41', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '42', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '44', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '45', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '46', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '47', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '48', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '49', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '5', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '50', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '6', '2', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '8', '1', EEG$RT.based_Groups)
EEG$RT.based_Groups = ifelse(EEG$participant == '9', '2', EEG$RT.based_Groups)
summaryBy(RT.based_Groups~participant, EEG, FUN=unique)

# Mean RT per group
summaryBy(MRT ~ RT.based_Groups, data=EEG, FUN=c(mean, SD))

EEG$RT.based_Groups <- revalue(EEG$RT.based_Groups, c("1"="Quick", "2"="Slow"))

# double-check number of ptps in each group
str(unique(factor(EEG[EEG$RT.based_Groups == 'Quick', 'participant'])))  # 23
str(unique(factor(EEG[EEG$RT.based_Groups == 'Slow', 'participant'])))   # 23


# save & read in
#saveRDS(EEG, 'EEG.rds')
EEG = readRDS('EEG.rds')
str(EEG)


# Add participants' age
EEG['Age_months'] = NA
EEG$Age_months = as.numeric(EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '1', 274.5172, NA)
EEG$Age_months = ifelse(EEG$participant == '2', 221.2581, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '3', 250.6839, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '4', 226.3226, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '5', 290.6172, EEG$Age_months)
EEG$Age_months = ifelse(EEG$participant == '6', 242.8505, EEG$Age_months)
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

# Check gender and handedness per group. There are this 
# many observations per participant...
str(EEG[EEG$participant=='1',])
# and therefore:
table(EEG$RT.based_Groups, EEG$Gender)
table(EEG$RT.based_Groups, EEG$Lefthanded)


# Set up areas (this is based on the MPI Psycholinguistics's custom, equi-
# distant ActiCAP, but is quite analogous to famous 10-20 layout). See at:
# https://goo.gl/W0sBNr

unique(EEG$electrode)
midfront <- c('C60', 'C59', 'C58', 'C31')
midback <- c('C29', 'C28', 'C27', 'C26')
leftfront <- c('C53', 'C57', 'C52', 'C46', 'C45', 'C39', 'C51',
 'C56', 'C44', 'C38', 'C34')
rightfront <- c('C21', 'C14', 'C7', 'C2', 'C6', 'C13', 'C20',
 'C12', 'C19', 'C25', 'C24')
leftback <- c('C33', 'C36', 'C42', 'C49', 'C35', 'C41', 'C48',
 'C55', 'C40', 'C47', 'C54')
rightback <- c('C1', 'C4', 'C10', 'C17', 'C3', 'C9', 'C16', 
'C23', 'C8', 'C15', 'C22')

# Add quadrant and location columns to EEG and to EEG.window

EEG <- EEG %>%  mutate (quadrant = ifelse(electrode %in% leftback, "leftback", NA  ))
EEG <- EEG %>%  mutate (quadrant = ifelse(electrode %in% rightback, "rightback", EEG$electrode ))
EEG <- EEG %>%  mutate (quadrant = ifelse(electrode %in% leftfront, "leftfront", EEG$electrode ))
EEG <- EEG %>%  mutate (quadrant = ifelse(electrode %in% rightfront, "rightfront", EEG$electrode ))

EEG <- EEG %>%
  mutate (quadrant = ifelse(electrode %in% leftback, "leftback",
                        ifelse(electrode %in% rightback, "rightback",
                               ifelse(electrode %in% leftfront, "leftfront",
                             ifelse(electrode %in% rightfront, "rightfront",
                                                     NA  )))))

EEG <- EEG %>%  mutate (location = ifelse(electrode %in% leftback, "posterior", NA))
EEG <- EEG %>%  mutate (location = ifelse(electrode %in% rightback, "posterior", EEG$location))
EEG <- EEG %>%  mutate (location = ifelse(electrode %in% leftfront, "posterior", EEG$location))
EEG <- EEG %>%  mutate (location = ifelse(electrode %in% rightfront, "posterior", EEG$location))
EEG <- EEG %>%  mutate (location = ifelse(electrode %in% midfront, "posterior", EEG$location))
EEG <- EEG %>%  mutate (location = ifelse(electrode %in% midback, "posterior", EEG$location))

EEG <- EEG %>%
  mutate (location = ifelse(electrode %in% leftback, "posterior",
                     ifelse(electrode %in% rightback, "posterior",
                     ifelse(electrode %in% leftfront, "anterior",
                     ifelse(electrode %in% rightfront, "anterior",
				ifelse(electrode %in% midfront, "anterior",
				ifelse(electrode %in% midback, "posterior",
                                         			NA  )))))))



# Mean-center and scale continuous IVS
EEG$s_MRT <- scale(EEG$MRT)
EEG$s_Age_months <- scale(EEG$Age_months)

unique(is.na(EEG[,'microvolts']))  # good: no missing values


###########################################################################################



# Import EEG markers

# KEY ON EEG MARKERS (three codes stamped at the onset of each target trial in Brain Vision Analyzer Recorder)
# The markers from each participant are in the 'Raw Files' folder (.vmrk files). The first character 
# is a capital 'S', not a number five.
# Codes for items:
# 1-108 = target trials
# 109-216 = context trials
# 
# Codes for CMS conditions:
# 217 = Condition TotalMatch, i.e., Visual-to-Visual
# 218 = Condition EmbodiedMismatch, i.e., Haptic-to-Visual
# 219 = Condition TotalMismatch, i.e., Auditory-to-Visual
# 
# Codes for conceptual modalities:
# 221 = Auditory
# 222 = Haptic
# 223 = Visual


setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Raw Files')

library(plyr)
library(dplyr)
library(foreign)
library(data.table)
library(stringr)
library(oce)


# Construct rawdata object (structured as a 'list' of results).
# Because the first stimulus marker is in different 
markers <- rbind(
read.table(textConnection(rev(rev(readLines('1.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('2.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('3.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('4.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('5.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('6.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('8.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('9.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('10.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('11.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('12.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('13.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('14.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('15.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('16.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('17.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('18.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('19.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('20.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('21.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('22.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('23.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('24.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('26.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('27.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('28.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('29.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('30.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('31.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('32.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('34.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('35.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('36.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('37.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('38.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('39.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('40.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('41.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('42.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('44.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('45.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('46.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('47.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('48.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('49.vmrk'))[1:648])), sep=',', header=FALSE),
read.table(textConnection(rev(rev(readLines('50.vmrk'))[1:648])), sep=',', header=FALSE)
 )

# Add participant IDs
markers$participant = rep(c(1:6, 8:24, 26:32, 34:42, 44:50), times=1, each=648)

# Keep only participant ID and the relevant markers (not technical EEG)
markers = markers[c(6,2)]

names(markers)[2] = 'marker'

# Remove the initial 'S' (Stimulus) from markers
markers$marker = sub('.', '', markers$marker)

# Split markers into target item, condition, and modality
markers$target_item = NA
markers$condition = NA
markers$modality = NA
markers$target_item = markers$marker[seq(1, 29806, by=3)]
markers$condition = markers$marker[seq(2, 29807, by=3)]
markers$modality = markers$marker[seq(3, 29808, by=3)]

# Now drop the long-format 'marker' column
markers = markers[,-2]
summary(duplicated(markers)) # duplicated rows = 2/3 of total, which makes sense
markers = markers[!duplicated(markers),] # remove duplicates

# Select only target trials, leave out context trials
markers = markers[as.numeric(markers$target_item) <= 108,]

# Remove modality column (all target trials belong to the visual modality)
markers = markers[,-4]

# Replace condition markers with names, to match in the EEG data set
markers$condition = revalue(markers$condition, c('217'='visual2visual',
'218'='haptic2visual', '219'='auditory2visual'))


# So far in these markers, trials are provided within switch conditions. Now trace
# the general order per participant (randomization was per participant)

markers$target_trial = c(1:108)

# Include sequence of 1 through 36 for trials in each condition which will match the trial order per
# condition as retrieved from Brain Vision Recorder, and will allow merging into main data set
markers[markers$condition=='visual2visual','trial'] = c(1:36)
markers[markers$condition=='auditory2visual','trial'] = c(1:36)
markers[markers$condition=='haptic2visual','trial'] = c(1:36)

head(markers,15)
tail(markers,15) # correct matching of trials per condition and overall


# Finally, bind the item and trial markers onto main data set
setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Trial-by-trial export files')
EEG = readRDS('EEG.rds')
names(EEG)

# This is heavy (36 million rows). Remove unnecessary columns from main data to ease computation
EEG[, c("s_MRT","s_Age_months","quadrant","group"):=NULL]

# Also, remove every other electrode. Due to the shuffled disposition of the EEG montage used, this can be done
# by just removing even numbered electrodes. In three parts:

EEG = EEG[EEG$electrode=='C2' | EEG$electrode=='C4' | EEG$electrode=='C6' | EEG$electrode=='C8' | EEG$electrode=='C10' | 
EEG$electrode=='C12' | EEG$electrode=='C14' | EEG$electrode=='C16' | EEG$electrode=='C18' | EEG$electrode=='C20' | 
EEG$electrode=='C22' | EEG$electrode=='C24' | EEG$electrode=='C26' | EEG$electrode=='C28' | EEG$electrode=='C30' | 
EEG$electrode=='C34' | EEG$electrode=='C36' | EEG$electrode=='C38' | EEG$electrode=='C40' | EEG$electrode=='C42' | 
EEG$electrode=='C44' | EEG$electrode=='C46' | EEG$electrode=='C48' | EEG$electrode=='C50' | EEG$electrode=='C52' | 
EEG$electrode=='C54' | EEG$electrode=='C56' | EEG$electrode=='C58' | EEG$electrode=='C60',]  # note: C32 was online ref.

# Bind
EEG = merge(EEG, markers, by=c('participant','condition','trial'),all.x=T)
which(is.na(EEG$target_trial))
head(EEG)

# Create data.table to ease computation
EEG = setDT(EEG)

# Check matching between the trial numbers per condition ('trial') and those per participant ('target_trial')
unique(EEG[EEG$target_trial=='1','trial'])
unique(EEG[EEG$target_trial=='50','trial'])
unique(EEG[EEG$target_trial=='108','trial'])
cor(EEG$trial, EEG$target_trial)		# all good

# Remove the trial-per-condition column
drop = 'trial'
EEG = EEG[, !(names(EEG) %in% drop)]

# Reorder variables
names(EEG)
EEG = EEG[,c(1,7,12,13,2,3,4,11,5,9,8,6,10)]

###########################################################################################



# For a follow-up analysis, we'll probe into the interaction of RT group and switch condition. 
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

setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment')
stimulus = read.csv('Stimulus per participant and trial.csv')

# Import stimuli and frequencies of co-occurrence (property to property) for the linguistic switch weight
stimulus$target_trial = stimulus$Event  # Since target trials 1-108, same as number as trial pairs or 'Events'
stimulus = stimulus[,c('participant','target_trial','log_cooccurrence_propertytoproperty','CONTEXT_property',
'CONTEXT_concept','TARGET_property','TARGET_concept','CONTEXT_mean_exc','TARGET_prop_exc')]

head(EEG)
names(stimulus)

EEG = merge(EEG, stimulus, by=c('participant','target_trial'))
head(EEG)
str(EEG) # all still correct
nrow(EEG)

# For the modality-switch weight, import modality scores from our norms.
setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality norms')
norms = read.csv('all.csv')

# capitalize words
norms$word = capitalize(as.character(norms$word))
norms$inflected_prop = capitalize(as.character(norms$inflected_prop))



# Starting with context trial properties
# split properties and select only modality scores
properties = norms[norms$cat=='prop',]
properties = properties[,c('word','inflected_prop','Auditory','Haptic','Visual')]

# extract column from EEG
adhoc = data.frame(unique(EEG$CONTEXT_property))
names(adhoc)[1] = 'CONTEXT_property'

# rename word column in norms transitorily
names(properties)[1] = 'CONTEXT_property'

# merge
adhoc = merge(adhoc, properties, by='CONTEXT_property', all.x=T)

# now for inflected adjs., rename that column
names(properties)[1] = 'changename'
names(properties)[2] = 'CONTEXT_inflec_property'
names(adhoc)[2] = 'CONTEXT_inflec_property'
head(properties,20)
head(adhoc,20)
adhoc$CONTEXT_inflec_property[is.na(adhoc$CONTEXT_inflec_property)] = 
as.character(adhoc$CONTEXT_property[is.na(adhoc$CONTEXT_inflec_property)])

# merge
adhoc = merge(adhoc, properties, by='CONTEXT_inflec_property', all.x=T)

# tidy up
names(adhoc)
adhoc = adhoc[,c(2,7,8,9)]
names(adhoc)[c(2,3,4)] = c('CONTEXT_property_Auditory','CONTEXT_property_Haptic','CONTEXT_property_Visual')

# finally, merge to EEG data
EEG = merge(EEG, adhoc, by='CONTEXT_property', all.x=T)

# check all right
head(EEG,30)
tail(EEG,30)
summary(is.na(EEG$CONTEXT_property_Auditory))  # good: no NAs


# Now get target trial properties

# reload
properties = norms[norms$cat=='prop',]
properties = properties[,c('word','inflected_prop','Auditory','Haptic','Visual')]

# extract column from EEG
adhoc = data.frame(unique(EEG$TARGET_property))
names(adhoc)[1] = 'TARGET_property'

# rename word column in norms transitorily
names(properties)[1] = 'TARGET_property'

# merge
adhoc = merge(adhoc, properties, by='TARGET_property', all.x=T)

# now for inflected adjs., rename that column
names(properties)[1] = 'changename'
names(properties)[2] = 'TARGET_inflec_property'
names(adhoc)[2] = 'TARGET_inflec_property'
head(properties,20)
head(adhoc,20)
adhoc$TARGET_inflec_property[is.na(adhoc$TARGET_inflec_property)] = 
as.character(adhoc$TARGET_property[is.na(adhoc$TARGET_inflec_property)])

# merge
adhoc = merge(adhoc, properties, by='TARGET_inflec_property', all.x=T)

# tidy up
names(adhoc)
adhoc = adhoc[,c(2,7,8,9)]
names(adhoc)[c(2,3,4)] = c('TARGET_property_Auditory','TARGET_property_Haptic','TARGET_property_Visual')

# finally, merge to EEG data
EEG = merge(EEG, adhoc, by='TARGET_property', all.x=T)

# check all right
head(EEG,30)
tail(EEG,30)
nrow(EEG)
summary(is.na(EEG$CONTEXT_property_Auditory))  # good: no NAs

# tidy up columns
names(EEG)
EEG = EEG[,c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,2,1,17,18,19,20,21,22,23,24,25,26)]


# SAVE
#saveRDS(EEG, 'EEG.rds', compress='xz')

# read in
#EEG = readRDS('EEG.rds')
