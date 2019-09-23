# ACKNOWLEDGMENT: Some of this code was borrowed from Gwilym Lockwood, PhD: https://osf.io/mu3nw/

# Waveform plots. See also difference topographies in the same Plots folder.

EEG = readRDS('EEG.rds')
str(EEG)

# Remove letter C before electrode names to avoid confusion with 10-20 montage
EEG$electrode = substring(EEG$electrode, 2)



install.packages('easyGgplot2')
install.packages('merTools)
install.packages('gtools')
install.packages('tabplot')
install.packages('plyr')
install.packages('ggplot2')
install.packages('Hmisc')
install.packages('pastecs')
install.packages('reshape')
install.packages('grid')
install.packages('lmerTest')
install.packages('dplyr')
install.packages('lsr')
install.packages('ggthemes')
install.packages('magrittr')
install.packages('doBy')
install.packages('Hmisc')
install.packages('stats')
install.packages('QuantPsyc')
install.packages('psych')
install.packages('car')
install.packages('dae')
install.packages('png')
install.packages('rasterImage')
install.packages('eeptools')
install.packages('MuMIn')
install.packages('stringi')
install.packages('data.table')
install.packages('lsmeans')
install.packages('gtools')

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
library('dplyr')
library('lsr')
library('ggthemes')
library('magrittr')
library('doBy')
library(Hmisc)
library(stats)
library(QuantPsyc)
library(psych)
library(car)
library(dae)
library(png)
library(rasterImage)
library(stringi)
library(data.table)
library(lsmeans)
library(gtools)



####################################

# PLOTS PER PARTICIPANT AND BRAIN AREA (aka Location)

# # # # # # # # # # # # # #


## ANTERIOR BRAIN AREA


# Set participant and brain area
1




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='1',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='1',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 1, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant 1, anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant1 = plotname


####################################



# Set participant and brain area
2 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='2',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='2',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 2, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant2 = plotname


####################################



# Set participant and brain area
3 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='3',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='3',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 3, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant3 = plotname


####################################



# Set participant and brain area
4 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='4',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='4',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 4, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant4 = plotname


####################################



# Set participant and brain area
5 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='5',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='5',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 5, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant5 = plotname


####################################



# Set participant and brain area
6 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='6',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='6',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 6, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant6 = plotname


####################################


# Set participant and brain area
8 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='8',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='8',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 8, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant8 = plotname


####################################



# Set participant and brain area
9 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='9',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='9',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 9, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant9 = plotname


####################################



# Set participant and brain area
10 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='10',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='10',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 10, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant10 = plotname


####################################



# Set participant and brain area
11 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='11',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='11',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 11, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant11 = plotname


####################################



# Set participant and brain area
12 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='12',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='12',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 12, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant12 = plotname


####################################



# Set participant and brain area
13




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='13',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='13',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 13, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant13 = plotname

####################################



# Set participant and brain area
14 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='14',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='14',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 14, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant14 = plotname


####################################



# Set participant and brain area
15 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='15',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='15',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 15, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant15 = plotname


####################################



# Set participant and brain area
16 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='16',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='16',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 16, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant16 = plotname


####################################



# Set participant and brain area
17 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='17',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='17',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 17, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant17 = plotname


####################################



# Set participant and brain area
18 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='18',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='18',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 18, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant18 = plotname


####################################



# Set participant and brain area
19 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='19',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='19',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 19, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant19 = plotname


####################################



# Set participant and brain area
20 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='20',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='20',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 20, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant20 = plotname


####################################



# Set participant and brain area
21 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='21',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='21',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 21, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant21 = plotname


####################################



# Set participant and brain area
22 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='22',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='22',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 22, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant22 = plotname

####################################



# Set participant and brain area
23




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='23',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='23',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 23, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant23 = plotname


####################################



# Set participant and brain area
24 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='24',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='24',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 24, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant24 = plotname


####################################



# Set participant and brain area
25 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='26',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='26',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 26, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant26 = plotname


####################################



# Set participant and brain area
26 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='27',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='27',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 27, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant27 = plotname


####################################



# Set participant and brain area
27 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='28',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='28',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 28, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant28 = plotname


####################################



# Set participant and brain area
28 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='29',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='29',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 29, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant29 = plotname


####################################



# Set participant and brain area
29 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='30',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='30',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 30, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant30 = plotname


####################################



# Set participant and brain area
30 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='31',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='31',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 31, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant31 = plotname


####################################



# Set participant and brain area
31 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='32',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='32',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 32, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant32 = plotname


####################################



# Set participant and brain area
33 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='34',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='34',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 34, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant34 = plotname


####################################



# Set participant and brain area
34 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='35',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='35',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 35, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant35 = plotname


####################################



# Set participant and brain area
35 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='36',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='36',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 36, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant36 = plotname


####################################



# Set participant and brain area
36 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='37',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='37',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 37, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant37 = plotname


####################################



# Set participant and brain area
37 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='38',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='38',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 38, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant38 = plotname


####################################



# Set participant and brain area
38 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='39',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='39',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 39, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant39 = plotname


####################################



# Set participant and brain area
39 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='40',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='40',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 40, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant40 = plotname


####################################



# Set participant and brain area
40 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='41',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='41',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 41, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant41 = plotname

####################################



# Set participant and brain area
41




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='42',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='42',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 42, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant42 = plotname


####################################


# Set participant and brain area
43




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='44',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='44',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 44, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant44 = plotname


####################################



# Set participant and brain area
44




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='45',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='45',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 45, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant45 = plotname



####################################



# Set participant and brain area
45 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='46',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='46',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 46, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant46 = plotname


####################################



# Set participant and brain area
46 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='47',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='47',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 47, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant47 = plotname

#####################

# Set participant and brain area
46 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='48',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='48',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 48, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant48 = plotname

############################

# Set participant and brain area
46 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='49',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='49',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 49, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant49 = plotname

#########################

# Set participant and brain area
46 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='50',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$participant=='50',], location == 'anterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 50, anterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , anterior brain area .png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Anteriorbrainarea_Participant50 = plotname

########################################################################




## POSTERIOR BRAIN AREA


# Set participant and brain area
1




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='1',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='1',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 1, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant1 = plotname


####################################



# Set participant and brain area
2 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='2',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='2',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 2, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant2 = plotname


####################################



# Set participant and brain area
3 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='3',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='3',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 3, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant3 = plotname


####################################



# Set participant and brain area
4 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='4',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='4',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 4, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant4 = plotname


####################################



# Set participant and brain area
5 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='5',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='5',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 5, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant5 = plotname


####################################



# Set participant and brain area
6 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='6',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='6',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 6, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant6 = plotname


####################################


# Set participant and brain area
8 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='8',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='8',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 8, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant8 = plotname


####################################



# Set participant and brain area
9 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='9',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='9',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 9, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant9 = plotname


####################################



# Set participant and brain area
10 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='10',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='10',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 10, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant10 = plotname


####################################



# Set participant and brain area
11 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='11',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='11',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 11, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant11 = plotname


####################################



# Set participant and brain area
12 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='12',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='12',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 12, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant12 = plotname


####################################



# Set participant and brain area
13




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='13',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='13',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 13, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant13 = plotname

####################################



# Set participant and brain area
14 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='14',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='14',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 14, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant14 = plotname


####################################



# Set participant and brain area
15 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='15',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='15',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 15, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant15 = plotname


####################################



# Set participant and brain area
16 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='16',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='16',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 16, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant16 = plotname


####################################



# Set participant and brain area
17 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='17',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='17',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 17, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant17 = plotname


####################################



# Set participant and brain area
18 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='18',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='18',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 18, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant18 = plotname


####################################



# Set participant and brain area
19 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='19',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='19',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 19, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant19 = plotname


####################################



# Set participant and brain area
20 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='20',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='20',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 20, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant20 = plotname


####################################



# Set participant and brain area
21 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='21',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='21',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 21, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant21 = plotname


####################################



# Set participant and brain area
22 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='22',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='22',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 22, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant22 = plotname

####################################



# Set participant and brain area
23




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='23',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='23',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 23, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant23 = plotname


####################################



# Set participant and brain area
24 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='24',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='24',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 24, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant24 = plotname


####################################



# Set participant and brain area
25 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='26',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='26',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 26, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant26 = plotname


####################################



# Set participant and brain area
26 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='27',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='27',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 27, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant27 = plotname


####################################



# Set participant and brain area
27 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='28',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='28',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 28, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant28 = plotname


####################################



# Set participant and brain area
28 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='29',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='29',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 29, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant29 = plotname


####################################



# Set participant and brain area
29 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='30',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='30',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 30, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant30 = plotname


####################################



# Set participant and brain area
30 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='31',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='31',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 31, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant31 = plotname


####################################



# Set participant and brain area
31 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='32',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='32',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 32, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant32 = plotname


####################################



# Set participant and brain area
33 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='34',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='34',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 34, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant34 = plotname


####################################



# Set participant and brain area
34 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='35',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='35',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 35, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant35 = plotname


####################################



# Set participant and brain area
35 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='36',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='36',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 36, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant36 = plotname


####################################



# Set participant and brain area
36 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='37',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='37',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 37, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant37 = plotname


####################################



# Set participant and brain area
37 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='38',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='38',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 38, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant38 = plotname


####################################



# Set participant and brain area
38 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='39',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='39',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 39, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant39 = plotname


####################################



# Set participant and brain area
39 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='40',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='40',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 40, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant40 = plotname


####################################



# Set participant and brain area
40 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='41',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='41',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 41, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant41 = plotname

####################################



# Set participant and brain area
41




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='42',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='42',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 42, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant42 = plotname


####################################


# Set participant and brain area
43




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='44',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='44',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 44, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant44 = plotname


####################################



# Set participant and brain area
44




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='45',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='45',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 45, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant45 = plotname



####################################



# Set participant and brain area
45 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='46',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='46',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 46, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant46 = plotname


####################################



# Set participant and brain area
46 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='47',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='47',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 47, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant47 = plotname

#####################

# Set participant and brain area
46 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='48',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='48',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 48, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant48 = plotname

############################

# Set participant and brain area
46 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='49',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='49',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 49, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant49 = plotname

#########################

# Set participant and brain area
46 




dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$participant=='50',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$participant=='50',], location == 'posterior')

std <- function(x) sd(x)/sqrt(length(x))
# i.e. std is a function to give you the standard error of the mean
# this is the standard deviation of a sample divided by the square root 
# of the sample size

SD <- rep(NA, length(df2$time))       # vector SD per time
SE <- rep(NA, length(df2$time))       # vector SE per time
SDupper <- rep(NA, length(df2$time))  # vector measurement + 1 SD
SDlower <- rep(NA, length(df2$time))  # vector measurement - 1 SD
SEupper <- rep(NA, length(df2$time))  # vector plotting SE per time
SElower <- rep(NA, length(df2$time))  # vector plotting SE per time
CIupper <- rep(NA, length(df2$time))  # vector upper 95% conf int
CIlower <- rep(NA, length(df2$time))  # vector lower 95% conf int

for (i in 1:length(df2$time)){
  something <- subset(df3, time==df2$time[i] & 
condition==df2$condition[i], select=microvolts)
  SD[i] = sd(something$microvolts)
  SE[i] = std(something$microvolts)
  SDupper[i] = df2$microvolts[i] + SD[i]
  SDlower[i] = df2$microvolts[i] - SD[i]
  SEupper[i] = df2$microvolts[i] + SE[i]
  SElower[i] = df2$microvolts[i] - SE[i]
  CIupper[i] = df2$microvolts[i] + (SE[i] * 1.96)
  CIlower[i] = df2$microvolts[i] - (SE[i] * 1.96)}
df2$CIL <- CIlower
df2$CIU <- CIupper


df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', ' Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant 50, posterior brain area (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-14.3, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1), labels=c('','','','','','','','+7 µV','','','','','','','0 µV','','','','','','','-7 µV','','','','','','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-14,14,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-6 µV', x = -32, y = 6, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+6 µV', x = -32, y = -6, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-12 µV', x = -32, y = 12, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  theme(plot.margin = unit(c(0.3,0.2,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))

# png(paste0('Participant , posterior brain area.png'), 
#	units='in', width=13, height=6, res=900)
# print(plotname)
# dev.off()

# Name for later
Posteriorbrainarea_Participant50 = plotname

########################################################################




# Print out plots on PDF
pdf("adhoc.pdf", width=13, height=5)
print(Anteriorbrainarea_Participant1)
print(Posteriorbrainarea_Participant1)
print(Anteriorbrainarea_Participant2)
print(Posteriorbrainarea_Participant2)
print(Anteriorbrainarea_Participant3)
print(Posteriorbrainarea_Participant3)
print(Anteriorbrainarea_Participant4)
print(Posteriorbrainarea_Participant4)
print(Anteriorbrainarea_Participant5)
print(Posteriorbrainarea_Participant5)
print(Anteriorbrainarea_Participant6)
print(Posteriorbrainarea_Participant6)
print(Anteriorbrainarea_Participant8)
print(Posteriorbrainarea_Participant8)
print(Anteriorbrainarea_Participant9)
print(Posteriorbrainarea_Participant9)
print(Anteriorbrainarea_Participant10)
print(Posteriorbrainarea_Participant10)
print(Anteriorbrainarea_Participant11)
print(Posteriorbrainarea_Participant11)
print(Anteriorbrainarea_Participant12)
print(Posteriorbrainarea_Participant12)
print(Anteriorbrainarea_Participant13)
print(Posteriorbrainarea_Participant13)
print(Anteriorbrainarea_Participant14)
print(Posteriorbrainarea_Participant14)
print(Anteriorbrainarea_Participant15)
print(Posteriorbrainarea_Participant15)
print(Anteriorbrainarea_Participant16)
print(Posteriorbrainarea_Participant16)
print(Anteriorbrainarea_Participant17)
print(Posteriorbrainarea_Participant17)
print(Anteriorbrainarea_Participant18)
print(Posteriorbrainarea_Participant18)
print(Anteriorbrainarea_Participant19)
print(Posteriorbrainarea_Participant19)
print(Anteriorbrainarea_Participant20)
print(Posteriorbrainarea_Participant20)
print(Anteriorbrainarea_Participant21)
print(Posteriorbrainarea_Participant21)
print(Anteriorbrainarea_Participant22)
print(Posteriorbrainarea_Participant22)
print(Anteriorbrainarea_Participant23)
print(Posteriorbrainarea_Participant23)
print(Anteriorbrainarea_Participant24)
print(Posteriorbrainarea_Participant24)
print(Anteriorbrainarea_Participant26)
print(Posteriorbrainarea_Participant26)
print(Anteriorbrainarea_Participant27)
print(Posteriorbrainarea_Participant27)
print(Anteriorbrainarea_Participant28)
print(Posteriorbrainarea_Participant28)
print(Anteriorbrainarea_Participant29)
print(Posteriorbrainarea_Participant29)
print(Anteriorbrainarea_Participant30)
print(Posteriorbrainarea_Participant30)
print(Anteriorbrainarea_Participant31)
print(Posteriorbrainarea_Participant31)
print(Anteriorbrainarea_Participant32)
print(Posteriorbrainarea_Participant32)
print(Anteriorbrainarea_Participant34)
print(Posteriorbrainarea_Participant34)
print(Anteriorbrainarea_Participant35)
print(Posteriorbrainarea_Participant35)
print(Anteriorbrainarea_Participant36)
print(Posteriorbrainarea_Participant36)
print(Anteriorbrainarea_Participant37)
print(Posteriorbrainarea_Participant37)
print(Anteriorbrainarea_Participant38)
print(Posteriorbrainarea_Participant38)
print(Anteriorbrainarea_Participant39)
print(Posteriorbrainarea_Participant39)
print(Anteriorbrainarea_Participant40)
print(Posteriorbrainarea_Participant40)
print(Anteriorbrainarea_Participant41)
print(Posteriorbrainarea_Participant41)
print(Anteriorbrainarea_Participant42)
print(Posteriorbrainarea_Participant42)
print(Anteriorbrainarea_Participant44)
print(Posteriorbrainarea_Participant44)
print(Anteriorbrainarea_Participant45)
print(Posteriorbrainarea_Participant45)
print(Anteriorbrainarea_Participant46)
print(Posteriorbrainarea_Participant46)
print(Anteriorbrainarea_Participant47)
print(Posteriorbrainarea_Participant47)
print(Anteriorbrainarea_Participant48)
print(Posteriorbrainarea_Participant48)
print(Anteriorbrainarea_Participant49)
print(Posteriorbrainarea_Participant49)
print(Anteriorbrainarea_Participant50)
print(Posteriorbrainarea_Participant50)
dev.off()