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


# Waveform plots for different factors. See also difference topographies from 
# Brain Vision Analyzer in the folder.

####################################

# PLOTS WITH THE RT-BASED GROUPS

# # # # # # # # # # # # # #

## QUICK GROUP


# Set electrode
elec = 1
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_1 = plotname


####################################



# Set electrode
elec = 2 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_2 = plotname


####################################



# Set electrode
elec = 3 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_3 = plotname


####################################



# Set electrode
elec = 4 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_4 = plotname


####################################



# Set electrode
elec = 5 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_5 = plotname


####################################



# Set electrode
elec = 6 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_6 = plotname


####################################



# Set electrode
elec = 7 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_7 = plotname


####################################



# Set electrode
elec = 8 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_8 = plotname


####################################



# Set electrode
elec = 9 


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_9 = plotname


####################################



# Set electrode
elec = 10 

# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_10 = plotname


####################################



# Set electrode
elec = 11 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_11 = plotname


####################################



# Set electrode
elec = 12 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_12 = plotname


####################################



# Set electrode
elec = 13
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_13 = plotname

####################################



# Set electrode
elec = 14 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_14 = plotname


####################################



# Set electrode
elec = 15 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_15 = plotname


####################################



# Set electrode
elec = 16 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_16 = plotname


####################################



# Set electrode
elec = 17 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_17 = plotname


####################################



# Set electrode
elec = 18 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_18 = plotname


####################################



# Set electrode
elec = 19 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_19 = plotname


####################################



# Set electrode
elec = 20 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_20 = plotname


####################################



# Set electrode
elec = 21 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_21 = plotname


####################################



# Set electrode
elec = 22 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_22 = plotname

####################################



# Set electrode
elec = 23
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_23 = plotname


####################################



# Set electrode
elec = 24 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_24 = plotname


####################################



# Set electrode
elec = 25 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_25 = plotname


####################################



# Set electrode
elec = 26 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_26 = plotname


####################################



# Set electrode
elec = 27 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_27 = plotname


####################################



# Set electrode
elec = 28 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_28 = plotname


####################################



# Set electrode
elec = 29 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_29 = plotname


####################################



# Set electrode
elec = 30 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_30 = plotname


####################################



# Set electrode
elec = 31 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_31 = plotname


####################################



# Set electrode
elec = 33 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_33 = plotname


####################################



# Set electrode
elec = 34 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_34 = plotname


####################################



# Set electrode
elec = 35 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_35 = plotname


####################################



# Set electrode
elec = 36 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_36 = plotname


####################################



# Set electrode
elec = 37 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_37 = plotname


####################################



# Set electrode
elec = 38 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_38 = plotname


####################################



# Set electrode
elec = 39 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_39 = plotname


####################################



# Set electrode
elec = 40 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_40 = plotname

####################################



# Set electrode
elec = 41
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_41 = plotname


####################################



# Set electrode
elec = 42 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_42 = plotname


####################################



# Set electrode
elec = 43
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_43 = plotname


####################################



# Set electrode
elec = 44
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_44 = plotname



####################################



# Set electrode
elec = 45 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_45 = plotname


####################################



# Set electrode
elec = 46 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_46 = plotname


####################################



# Set electrode
elec = 47 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_47 = plotname


####################################



# Set electrode
elec = 48 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_48 = plotname


####################################



# Set electrode
elec = 49 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_49 = plotname


####################################



# Set electrode
elec = 50 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_50 = plotname

####################################



# Set electrode
elec = 51
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_51 = plotname


####################################



# Set electrode
elec = 52 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_52 = plotname


####################################



# Set electrode
elec = 53 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_53 = plotname


####################################



# Set electrode
elec = 54 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_54 = plotname


####################################



# Set electrode
elec = 55 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_55 = plotname


####################################



# Set electrode
elec = 56 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_56 = plotname


####################################



# Set electrode
elec = 57 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_57 = plotname


####################################



# Set electrode
elec = 58 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_58 = plotname


####################################



# Set electrode
elec = 59 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_59 = plotname


####################################



# Set electrode
elec = 60 
elec=as.character(elec)


# Subset: Quick group, anterior electrode
dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Quick group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Quick group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Quick_60 = plotname












## SLOW GROUP


# Set electrode
elec = 1
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_1 = plotname


####################################



# Set electrode
elec = 2 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_2 = plotname


####################################



# Set electrode
elec = 3 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_3 = plotname


####################################



# Set electrode
elec = 4 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_4 = plotname


####################################



# Set electrode
elec = 5 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_5 = plotname


####################################



# Set electrode
elec = 6 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_6 = plotname


####################################



# Set electrode
elec = 7 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_7 = plotname


####################################



# Set electrode
elec = 8 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_8 = plotname


####################################



# Set electrode
elec = 9 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_9 = plotname


####################################



# Set electrode
elec = 10 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_10 = plotname


####################################



# Set electrode
elec = 11 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_11 = plotname


####################################



# Set electrode
elec = 12 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_12 = plotname


####################################



# Set electrode
elec = 13
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_13 = plotname

####################################



# Set electrode
elec = 14 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_14 = plotname


####################################



# Set electrode
elec = 15 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_15 = plotname


####################################



# Set electrode
elec = 16 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_16 = plotname


####################################



# Set electrode
elec = 17 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_17 = plotname


####################################



# Set electrode
elec = 18 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_18 = plotname


####################################



# Set electrode
elec = 19 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_19 = plotname


####################################



# Set electrode
elec = 20 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_20 = plotname


####################################



# Set electrode
elec = 21 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_21 = plotname


####################################



# Set electrode
elec = 22 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_22 = plotname

####################################



# Set electrode
elec = 23
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_23 = plotname


####################################



# Set electrode
elec = 24 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_24 = plotname


####################################



# Set electrode
elec = 25 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_25 = plotname


####################################



# Set electrode
elec = 26 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_26 = plotname


####################################



# Set electrode
elec = 27 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_27 = plotname


####################################



# Set electrode
elec = 28 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_28 = plotname


####################################



# Set electrode
elec = 29 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_29 = plotname


####################################



# Set electrode
elec = 30 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_30 = plotname


####################################



# Set electrode
elec = 31 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_31 = plotname


####################################



# Set electrode
elec = 33 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_33 = plotname


####################################



# Set electrode
elec = 34 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_34 = plotname


####################################



# Set electrode
elec = 35 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_35 = plotname


####################################



# Set electrode
elec = 36 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_36 = plotname


####################################



# Set electrode
elec = 37 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_37 = plotname


####################################



# Set electrode
elec = 38 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_38 = plotname


####################################



# Set electrode
elec = 39 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_39 = plotname


####################################



# Set electrode
elec = 40 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_40 = plotname

####################################



# Set electrode
elec = 41
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_41 = plotname


####################################



# Set electrode
elec = 42 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_42 = plotname


####################################



# Set electrode
elec = 43
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_43 = plotname


####################################



# Set electrode
elec = 44
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_44 = plotname



####################################



# Set electrode
elec = 45 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_45 = plotname


####################################



# Set electrode
elec = 46 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_46 = plotname


####################################



# Set electrode
elec = 47 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_47 = plotname


####################################



# Set electrode
elec = 48 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_48 = plotname


####################################



# Set electrode
elec = 49 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_49 = plotname


####################################



# Set electrode
elec = 50 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_50 = plotname

####################################



# Set electrode
elec = 51
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_51 = plotname


####################################



# Set electrode
elec = 52 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_52 = plotname


####################################



# Set electrode
elec = 53 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_53 = plotname


####################################



# Set electrode
elec = 54 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_54 = plotname


####################################



# Set electrode
elec = 55 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_55 = plotname


####################################



# Set electrode
elec = 56 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_56 = plotname


####################################



# Set electrode
elec = 57 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_57 = plotname


####################################



# Set electrode
elec = 58 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_58 = plotname


####################################



# Set electrode
elec = 59 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_59 = plotname


####################################



# Set electrode
elec = 60 
elec=as.character(elec)



dfelectrode <- aggregate(microvolts ~ time*condition*electrode, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, electrode == elec)

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], electrode == elec)

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

spec_title = paste0('ERP waveforms for Slow group, electrode ', elec, ' (negative values up and time windows displayed)')

plotname <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-8.3, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1), labels=c('','','+6 µV','','','','','',' 0 µV','','','','','','-6 µV','',''))+
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), labels= c('-200','-100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle(spec_title)+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-8,8,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17))+
  theme(legend.title = element_text(size=17, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '-3 µV', x = -29, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -29, y = -3, size = 5, color = 'grey32', family='sans')+ 
  annotate('text', label = '-6 µV', x = -29, y = 6, size = 5, color = 'grey32', family='sans')+

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

# png(paste0('Slow group, electrode ', elec, '.png'), 
#	units='in', width=13, height=5, res=900)
# print(plotname)
# dev.off()

# Name for later
Slow_60 = plotname


# Print out plots on PDF
pdf("List plots DEFINITIVE groups.pdf", width=13, height=5)
print(Quick_1)
print(Slow_1)
print(Quick_2)
print(Slow_2)
print(Quick_3)
print(Slow_3)
print(Quick_4)
print(Slow_4)
print(Quick_5)
print(Slow_5)
print(Quick_6)
print(Slow_6)
print(Quick_7)
print(Slow_7)
print(Quick_8)
print(Slow_8)
print(Quick_9)
print(Slow_9)
print(Quick_10)
print(Slow_10)
print(Quick_11)
print(Slow_11)
print(Quick_12)
print(Slow_12)
print(Quick_13)
print(Slow_13)
print(Quick_14)
print(Slow_14)
print(Quick_15)
print(Slow_15)
print(Quick_16)
print(Slow_16)
print(Quick_17)
print(Slow_17)
print(Quick_18)
print(Slow_18)
print(Quick_19)
print(Slow_19)
print(Quick_20)
print(Slow_20)
print(Quick_21)
print(Slow_21)
print(Quick_22)
print(Slow_22)
print(Quick_23)
print(Slow_23)
print(Quick_24)
print(Slow_24)
print(Quick_25)
print(Slow_25)
print(Quick_26)
print(Slow_26)
print(Quick_27)
print(Slow_27)
print(Quick_28)
print(Slow_28)
print(Quick_29)
print(Slow_29)
print(Quick_30)
print(Slow_30)
print(Quick_31)
print(Slow_31)
print(Quick_33)
print(Slow_33)
print(Quick_34)
print(Slow_34)
print(Quick_35)
print(Slow_35)
print(Quick_36)
print(Slow_36)
print(Quick_37)
print(Slow_37)
print(Quick_38)
print(Slow_38)
print(Quick_39)
print(Slow_39)
print(Quick_40)
print(Slow_40)
print(Quick_41)
print(Slow_41)
print(Quick_42)
print(Slow_42)
print(Quick_43)
print(Slow_43)
print(Quick_44)
print(Slow_44)
print(Quick_45)
print(Slow_45)
print(Quick_46)
print(Slow_46)
print(Quick_47)
print(Slow_47)
print(Quick_48)
print(Slow_48)
print(Quick_49)
print(Slow_49)
print(Quick_50)
print(Slow_50)
print(Quick_51)
print(Slow_51)
print(Quick_52)
print(Slow_52)
print(Quick_53)
print(Slow_53)
print(Quick_54)
print(Slow_54)
print(Quick_55)
print(Slow_55)
print(Quick_56)
print(Slow_56)
print(Quick_57)
print(Slow_57)
print(Quick_58)
print(Slow_58)
print(Quick_59)
print(Slow_59)
print(Quick_60)
print(Slow_60)
dev.off()