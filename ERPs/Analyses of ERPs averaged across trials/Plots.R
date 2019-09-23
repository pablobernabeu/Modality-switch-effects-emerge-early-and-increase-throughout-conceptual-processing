setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Export Files')

# Waveform plots

# ACKNOWLEDGMENT: Some of this code was borrowed from Gwilym Lockwood, PhD: https://osf.io/mu3nw/

install.packages("devtools")
library(devtools)
install_github("kassambara/easyGgplot2")
install.packages('merTools')
install.packages('gtools')
install.packages('tabplot')
install.packages('plyr')
install.packages('ggplot2')
install.packages('Hmisc')
install.packages('pastecs')
install.packages('reshape')
install.packages('grid')
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
install.packages('emmeans')
install.packages('gridExtra')

library(foreign)
library(easyGgplot2)
library(merTools)
library(gtools)
library(tabplot)
library(plyr)
library(ggplot2)
library(Hmisc)
library(pastecs)
library(reshape)
library(grid)
library(dplyr)
library(lsr)
library(ggthemes)
library(magrittr)
library(doBy)
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
library(emmeans)
library(gridExtra)


# * See also difference topographies from Brain Vision Analyzer in the folder.


# WAVEFORMS 

EEG = readRDS('EEG.rds')
str(EEG)
head(EEG)

# Get cap images to be overlaid
iconQuickAnt <- readPNG('iconQuickAnt.png')
iconQuickPos <- readPNG('iconQuickPos.png')
iconSlowAnt <- readPNG('iconSlowAnt.png')
iconSlowPos <- readPNG('iconSlowPos.png')


####################################

# PLOTS WITH THE RT-BASED GROUPS (see below plots with the original experimental groups)

# # # # # # # # # # # # # #

# Plots with full baseline, 200 ms

# In colour
# Subset: Quick group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location,
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], location == 'anterior')

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

Quick_ant <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) + 
  annotation_raster(iconQuickAnt, ymin = -0.95,ymax= -4,xmin = -165,xmax = -42)+
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004), 
  labels= c('','+3 µV','','',' 0 µV','','','–3 µV',''))+
  scale_x_continuous(limits=c(-200,800),breaks=seq(-200,800,by=100), expand = c(0.005,0),
  labels= c('–200','–100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Quick group, anterior electrodes')+ theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.130, 0.890))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=18))+
  theme(legend.title = element_text(size=18, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_text(hjust= -0.02), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.8, yend=-3.8, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 

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
    theme(plot.margin = unit(c(0.3,0.3,0.2,0.1), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))


# Subset: Slow group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], location == 'anterior')

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

Slow_ant <- ggplot(df2, aes(x=time, y=-microvolts, color=condition))+
  annotation_raster(iconSlowAnt, ymin = -0.85,ymax= -4,xmin = -165,xmax = -42) + 
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004), 
  labels= c('','+3 µV','','',' 0 µV','','','–3 µV',''))+
  scale_x_continuous(limits=c(-200,800),breaks=seq(-200,800,by=100), expand = c(0.005,0), 
  labels= c('–200','–100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Slow group, anterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.130, 0.890))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=18))+
  theme(legend.title = element_text(size=18, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_text(hjust= -0.02), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.8, yend=-3.8, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 

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
    theme(plot.margin = unit(c(0.3,0.3,0.2,0.1), 'cm'))+ 
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  guides(color=guide_legend(override.aes = list(size=2.5)))



# Subset: Quick group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], location == 'posterior')

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

Quick_pos <- ggplot(df2, aes(x=time, y=-microvolts, color=condition))+
  annotation_raster(iconQuickPos, ymin = 0.85,ymax= 4.10,xmin = -165,xmax = -42) + 
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004), 
  labels= c('','+3 µV','','',' 0 µV','','','–3 µV',''))+
  scale_x_continuous(limits=c(-200,800),breaks=seq(-200,800,by=100), expand = c(0.005,0), 
  labels= c('–200','–100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Quick group, posterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.130, 0.180))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=18))+
  theme(legend.title = element_text(size=18, face='bold'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_text(hjust= -0.02), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.8, yend=-3.8, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours) +
    theme(plot.margin = unit(c(0.3,0.3,0.2,0.1), 'cm'))+ 
  guides(color=guide_legend(override.aes = list(size=2.5)))+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) 




# Subset: Slow group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], location == 'posterior')

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

Slow_pos <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  annotation_raster(iconSlowPos, ymin = 0.95,ymax= 4.10,xmin = -165,xmax = -42) + 
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004), 
  labels= c('','+3 µV','','',' 0 µV','','','–3 µV',''))+
  scale_x_continuous(limits=c(-200,800),breaks=seq(-200,800,by=100), expand = c(0.005,0), 
  labels= c('–200','–100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Slow group, posterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.130, 0.180))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=18))+
  theme(legend.title = element_text(size=18, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_text(hjust= -0.02), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.8, yend=-3.8, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 

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
    theme(plot.margin = unit(c(0.3,0.3,0.2,0.1), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))


# Single, high-resolution plots
png(file='Quick RT group, anterior electrodes.png', units='in', width=12, height=5, res=1200)
plot(Quick_ant)
dev.off()

png(file='Quick RT group, posterior electrodes.png', units='in', width=12, height=5, res=1200)
plot(Quick_pos)
dev.off()

png(file='Slow RT group, anterior electrodes.png', units='in', width=12, height=5, res=1200)
plot(Slow_ant)
dev.off()

png(file='Slow RT group, posterior electrodes.png', units='in', width=12, height=5, res=1200)
plot(Slow_pos)
dev.off()

##################################################################################################


# 2x2 plots combi for poster showing reduced baseline, 50 ms

# COLOUR PLOTS
# Subset: Quick group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], location == 'anterior')

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

Quick_ant <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) + 
  annotation_raster(iconQuickAnt, ymin = -0.95,ymax= -4.01,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Quick group, anterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.122, 0.190))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.8, yend=-3.8, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	legend.position='none')+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -26, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -26, y = -3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_color_manual(name = ' Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.1,0.4,0.6,0.2), 'cm'))



# Subset: Slow group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], location == 'anterior')

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

Slow_ant <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) + 
  annotation_raster(iconSlowAnt, ymin = -0.95,ymax= -4.05,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Slow group, anterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.115, 0.190))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.8, yend=-3.8, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	legend.position='none')+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -26, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -26, y = -3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_color_manual(name = ' Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.1,0.2,0.6,0.2), 'cm'))



# Subset: Quick group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], location == 'posterior')

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

Quick_pos <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  annotation_raster(iconQuickPos, ymin = 0.95,ymax= 4.05,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Quick group, posterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.115, 0.180))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.6,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.8, yend=-3.8, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -26, y = 3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_color_manual(name = ' Context / Target trial', values=colours) +
    theme(plot.margin = unit(c(0.1,0.4,0.1,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))




# Subset: Slow group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], location == 'posterior')

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

Slow_pos <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) + 
  annotation_raster(iconSlowPos, ymin = 0.95,ymax= 4.05,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Slow group, posterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.115, 0.190))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.8, yend=-3.8, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	legend.position='none')+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -26, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -26, y = -3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_color_manual(name = ' Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.1,0.2,0.1,0.2), 'cm'))

png(file='plotforposter.png', units='in', width=22, height=10, res=1200)
ggplot2.multiplot(Quick_ant, Slow_ant, Quick_pos, Slow_pos, cols = 2)
dev.off()
# Warnings normal: data not showing is from the earlier baseline period, -200 to -50 ms.

#######################################################################################






# 1x4 plots combi for poster and proceedings paper, showing reduced, 50 ms baseline

# COLOUR PLOTS
# Subset: Quick group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location, EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')
df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], location == 'anterior')

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

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

Quick_ant <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  annotation_raster(iconQuickAnt, ymin = -0.5,ymax= -3.8,xmin = 25,xmax = 125) +
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.08, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.08, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.68, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.08, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  annotate('segment', x=160, xend=216, y=-4.08, yend=-4.08, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.08, yend=-4.08, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.68, yend=-3.68, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.08, yend=-4.08, colour = 'grey85', size = 1.5)+
  geom_line(size=1, alpha = 1) + scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.38, 4.38), breaks=seq(-4,4,by=1), expand = c(0,0.004))+
  scale_x_continuous(limits=c(-50,809),breaks=seq(0,800,by=100), expand = c(0,0),
  labels= c('0 ms','100 ms','200','300 ms','400','500 ms','600','700 ms','800'))+
  ggtitle('Quick group, anterior electrodes') + theme_bw() + geom_vline(xintercept=0, size=.8) +
  annotate(geom='segment', y=seq(-5,5,1), yend=seq(-5,5,1), x = 0, xend=10, color='black') +
  annotate(geom='segment', y=0, yend=0, x=-50, xend=798, color='black', size=.8) +
  annotate(geom='segment', y=-4.2, yend=-4.38, x=seq(100,800,100), xend=seq(100,800,100), color='black') +
  theme( axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks.x = element_blank(), 
	plot.title= element_text(size=20, hjust = 0.5, vjust=2), axis.text.y = element_blank(),
	axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12), axis.line.x=element_blank(),
	panel.border = element_blank(), axis.title.y=element_blank(), panel.grid.major = element_blank(), 
	panel.grid.minor = element_blank(), legend.position='none', plot.margin = unit(c(0.2,0.2,0.2,0), 'cm')) +
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -26, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -26, y = -3, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition), stat='identity', alpha = 0.15, show.legend=FALSE) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
  #stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
  #stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(labels = c(' Auditory > Visual', " Haptic > Visual", " Visual > Visual"), values=colours)+
  scale_color_manual(labels = c(' Auditory > Visual', " Haptic > Visual", " Visual > Visual"), values=colours) +
  guides(color=guide_legend(override.aes = list(size=2.5)))



# Subset: Slow group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location, EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')
df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], location == 'anterior')

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

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

Slow_ant <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  annotation_raster(iconSlowAnt, ymin = -0.5,ymax= -3.8,xmin = 25,xmax = 125) +
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.08, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.08, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.68, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.08, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  annotate('segment', x=160, xend=216, y=-4.08, yend=-4.08, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.08, yend=-4.08, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.68, yend=-3.68, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.08, yend=-4.08, colour = 'grey85', size = 1.5)+
  geom_line(size=1, alpha = 1) + annotate(geom='segment', y=0, yend=0, x=-50, xend=798, color='black', size=.8) +
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.38, 4.38), breaks=seq(-4,4,by=1), expand = c(0,0.004))+
  scale_x_continuous(limits=c(-50,809),breaks=seq(0,800,by=100), expand = c(0,0),
  labels= c('0 ms','100 ms','200','300 ms','400','500 ms','600','700 ms','800'))+
  ggtitle('Slow group, anterior electrodes') + theme_bw() + geom_vline(xintercept=0, size=.8) +
  annotate(geom='segment', y=seq(-5,5,1), yend=seq(-5,5,1), x = 0, xend=10, color='black') +
  annotate(geom='segment', y=-4.2, yend=-4.38, x=seq(100,800,100), xend=seq(100,800,100), color='black') +
  theme( axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks.x = element_blank(),
	plot.title= element_text(size=20, hjust = 0.5, vjust=2), axis.text.y = element_blank(),
	axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12), axis.line.x=element_blank(),
	panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
	legend.position='none', axis.title.y=element_blank(), plot.margin = unit(c(0.6,0.2,0.2,0), 'cm')) +
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -26, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -26, y = -3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition), stat='identity', alpha = 0.15, show.legend=FALSE) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
  #stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
  #stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(labels = c(' Auditory > Visual', " Haptic > Visual", " Visual > Visual"), values=colours)+
  scale_color_manual(labels = c(' Auditory > Visual', " Haptic > Visual", " Visual > Visual"), values=colours) +
  guides(color=guide_legend(override.aes = list(size=2.5)))



# Subset: Quick group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')
df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], location == 'posterior')

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

df2$condition <- gsub('visual2visual', ' Visual > Visual', df2$condition)
df2$condition <- gsub('haptic2visual', ' Haptic > Visual', df2$condition)
df2$condition <- gsub('auditory2visual', ' Auditory > Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

Quick_pos <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  annotation_raster(iconQuickPos, ymin = .9,ymax= 4.32,xmin = 25,xmax = 125) +
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.08, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.08, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.68, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.08, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  annotate('segment', x=160, xend=216, y=-4.08, yend=-4.08, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.08, yend=-4.08, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.68, yend=-3.68, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.08, yend=-4.08, colour = 'grey85', size = 1.5)+
  geom_line(size=1, alpha = 1) + annotate(geom='segment', y=0, yend=0, x=-50, xend=798, color='black', size=.8) +
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.38, 4.38), breaks=seq(-4,4,by=1), expand = c(0,0.004))+
  scale_x_continuous(limits=c(-50,809),breaks=seq(0,800,by=100), expand = c(0,0),
  labels= c('0 ms','100 ms','200','300 ms','400','500 ms','600','700 ms','800'))+
  ggtitle('Quick group, posterior electrodes') + theme_bw() + geom_vline(xintercept=0, size=.8) +
  annotate(geom='segment', y=seq(-5,5,1), yend=seq(-5,5,1), x = 0, xend=10, color='black') +
  annotate(geom='segment', y=-4.2, yend=-4.38, x=seq(100,800,100), xend=seq(100,800,100), color='black') +
  theme( legend.position = c(0.12, 0.18), legend.background = element_rect(fill='grey95', size=0),
	legend.key.width = unit(1.45,'cm'), legend.text=element_text(size=21),
	legend.title = element_blank(), axis.title=element_blank(), axis.text=element_text(size=15),
	axis.ticks.x = element_blank(), plot.title= element_text(size=20, hjust = 0.5, vjust=2),
	axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	axis.line.x=element_blank(), panel.border = element_blank(), axis.title.y=element_blank(),
	panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	plot.margin = unit(c(0.6,0.2,0.2,0), 'cm')) +
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -26, y = 3, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition), stat='identity', alpha = 0.15, show.legend=FALSE) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
  #stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
  #stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(labels = c(' Auditory > Visual', " Haptic > Visual", " Visual > Visual"), values=colours)+
  scale_color_manual(labels = c(' Auditory > Visual', " Haptic > Visual", " Visual > Visual"), values=colours) +
  guides(color=guide_legend(override.aes = list(size=2.5)))



# Subset: Slow group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')
df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], location == 'posterior')

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

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

Slow_pos <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  annotation_raster(iconSlowPos, ymin = 1,ymax= 4.38,xmin = 25,xmax = 125) +
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.08, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.08, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.68, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.08, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  annotate('segment', x=160, xend=216, y=-4.08, yend=-4.08, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.08, yend=-4.08, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.68, yend=-3.68, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.08, yend=-4.08, colour = 'grey85', size = 1.5)+
  geom_line(size=1, alpha = 1) + annotate(geom='segment', y=0, yend=0, x=-50, xend=798, color='black', size=.8) +
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.38, 4.38), breaks=seq(-4,4,by=1), expand = c(0,0.004))+
  scale_x_continuous(limits=c(-50,809),breaks=seq(0,800,by=100), expand = c(0,0),
  labels= c('0 ms','100 ms','200','300 ms','400','500 ms','600','700 ms','800'))+
  ggtitle('Slow group, posterior electrodes') + theme_bw() + geom_vline(xintercept=0, size=.8) +
  annotate(geom='segment', y=seq(-5,5,1), yend=seq(-5,5,1), x = 0, xend=10, color='black') +
  annotate(geom='segment', y=-4.2, yend=-4.38, x=seq(100,800,100), xend=seq(100,800,100), color='black') +
  theme( axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks.x = element_blank(),
	plot.title= element_text(size=20, hjust = 0.5, vjust=2), axis.text.y = element_blank(),
	axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12), axis.line.x=element_blank(),
	panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
	legend.position='none', axis.title.y=element_blank(), plot.margin = unit(c(0.6,0.2,0,0), 'cm')) + 
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -26, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -26, y = -3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition), stat='identity', alpha = 0.15, show.legend=FALSE) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
  #stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
  #stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(labels = c(' Auditory > Visual', " Haptic > Visual", " Visual > Visual"), values=colours)+
  scale_color_manual(labels = c(' Auditory > Visual', " Haptic > Visual", " Visual > Visual"), values=colours) +
  guides(color=guide_legend(override.aes = list(size=2.5)))



png(file='Four main waveform plots stacked.png', units='in', width=12, height=17, res=500)
ggplot2.multiplot(Quick_ant, Quick_pos, Slow_ant, Slow_pos, cols = 1)
dev.off()



########################################################################################




# BLACK & WHITE PLOTS

# Subset: Quick group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], location == 'anterior')

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
colours <- c('grey37', 'grey37', 'grey37')
# shades for 95% CIs, same for all conditions

Quick_ant <- ggplot(df2, aes(x=time, y=-microvolts, linetype=condition))+ 
  annotation_raster(iconQuickAnt, ymin = -0.95,ymax= -4,xmin = 20,xmax = 120) + 
  geom_rect(xmin=160, xmax=216, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-4, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1, color='black')+
  scale_linetype_manual(values=c('dashed','dotted','solid'))+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.005))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Quick group, anterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.122, 0.190))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-4, yend=-4, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	legend.position='none')+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -31, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -31, y = -3, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
 geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1, color='black')+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_linetype_manual(name = ' Context / Target trial', values=c('dashed','dotted','solid'))+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.2,0.2,0.3,0.1), 'cm'))



# Subset: Slow group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], location == 'anterior')

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
colours <- c('grey37', 'grey37', 'grey37')
# shades for 95% CIs, same for all conditions

Slow_ant <- ggplot(df2, aes(x=time, y=-microvolts, linetype=condition)) + 
  annotation_raster(iconSlowAnt, ymin = -0.95,ymax= -4,xmin = 20,xmax = 120) + 
  geom_rect(xmin=160, xmax=216, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-4, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1, color='black')+
  scale_linetype_manual(values=c('dashed','dotted','solid'))+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.005))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Slow group, anterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.115, 0.190))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-4, yend=-4, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	legend.position='none')+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -31, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -31, y = -3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1, color='black')+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_linetype_manual(name = ' Context / Target trial', values=c('dashed','dotted','solid'))+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.6,0.2,0.3,0.1), 'cm'))



# Subset: Quick group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$RT.based_Groups=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$RT.based_Groups=='Quick',], location == 'posterior')

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
colours <- c('grey37', 'grey37', 'grey37')
# shades for 95% CIs, same for all conditions

Quick_pos <- ggplot(df2, aes(x=time, y=-microvolts, linetype=condition)) +
  annotation_raster(iconQuickPos, ymin = 0.95,ymax= 4,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-4, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1, color='black')+
  scale_linetype_manual(values=c('dashed','dotted','solid'))+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.005))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Quick group, posterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.115, 0.180))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-4, yend=-4, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -31, y = 3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1, color='black')+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_linetype_manual(name = ' Context / Target trial', values=c('dashed','dotted','solid'))+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.6,0.2,0.3,0.1), 'cm'))




# Subset: Slow group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$RT.based_Groups=='Slow',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$RT.based_Groups=='Slow',], location == 'posterior')

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
colours <- c('grey37', 'grey37', 'grey37')
# shades for 95% CIs, same for all conditions

Slow_pos <- ggplot(df2, aes(x=time, y=-microvolts, linetype=condition)) + 
  annotation_raster(iconSlowPos, ymin = 0.95,ymax= 4,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-4, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1, color='black')+
  scale_linetype_manual(values=c('dashed','dotted','solid'))+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.005))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Slow group, posterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.115, 0.190))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-4, yend=-4, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	legend.position='none')+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -31, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -31, y = -3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1, color='black')+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_linetype_manual(name = ' Context / Target trial', values=c('dashed','dotted','solid'))+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.6,0.2,0.2,0.1), 'cm'))

png(file='Proceeds.png', units='in', width=12, height=18, res=500)
ggplot2.multiplot(Quick_ant, Quick_pos, Slow_ant, Slow_pos, cols = 1)
dev.off()
warnings() 
# Normal warnings: data not showing is from the earlier baseline period, -200 to -50 ms.

##########################################################################################






###########################################################

# PLOTS WITH THE ORIGINAL EXPERIMENTAL GROUPS (Null group not plotted due to having only 4 valid participants)

# # # # # # # # # # # # # #

# Plots with full baseline, 200 ms

# In colour
# Subset: Quick group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$group=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$group=='Quick',], location == 'anterior')

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

Quick_ant <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  annotation_raster(iconQuickAnt, ymin = -0.95,ymax= -4,xmin = -165,xmax = -42) + 
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004), 
  labels= c('','+3 µV','','',' 0 µV','','','–3 µV',''))+
  scale_x_continuous(limits=c(-200,800),breaks=seq(-200,800,by=100), expand = c(0.005,0), 
  labels= c('–200','–100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Quick group, anterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.130, 0.890))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=18))+
  theme(legend.title = element_text(size=18, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_text(hjust= -0.02), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.8, yend=-3.8, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 

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
    theme(plot.margin = unit(c(0.3,0.3,0.2,0.1), 'cm'))+ 
  guides(color=guide_legend(override.aes = list(size=2.5)))



# Subset: Selfpaced group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$group=='Selfpaced',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$group=='Selfpaced',], location == 'anterior')

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

Self_ant <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  annotation_raster(iconSlowAnt, ymin = -0.85,ymax= -4,xmin = -165,xmax = -42) + 
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004), 
  labels= c('','+3 µV','','',' 0 µV','','','–3 µV',''))+
  scale_x_continuous(limits=c(-200,800),breaks=seq(-200,800,by=100), expand = c(0.005,0), 
  labels= c('–200','–100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Self-paced group, anterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.130, 0.890))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=18))+
  theme(legend.title = element_text(size=18, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_text(hjust= -0.02), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.8, yend=-3.8, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 

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
    theme(plot.margin = unit(c(0.3,0.3,0.2,0.1), 'cm'))+ 
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
  guides(color=guide_legend(override.aes = list(size=2.5)))



# Subset: Quick group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$group=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$group=='Quick',], location == 'posterior')

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

Quick_pos <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  annotation_raster(iconQuickPos, ymin = 0.85,ymax= 4.10,xmin = -165,xmax = -42)+ 
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004), 
  labels= c('','+3 µV','','',' 0 µV','','','–3 µV',''))+
  scale_x_continuous(limits=c(-200,800),breaks=seq(-200,800,by=100), expand = c(0.005,0), 
  labels= c('–200','–100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Quick group, posterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.130, 0.180))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=18))+
  theme(legend.title = element_text(size=18, face='bold'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_text(hjust= -0.02), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.8, yend=-3.8, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = 'Context / Target trial', values=colours)+
  scale_color_manual(name = 'Context / Target trial', values=colours) +
    theme(plot.margin = unit(c(0.3,0.3,0.2,0.1), 'cm'))+ 
  guides(color=guide_legend(override.aes = list(size=2.5)))+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) 




# Subset: Selfpaced group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$group=='Selfpaced',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$group=='Selfpaced',], location == 'posterior')

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

Self_pos <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  annotation_raster(iconSlowPos, ymin = 0.95,ymax= 4.10,xmin = -165,xmax = -42)+ 
  geom_rect(xmin=160, xmax=216, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-3.8, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=3.89, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004), 
  labels= c('','+3 µV','','',' 0 µV','','','–3 µV',''))+
  scale_x_continuous(limits=c(-200,800),breaks=seq(-200,800,by=100), expand = c(0.005,0), 
  labels= c('–200','–100','0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Self-paced group, posterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.130, 0.180))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=18))+
  theme(legend.title = element_text(size=18, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_text(hjust= -0.02), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-3.8, yend=-3.8, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 

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
    theme(plot.margin = unit(c(0.3,0.3,0.2,0.1), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))


# Single, high-resolution plots
png(file='Quick group, anterior electrodes.png', units='in', width=12, height=5, res=1200)
plot(Quick_ant)
dev.off()

png(file='Quick group, posterior electrodes.png', units='in', width=12, height=5, res=1200)
plot(Quick_pos)
dev.off()

png(file='Self-paced group, anterior electrodes.png', units='in', width=12, height=5, res=1200)
plot(Self_ant)
dev.off()

png(file='Self-paced group, posterior electrodes.png', units='in', width=12, height=5, res=1200)
plot(Self_pos)
dev.off()

##################################################################################################



# Plots combi for proceedings paper, in colour, mid resolution

# Plots showing reduced baseline, 50 ms

# COLOUR PLOTS
# Subset: Quick group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$group=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$group=='Quick',], location == 'anterior')

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

Quick_ant <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) + 
  annotation_raster(iconQuickAnt, ymin = -0.95,ymax= -4.01,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-4, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Quick group, anterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.122, 0.190))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-4, yend=-4, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	legend.position='none')+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -26, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -26, y = -3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_color_manual(name = ' Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.3,0.1,0.2,0.2), 'cm'))



# Subset: Selfpaced group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$group=='Selfpaced',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$group=='Selfpaced',], location == 'anterior')

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

Self_ant <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) + 
  annotation_raster(iconSlowAnt, ymin = -0.95,ymax= -4.05,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-4, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Self-paced group, anterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.115, 0.190))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-4, yend=-4, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	legend.position='none')+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -26, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -26, y = -3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_color_manual(name = ' Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.3,0.1,0.2,0.2), 'cm'))



# Subset: Quick group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$group=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$group=='Quick',], location == 'posterior')

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

Quick_pos <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  annotation_raster(iconQuickPos, ymin = 0.95,ymax= 4.05,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-4, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Quick group, posterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.122, 0.180))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(1.6,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-4, yend=-4, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -26, y = 3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_color_manual(name = ' Context / Target trial', values=colours) +
    theme(plot.margin = unit(c(0.6,0.1,0.2,0.2), 'cm'))+
  guides(color=guide_legend(override.aes = list(size=2.5)))




# Subset: Selfpaced group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$group=='Selfpaced',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$group=='Selfpaced',], location == 'posterior')

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

Self_pos <- ggplot(df2, aes(x=time, y=-microvolts, color=condition)) + 
  annotation_raster(iconSlowPos, ymin = 0.95,ymax= 4.05,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-4, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1)+
  scale_linetype_manual(values=colours)+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.004))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Self-paced group, posterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.115, 0.190))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-4, yend=-4, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	legend.position='none')+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -26, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -26, y = -3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1)+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_color_manual(name = ' Context / Target trial', values=colours)+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.6,0.1,0.2,0.2), 'cm'))

png(file='originalgroups.png', units='in', width=22, height=10, res=500)
ggplot2.multiplot(Quick_ant, Self_ant, Quick_pos, Self_pos, cols = 2)
dev.off()


############################################################################################
############################################################################################



# BLACK & WHITE PLOTS

# Subset: Quick group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$group=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$group=='Quick',], location == 'anterior')

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
colours <- c('grey37', 'grey37', 'grey37')
# shades for 95% CIs, same for all conditions

Quick_ant <- ggplot(df2, aes(x=time, y=-microvolts, linetype=condition)) + 
  annotation_raster(iconQuickAnt, ymin = -0.95,ymax= -4,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-4, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1, color='black')+
  scale_linetype_manual(values=c('dashed','dotted','solid'))+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.005))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Quick group, anterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.122, 0.190))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-4, yend=-4, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	legend.position='none')+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -31, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -31, y = -3, size = 5, color = 'grey32', family='sans')+

# Confidence interval shading:
 geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1, color='black')+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_linetype_manual(name = ' Context / Target trial', values=c('dashed','dotted','solid'))+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.2,0.2,0.3,0.1), 'cm'))



# Subset: Selfpaced group, anterior location
dfelectrode <- aggregate(microvolts ~ time*condition*location,
EEG[EEG$group=='Selfpaced',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'anterior')

df3 <- filter(EEG[EEG$group=='Selfpaced',], location == 'anterior')

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
colours <- c('grey37', 'grey37', 'grey37')
# shades for 95% CIs, same for all conditions

Self_ant <- ggplot(df2, aes(x=time, y=-microvolts, linetype=condition)) + 
  annotation_raster(iconSlowAnt, ymin = -0.95,ymax= -4,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-4, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1, color='black')+
  scale_linetype_manual(values=c('dashed','dotted','solid'))+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.005))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Self-paced group, anterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.115, 0.190))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-4, yend=-4, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	legend.position='none')+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -31, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -31, y = -3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1, color='black')+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_linetype_manual(name = ' Context / Target trial', values=c('dashed','dotted','solid'))+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.6,0.2,0.3,0.1), 'cm'))



# Subset: Quick group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$group=='Quick',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$group=='Quick',], location == 'posterior')

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
colours <- c('grey37', 'grey37', 'grey37')
# shades for 95% CIs, same for all conditions

Quick_pos <- ggplot(df2, aes(x=time, y=-microvolts, linetype=condition)) +
  annotation_raster(iconQuickPos, ymin = 0.95,ymax= 4,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-4, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1, color='black')+
  scale_linetype_manual(values=c('dashed','dotted','solid'))+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.005))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Quick group, posterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.115, 0.180))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-4, yend=-4, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -31, y = 3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1, color='black')+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_linetype_manual(name = ' Context / Target trial', values=c('dashed','dotted','solid'))+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.6,0.2,0.3,0.1), 'cm'))




# Subset: Selfpaced group, posterior regions
dfelectrode <- aggregate(microvolts ~ time*condition*location, 
EEG[EEG$group=='Selfpaced',], mean)

# Select brain area
df2 <- filter(dfelectrode, location == 'posterior')

df3 <- filter(EEG[EEG$group=='Selfpaced',], location == 'posterior')

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
colours <- c('grey37', 'grey37', 'grey37')
# shades for 95% CIs, same for all conditions

Self_pos <- ggplot(df2, aes(x=time, y=-microvolts, linetype=condition)) + 
  annotation_raster(iconSlowPos, ymin = 0.95,ymax= 4,xmin = 20,xmax = 120)+ 
  geom_rect(xmin=160, xmax=216, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=270, xmax=370, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=350, xmax=550, ymin=4.29, ymax=-4, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_rect(xmin=500, xmax=750, ymin=4.09, ymax=-4.2, color = 'grey75', fill='black', alpha=0, linetype='longdash')+
  geom_line(size=1, alpha = 1, color='black')+
  scale_linetype_manual(values=c('dashed','dotted','solid'))+
  scale_y_continuous(limits=c(-4.3, 4.3), breaks=seq(-4,4,by=1), expand = c(0,0.005))+
  scale_x_continuous(limits=c(-50,800),breaks=seq(0,800,by=100), expand = c(0.005,0), 
  labels= c('0 ms','100','200','300 ms','400','500','600 ms','700','800'))+
  ggtitle('Self-paced group, posterior electrodes')+
  theme_bw() + geom_vline(xintercept=0) +
  theme(axis.title=element_blank(), axis.text=element_text(size=15), axis.ticks = element_line(size = 2.5, color='grey70'))+
  annotate(geom='point', y=seq(-5,5,1), x = 0, xmin=-10, xmax=10, color='grey70') +
  geom_segment(x = -50, y = 0, xend = 800, yend = 0, size=0.5, color='black') + theme(legend.position = c(0.115, 0.190))+
  theme(legend.background = element_rect(fill='#EEEEEE', size=0))+
  theme(legend.key.width = unit(2,'cm'), legend.text=element_text(size=20))+
  theme(legend.title = element_text(size=20, face='bold'))+
  theme(plot.title= element_text(size=20, hjust = 0.5, vjust=2))+
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(), axis.text.x = element_text(vjust= 2.12),
	  panel.border = element_blank())+
  annotate('segment', x=160, xend=216, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=270, xend=370, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  annotate('segment', x=350, xend=550, y=-4, yend=-4, colour = 'grey85', size = 1.5)+
  annotate('segment', x=500, xend=750, y=-4.2, yend=-4.2, colour = 'grey85', size = 1.5)+
  theme(axis.title.y=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	legend.position='none')+
# Print y axis labels within plot area:
  annotate('text', label = '–3 µV', x = -31, y = 3, size = 5, color = 'grey32', family='sans')+
  annotate('text', label = '+3 µV', x = -31, y = -3, size = 5, color = 'grey32', family='sans')+ 

# Confidence interval shading:
geom_smooth(aes(ymin = -CIL, ymax = -CIU, fill=condition),
stat='identity', alpha = 0.15) + #95% CIs
  geom_line(size=1, alpha = 1, color='black')+
  #geom_smooth(aes(ymin = SElower, ymax = SEupper, fill=condition),
#stat='identity', alpha = 0.15) + #SEs
  #geom_smooth(aes(ymin = SDlower, ymax = SDupper, fill=condition),
#stat='identity', alpha = 0.15) + #SDs
  scale_fill_manual(name = ' Context / Target trial', values=colours)+
  scale_linetype_manual(name = ' Context / Target trial', values=c('dashed','dotted','solid'))+
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
    theme(plot.margin = unit(c(0.6,0.2,0.2,0.1), 'cm'))

# lower resolution for Word
png(file='Proceeds_longer.png', units='in', width=12, height=21, res=500)
ggplot2.multiplot(Quick_ant, Quick_pos, Self_ant, Self_pos, cols = 1)
dev.off()
warnings()

png(file='Proceeds.png', units='in', width=12, height=18, res=500)
ggplot2.multiplot(Quick_ant, Quick_pos, Self_ant, Self_pos, cols = 1)
dev.off()
warnings() 
# Normal warnings: data not showing is from the earlier baseline period, -200 to -50 ms.

#####################################################################################




# Plots on the various interactions


# Two-way interaction: Switch effect by time window and anterior vs posterior brain areas

# Read in the results of the LMEs
results_table = read.csv('Switch effect by time window and anterior vs posterior brain areas.csv')

# Qualify variable names

results_table$Brain.area = gsub('anterior', ' Anterior brain area', results_table$Brain.area )
results_table$Brain.area = gsub('posterior', ' Posterior brain area', results_table$Brain.area )

results_table$Time.window = gsub('Window 1', '160–216 ms', results_table$Time.window)
results_table$Time.window = gsub('Window 2', '270–370 ms', results_table$Time.window)
results_table$Time.window = gsub('Window 3', '350–550 ms', results_table$Time.window)
results_table$Time.window = gsub('Window 4', '500–750 ms', results_table$Time.window)

saveplot =
ggplot(results_table, aes(Time.window, t, fill=Brain.area)) +
  ylab(expression(paste(italic('t'), '-value for switch minus non-switch conditions'))) +
  scale_fill_grey(start = 0, end = .6) +
  geom_bar(width=.72, stat = "identity", position = position_dodge(width = .73)) +
  scale_y_continuous(expand = c(.015,0)) + scale_x_discrete(expand = c(.06,.1)) +
  theme( axis.title.x=element_blank(), axis.text.x = element_text(size = 17, face="bold", margin=margin(t=1)),
	axis.title.y = element_text(size=19, face="bold", margin = margin(0, 4, 0, 0)),
	axis.text.y = element_text(size = 13, margin=margin(0, 0, 0, 2)), legend.title = element_blank(),
	legend.text = element_text(size=17), legend.background = element_rect(fill="white", size=0.1, linetype="solid"),
	axis.ticks.y=element_blank(), axis.ticks.x=element_blank(), legend.key.size = unit(1.7,"line"),
	legend.position = c(.25, .18), panel.background = element_blank(), plot.title = element_blank(),
	legend.margin=margin(c(-.05,.14,.11,.1), unit='cm'), panel.grid.major.y = element_line(colour = "grey94"),
	panel.grid.major.x = element_blank(), plot.margin = unit(c(.008, .225, .02, .1), 'cm'),
	strip.text.x = element_text(face='bold', size=15, margin = margin(.27, .08, .2, 0, "cm")),
	strip.text.y = element_text(face='bold', size=15, margin = margin(0, .2, 0, .2, "cm")),
	strip.background = element_rect(colour="grey48", fill='grey95'), panel.spacing = unit(0, "lines")) +
  geom_text(aes(y=t, label=p.asterisks), vjust=-.01, colour= 'white', size=15, hjust=.49,
  	position = position_dodge(width = .76) )

# Save
png(file =
 "Switch effect by time window and anterior vs posterior brain areas.png", units="in", width=6.9, height=6, res=2000)
plot(saveplot)
# Warning message normal: about bars without p-value asterisks
dev.off()

#####################################################################################




# Three-way interaction: Switch effect by group, time window, and anterior vs posterior brain areas

# Read in the results of the LMEs
results_table = read.csv('Switch effect by group, time window, and anterior vs posterior brain areas.csv')

# Qualify variable names

results_table$Group = gsub('Slow', ' Slow group', results_table$Group)
results_table$Group = gsub('Quick', ' Quick group', results_table$Group)

results_table$Brain.area = gsub('anterior', 'Anterior brain area', results_table$Brain.area )
results_table$Brain.area = gsub('posterior', 'Posterior brain area', results_table$Brain.area )

results_table$Time.window = gsub('Window 1', '160–216 ms', results_table$Time.window)
results_table$Time.window = gsub('Window 2', '270–370 ms', results_table$Time.window)
results_table$Time.window = gsub('Window 3', '350–550 ms', results_table$Time.window)
results_table$Time.window = gsub('Window 4', '500–750 ms', results_table$Time.window)

saveplot =
ggplot(results_table, aes(Time.window, t, fill=Group)) +
  ylab(expression(paste(italic('t'), '-value for switch minus non-switch conditions'))) +
  scale_y_continuous(expand = c(.015,0)) +  scale_x_discrete(expand = c(-.25,-.25)) +
  facet_grid(list('Brain.area', 'Time.window'), scales="free_x", switch="both") + scale_fill_grey(start = 0, end = .6) +
      geom_bar(width=.74, stat = "identity", position = position_dodge(width = .754)) +
  theme( axis.title.x=element_blank(), axis.text.x = element_blank(),
	axis.title.y = element_text(size=19, face="bold", margin = margin(0, 6, 0, 0)),
	axis.text.y = element_text(size = 13, margin = margin(0, -3, 0, 3)), legend.title = element_blank(),
	  legend.text = element_text(size=17),
	legend.background = element_rect(fill="white", size=0.1, linetype="solid"),
	legend.key.size = unit(1.7,"line"), axis.ticks=element_blank(), legend.position = c(.2, .595),
	panel.background = element_blank(),	plot.title = element_blank(),
	legend.margin=margin(c(-.05,.14,.11,.1), unit='cm'), panel.grid.major.y = element_line(colour = "grey94"),
	panel.grid.major.x = element_blank(), plot.margin = unit(c(.08, -.3, -.1, .1), 'cm'),
	strip.text.x = element_text(face='bold', size=15, margin = margin(.05, 0, .2, 0, "cm")),
	strip.text.y = element_text(face='bold', size=15, margin = margin(0, .2, 0, .2, "cm")),
	strip.background.y = element_rect(colour="grey85", fill='grey99'), panel.spacing = unit(0, "lines"),
	strip.background.x = element_blank(),  strip.placement = "outside") +
  geom_text(aes(x=Time.window, y=t, label=p.asterisks), vjust=-.01, colour= 'white', size=15, hjust=.51,
  	position = position_dodge(width = .774) )

# Save
png(file =
 "Switch effect by group, time window, and anterior vs posterior brain areas.png", units="in", width=6.9, height=6, res=2000)
plot(saveplot)
# Warning message normal: about bars without p-value asterisks
dev.off()

##############################################################################################################





# Switch effect by group, time window, and language vs vision brain areas

# Read in LME results
results_table = read.csv('Switch effect by group, time window, and language vs vision brain areas.csv')

# Qualify variable names

results_table$Group = gsub('Slow', ' Slow group', results_table$Group)
results_table$Group = gsub('Quick', ' Quick group', results_table$Group)

results_table$Brain.area = gsub('language', 'Language', results_table$Brain.area )
results_table$Brain.area = gsub('vision', 'Vision', results_table$Brain.area )

results_table$Time.window = gsub('Window 1', '160 - 216 ms', results_table$Time.window)
results_table$Time.window = gsub('Window 2', '270 - 370 ms', results_table$Time.window)
results_table$Time.window = gsub('Window 3', '350 - 550 ms', results_table$Time.window)
results_table$Time.window = gsub('Window 4', '500 - 750 ms', results_table$Time.window)

results_table$Time.window = as.factor(results_table$Time.window)
results_table$Brain.area = as.factor(results_table$Brain.area)
results_table$Group = as.factor(results_table$Group)

plot =
ggplot(results_table, aes(Brain.area, t, group=Group)) +
  ylab(expression(paste(italic('t'), '-value for switch minus non-switch conditions'))) +
  facet_wrap(~Time.window, nrow=1) + geom_point(size=1.9, shape = 21, fill = "black") +
  geom_line(aes(linetype= Group), stat = "identity", size=1.3, colour='grey26') + 
  scale_y_continuous(expand = c(.019,0)) + scale_x_discrete(expand = c(.28,.28)) +
  geom_abline(intercept = 0, slope = 0, size=.8) +
  theme( axis.title.x=element_blank(), axis.text.x = element_text(size = 17, face="bold", margin=margin(t=-1)),
	axis.title.y = element_text(size=19, face="bold", margin = margin(0, 3, 0, 0)),
	axis.text.y = element_text(size = 13), legend.key.width = unit(1.4,'cm'),
	legend.title = element_blank(), legend.text = element_text(size=17),
	legend.background = element_rect(fill="white", size=0.1, linetype="solid"),
	legend.key.size = unit(1.7,"line"), axis.ticks.x=element_blank(), legend.position = c(.8, .76),
	panel.background = element_blank(),	plot.title = element_blank(),
	legend.margin=margin(c(-.05,.14,.11,.1), unit='cm'), panel.grid.major.y = element_line(colour = "grey90"),
	panel.grid.major.x = element_blank(), plot.margin = unit(c(.13, .1, .16, .13), 'cm'),
	strip.text.x = element_text(face='bold', size=15, margin = margin(.2, 0, .2, 0, "cm")),
	strip.background = element_rect(colour="grey48", fill='grey95'), panel.spacing = unit(0, "lines"))

png(file = "Switch effect by group, time window, and visual vs linguistic electrode brain areas.png",
 units="in", width=12, height=6, res=2000)
plot(plot)
dev.off()

######################################################################################################################







# Switch effect by group and language vs vision brain areas over time bins (cf Louwerse & Hutchinson, 2012, Figure 3A), computed across Switch conditions

coeffs = 
read.csv('Switch effect by group and language vs vision brain areas over time bins, computed across Switch conditions.csv')

# Remove intercept for the plotting
coeffs = coeffs[!coeffs$Time.bin == 'Intercept',]

str(coeffs)

# Isolate t value from df
coeffs$t = gsub( " .*$", "", coeffs$t.and.df)
coeffs$t = as.numeric(coeffs$t)

# sort time bins
coeffs$Time.bin = as.factor(coeffs$Time.bin)
sorted_bins <- order(levels(coeffs$Time.bin))
coeffs$Time.bin = factor(coeffs$Time.bin, levels = sorted_bins)
coeffs$Time.bin = as.factor(coeffs$Time.bin)

coeffs$Group <- gsub('Quick', 'Quick group', coeffs$Group)
coeffs$Group <- gsub('Slow', 'Slow group', coeffs$Group)
coeffs$Group = as.factor(coeffs$Group)

saveplot =
ggplot(coeffs, aes(x = Time.bin, y = t)) +
    geom_point(aes(shape=Group), size=3)  +  scale_shape_manual(values = c(16, 21)) +
    scale_y_continuous(limits=c(-10.4, 10.4), breaks=seq(-10,10,by=5), expand = c(0,0)) +
    scale_x_discrete(expand = c(.015,.015)) + geom_abline(intercept = 0, slope = 0, size=.8) +
    labs(y = expression(paste(italic("t"), "-values (negative = linguistic, positive = perceptual)"))) +
    labs(x = "Time bins from 0 ms (word onset) to 800 ms")  +  theme_bw() +
    theme(strip.text = element_text(size = 12, face='bold'), legend.title.align=0.5,
	legend.position = c(0.854, 0.789), legend.title = element_blank(),
	legend.text=element_text(size=14), legend.background = element_rect(fill='white', size=0),
	axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
	axis.title.y = element_text(size=13, margin = margin(0,-.8,0,0)),
	axis.title.x = element_text(size=13, margin = margin(6.5,0,0,0)),
	panel.grid.major.y = element_line(colour='grey60', size=.01), 
	panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
	plot.margin = unit(c(0.2,0.1,0.1,0.1), 'cm'))

# Save plot
png(file =
'Switch effect by group and language vs vision brain areas over time bins (cf Louwerse & Hutchinson, 2012, Figure 3A), computed across Switch conditions.png',
units='in', width=10, height=6, res=300)
plot(saveplot)
dev.off()

######################################################################################################################






# Switch effect by group and language vs vision brain areas over time bins (cf Louwerse & Hutchinson, 2012, Figure 3A), computed on Switch effect subtraction

coeffs = 
read.csv('Switch effect by group and language vs vision brain areas over time bins, computed on Switch effect subtraction.csv')

# Remove intercept for the plotting
coeffs = coeffs[!coeffs$Time.bin == 'Intercept',]

str(coeffs)

# Isolate t value from df
coeffs$t = gsub( " .*$", "", coeffs$t.and.df)
coeffs$t = as.numeric(coeffs$t)

# sort time bins
coeffs$Time.bin = as.factor(coeffs$Time.bin)
sorted_bins <- order(levels(coeffs$Time.bin))
coeffs$Time.bin = factor(coeffs$Time.bin, levels = sorted_bins)
coeffs$Time.bin = as.factor(coeffs$Time.bin)

coeffs$Group <- gsub('Quick', 'Quick group', coeffs$Group)
coeffs$Group <- gsub('Slow', 'Slow group', coeffs$Group)
coeffs$Group = as.factor(coeffs$Group)

saveplot =
ggplot(coeffs, aes(x = Time.bin, y = t)) +
    geom_point(aes(shape=Group), size=3)  +  scale_shape_manual(values = c(16, 21)) +
    scale_y_continuous(limits=c(-10.4, 10.4), breaks=seq(-10,10,by=5), expand = c(0,0)) +
    scale_x_discrete(expand = c(.015,.015)) + geom_abline(intercept = 0, slope = 0, size=.8) +
    labs(y = expression(paste(italic("t"), "-values (negative = linguistic, positive = perceptual)"))) +
    labs(x = "Time bins from 0 ms (word onset) to 800 ms")  +  theme_bw() +
    theme(strip.text = element_text(size = 12, face='bold'), legend.title.align=0.5,
	legend.position = c(0.854, 0.789), legend.title = element_blank(),
	legend.text=element_text(size=14), legend.background = element_rect(fill='white', size=0),
	axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
	axis.title.y = element_text(size=13, margin = margin(0,-.8,0,0)),
	axis.title.x = element_text(size=13, margin = margin(6.5,0,0,0)),
	panel.grid.major.y = element_line(colour='grey60', size=.01), 
	panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
	plot.margin = unit(c(0.2,0.1,0.1,0.1), 'cm'))

# Save plot
png(file =
'Switch effect by group and language vs vision brain areas over time bins (cf Louwerse & Hutchinson, 2012, Figure 3A), computed on Switch effect subtraction.png',
units='in', width=10, height=6, res=300)
plot(saveplot)
dev.off()

######################################################################################################################






# Switch effect by group and language vs vision brain areas over time bins (cf Louwerse & Hutchinson, 2012, Figure 3A), computed on Visual-to-visual condition

coeffs = 
read.csv('Switch effect by group and language vs vision brain areas over time bins, computed on Visual-to-visual condition.csv')

# Remove intercept for the plotting
coeffs = coeffs[!coeffs$Time.bin == 'Intercept',]

str(coeffs)

# Isolate t value from df
coeffs$t = gsub( " .*$", "", coeffs$t.and.df)
coeffs$t = as.numeric(coeffs$t)

# sort time bins
coeffs$Time.bin = as.factor(coeffs$Time.bin)
sorted_bins <- order(levels(coeffs$Time.bin))
coeffs$Time.bin = factor(coeffs$Time.bin, levels = sorted_bins)
coeffs$Time.bin = as.factor(coeffs$Time.bin)

coeffs$Group <- gsub('Quick', 'Quick group', coeffs$Group)
coeffs$Group <- gsub('Slow', 'Slow group', coeffs$Group)
coeffs$Group = as.factor(coeffs$Group)

saveplot =
ggplot(coeffs, aes(x = Time.bin, y = t)) +
    geom_point(aes(shape=Group), size=3)  +  scale_shape_manual(values = c(16, 21)) +
    scale_y_continuous(limits=c(-10.4, 10.4), breaks=seq(-10,10,by=5), expand = c(0,0)) +
    scale_x_discrete(expand = c(.015,.015)) + geom_abline(intercept = 0, slope = 0, size=.8) +
    labs(y = expression(paste(italic("t"), "-values (negative = linguistic, positive = perceptual)"))) +
    labs(x = "Time bins from 0 ms (word onset) to 800 ms")  +  theme_bw() +
    theme(strip.text = element_text(size = 12, face='bold'), legend.title.align=0.5,
	legend.position = c(0.854, 0.789), legend.title = element_blank(),
	legend.text=element_text(size=14), legend.background = element_rect(fill='white', size=0),
	axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
	axis.title.y = element_text(size=13, margin = margin(0,-.8,0,0)),
	axis.title.x = element_text(size=13, margin = margin(6.5,0,0,0)),
	panel.grid.major.y = element_line(colour='grey60', size=.01), 
	panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
	plot.margin = unit(c(0.2,0.1,0.1,0.1), 'cm'))

# Save plot
png(file =
'Switch effect by group and language vs vision brain areas over time bins (cf Louwerse & Hutchinson, 2012, Figure 3A), computed on Visual-to-visual condition.png',
units='in', width=10, height=6, res=300)
plot(saveplot)
dev.off()

######################################################################################################################







# Waveforms by group, switch condition and language vs vision brain area over time bins

# This plot shows the waveforms by group, switch condition and language vs vision brain area over time bins.
# It serves to show that the different Switch conditions cannot influence the plot right above in any 
# considerable way, as the difference across Switch conditions does not entail differences in the polarity
# of the waveforms.

EEG_aggreg = aggregate(microvolts ~ condition*time.bins*RT.based_Groups*languageVSvision, EEG, mean)

EEG_aggreg$languageVSvision = as.factor(EEG_aggreg$languageVSvision)
EEG_aggreg = EEG_aggreg[!as.numeric(EEG_aggreg$time.bins)<0,]
EEG_aggreg$time.bins = as.factor(as.character(EEG_aggreg$time.bins))
EEG_aggreg$condition = as.factor(EEG_aggreg$condition)

# sort time bins
sorted_bins <- order(levels(EEG_aggreg$time.bins))
EEG_aggreg$time.bins = factor(EEG_aggreg$time.bins, levels = sorted_bins)

EEG_aggreg$condition <- gsub('visual2visual', 'Visual-to-visual switch', EEG_aggreg$condition)
EEG_aggreg$condition <- gsub('haptic2visual', 'Haptic-to-visual switch', EEG_aggreg$condition)
EEG_aggreg$condition <- gsub('auditory2visual', 'Auditory-to-visual switch', EEG_aggreg$condition)

EEG_aggreg$RT.based_Groups <- gsub('Quick', 'Quick group', EEG_aggreg$RT.based_Groups)
EEG_aggreg$RT.based_Groups <- gsub('Slow', 'Slow group', EEG_aggreg$RT.based_Groups)

EEG_aggreg$languageVSvision <- gsub('language', 'Language electrodes', EEG_aggreg$languageVSvision)
EEG_aggreg$languageVSvision <- gsub('vision', 'Vision electrodes', EEG_aggreg$languageVSvision)

saveplot = 
ggplot(EEG_aggreg, aes(x=time.bins, y=-microvolts, linetype=languageVSvision)) +
    geom_point(aes(shape=languageVSvision), size=3) + facet_grid(RT.based_Groups ~ condition) +
    scale_shape_manual(name='Brain area', values = c(16, 21)) + 
    scale_y_continuous(limits=c(-3.6, 2.6), breaks=seq(-3,2,by=1), expand = c(0,0),
	labels=c('+3 µV','+2 µV','+1 µV','0 µV','–1 µV','–2 µV')) +
    geom_abline(intercept = 0, slope = 0, size = .8) + theme_bw() +
    labs(x = "Time bins from 0 ms (word onset) to 800 ms")  +  theme_bw() +
    theme(strip.text = element_text(size = 12, face='bold'), legend.title.align=0.5,
	legend.position = c(0.254, 0.589), legend.title = element_blank(),
	legend.text=element_text(size=12), legend.background = element_rect(fill='#EEEEEE', size=0),
	axis.text.y = element_text(size=11), axis.text.x = element_text(size=11),
	axis.title.y = element_blank(), axis.title.x = element_text(size=12, face='bold'),
	panel.grid.major.y = element_line(colour='grey60', size=.01), panel.grid.major.x = element_blank(), 
	panel.grid.minor = element_blank(), plot.margin = unit(c(0.2,0.1,0.1,0.1), 'cm'))

# Save plot
png(file='Waveforms by group, switch condition and language vs vision brain area over time bins.png', 
	units='in', width=15, height=6, res=300)
plot(saveplot)
dev.off()

###################################################################################################################








# TOST Equivalence tests. Part of code from Lakens et al. (2018; at https://osf.io/bjvau/).

equivalence.test.results = read.csv('TOST Equivalence test results.csv')
TimeWindow.1 = equivalence.test.results[equivalence.test.results$time.window=='Window 1',]
TimeWindow.2 = equivalence.test.results[equivalence.test.results$time.window=='Window 2',]
TimeWindow.3 = equivalence.test.results[equivalence.test.results$time.window=='Window 3',]
TimeWindow.4 = equivalence.test.results[equivalence.test.results$time.window=='Window 4',]

baseplot <- ggplot(data.frame()) +
  scale_x_continuous(breaks=NULL) + coord_flip() + theme_classic(base_size = 10) + geom_hline(yintercept=0, lty=1) +
  theme(plot.title = element_text(size = rel(1), face = "bold"), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=rel(0.7), lineheight = 0.5))

nhst.size <- 0.5
tost.size <- 1.2
point.size <- 2

plot_TimeWindow.1 = baseplot +
  annotate(geom = "pointrange", x = 0.5, y = TimeWindow.1$diff, ymin = TimeWindow.1$LL_CI_TTEST,
           ymax = TimeWindow.1$UL_CI_TTEST, size = nhst.size, fatten = point.size, colour='black') +
  annotate(geom = "pointrange", x = 0.5, y = TimeWindow.1$diff, ymin = TimeWindow.1$LL_CI_TOST,
           ymax = TimeWindow.1$UL_CI_TOST, size = tost.size, fatten = point.size, colour='grey20') +
  geom_hline(yintercept=TimeWindow.1$low_eqbound, lty=2) +
  geom_hline(yintercept=TimeWindow.1$high_eqbound, lty=2) +
  labs(title = "Time Window 1: 160–216 ms", y = "") +
  ylim(c(TimeWindow.1$low_eqbound*3.8, TimeWindow.1$high_eqbound*3.8)) +
  theme(plot.title = element_text(hjust = 0.5, face='plain', size=11), axis.text.x = element_text(size=9),
        axis.title.x = element_text(size=13, face='plain'), plot.margin = unit(c(.05,.05,.2,.05), 'cm'))

plot_TimeWindow.2 = baseplot +
  annotate(geom = "pointrange", x = 0.5, y = TimeWindow.1$diff, ymin = TimeWindow.1$LL_CI_TTEST,
           ymax = TimeWindow.1$UL_CI_TTEST, size = nhst.size, fatten = point.size, colour='black') +
  annotate(geom = "pointrange", x = 0.5, y = TimeWindow.1$diff, ymin = TimeWindow.1$LL_CI_TOST,
           ymax = TimeWindow.1$UL_CI_TOST, size = tost.size, fatten = point.size, colour='grey20') +
  geom_hline(yintercept=TimeWindow.2$low_eqbound, lty=2) +
  geom_hline(yintercept=TimeWindow.2$high_eqbound, lty=2) +
  labs(title = "Time Window 2: 270–370 ms", y = "") +
  ylim(c(TimeWindow.2$low_eqbound*2.1, TimeWindow.2$high_eqbound*2.1)) +
  theme(plot.title = element_text(hjust = 0.5, face='plain', size=11), axis.text.x = element_text(size=9),
        axis.title.x = element_text(size=13, face='plain'), plot.margin = unit(c(.1,.05,.2,.05), 'cm'))

plot_TimeWindow.3 = baseplot +
  annotate(geom = "pointrange", x = 0.5, y = TimeWindow.1$diff, ymin = TimeWindow.1$LL_CI_TTEST,
           ymax = TimeWindow.1$UL_CI_TTEST, size = nhst.size, fatten = point.size, colour='black') +
  annotate(geom = "pointrange", x = 0.5, y = TimeWindow.1$diff, ymin = TimeWindow.1$LL_CI_TOST,
           ymax = TimeWindow.1$UL_CI_TOST, size = tost.size, fatten = point.size, colour='grey20') +
  geom_hline(yintercept=TimeWindow.3$low_eqbound, lty=2) +
  geom_hline(yintercept=TimeWindow.3$high_eqbound, lty=2) +
  labs(title = "Time Window 3: 350–550 ms", y = "") +
  ylim(c(TimeWindow.3$low_eqbound*1.6, TimeWindow.3$high_eqbound*1.6)) +
  theme(plot.title = element_text(hjust = 0.5, face='plain', size=11), axis.text.x = element_text(size=9),
        axis.title.x = element_text(size=13, face='plain'), plot.margin = unit(c(.1,.05,.2,.05), 'cm'))

plot_TimeWindow.4 = baseplot +
  annotate(geom = "pointrange", x = 0.5, y = TimeWindow.1$diff, ymin = TimeWindow.1$LL_CI_TTEST,
           ymax = TimeWindow.1$UL_CI_TTEST, size = nhst.size, fatten = point.size, colour='black') +
  annotate(geom = "pointrange", x = 0.5, y = TimeWindow.1$diff, ymin = TimeWindow.1$LL_CI_TOST,
           ymax = TimeWindow.1$UL_CI_TOST, size = tost.size, fatten = point.size, colour='grey20') +
  geom_hline(yintercept=TimeWindow.4$low_eqbound, lty=2) +
  geom_hline(yintercept=TimeWindow.4$high_eqbound, lty=2) +
  labs(title = "Time Window 4: 500–750 ms", y = "x = Conceptual Modality Switch effect") +
  ylim(c(TimeWindow.4$low_eqbound*1.8, TimeWindow.4$high_eqbound*1.8)) +
  theme(plot.title = element_text(hjust = 0.5, face='plain', size=11), axis.text.x = element_text(size=9),
        axis.title.x = element_text(size=12.2, face='plain', margin = margin(8, 0, 0, 0)), 
        plot.margin = unit(c(.1,.05,.05,.05), 'cm'))

# Combine plots on grid
png(file='TOST Equivalence tests.png', units='in', width=8, height=5, res=1200)
grid.arrange(plot_TimeWindow.1, plot_TimeWindow.2, plot_TimeWindow.3, plot_TimeWindow.4, nrow = 4)
dev.off()

