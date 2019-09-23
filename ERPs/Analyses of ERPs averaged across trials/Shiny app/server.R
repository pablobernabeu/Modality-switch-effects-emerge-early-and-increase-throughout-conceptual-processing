# server

library(shiny)
library(ggplot2)

EEG.ParticipantAndElectrode = readRDS('EEG.ParticipantAndElectrode.rds')
EEG.ParticipantAndBrainArea = readRDS('EEG.ParticipantAndBrainArea.rds')
EEG.GroupAndElectrode = readRDS('EEG.GroupAndElectrode.rds')
EEG.OLDGroupAndElectrode = readRDS('EEG.OLDGroupAndElectrode.rds')


server =

shinyServer(

  function(input, output) {


# plot_GroupAndElectrode:
    output$plot_GroupAndElectrode <- renderPlot({

dfelectrode <- aggregate(microvolts ~ electrode*time*condition, 
EEG.GroupAndElectrode[EEG.GroupAndElectrode$RT.based_Groups==input$var.Group,], mean)

df2 <- subset(dfelectrode, electrode == input$var.Electrodes.1)

df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', 'Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', 'Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', 'Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for ', input$var.Group, ' Group and Electrode ', input$var.Electrodes.1)

plot_GroupAndElectrode = ggplot(df2, aes(x=time, y=-microvolts, color=condition)) +
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_line(size=1, alpha = 1) + scale_linetype_manual(values=colours) +
  scale_y_continuous(limits=c(-8.38, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1)) +
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), 
	expand = c(0.005,0), labels= c(expression('\u2013' * '200'),expression('\u2013' * '100 ms'),'0','100 ms','200','300 ms','400','500 ms','600','700 ms','800')) +
  ggtitle(spec_title, subtitle = '(negative values upward; time windows displayed)') + theme_bw() + geom_vline(xintercept=0) +
  annotate(geom='segment', y=seq(-8,8,1), yend=seq(-8,8,1), x=-4, xend=8, color='black') +
  annotate(geom='segment', y=-8.2, yend=-8.38, x=seq(-200,800,100), xend=seq(-200,800,100), color='black') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150), legend.background = element_rect(fill='#EEEEEE', size=0),
	axis.title=element_blank(), legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17),
	legend.title = element_text(size=17, face='bold'), plot.title = element_text(size=20, hjust = 0.5, vjust=2),
	plot.subtitle = element_text(size=16, hjust = 0.5),
	axis.text.y = element_blank(), axis.text.x = element_text(size = 14, vjust= 2.12, face='bold', color = 'grey32', family='sans'),
	axis.ticks=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), 
	panel.grid.minor = element_blank(), plot.margin = unit(c(0.5,0.1,0,0), 'cm')) +
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5) +
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5) +
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5) +
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5) +
  scale_fill_manual(name = 'Context / Target trial', values=colours) +
  scale_color_manual(name = 'Context / Target trial', values=colours) +
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
   guides(color=guide_legend(override.aes = list(size=2.5))) +
# Print y axis labels within plot area ('u03bc' = Greek micro letter):
  annotate('text', label = expression(bold('\u2013' * '3 ' * '\u03bc' * 'V')), x = -29, y = 3, size = 4.5, color = 'grey32', family='sans') +
  annotate('text', label = expression(bold('+3 ' * '\u03bc' * 'V')), x = -29, y = -3, size = 4.5, color = 'grey32', family='sans') +
  annotate('text', label = expression(bold('\u2013' * '6 ' * '\u03bc' * 'V')), x = -29, y = 6, size = 4.5, color = 'grey32', family='sans')

print(plot_GroupAndElectrode)

output$downloadPlot.1 <- downloadHandler(
	filename <- function(file){
	paste0(input$var.Group, ' Group and Electrode ', input$var.Electrodes.1, ', ', Sys.Date(), '.png')},
   	content <- function(file){
      		png(file, units='in', width=13, height=5, res=900)
      		print(plot_GroupAndElectrode)
      		dev.off()},
	contentType = 'image/png')
  } )



# plot_ParticipantAndLocation:
    output$plot_ParticipantAndLocation <- renderPlot({

dfelectrode <- aggregate(microvolts ~ location*time*condition, 
EEG.ParticipantAndBrainArea[EEG.ParticipantAndBrainArea$participant==input$var.Participant.1,], mean)

df2 <- subset(dfelectrode, location==input$var.Locations)

df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', 'Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', 'Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', 'Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant ', input$var.Participant.1, ', ', input$var.Locations, ' brain area')

plot_ParticipantAndLocation = ggplot(df2, aes(x=time, y=-microvolts, color=condition)) + 
  geom_rect(xmin=160, xmax=216, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_rect(xmin=270, xmax=370, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_rect(xmin=350, xmax=550, ymin=14, ymax=-13, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_rect(xmin=500, xmax=750, ymin=13, ymax=-14, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_line(size=1, alpha = 1) + scale_linetype_manual(values=colours) +
  scale_y_continuous(limits=c(-14.58, 14.3), breaks=seq(-14,14,by=1), expand = c(0,0.1)) +
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), 
	labels= c(expression('\u2013' * '200'),expression('\u2013' * '100 ms'),'0','100 ms','200','300 ms','400','500 ms','600','700 ms','800')) +
  ggtitle(spec_title, subtitle = '(negative values upward; time windows displayed)') + theme_bw() + geom_vline(xintercept=0) +
  annotate(geom='segment', y=seq(-14,14,1), yend=seq(-14,14,1), x=-4, xend=8, color='black') +
  annotate(geom='segment', y=-14.2, yend=-14.58, x=seq(-200,800,100), xend=seq(-200,800,100), color='black') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.130), legend.background = element_rect(fill='#EEEEEE', size=0),
	axis.title=element_blank(), legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17),
	legend.title = element_text(size=17, face='bold'), plot.title = element_text(size=20, hjust = 0.5, vjust=2),
	plot.subtitle = element_text(size=16, hjust = 0.5),
	axis.text.y = element_blank(), axis.text.x = element_text(size = 14, vjust= 2.12, face='bold', color = 'grey32', family='sans'), 
	axis.ticks=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
	plot.margin = unit(c(0.5,0.1,0,0), 'cm')) +
  annotate('segment', x=160, xend=216, y=-14, yend=-14, colour = 'grey75', size = 1.5) +
  annotate('segment', x=270, xend=370, y=-14, yend=-14, colour = 'grey75', size = 1.5) +
  annotate('segment', x=350, xend=550, y=-13, yend=-13, colour = 'grey75', size = 1.5) +
  annotate('segment', x=500, xend=750, y=-14, yend=-14, colour = 'grey75', size = 1.5) +
  scale_fill_manual(name = 'Context / Target trial', values=colours) +
  scale_color_manual(name = 'Context / Target trial', values=colours) +
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
   guides(color=guide_legend(override.aes = list(size=2.5))) +
# Print y axis labels within plot area ('u03bc' = Greek micro letter):
  annotate('text', label = expression(bold('\u2013' * '6 ' * '\u03bc' * 'V')), x = -32, y = 6, size = 4.5, color = 'grey32', family='sans') +
  annotate('text', label = expression(bold('+6 ' * '\u03bc' * 'V')), x = -32, y = -6, size = 4.5, color = 'grey32', family='sans') + 
  annotate('text', label = expression(bold('\u2013' * '12 ' * '\u03bc' * 'V')), x = -32, y = 12, size = 4.5, color = 'grey32', family='sans')

print(plot_ParticipantAndLocation)

output$downloadPlot.2 <- downloadHandler(
	filename <- function(file){
	paste0('Participant ', input$var.Participant.1, ', ', input$var.Locations, ' brain area, ', Sys.Date(), '.png')},
   	content <- function(file){
			png(file, units='in', width=13, height=5, res=900)
			print(plot_ParticipantAndLocation)
			dev.off()},
	contentType = 'image/png')
  } )    



# plot_ParticipantAndElectrode:
    output$plot_ParticipantAndElectrode <- renderPlot({

dfelectrode <- aggregate(microvolts ~ electrode*time*condition, 
EEG.ParticipantAndElectrode[EEG.ParticipantAndElectrode$participant==input$var.Participant.2,], mean)

df2 <- subset(dfelectrode, electrode == input$var.Electrodes.2)

df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', 'Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', 'Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', 'Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title = paste0('ERP waveforms for Participant ', input$var.Participant.2, ' and Electrode ', input$var.Electrodes.2)

plot_ParticipantAndElectrode = ggplot(df2, aes(x=time, y=-microvolts, color=condition)) + 
  geom_rect(xmin=160, xmax=216, ymin=17, ymax=-18, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_rect(xmin=270, xmax=370, ymin=17, ymax=-18, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_rect(xmin=350, xmax=550, ymin=18, ymax=-17, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_rect(xmin=500, xmax=750, ymin=17, ymax=-18, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_line(size=1, alpha = 1) + scale_linetype_manual(values=colours) +
  scale_y_continuous(limits=c(-18.58, 18.3), breaks=seq(-18,18,by=1), expand = c(0,0.1)) +
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), 
	labels= c(expression('\u2013' * '200'),expression('\u2013' * '100 ms'),'0','100 ms','200','300 ms','400','500 ms','600','700 ms','800')) +
  ggtitle(spec_title, subtitle = '(negative values upward; time windows displayed)') + theme_bw() + geom_vline(xintercept=0) +
  annotate(geom='segment', y=seq(-18,18,1), yend=seq(-18,18,1), x=-4, xend=8, color='black') +
  annotate(geom='segment', y=-18.2, yend=-18.58, x=seq(-200,800,100), xend=seq(-200,800,100), color='black') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.130), legend.background = element_rect(fill='#EEEEEE', size=0),
	axis.title=element_blank(), legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17),
	legend.title = element_text(size=17, face='bold'), plot.title = element_text(size=20, hjust = 0.5, vjust=2),
	plot.subtitle = element_text(size=16, hjust = 0.5),
	axis.text.y = element_blank(), axis.text.x = element_text(size = 14, vjust= 2.12, face='bold', color = 'grey32', family='sans'), 
	axis.ticks=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
	plot.margin = unit(c(0.5,0.1,0,0), 'cm')) +
  annotate('segment', x=160, xend=216, y=-18, yend=-18, colour = 'grey75', size = 1.5) +
  annotate('segment', x=270, xend=370, y=-18, yend=-18, colour = 'grey75', size = 1.5) +
  annotate('segment', x=350, xend=550, y=-17, yend=-17, colour = 'grey75', size = 1.5) +
  annotate('segment', x=500, xend=750, y=-18, yend=-18, colour = 'grey75', size = 1.5) +
  scale_fill_manual(name = 'Context / Target trial', values=colours) +
  scale_color_manual(name = 'Context / Target trial', values=colours) +
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
   guides(color=guide_legend(override.aes = list(size=2.5))) +
# Print y axis labels within plot area ('u03bc' = Greek micro letter):
  annotate('text', label = expression(bold('\u2013' * '8 ' * '\u03bc' * 'V')), x = -32, y = 8, size = 4.5, color = 'grey32', family='sans') +
  annotate('text', label = expression(bold('+8 ' * '\u03bc' * 'V')), x = -32, y = -8, size = 4.5, color = 'grey32', family='sans') + 
  annotate('text', label = expression(bold('\u2013' * '16 ' * '\u03bc' * 'V')), x = -32, y = 16, size = 4.5, color = 'grey32', family='sans')

print(plot_ParticipantAndElectrode)

output$downloadPlot.3 <- downloadHandler(
	filename <- function(file){
	paste0('Participant ', input$var.Participant.2, ' and Electrode ', input$var.Electrodes.2, ', ', Sys.Date(), '.png')},
   	content <- function(file){
      		png(file, units='in', width=13, height=5, res=900)
      		print(plot_ParticipantAndElectrode)
      		dev.off()},
	contentType = 'image/png')
  } )



# plot_OLDGroupAndElectrode:
    output$plot_OLDGroupAndElectrode <- renderPlot({

dfelectrode <- aggregate(microvolts ~ electrode*time*condition, 
EEG.OLDGroupAndElectrode[EEG.OLDGroupAndElectrode$group==input$var.OLDGroup,], mean)

df2 <- subset(dfelectrode, electrode == input$var.Electrodes.3)

df2$condition= as.factor(df2$condition)
df2$condition <- gsub('visual2visual', 'Visual / Visual', df2$condition)
df2$condition <- gsub('haptic2visual', 'Haptic / Visual', df2$condition)
df2$condition <- gsub('auditory2visual', 'Auditory / Visual', df2$condition)

df2$time <- as.integer(as.character(df2$time))
colours <- c('firebrick1', 'dodgerblue', 'forestgreen')
# green:visual2visual, blue:haptic2visual, red:auditory2visual

spec_title <- paste0('ERP waveforms for ORIGINAL ', input$var.OLDGroup, ' Group and Electrode ', input$var.Electrodes.3)

plot_OLDGroupAndElectrode = ggplot(df2, aes(x=time, y=-microvolts, color=condition)) + 
  geom_rect(xmin=160, xmax=216, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_rect(xmin=270, xmax=370, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_rect(xmin=350, xmax=550, ymin=8, ymax=-7.5, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_rect(xmin=500, xmax=750, ymin=7.5, ymax=-8, color = 'grey75', fill='black', alpha=0, linetype='longdash') +
  geom_line(size=1, alpha = 1) + scale_linetype_manual(values=colours) +
  scale_y_continuous(limits=c(-8.38, 8.3), breaks=seq(-8,8,by=1), expand = c(0,0.1)) +
  scale_x_continuous(limits=c(-208,808),breaks=seq(-200,800,by=100), expand = c(0.005,0), 
	labels= c(expression('\u2013' * '200'),expression('\u2013' * '100 ms'),'0','100 ms','200','300 ms','400','500 ms','600','700 ms','800')) +
  ggtitle(spec_title, subtitle = '(negative values upward; time windows displayed)') + theme_bw() + geom_vline(xintercept=0) +
  annotate(geom='segment', y=seq(-8,8,1), yend=seq(-8,8,1), x=-4, xend=8, color='black') +
  annotate(geom='segment', y=-8.2, yend=-8.38, x=seq(-200,800,100), xend=seq(-200,800,100), color='black') +
  geom_segment(x = -200, y = 0, xend = 800, yend = 0, size=0.5, color='black') +
  theme(legend.position = c(0.100, 0.150), legend.background = element_rect(fill='#EEEEEE', size=0),
	axis.title=element_blank(), legend.key.width = unit(1.2,'cm'), legend.text=element_text(size=17),
	legend.title = element_text(size=17, face='bold'), plot.title = element_text(size=20, hjust = 0.5, vjust=2),
	plot.subtitle = element_text(size=16, hjust = 0.5),
	axis.text.y = element_blank(), axis.text.x = element_text(size = 14, vjust= 2.12, face='bold', color = 'grey32', family='sans'),
	axis.ticks=element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	plot.margin = unit(c(0.5,0.1,0,0), 'cm')) +
  annotate('segment', x=160, xend=216, y=-8, yend=-8, colour = 'grey75', size = 1.5) +
  annotate('segment', x=270, xend=370, y=-8, yend=-8, colour = 'grey75', size = 1.5) +
  annotate('segment', x=350, xend=550, y=-7.5, yend=-7.5, colour = 'grey75', size = 1.5) +
  annotate('segment', x=500, xend=750, y=-8, yend=-8, colour = 'grey75', size = 1.5) +
  scale_fill_manual(name = 'Context / Target trial', values=colours) +
  scale_color_manual(name = 'Context / Target trial', values=colours) +
  guides(linetype=guide_legend(override.aes = list(size=1.2))) +
   guides(color=guide_legend(override.aes = list(size=2.5))) +
# Print y axis labels within plot area ('u03bc' = Greek micro letter):
  annotate('text', label = expression(bold('\u2013' * '3 ' * '\u03bc' * 'V')), x = -29, y = 3, size = 4.5, color = 'grey32', family='sans') +
  annotate('text', label = expression(bold('+3 ' * '\u03bc' * 'V')), x = -29, y = -3, size = 4.5, color = 'grey32', family='sans') +
  annotate('text', label = expression(bold('\u2013' * '6 ' * '\u03bc' * 'V')), x = -29, y = 6, size = 4.5, color = 'grey32', family='sans')

print(plot_OLDGroupAndElectrode)

output$downloadPlot.4 <- downloadHandler(
	filename <- function(file){
	paste0('Original ', input$var.OLDGroup, ' Group and Electrode ', input$var.Electrodes.3, ', ', Sys.Date(), '.png')},
   	content <- function(file){
      		png(file, units='in', width=13, height=5, res=900)
      		print(plot_OLDGroupAndElectrode)
      		dev.off()},
	contentType = 'image/png')
  } )
} )