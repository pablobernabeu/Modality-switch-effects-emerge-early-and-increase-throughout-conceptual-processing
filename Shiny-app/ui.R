# UI

library(shiny)
library(ggplot2)

EEG.GroupAndElectrode = readRDS('EEG.GroupAndElectrode.rds')
EEG.ParticipantAndBrainArea = readRDS('EEG.ParticipantAndBrainArea.rds')
EEG.ParticipantAndElectrode = readRDS('EEG.ParticipantAndElectrode.rds')
EEG.OLDGroupAndElectrode = readRDS('EEG.OLDGroupAndElectrode.rds')


ui =

shinyUI(

   fluidPage(
    # Web favicon
    tags$head(tags$link(rel='shortcut icon', href='https://image.ibb.co/fXUwzb/favic.png')),
    # Load CSS libraries for icons
    tags$head(tags$link(rel='stylesheet', href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css')),
    tags$head(tags$link(rel='stylesheet', href='https://use.fontawesome.com/releases/v5.7.0/css/all.css', integrity='sha384-lZN37f5QGtY3VHgisS14W3ExzMWZxybE1SJSEsQp9S+oqd12jhcu+A56Ebc1zFSJ', crossorigin='anonymous')),
    tags$meta(charset='UTF-8'),
    tags$meta(name='description', content='This R Shiny visualisation dashboard presents data from a psycholinguistic ERP experiment (Bernabeu et al., 2017).'),
    tags$meta(name='keywords', content='R, Shiny, ggplot2, visualisation, data, psycholinguistics, concept, semantic processing, modality switch, embodied cognition'),
    tags$meta(name='viewport', content='width=device-width, initial-scale=1.0'),

    titlePanel( h3(HTML('<div style="padding-bottom:10px; padding-top:5px; color:black; text-align:center;"><b> ERP waveforms from experiment on Conceptual Modality Switch </b><a style="color:#3E454E; text-decoration:plain; font-weight:normal;" href="https://osf.io/97unm/wiki/home/"> (Bernabeu et al., 2017) </a></div>')),
      windowTitle = 'ERP waveforms from experiment on Conceptual Modality Switch (Bernabeu et al., 2017)'
      ),

    sidebarLayout(
	sidebarPanel(width = 2,


# Condition 1 for reactivity between tabs and sidebars

   conditionalPanel(
	condition = 'input.tabvals == 1',

	h5(a(strong('Paper, statistics, all data.'), 'Plots by group and brain area shown in paper.',
	href='https://osf.io/97unm/wiki/home/',
	class = 'glyphicon glyphicon-new-window')),

	HTML('<div style="padding-bottom:8px;"></div>'),	

	selectInput('var.Group', label = 'Group', choices = list('Quick','Slow'), selected = 'Quick'),
	h6('Quick G.: 23 participants'),
	h6('Slow G.: 23 participants'),

	HTML('<div style="padding-bottom:4px;"></div>'),
	
	selectInput('var.Electrodes.1', label = h5(strong('Electrode'), br(), '(see montage below)'),
                  choices = list('1','2','3','4','5','6','7','8','9','10',
			'11','12','13','14','15','16','17','18','19','20','21',
			'22','23','24','25','26','27','28','29','30','31','33',
			'34','35','36','37','38','39','40','41','42','43','44',
			'45','46','47','48','49','50','51','52','53','54','55',
			'56','57','58','59','60'), selected = '30' ),

	h6(HTML('<div style="font-size: 12px; font-weight: bold; font-family: "Courier New", Courier, monospace;"><i class="fab fa-r-project" aria-hidden="true" style="font-size:20px; color:#5A647B; padding-top:16px;"></i> &#8201; <b>Source code</b></div>')),
	h6(HTML('<span> &nbsp; </span>'), a('server.R', href='https://osf.io/uj8z4/'), HTML('<span> &nbsp; <b>|</b> &nbsp; </span>'), a('ui.R', href='https://osf.io/8bwcx/')),
	h6(HTML('<span> &nbsp; </span>'), a('Edit & run in RStudio', href='https://mybinder.org/v2/gh/pablobernabeu/Modality-switch-effects-emerge-early-and-increase-throughout-conceptual-processing/master?urlpath=rstudio')),
	h6(HTML('<div style="padding-top:20px; text-align:center"><a href="http://creativecommons.org/licenses/by/4.0/" rel="license"> Licence: <img style="border-width: 0;" src="https://i.creativecommons.org/l/by/4.0/80x15.png" alt="Creative Commons License" /></a></div>'))
	),


# Condition 2 for reactivity between tabs and sidebars

   conditionalPanel(
	condition = 'input.tabvals == 2',

	h5(a(strong('Paper, statistics, all data.'), 'Plots by group and brain area shown in paper.',
	     href='https://osf.io/97unm/wiki/home/',
	     class = 'glyphicon glyphicon-new-window')),
	
	HTML('<div style="padding-bottom:8px;"></div>'),	
	
	selectInput('var.Participant.1', label = 'Group, Participant',
                  choices = list('Slow, 1'=1,'Slow, 2'=2,'Quick, 3'=3,'Slow, 4'=4,'Slow, 5'=5,'Slow, 6'=6,'NA, 7 *'=7,
			'Quick, 8'=8,'Slow, 9'=9,'Quick, 10'=10,'Slow, 11'=11,'Quick, 12'=12,'Quick, 13'=13,'Slow, 14'=14,
			'Slow, 15'=15,'Slow, 16'=16,'Quick, 17'=17,'Quick, 18'=18,'Slow, 19'=19,
			'Quick, 20'=20,'Slow, 21'=21,'Slow, 22'=22,'Slow, 23'=23,'Slow, 24'=24,
			'Quick, 26'=26,'Quick, 27'=27,'Quick, 28'=28,'Slow, 29'=29,'Quick, 30'=30,'Quick, 31'=31,'Quick, 32'=32,
			'Slow, 34'=34,'Slow, 35'=35,'Quick, 36'=36,'Slow, 37'=37,'Quick, 38'=38,
			'Slow, 39'=39,'Quick, 40'=40,'Slow, 41'=41,'Slow, 42'=42,'Quick, 44'=44,'Slow, 45'=45,
			'Quick, 46'=46,'Quick, 47'=47,'Quick, 48'=48,'Quick, 49'=49,'Quick, 50'=50),
                  selected = '1'),
	h6(a('See old grouping', href='https://osf.io/57mwv/'), style='text-decoration: underline;'),
	h6('  * Ptp 7: EEG not valid.'),
	
	HTML('<div style="padding-bottom:4px;"></div>'),
	
	selectInput('var.Locations', label = h5(strong('Brain area'), br(), '(see montage below)'), 
	            choices = list('anterior', 'posterior'), selected = 'anterior'),
	
	h6(HTML('<div style="font-size: 12px; font-weight: bold; font-family: "Courier New", Courier, monospace;"><i class="fab fa-r-project" aria-hidden="true" style="font-size:20px; color:#5A647B; padding-top:16px;"></i> &#8201; <b>Source code</b></div>')),
	h6(HTML('<span> &nbsp; </span>'), a('server.R', href='https://osf.io/uj8z4/'), HTML('<span> &nbsp; <b>|</b> &nbsp; </span>'), a('ui.R', href='https://osf.io/8bwcx/')),
	h6(HTML('<span> &nbsp; </span>'), a('Edit & run in RStudio', href='https://mybinder.org/v2/gh/pablobernabeu/Modality-switch-effects-emerge-early-and-increase-throughout-conceptual-processing/master?urlpath=rstudio')),
	h6(HTML('<div style="padding-top:20px; text-align:center"><a href="http://creativecommons.org/licenses/by/4.0/" rel="license"> Licence: <img style="border-width: 0;" src="https://i.creativecommons.org/l/by/4.0/80x15.png" alt="Creative Commons License" /></a></div>'))
   ),


# Condition 3 for reactivity between tabs and sidebars

conditionalPanel(
  condition = 'input.tabvals == 3',

  h5(a(strong('Paper, statistics, all data.'), 'Plots by group and brain area shown in paper.',
       href='https://osf.io/97unm/wiki/home/',
       class = 'glyphicon glyphicon-new-window')),
  
  HTML('<div style="padding-bottom:8px;"></div>'),	
  
  selectInput('var.Participant.2', label = 'Group, Participant',
              choices = list('Slow, 1'=1,'Slow, 2'=2,'Quick, 3'=3,'Slow, 4'=4,'Slow, 5'=5,'Slow, 6'=6,'NA, 7 *'=7,
                             'Quick, 8'=8,'Slow, 9'=9,'Quick, 10'=10,'Slow, 11'=11,'Quick, 12'=12,'Quick, 13'=13,'Slow, 14'=14,
                             'Slow, 15'=15,'Slow, 16'=16,'Quick, 17'=17,'Quick, 18'=18,'Slow, 19'=19,
                             'Quick, 20'=20,'Slow, 21'=21,'Slow, 22'=22,'Slow, 23'=23,'Slow, 24'=24,
                             'Quick, 26'=26,'Quick, 27'=27,'Quick, 28'=28,'Slow, 29'=29,'Quick, 30'=30,'Quick, 31'=31,'Quick, 32'=32,
                             'Slow, 34'=34,'Slow, 35'=35,'Quick, 36'=36,'Slow, 37'=37,'Quick, 38'=38,
                             'Slow, 39'=39,'Quick, 40'=40,'Slow, 41'=41,'Slow, 42'=42,'Quick, 44'=44,'Slow, 45'=45,
                             'Quick, 46'=46,'Quick, 47'=47,'Quick, 48'=48,'Quick, 49'=49,'Quick, 50'=50),
              selected = '1'),
  h6(a('See old grouping', href='https://osf.io/57mwv/'), style='text-decoration: underline;'),
  h6('  * Ptp 7: EEG not valid.'),
  
  HTML('<div style="padding-bottom:4px;"></div>'),
  
  selectInput('var.Electrodes.2', label = h5(strong('Electrode'), br(), '(see montage below)'),
              choices = list('1','2','3','4','5','6','7','8','9','10',
                             '11','12','13','14','15','16','17','18','19','20','21',
                             '22','23','24','25','26','27','28','29','30','31','33',
                             '34','35','36','37','38','39','40','41','42','43','44',
                             '45','46','47','48','49','50','51','52','53','54','55',
                             '56','57','58','59','60'), selected = '30' ),
  h6(HTML('<div style="font-size: 12px; font-weight: bold; font-family: "Courier New", Courier, monospace;"><i class="fab fa-r-project" aria-hidden="true" style="font-size:20px; color:#5A647B; padding-top:16px;"></i> &#8201; <b>Source code</b></div>')),
  h6(HTML('<span> &nbsp; </span>'), a('server.R', href='https://osf.io/uj8z4/'), HTML('<span> &nbsp; <b>|</b> &nbsp; </span>'), a('ui.R', href='https://osf.io/8bwcx/')),
  h6(HTML('<span> &nbsp; </span>'), a('Edit & run in RStudio', href='https://mybinder.org/v2/gh/pablobernabeu/Modality-switch-effects-emerge-early-and-increase-throughout-conceptual-processing/master?urlpath=rstudio')),
  h6(HTML('<div style="padding-top:20px; text-align:center"><a href="http://creativecommons.org/licenses/by/4.0/" rel="license"> Licence: <img style="border-width: 0;" src="https://i.creativecommons.org/l/by/4.0/80x15.png" alt="Creative Commons License" /></a></div>'))
),


# Condition 4 for reactivity between tabs and sidebars

conditionalPanel(
  condition = 'input.tabvals == 4',
  
  h5(HTML('<div style="color:red; padding-bottom:5px;"> <font style="font-size:1.5em; font-weight:bold;">Original groups in this tab</font> <font style="font-size:1em;">(<a href="https://mindmodeling.org/cogsci2017/papers/0318/paper0318.pdf" style="color:blue"><i class="fas fa-external-link-alt" aria-hidden="true"></i> see paper for explanation</a>)</font></div>')),

  selectInput('var.OLDGroup', label = 'ORIGINAL Group', choices = list('Quick','Selfpaced','null'), selected = 'Quick'),
  h6('Quick G.: 21 participants'),
  h6('Self-paced G.: 21 participants'),
  h6('Null G.: 4 participants'),
  
  HTML('<div style="padding-bottom:4px;"></div>'),

  selectInput('var.Electrodes.3', label = h5(strong('Electrode'), br(), '(see montage below)'),
              choices = list('1','2','3','4','5','6','7','8','9','10',
                             '11','12','13','14','15','16','17','18','19','20','21',
                             '22','23','24','25','26','27','28','29','30','31','33',
                             '34','35','36','37','38','39','40','41','42','43','44',
                             '45','46','47','48','49','50','51','52','53','54','55',
                             '56','57','58','59','60'), selected = '30' ),
  h6(HTML('<div style="font-size: 12px; font-weight: bold; font-family: "Courier New", Courier, monospace;"><i class="fab fa-r-project" aria-hidden="true" style="font-size:20px; color:#5A647B; padding-top:16px;"></i> &#8201; <b>Source code</b></div>')),
  h6(HTML('<span> &nbsp; </span>'), a('server.R', href='https://osf.io/uj8z4/'), HTML('<span> &nbsp; <b>|</b> &nbsp; </span>'), a('ui.R', href='https://osf.io/8bwcx/')),
  h6(HTML('<span> &nbsp; </span>'), a('Edit & run in RStudio', href='https://mybinder.org/v2/gh/pablobernabeu/Modality-switch-effects-emerge-early-and-increase-throughout-conceptual-processing/master?urlpath=rstudio')),
  h6(HTML('<div style="padding-top:20px; text-align:center"><a href="http://creativecommons.org/licenses/by/4.0/" rel="license"> Licence: <img style="border-width: 0;" src="https://i.creativecommons.org/l/by/4.0/80x15.png" alt="Creative Commons License" /></a></div>'))
) ),


mainPanel(
  
  tags$style(HTML('
	    .tabbable > .nav > li > a                  		  {padding-top:1px !important; padding-bottom:0px !important; background-color:#F5F5F5; color:black;}
	    .tabbable > .nav > li > a:hover            		  {border-bottom:4px solid grey; padding-top:1px !important; padding-bottom:0px !important; background-color:#FFDDC0; color:#00064C; font-weight: bold !important;}
	    .tabbable > .nav > li[class=active] > a 		    {border-bottom:2px solid #FF9003; padding-top:1px !important; padding-bottom:0px !important; background-color:#FFEFE1; color:black; font-weight: bold !important;}
	    .tabbable > .nav > li[class=active] > a:hover	  {padding-top:1px !important; padding-bottom:0px !important; background-color:#FFE7D1; color:black; font-weight: bold !important;}
	')),

  tabsetPanel(id='tabvals',

              tabPanel(value=1, h4(HTML('<div> Group & Electrode </div>')), br(), plotOutput('plot_GroupAndElectrode'),
                       h5(a(HTML('<p style="font-size:15px;"><i class="glyphicon glyphicon-new-window" aria-hidden="true"></i> &#8201; Plots with 95% Confidence Intervals </p>'), 
                            href='https://osf.io/2tpxn/'), align = 'left'),
                       downloadLink('downloadPlot.1', HTML('<div style="font-size:15px;"><i class="fa fa-download" aria-hidden="true"></i> &#8201; Download HD plot </div>'))
              ),

              tabPanel(value=2, h4('Participant & Area'), br(), plotOutput('plot_ParticipantAndLocation'),
                       h5(a(HTML('<p style="font-size:15px;"><i class="glyphicon glyphicon-new-window" aria-hidden="true"></i> &#8201; Plots with 95% Confidence Intervals </p>'), 
                            href='https://osf.io/86ch9/'), align = 'left'),
                       downloadLink('downloadPlot.2', HTML('<div style="font-size:15px;"><i class="fa fa-download" aria-hidden="true"></i> &#8201; Download HD plot </div>'))
              ),

              tabPanel(value=3, h4('Participant & Electrode'), br(), plotOutput('plot_ParticipantAndElectrode'), br(),
                       downloadLink('downloadPlot.3', HTML('<div style="font-size:15px;"><i class="fa fa-download" aria-hidden="true"></i> &#8201; Download HD plot </div>'))
              ),

              tabPanel(value=4, h4('OLD Group & Electrode'), br(), plotOutput('plot_OLDGroupAndElectrode'),
                       h5(a(HTML('<p style="font-size:15px;"><i class="glyphicon glyphicon-new-window" aria-hidden="true"></i> &#8201; Plots with 95% Confidence Intervals </p>'), 
                            href='https://osf.io/yka4e/'), align = 'left'),
                       downloadLink('downloadPlot.4', HTML('<div style="font-size:15px;"><i class="fa fa-download" aria-hidden="true"></i> &#8201; Download HD plot </div>'))
              )),
  br(),
  
  height = 20, width = 10 ) ),

# EEG montage
h4(HTML('<div style="background-color:#FEF9FF; padding-left:30px; padding-bottom:7px; padding-top:7px; font-weight:bold;"> EEG montage </div>')),
HTML('<img src="https://preview.ibb.co/n7qiYR/EEG_montage.png" style="padding-left:30px; height:450px !important; width:900px !important;"></img>'),
br(), br(),
# Paper info
h4(strong(HTML('<div style="background-color:#FEF9FF; padding-left:30px; padding-bottom:7px; padding-top:7px;"> Paper </div>'))),
h5(HTML('<div style = "text-indent:-30px; padding-left:60px; padding-right:30px; padding-bottom:7px; font-size:15px;"> Bernabeu, P., Willems, R. M., & Louwerse, M. M. (2017). <a href="https://mindmodeling.org/cogsci2017/papers/0318/index.html">Modality switch effects emerge early and increase throughout conceptual processing: Evidence from ERPs.</a> In G. Gunzelmann, A. Howes,  T. Tenbrink, & E. J. Davelaar (Eds.), <i>Proceedings of the 39th Annual Conference of the Cognitive Science Society</i> (pp. 1629-1634). Austin, TX: Cognitive Science Society. </div>'), align = 'justify'),
h5(strong(HTML('<div style="background-color:#FDFCFF; padding-left:30px; font-size:15px; padding-bottom:6px; padding-top:6px;"> Abstract </div>'))),
h5(HTML('<div style="padding-left:30px; padding-right:30px; padding-bottom:15px; font-size:15px;"> We tested whether conceptual processing is modality-specific by tracking the time course of the Conceptual Modality Switch effect. Forty-six participants verified the relation between property words and concept words. The conceptual modality of consecutive trials was manipulated in order to produce an Auditory-to-visual switch condition, a Haptic-to-visual switch condition, and a Visual-to-visual, no-switch condition. Event-Related Potentials (ERPs) were time-locked to the onset of the first word (property) in the target trials so as to measure the effect online and to avoid a within-trial confound. A switch effect was found, characterized by more negative ERP amplitudes for modality switches than no-switches. It proved significant in four typical time windows from 160 to 750 milliseconds post word onset, with greater strength in posterior brain regions, and after 350 milliseconds. These results suggest that conceptual processing may be modality-specific in certain tasks, but also that the early stage of processing is relatively amodal. </div>'), align = 'justify'),
h4(strong(HTML('<div style="background-color:#FEF9FF; padding-left:30px; padding-bottom:7px; padding-top:7px;"> Acknowledgments </div>'))),
h5(HTML('<div style="text-align:left; padding-left:30px; padding-right:30px; padding-bottom:15px; font-size:15px;"> This experiment was hosted and co-funded by the <a href="https://www.mpi.nl/department/neurobiology-language/4"> Neurobiology of Language department </a> at the <a href="https://www.mpi.nl"> Max Planck Institute for Psycholinguistics</a>. </div>')),
h4(strong(HTML('<div style="background-color:#FEF9FF; padding-left:30px; padding-bottom:7px; padding-top:7px;"> See also: Modality norms </div>'))),
h5(HTML('<div style="text-align:left; padding-left:30px; padding-right:30px; padding-bottom:15px; font-size:15px;"><a href="https://pablobernabeu.shinyapps.io/Dutch-modality-exclusivity-norms/"><i class="glyphicon glyphicon-new-window" aria-hidden="true"></i> &#8201; Dashboard presenting the modality norms used for the stimuli of this experiment</a> (in case of website downtime, please <a href="http://rpubs.com/pcbernabeu/Dutch-modality-exclusivity-norms">visit this alternative</a>).</div>')),
br(), br(), br()
   ) )