setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Export Files')


# Mean differences with 95% Confidence Intervals (code and output notebook)

install.packages('tinytex')
library(tinytex)
tinytex::install_tinytex()
tinytex:::is_tinytex()	# Should be TRUE

install.packages('rmarkdown')
library(markdown)

# Run code below. If library error, try running again.
rmarkdown::render('General statistical analysis.R', 'word_document', 
			output_file='General statistical analysis.docx')

# Rendering takes a few minutes. At the end, find file in working directory.

# In the Word document, it is recommended to widen the horizontal margins.

rmarkdown::render('Mean differences with 95% Confidence Intervals.R', 'word_document', output_file='Mean differences with 95% Confidence Intervals.docx', quiet=T)





# General statistical analysis (RMarkdown report)

install.packages('tinytex')
library(tinytex)
tinytex::install_tinytex()
tinytex:::is_tinytex()	# Should be TRUE

install.packages('knitr')
library(knitr)

install.packages('rmarkdown')
library(rmarkdown)

rmarkdown::render('RMarkdown_report.Rmd', c(pdf_document, 'html_document'))