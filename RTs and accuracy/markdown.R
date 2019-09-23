setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/RTs and accuracy')

# This script is for the creation of Word and PDF documents via RMarkdown.
# Pandoc and Miktex were previously installed. Where this is a good way to 
# automatically create a readable version of the code with results, the
# purpose is not to create a nice-looking, edited document.

install.packages('rmarkdown')
library(markdown)

# Run code below. If library error, just try running again
rmarkdown::render("RT models.R", "word_document")

# rendering takes a few minutes
# at the end, find file in working directory

# In the Word document, it is recommended to widen the horizontal margins.