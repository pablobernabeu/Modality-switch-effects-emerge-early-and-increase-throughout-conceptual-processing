setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/ERPs/Analyses of ERPs averaged across trials/Shiny app')

# Coding for an interactive Shiny plotting online. General method: ui.R and server.R as separate scripts.

install.packages("curl")
install.packages('leaflet')
install.packages('RCurl')
install.packages('shiny')
install.packages('scales')
install.packages('rsconnect')
install.packages('ggplot2')
install.packages('Rcpp')
install.packages('colorspace')
install.packages('dplyr')
install.packages('rlang')

library(curl)
library(leaflet)
library(RCurl)
library(Rcpp)
library(shiny)
library(scales)
library(rsconnect)
library(ggplot2)
library(colorspace)
library(dplyr)
library(rlang)


# Load data, cropped as far as possible
EEG.ParticipantAndElectrode = readRDS('EEG.ParticipantAndElectrode.rds')
EEG.ParticipantAndBrainArea = readRDS('EEG.ParticipantAndBrainArea.rds')
EEG.GroupAndElectrode = readRDS('EEG.GroupAndElectrode.rds')
EEG.OLDGroupAndElectrode = readRDS('EEG.OLDGroupAndElectrode.rds')

# Remove letter C before electrode names to avoid confusion with 10-20 montage (done)
# EEG.GroupAndElectrode$electrode = substring(EEG.GroupAndElectrode$electrode, 2)
# EEG.ParticipantAndElectrode$electrode = substring(EEG.ParticipantAndElectrode$electrode, 2)
# EEG.OLDGroupAndElectrode$electrode = substring(EEG.OLDGroupAndElectrode$electrode, 2)

# saveRDS(EEG.ParticipantAndElectrode, 'EEG.ParticipantAndElectrode.rds', compress='xz')
# saveRDS(EEG.ParticipantAndBrainArea, 'EEG.ParticipantAndBrainArea.rds', compress='xz')
# saveRDS(EEG.GroupAndElectrode, 'EEG.GroupAndElectrode.rds', compress='xz')
# saveRDS(EEG.OLDGroupAndElectrode, 'EEG.OLDGroupAndElectrode.rds', compress='xz')

# Done and saved.


# Local launch
shinyApp(ui, server)

# Web launch
deployApp()

# Launch log
showLogs(appPath = getwd(), appFile = NULL, appName = NULL, account = NULL, entries = 100, streaming = FALSE)

