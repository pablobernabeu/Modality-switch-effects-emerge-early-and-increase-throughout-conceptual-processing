
setwd('C:/Users/Pablo/Dropbox/STUDIES/R/Experiment Data/Modality-switching experiment/RTs and accuracy')

beh <- read.csv('beh.csv')
str(beh)

# There's a bizarre bug with NA participants. Solve:
beh = beh[!is.na(beh$Ptp),]

# Reorder Condition levels
beh$Condition <- factor(beh$Condition, levels = c('visual2visual', 
'haptic2visual', 'auditory2visual'))



########   IMPORTANT   ########
# Remove participants 7 and 33, one due to too noisy ERPs, the other due to accuracy > 50% #
beh = beh[!beh$Ptp=='7' & !beh$Ptp=='33',]
str(beh)	# 9936 observations = 46 participants * 216 trials
#################################



# Couple context and target trials into the same rows
context_trials = beh[beh$Position=='context',]
target_trials = beh[beh$Position=='target',]

names(context_trials)
names(target_trials)


names(context_trials)[1] = "participant"                    
names(context_trials)[2] = "group"                  
names(context_trials)[3] = "Gender"                 
names(context_trials)[4] = "Lefthanded"             
names(context_trials)[5] = "A_V_OK"                 
names(context_trials)[6] = "Event"                  
names(context_trials)[7] = "CONTEXT_Trial"                  
names(context_trials)[8] = "CONTEXT_Item"                   
names(context_trials)[9] = "condition"              
names(context_trials)[10] = "CONTEXT_Accuracy"               
names(context_trials)[11] = "CONTEXT_RT"                     
names(context_trials)[12] = "CONTEXT_Position"               
names(context_trials)[13] = "CONTEXT_cat"                    
names(context_trials)[14] = "CONTEXT_catspec"                
names(context_trials)[15] = "CONTEXT_solution"               
names(context_trials)[16] = "CONTEXT_property"               
names(context_trials)[17] = "CONTEXT_concept"                
names(context_trials)[18] = "CONTEXT_LSA_distance"           
names(context_trials)[19] = "CONTEXT_mean_perceptualstrength"
names(context_trials)[20] = "CONTEXT_mean_concreteness"      
names(context_trials)[21] = "CONTEXT_mean_exc"               
names(context_trials)[22] = "CONTEXT_prop_concreteness"      
names(context_trials)[23] = "CONTEXT_prop_exc"               
names(context_trials)[24] = "CONTEXT_prop_lg10CD"            
names(context_trials)[25] = "CONTEXT_prop_orthneigh"         
names(context_trials)[26] = "CONTEXT_prop_letters"           
names(context_trials)[27] = "CONTEXT_conc_concreteness"      
names(context_trials)[28] = "CONTEXT_conc_exc"               
names(context_trials)[29] = "CONTEXT_conc_lg10CD"            
names(context_trials)[30] = "CONTEXT_conc_letters"           
names(context_trials)[31] = "CONTEXT_mean_lg10CD"            
names(context_trials)[32] = "CONTEXT_conc_orthneigh"         


names(target_trials)[1] = "participant"                    
names(target_trials)[2] = "group"                  
names(target_trials)[3] = "Gender"                 
names(target_trials)[4] = "Lefthanded"             
names(target_trials)[5] = "A_V_OK"                 
names(target_trials)[6] = "Event"                  
names(target_trials)[7] = "TARGET_Trial"                  
names(target_trials)[8] = "TARGET_Item"                   
names(target_trials)[9] = "condition"              
names(target_trials)[10] = "TARGET_Accuracy"               
names(target_trials)[11] = "TARGET_RT"                     
names(target_trials)[12] = "TARGET_Position"               
names(target_trials)[13] = "TARGET_cat"                    
names(target_trials)[14] = "TARGET_catspec"                
names(target_trials)[15] = "TARGET_solution"               
names(target_trials)[16] = "TARGET_property"               
names(target_trials)[17] = "TARGET_concept"                
names(target_trials)[18] = "TARGET_LSA_distance"           
names(target_trials)[19] = "TARGET_mean_perceptualstrength"
names(target_trials)[20] = "TARGET_mean_concreteness"      
names(target_trials)[21] = "TARGET_mean_exc"               
names(target_trials)[22] = "TARGET_prop_concreteness"      
names(target_trials)[23] = "TARGET_prop_exc"               
names(target_trials)[24] = "TARGET_prop_lg10CD"            
names(target_trials)[25] = "TARGET_prop_orthneigh"         
names(target_trials)[26] = "TARGET_prop_letters"           
names(target_trials)[27] = "TARGET_conc_concreteness"      
names(target_trials)[28] = "TARGET_conc_exc"               
names(target_trials)[29] = "TARGET_conc_lg10CD"            
names(target_trials)[30] = "TARGET_conc_letters"           
names(target_trials)[31] = "TARGET_mean_lg10CD"            
names(target_trials)[32] = "TARGET_conc_orthneigh"         


head(context_trials)

Stimulus_per_participant_and_trial = merge(context_trials, target_trials, by = c('participant', 
'group', 'Lefthanded', 'Gender', 'A_V_OK', 'Event', 'condition'), all=T)
head(Stimulus_per_participant_and_trial)
str(Stimulus_per_participant_and_trial)	# 4968 obs = 46 participants * 108 events (context-target trial pairs)
write.csv(Stimulus_per_participant_and_trial, 'Stimulus per participant and trial.csv')


# Files for co-occurrence calculations
successions = Stimulus_per_participant_and_trial[,c('participant', 'Event', 'CONTEXT_property', 'CONTEXT_concept', 'TARGET_property')]
participantandevent = Stimulus_per_participant_and_trial[,c('participant', 'Event')]
pasted = paste(Stimulus_per_participant_and_trial$CONTEXT_property, Stimulus_per_participant_and_trial$TARGET_property)
property_property = cbind(participantandevent, pasted)
names(property_property)[3] = 'CONTEXT.property_TARGET.property'


write.csv(successions, 'successions.csv')
write.csv(property_property, 'property_property.csv')