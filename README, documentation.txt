
METHOD

First, we wish to note that in some of the raw folders (experiment set-up, EEG raw files, and RT logfiles), the Conceptual Modality Switch 
conditions (e.g., Visual-to-visual) are named on the basis of research by Louwerse and Connell (2011, Cognitive Science). The key is:

TotalMatch = Visual to visual switch
EmbodiedMismatch = Haptic to visual switch
TotalMismatch = Auditory to visual switch

The stimuli were broadly randomized within participant. The exact stimuli presented to each participant in each trial are available in 
the main folder, in the table 'Stimulus per participant and trial' (corresponding R code in folder 'RTs and accuracy').

The ERP data were preprocessed in Brain Vision Analyzer 2, averaged per switch condition. These data are stored in the folder 'Averaged-trials Export Files'.
Statistics were performed on these grand-averaged data. The main analyses are stored in the folder 'Analyses of ERPs averaged across trials'.

Trial-by-trial ERPs are also available in the corresponding folder.

The analysis of ERPs is based on N = 46, whereas the analysis of the RTs has N = 47, because one participant had too noisy ERPs but valid RTs.

The data set contains participant groups of different kinds. The column 'Group' is for the original groups that were distinguished during the 
experiment (Quick, Self-paced and null). Because eventually those groups hardly differed in terms of response time, we recalculated the groups
by pooling together all participants and then splitting into two equal-sized groups on the basis of each participant's average RT. This new 
group factor, the final one, is called 'RT.based_groups'.

The data set includes a folder with the results obtained based on the original participant groups, simply for the sake of information. The 
results with the old groups and with the new RT-based groups were highly similar--both plot-wise and statistically. Plots and statistics for 
the old groups are available at: https://figshare.com/articles/EEG_study_on_conceptual_modality-switching_Bernabeu_et_al_in_prep_/4210863/125



PAPERS AND FILES

A conference paper was first published in 2017, in the Proceedings of the 39th Conference of the Cognitive Science Society.

Subsequently, further analysis were performed, and they are to be submitted to a journal. 

The analyses performed for the conference paper and for the journal paper are based on the exact same raw data. Any analysis files that differ
for the two papers are so distinguished in their titles.



Summary of new analyses in the journal paper

The novelties with regard to the Cogsci 2017 paper are: (1) better, more conservative mixed models, (2) more analyses of the effect within groups, brain areas, and time windows, and (3) follow-up analysis on the non-significant main effect of Modality Switch in Time Windows 1, 2, and 4. Naturally, the raw data (plots) haven’t changed.

1.  Better, more conservative mixed models 
I re-analysed the switching effect statistically using more robust mixed models than before. These models are more protective against Type I error. The main differences are:
-	the random effects structure was built from the top down, lightening the structure when required for convergence;
-	in R, instead of using the lme4 package, I used afex, which is based on the same principle but produces the p-values in a more structured way;
-	p-values were corrected for multiple comparisons in every analysis. This hasn’t been reported in the previous, most related studies, and indeed it seems to remain unclear under which conditions this has to be done, but I’ve gone with the most conservative advice.
Results: The difference between these results and the first (Cogsci 2017) ones is that now the main effect of Modality Switch is only present in Time Window 3, the N400 window. Other windows do present the effect but within groups and brain areas.

2.  More analyses of the effect within groups, brain areas, and time windows
Time Windows 1 (160-216 ms) and 2 (270-370 ms) no longer have a significant switching effect. This is indeed an important difference from the Cogsci paper. It’s down to the conservativeness of the statistics, which is more accurate now. The two-way interaction of modality switch by brain area is significant in all time windows, as is the three-way interaction which also includes group.
Time Window 3 (350-550 ms) clearly presents the strongest switching effect, with a main effect across groups and brain areas. Time Window 4 (500-750 ms) presents the switching effect in the posterior brain area, across groups. 


Furthermore, I compared the switching effect within language and visual electrodes. In every time window, there was a significant three-way interaction of modality switch by group by brain area. The switch effect invokes a cross-over interaction between group and brain region. In the Quick group, the switching effect is larger in language brain regions, whereas the Slow group presents a larger effect in visual regions. This is consistent with previous findings (Louwerse & Hutchinson, 2012; Louwerse & Connell, 2011). 

 
3.  Follow-up analysis on the non-significant main effect of Modality Switch in Time Windows 1, 2, and 4
I applied a new technique in order to double-check the non-significant main effects of Switch outside of the N400 window. This analysis is called Equivalence Testing (Lakens, Scheel, & Isager, 2018), and it’s based on two one-sided t-tests. The results suggested:
-	Time Window 1:  too little data to conclude anything
-	Time Windows 2 and 4:  the main effect might have been significant with more power.