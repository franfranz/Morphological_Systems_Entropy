###
#
#     E N T R O P Y  
#
#             of   M O R P H O L O G I C A L 
#                 
#                                       S Y S T E M S 
#   
# 
###


# EMS 00 - Merge text resources
#
# v. FULL 1.0.0
# 
# https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Full


# this is the first script of the EMS analysis series   
# EMS Full version: EMS-00, EMS-01, EMS-02, EMS-03, Graph-EMS 


# in this script: 

# 1) the morphologically annotated list Morph-It! (Zanchetta & Baroni, 2005)
# is merged with a frequency list of nouns collected from Itwac (Baroni et al., 2009) 
# in order to obtain a frequency list of nouns tagged for Gender and Number ro be used in EMS-01
# the morph-it list was downloaded from: https://docs.sslmit.unibo.it/doku.php?id=resources:morph-it#download 
# the itwac corpus is available at: https://cqpweb.lancs.ac.uk/itwac/ 
# in this study we are using a list of all nouns present in the corpus
# for the methods of compilation of the list, please refer to: https://github.com/franfranz/Word_Frequency_Lists_ITA 

# 2) a list of animate nouns is prepared for use (amalysis in EMS-01)

# 3) a subset of nouns is randomly selected from the all nouns list, 
# in order to have a sample of nouns to compare for context entropy (EMS-02/EMS-03)


###
#                                   Entropy of Morphological Systems    - EMS 00
#
#           S E T   I N P U T S  
#
#   
#
###

# required packages: pryr, stringr, tidyverse, treemap

library(pryr)

## input required: graphical parameters 

# palette
col_fp = "#00008B" # "blue4" 
col_fs = "#00B2EE" # "deepskyblue2" 
col_mp = "#CD2626" # "firebrick3"  
col_ms = "#FF8C00" # "darkorange"  
pal_01=c(col_fp, col_fs, col_mp, col_ms)

legendcontent=c("Fem. Plur.","Fem. Sing.","Masc. Plur.", "Masc. Sing.")

myfavlinescol1="#4A4A4A" #"grey29"


# number of decimal places for rounded numbers
roundnum=3

## input directories 

# this is the folder where this code is stored:
wd_code="PATH"

setwd(wd_code)

# this is the subdirectory of wd_code where the corpora are stored
# input data consist in 
# 1)frequency list of all nouns in Itwac 2)morph-it! list 3) list of animate nouns to be used in the study
wd_0=paste0(wd_code, "\\wd0")

# this is the subdirectory of wd_code where the dfs are going to be output for the analysis 
wd_1=paste0(wd_code, "\\wd1")


###                                Entropy of Morphological Systems    - EMS 00
#
#
#          1-  I M P O R T   D A T A S E T S 
#
#   
#
###

#
#   Database with all nouns in Italian 
#   

setwd(wd_0)
# import itwac nouns 
# frequency list of all word forms tagged as nouns in Itwac 

allnouns_itwac= read.csv("itwac_nouns_lemmas_raw_2_0_0.csv", sep=",", enc="utf-8")
head(allnouns_itwac, 20)

# are there only nouns
unique(allnouns_itwac$POS)=="NOUN"
allnouns_itwac$POS=NULL

# total token frequency
sum(allnouns_itwac$Freq)

# import morph-it 
# 
morphit=read.delim("morph-it_048.txt", header=F)
head(morphit,20)
colnames(morphit)<-list("Form", "Lemma", "POS")

# subset only nouns from morph-it
morphit_nouns= morphit[morphit$POS=="NOUN-F:p"
                       |morphit$POS=="NOUN-F:s"
                       |morphit$POS=="NOUN-M:p"
                       |morphit$POS=="NOUN-M:s", ]

#
# Merge itwac and morph-it
#

# merge the nouns from itwac and the nouns from morph-it 
all_nouns_raw=merge(allnouns_itwac, morphit_nouns, by.x="Form", by.y = "Form")

# keep lemma tagging from itwac - discard columns not to be used in the study
all_nouns_raw$Lemma=NULL
all_nouns_raw$Zipf=NULL
all_nouns_raw$fpmw=NULL

# create a column with 'base' form (lemma without suffix)
all_nouns_raw$base <-gsub('.{1}$', '_', all_nouns_raw$lemma)
str(all_nouns_raw$base)




#
# Discard "ambiguous" noun forms
#

# retrieve nouns whose forms are tagged as ambiguously related to more than one inflectional feature
all_nouns_amb0=all_nouns_raw[grep ("\\|", all_nouns_raw$lemma), ]

# subtract these forms from df 
all_nouns_clean0=all_nouns_raw[!(all_nouns_raw$Form %in% all_nouns_amb0$Form), ]

head(all_nouns_clean0)



summary(all_nouns_clean0$Freq)
all_nouns_clean0$logtoken=log(all_nouns_clean0$Freq)
all_nouns_clean0$logtoken=(round(all_nouns_clean0$logtoken, roundnum))
all_nouns_clean1=all_nouns_clean0[all_nouns_clean0$logtoken>0, ]


#
#   Split across INFLECTIONAL FEATURES - All nouns
#

# object with all Fp
datalln_fp= all_nouns_clean1[all_nouns_clean1$POS=="NOUN-F:p", ]
summary(datalln_fp)
datalln_fp$POS=as.factor(as.character(datalln_fp$POS))

# object with all Fs
datalln_fs= all_nouns_clean1[all_nouns_clean1$POS=="NOUN-F:s", ]
summary(datalln_fs)
str(datalln_fs$Freq)
datalln_fs$POS=as.factor(as.character(datalln_fs$POS))

# object with all Mp
datalln_mp= all_nouns_clean1[all_nouns_clean1$POS=="NOUN-M:p", ]
summary(datalln_mp)
datalln_mp$POS=as.factor(as.character(datalln_mp$POS))

# object with all Ms
datalln_ms= all_nouns_clean1[all_nouns_clean1$POS=="NOUN-M:s", ]
summary(datalln_ms)
datalln_ms$POS=as.factor(as.character(datalln_ms$POS))

# count: frequency observed in all inflection (token)
# sum partials 
tokenfreq_alln_fs=sum(datalln_fs$Freq)
tokenfreq_alln_fp=sum(datalln_fp$Freq)
tokenfreq_alln_ms=sum(datalln_ms$Freq)
tokenfreq_alln_mp=sum(datalln_mp$Freq)
tokenfreq_total_alln=sum(all_nouns_clean1$Freq)
tokenfreq_alln_fs+tokenfreq_alln_fp+tokenfreq_alln_ms+tokenfreq_alln_mp==tokenfreq_total_alln

all_nouns_amb1=datalln_fp[(datalln_fp$Form %in% datalln_fs$Form==T)
                         |(datalln_fp$Form %in% datalln_mp$Form==T)
                         |(datalln_fp$Form %in% datalln_ms$Form==T), ]

all_nouns_amb2=datalln_fs[(datalln_fs$Form %in% datalln_fp$Form==T)
                       |(datalln_fs$Form %in% datalln_mp$Form==T)
                       |(datalln_fs$Form %in% datalln_ms$Form==T), ]

all_nouns_amb3=datalln_mp[(datalln_mp$Form %in% datalln_fp$Form==T)
                       |(datalln_mp$Form %in% datalln_fs$Form==T)
                       |(datalln_mp$Form %in% datalln_ms$Form==T), ]

all_nouns_amb4=datalln_ms[(datalln_ms$Form %in% datalln_fp$Form==T)
                       |(datalln_ms$Form %in% datalln_fs$Form==T)
                       |(datalln_ms$Form %in% datalln_mp$Form==T), ]
all_nouns_amb=rbind(all_nouns_amb1, all_nouns_amb2, all_nouns_amb3, all_nouns_amb4)
summary(all_nouns_amb)

all_nouns_clean2=all_nouns_clean1[!(all_nouns_clean1$Form %in% all_nouns_amb$Form), ]

# column with nountype 
all_nouns_clean2$nountype=rep("allnouns")

# total discarded types
length(all_nouns_amb$Form)
# discarded types/total types
round(length(all_nouns_amb$Form)/length(all_nouns_raw$Form), roundnum)

# Femm plur: how many discarded types
length(all_nouns_amb1$Form)
# discarded fem plur/total fem plur 
round((length(all_nouns_amb1$Form)/length(datalln_fp$Form)), roundnum)

# Femm sing: how many discarded types
length(all_nouns_amb2$Form)
# discarded fem sing/total fem sing
round((length(all_nouns_amb2$Form)/length(datalln_fs$Form)), roundnum)

# Masc plur: how many discarded types
length(all_nouns_amb3$Form)
# discarded fem plur/total masc plur
round((length(all_nouns_amb3$Form)/length(datalln_mp$Form)), roundnum)

# Masc plur: how many discarded types
length(all_nouns_amb4$Form)
# discarded fem sing/total masc sing
round((length(all_nouns_amb4$Form)/length(datalln_ms$Form)), roundnum)



# How are the discarded nouns distributed across features?
# Graph: proportion of discarded types in each feature: all nouns #--------------- mygraph_prop_disp_nouns_types  

library(treemap)

amb_table=as.data.frame(table(all_nouns_amb$POS))
colnames(amb_table)<- list("POS", "Freq")
amb_table$prop=amb_table$Freq/sum(amb_table$Freq)
amb_table$prop=round(amb_table$prop, roundnum)

mygraph_prop_disp_nouns_types %<a-% {
  amb_table$label <- paste(legendcontent, " ", amb_table$prop)
  treemap(amb_table,
          index=c("label"),
          vSize="Freq",
          type="index",
          # Main
          title="Discarded nouns - Types",
          palette=pal_01,
          border.col=c("white"),             
          border.lwds=1,                         
          fontsize.labels=20,
          fontcolor.labels="white",
          fontface.labels=1,            
          bg.labels=c("transparent"),              
          align.labels=c("left", "top"),                                  
          overlap.labels=0.5,
          inflate.labels=F      )                 
}
mygraph_prop_disp_nouns_types



###
#                                   Entropy of Morphological Systems    - EMS 00     
#
#           2 - A N I M A T E    N O U N S 
#
#   
#
###

#
# Import animate nouns

setwd(wd_0)
anim_nouns=read.csv("animate_nounlist.csv", header =  T)

# column  with base 
anim_nouns$base=gsub('.{1}$', '_', anim_nouns$lemma)

# all of the nouns have 4 forms for the same 'base'
table(anim_nouns$base)
summary(table(anim_nouns$base)==4)
head(anim_nouns)

# column with log frequency
anim_nouns$logtoken=log(anim_nouns$Freq)
anim_nouns$logtoken=round(anim_nouns$logtoken, roundnum)

#column with nountype
anim_nouns$nountype=rep("animate")
anim_nouns$X=NULL
head(anim_nouns)



###
#                                   Entropy of Morphological Systems    - EMS 00     
#
#           3 - C O N T R O L   S A M P L E    N O U N S 
#
#   
#
###


# select control sample from the sample containing all nouns
summary(all_nouns_clean2)

#
#   Split across INFLECTIONAL FEATURES - All nouns
#

# object with all Fp
alln_fp= all_nouns_clean2[all_nouns_clean2$POS=="NOUN-F:p", ]
summary(alln_fp)
alln_fp$POS=as.factor(as.character(alln_fp$POS))

# object with all Fs
alln_fs= all_nouns_clean2[all_nouns_clean2$POS=="NOUN-F:s", ]
summary(alln_fs)
str(alln_fs$FREQ)
alln_fs$POS=as.factor(as.character(alln_fs$POS))

# object with all Mp
alln_mp= all_nouns_clean2[all_nouns_clean2$POS=="NOUN-M:p", ]
summary(alln_mp)
alln_mp$POS=as.factor(as.character(alln_mp$POS))

# object with all Ms
alln_ms= all_nouns_clean2[all_nouns_clean2$POS=="NOUN-M:s", ]
summary(alln_ms)
alln_ms$POS=as.factor(as.character(alln_ms$POS))

#draw sample from nouns 
#set a seed to have the selection replicated 
#myseed=1

set.seed(myseed)
matched_control_fp <- alln_fp[sample(1:nrow(alln_fp), 47, replace=F),]
summary(matched_control_fp)

set.seed(myseed)
matched_control_fs <- alln_fs[sample(1:nrow(alln_fs), 47, replace=F),]
summary(matched_control_fs)

set.seed(myseed)
matched_control_mp <- alln_mp[sample(1:nrow(alln_mp), 47, replace=F),]
summary(matched_control_mp)

set.seed(myseed)
matched_control_ms <- alln_ms[sample(1:nrow(alln_ms), 47, replace=F),]
summary(matched_control_ms)

# distance of sampled control nouns from all nouns, within each feature 
wilcox.test(alln_fp$logtoken, matched_control_fp$logtoken)
wilcox.test(alln_fs$logtoken, matched_control_fs$logtoken)
wilcox.test(alln_mp$logtoken, matched_control_mp$logtoken)
wilcox.test(alln_ms$logtoken, matched_control_ms$logtoken)


# match the extracted plural forms to the respective singular forms
matched_femm_sg= merge(matched_control_fs, alln_fp, by.x="lemma", by.y = "lemma")
matched_femm_sg=all_nouns_clean2[match(matched_femm_sg$Form.y, all_nouns_clean2$Form), ]
dim(matched_femm_sg)
summary(matched_femm_sg)

matched_masc_sg= merge(matched_control_ms, alln_mp, by.x="lemma", by.y = "lemma")
matched_masc_sg=all_nouns_clean2[match(matched_masc_sg$Form.y, all_nouns_clean2$Form), ]
dim(matched_masc_sg)
summary(matched_masc_sg)

# match the extracted plural forms to the respective singular forms
matched_femm_pl= merge(matched_control_fp, alln_fs, by.x="lemma", by.y = "lemma")
matched_femm_pl=all_nouns_clean2[match(matched_femm_pl$Form.y, all_nouns_clean2$Form), ]
dim(matched_femm_pl)
summary(matched_femm_pl)

matched_masc_pl= merge(matched_control_mp, alln_ms, by.x="lemma", by.y = "lemma")
matched_masc_pl=all_nouns_clean2[match(matched_masc_pl$Form.y, all_nouns_clean2$Form), ]
dim(matched_masc_pl)
summary(matched_masc_pl)

#merge into df with all nouns 
matched_control=rbind(matched_control_fp, matched_control_fs, matched_control_mp, matched_control_ms)
matched_control$POS=as.factor(as.character(matched_control$POS))

summary(matched_control)


matched_singandplur=merge(matched_control, all_nouns_clean2, by.x="lemma", by.y = "lemma")
full_contsamp=all_nouns_clean2[match(matched_singandplur$Form.y, all_nouns_clean2$Form), ]
dim(full_contsamp)
summary(full_contsamp)
length(unique(full_contsamp$Form))
head(full_contsamp)

#remove possible duplicates (nouns randomly selected twice)
library(tidyverse)
full_contsamp$notunique_forms=duplicated(full_contsamp$Form)
fcs2=full_contsamp[which(full_contsamp$notunique_forms==T), ]
summary(fcs2)
fcs2$Form

summary(full_contsamp)

full_contsamp$POS=as.factor(as.character(full_contsamp$POS))
full_contsamp$nountype=rep("control")
str(full_contsamp$Form)
table(full_contsamp$POS)
full_contsamp$notunique_forms=NULL



###
#                                   Entropy of Morphological Systems    - EMS 00
#
#          O U T P U T   M E R G E D   L I S T S  
#
#   
#
###


#  Output merged lists to wd_1
setwd(wd_1)

# list with all nouns
summary(all_nouns_clean2)
#write.csv(all_nouns_clean2, "all_nouns_tagged_ITA.csv", sep=",")

# list with animate nouns 
summary(anim_nouns)
#write.csv(anim_nouns, "animate_nouns_tagged_ITA.csv", sep=",")

# list with control sample nouns 
summary(full_contsamp)
#write.csv(full_contsamp, "control_sample_tagged_ITA.csv", sep=",")


# Graphs
# generate a vector collecting the name of the graphs to plot
complete_graphlist_EMS00=ls()[grep(("mygraph"), ls())]


# proceed to 01_EMS_Context_Entropy.R to calculate the context entropy of nouns of the animate and control sample


