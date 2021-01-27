#19-01-2021

# 
# this is the first of the series of analysis scripts for the
# EMS entropy in Morphological Systems 
#
# 00 - Merge text resources
#
# v 1.0.0

# in this script, a frequency list of nouns collected from Itwac (Baroni et al., 2009) 
# is merged with the morphologically annotated list Morph-It! (Zanchetta & Baroni, 2005)
# in order to obtain a frequency list of nouns tagged for Gender and Number



###
#                                   Entropy of Morphological Systems    - EMS 00
#
#           S E T   I N P U T S  
#
#   
#
###
# required packages: readr, pryr, stringr,treemap, tidyverse, multcomp(x Tukey hsd)
# for context entropy: tools
library(pryr)

## input required: graphical parameters 

# palette
col_fp = "#00008B" # "blue4" 
col_fs = "#00B2EE" # "deepskyblue2" 
col_mp = "#CD2626" # "firebrick3"  
col_ms = "#FF8C00" # "darkorange"  
pal_01=c(col_fp, col_fs, col_mp, col_ms)

legendcontent=c("Fem. Plur.","Fem. Sing.","Masc. Plur.", "Masc. Sing.")


## input directories 

# this is the folder where this code is stored:
wd_code="C:\\Users\\FF\\Documents\\Analisi varie\\Inflectional Entropy ITA\\Animacy and Morphology 0_0_1"
setwd(wd_code)

# this is the folder where the corpora are stored
# input data consist in 
# 1)frequency list of all nouns in Itwac 2)morph-it! list 
wd_0=paste0(wd_code, "\\wd0")

# this is the directory where the df with all nouns is going to be output
wd_1=paste0(wd_code, "\\wd1")


###                                Entropy of Morphological Systems    - EMS 00
#
#
#           I M P O R T   D A T A S E T S 
#
#   
#
###

#   Database with all nouns in Italian 
#   


setwd(wd_0)
# import itwac nouns 

#allnouns_itwac= read.csv("itwac_nouns_lemmas_notail_2_0_0.csv", sep=",", enc="utf-8")
allnouns_itwac= read.csv("itwac_nouns_lemmas_raw_2_0_0.csv", sep=",", enc="utf-8")
head(allnouns_itwac, 20)

# are there only nouns
unique(allnouns_itwac$POS)=="NOUN"
allnouns_itwac$POS=NULL

# total token frequency
sum(allnouns_itwac$Freq)

# import morph-it 
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

# keep lemma tagging from itwac 
all_nouns_raw$Lemma=NULL


#
# Eliminate "ambiguous" noun forms
#

# retrieve nouns whose forms are tagged as ambiguously related to more than one inflectional feature
all_nouns_amb0=all_nouns_raw[grep ("\\|", all_nouns_raw$lemma), ]

# subtract these forms from df 
all_nouns_clean0=all_nouns_raw[!(all_nouns_raw$Form %in% all_nouns_amb0$Form), ]

head(all_nouns_clean0)



summary(all_nouns_clean0$Freq)
all_nouns_clean0$logtoken=log(all_nouns_clean0$Freq)
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
summary(all_nouns_clean2)

# total discarded types
length(all_nouns_amb$Form)
# discarded types/total types
round(length(all_nouns_amb$Form)/length(all_nouns_raw$Form), 3)

# Femm plur: how many discarded types
length(all_nouns_amb1$Form)
# discarded fem plur/total fem plur 
round((length(all_nouns_amb1$Form)/length(datalln_fp$Form)), 3)

# Femm sing: how many discarded types
length(all_nouns_amb2$Form)
# discarded fem sing/total fem sing
round((length(all_nouns_amb2$Form)/length(datalln_fs$Form)), 3)

# Masc plur: how many discarded types
length(all_nouns_amb3$Form)
# discarded fem plur/total masc plur
round((length(all_nouns_amb3$Form)/length(datalln_mp$Form)), 3)

# Masc plur: how many discarded types
length(all_nouns_amb4$Form)
# discarded fem sing/total masc sing
round((length(all_nouns_amb4$Form)/length(datalln_ms$Form)), 3)



# How are the discarded nouns distributed across features?
# Graph: proportion of discarded types in each feature: all nouns #--------------- mygraph_prop_disp_nouns_types  

library(treemap)

amb_table=as.data.frame(table(all_nouns_amb$POS))
colnames(amb_table)<- list("POS", "Freq")
amb_table$prop=amb_table$Freq/sum(amb_table$Freq)
amb_table$prop=round(amb_table$prop, 3)

mygraph_prop_disp_nouns_types %<a-% {
  amb_table$label <- paste(legendcontent, " ", amb_table$prop)
  treemap(amb_table,
          
          # data
          index=c("label"),
          vSize="Freq",
          type="index",
          
          # Main
          title="Discarded nouns - Types",
          palette=pal_01,
          
          # Borders:
          border.col=c("white"),             
          border.lwds=1,                         
          
          # Labels
          fontsize.labels=20,
          fontcolor.labels="white",
          fontface.labels=1,            
          bg.labels=c("transparent"),              
          align.labels=c("left", "top"),                                  
          overlap.labels=0.5,
          inflate.labels=F      )                 
}
mygraph_prop_disp_nouns_types

summary(all_nouns_clean2)
setwd(wd_1)

write.csv(all_nouns_clean2, "all_nouns_tagged.csv", sep=",")
