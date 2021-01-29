
#19-01-2021
#v 2.0.0


#this script is partly derived from the E N T R O P Y  in   I N F L E C T I O N A L   M O R P H L O G Y - ITA started on 22-01-2020
#the script to take into account for context entropy is  CONTEXT ENTROPY in INFLECTIONAL MORPHOLOGY ITA -  3-11-2020

# articulation of the script series 
# observed vs reference distributions: all nouns
# observed vs reference distributions: animate nouns
# sample control nouns

# calculate context entropy 

# context entropy: sampled nouns
# context entropy: animate nouns


# graphical settings 
# what packages
# what input


# ## ## ##  ENTROPY OF INFLECTIONAL SYSTEMS
#
#    ## ###  All nominal lexicon 
#     # ### ##  Animate nouns 
#      ### ### ###  Control dataset (same size as Anim_dataset - 360 nouns)
#       # ## ### ## Reference Distribution 
#
# ## ## ##  CONTEXT ENTROPY 
#
#     # ### ##  Animate nouns 
#       ### ### ###  Control dataset
#
#
#
# ## ## ## Quick Summary 
#          
# ## ## ## Quick Graphs 
#
##


###
#                                   Entropy of Morphological Systems    - EMS 01   
#
#           S E T   I N P U T S  
#
#   
#
###
# required packages: readr, pryr, stringr,treemap, LaplacesDemon, tidyverse, multcomp(x Tukey hsd)
# for context entropy: tools
library(pryr)

## input required: graphical parameters 

# palette
col_fp = "#00008B" # "blue4" 
col_fs = "#00B2EE" # "deepskyblue2" 
col_mp = "#CD2626" # "firebrick3"  
col_ms = "#FF8C00" # "darkorange"  
pal_01=c(col_fp, col_fs, col_mp, col_ms)


# some common features of graphics
myfavcex=1.3
myfavpch=19
myfavlwd=2.7

# lty 
myfavlty_fp=1
myfavlty_fs=1
myfavlty_mp=1
myfavlty_ms=1
myfavlty_ans=1
myfavlty_cont=2


# some common features of graphics axes
ticks_h1=c(seq.int(1:4))
ticks_g1=c(seq.int(0,16,2))
mymostcommon_xlim=c(0, 16)
mymostcommon_ylim=c(0, 0.4)


# plot with box
#myaxesset=T
#rm(lightaxes)
#lightaxes=NULL

# plot with axes on left and bottom positions
myaxesset=F
lightaxes %<a-% {
  axis(side=1, at=ticks, labels = T)
  axis(side=2)}


# some common features of graphic grids
myfavgridcol="lightgray"
myfavgridlty="dotted"

# some common features of legends
myfavlegendcex=0.8
myfavlegendinset=0.02
legendcontent=c("Fem. Plur.","Fem. Sing.","Masc. Plur.", "Masc. Sing.")



## input directories 

# this is the folder where this code is stored:
wd_code="C:\\Users\\FF\\Documents\\Analisi varie\\Inflectional Entropy ITA\\Animacy and Morphology 0_0_1"
setwd(wd_code)

# this is the folder where the input dfs are stored
# input data consist in 1)frequency list of all nouns in Itwac 2)morp-it! list 3)a database with animate nouns
wd_1=paste0(wd_code, "\\wd1")


## output directories 

# this is the directory where the experimental results are output
wd_2=paste0(wd_code, "\\wd2")
#dir.create(wd2)

wd_3=paste0(wd_code, "\\wd3")
#dir.create(wd3)
  
# this is the folder to save graphics 
wd_graphs=paste0(wd_code, "\\Graphics") 
#dir.create(wd_graphs)

# this is the folder to save text - table outputs 
#wd_tables=paste0(wd_code, "\\Texttables")
#dire.create(wd_tables)



###                               Entropy of Morphological Systems    - EMS 01  
#
#
#           1 - R E F E R E N C E   D I S T R I B U T I O N S
#
#   
#
###

# generate uniform distribution, 4 discrete values
# size: approx the size of corpus = tokenfreq_total_alln
univec_length= 100000
uni_d=floor(runif(univec_length, 1, 5))
str(uni_d)

hist(uni_d, ylim=c(0,3000), axes=F, main = "Uniform Categorial Distribution")
axis(side=1, at=hticks, labels = T)
axis(side=2)

uni_d=as.character(uni_d)

# entropy of the discrete uniform distribution
freqs_uni =table(uni_d)/length(uni_d)
entropy_uni= -sum(freqs_uni * log2(freqs_uni))
entropy_uni=round(entropy_uni, 3)

par(mfrow=c(1,1))
hticks=c(1,2,3,4)

plot(uni_d, axes=F, main = "Uniform Categorial Distribution")
axis(side=2, at=hticks)
axis(side=1)

#freqs_uni_df=as.data.frame(freqs_uni)
#colnames(freqs_uni_df)<-list("Value", "Freq")




###   
#                                  Entropy of Morphological Systems    - EMS 01    
#
#           2 - A N I M A T E    N O U N S 
#
#                                   
#
###


setwd(wd_1)
dat_anim=read.csv("animate_sample_ITA.csv", sep=",", T)

dat_anim$nountype=rep("animate")
# column with log frequency
dat_anim$logtoken=log(dat_anim$Freq)

# column  with base 
dat_anim$base=gsub('.{1}$', '_', dat_anim$lemma)
table(dat_anim$base)

#
#   Split across INFLECTIONAL FEATURES - Animate nouns
#

#object with all Fp
anim_fp= dat_anim[dat_anim$POS=="NOUN-F:p", ]
summary(anim_fp)
anim_fp$POS=as.factor(as.character(anim_fp$POS))
length(anim_fp$POS)

#object with all Fs
anim_fs= dat_anim[dat_anim$POS=="NOUN-F:s", ]
summary(anim_fs)
anim_fs$POS=as.factor(as.character(anim_fs$POS))
length(anim_fs$POS)

#object with all Mp
anim_mp= dat_anim[dat_anim$POS=="NOUN-M:p", ]
summary(anim_mp)
anim_mp$POS=as.factor(as.character(anim_mp$POS))
length(anim_mp$POS)

#object with all Ms
anim_ms= dat_anim[dat_anim$POS=="NOUN-M:s", ]
summary(anim_ms)
anim_ms$POS=as.factor(as.character(anim_ms$POS))
length(anim_ms$POS)



#
#   ENTROPY - Features - Animate nouns
#

# count: frequency observed in animate inflection (type)
typefreq_anim=table(dat_anim$POS)/length(dat_anim$POS)
entropy_type_anim= -(sum((typefreq_anim)*log2(typefreq_anim)))
entropy_type_anim=round(entropy_type_anim, 3)

# count: frequency observed in animate inflection (token)
# sum partials 
tokenfreq_anim_fp=sum(anim_fp$Freq)
tokenfreq_anim_fs=sum(anim_fs$Freq)
tokenfreq_anim_mp=sum(anim_mp$Freq)
tokenfreq_anim_ms=sum(anim_ms$Freq)

#check with total sum
tokenfreq_total_anim=sum(dat_anim$Freq)
tokenfreq_anim_fp+tokenfreq_anim_fs+tokenfreq_anim_mp+tokenfreq_anim_ms==tokenfreq_total_anim

# entropy: token - animate
freq_tokens_anim=c(tokenfreq_anim_fp, tokenfreq_anim_fs, tokenfreq_anim_ms, tokenfreq_anim_mp)/tokenfreq_total_anim
entropy_token_anim= -sum(freq_tokens_anim*log2(freq_tokens_anim))
entropy_token_anim=round(entropy_token_anim, 3)  


# Graph: proportion of types in each feature: animate nouns #--------------- mygraph_prop_animnouns_types  

library(treemap)

prop_animnouns_types=as.data.frame(typefreq_anim)
prop_cols=c("POS", "Freq")
colnames(prop_animnouns_types)<- prop_cols
prop_animnouns_types$Freq=round(prop_animnouns_types$Freq, 3)



#prop_animnouns_types$label <- paste(prop_animnouns_types$POS, prop_animnouns_types$Freq, sep = "\n")
mygraph_prop_animnouns_types %<a-% {
  prop_animnouns_types$label <- paste(legendcontent, " ", prop_animnouns_types$Freq)
  treemap(prop_animnouns_types,
          
          # data
          index=c("label"),
          vSize="Freq",
          type="index",
          
          # Main
          title="Animate nouns - Types",
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
          inflate.labels=F      )                  # If true, labels are bigger when rectangle is bigger.
}
mygraph_prop_animnouns_types



# Graph: proportion of tokens in each feature: all nouns #--------------- mygraph_prop_animnouns_token 
prop_animnouns_token=as.data.frame(freq_tokens_anim)
prop_animnouns_token$Freq=prop_animnouns_token$freq_tokens_anim
prop_animnouns_token$POS=legendcontent
prop_animnouns_token$freq_tokens_anim=NULL

prop_animnouns_token$Freq=round(prop_animnouns_token$Freq, 3)

mygraph_prop_animnouns_token %<a-% {
  #prop_animnouns_token$label <- paste(prop_animnouns_token$POS, prop_animnouns_token$Freq, sep = "\n")
  prop_animnouns_token$label <- paste(legendcontent, " ", prop_animnouns_token$Freq)
  treemap(prop_animnouns_token,
          
          # data
          index=c("label"),
          vSize="Freq",
          type="index",
          
          # Main
          title="Animate nouns - Tokens",
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
mygraph_prop_animnouns_token


# token frequency - density - animate nouns  #--------------- mygraph_dens_animnouns

ticks=ticks_g1
mygraph_dens_animnouns %<a-% {
  plot(density(anim_fp$logtoken), 
       col=col_fp, 
       lwd=myfavlwd, 
       lty=myfavlty_fp,
       main="Animate nouns", 
       xlab="Token frequency of occurrence (log)", 
       ylim=mymostcommon_ylim, xlim=mymostcommon_xlim,
       axes=myaxesset)
  lightaxes
  lines(density(anim_fs$logtoken), col=col_fs, lwd=myfavlwd, lty=myfavlty_fs)
  lines(density(anim_mp$logtoken), col=col_mp, lwd=myfavlwd, lty=myfavlty_mp)
  lines(density(anim_ms$logtoken), col=col_ms, lwd=myfavlwd, lty=myfavlty_ms)
  legend("topright", 
         inset=myfavlegendinset, 
         # title="Inflection",
         legend=legendcontent, 
         fill=pal_01, 
         bty="n", 
         cex=myfavlegendcex)
}
mygraph_dens_animnouns


###
#                                   Entropy of Morphological Systems    - EMS 01     
#
#           3 - A L L    N O U N S 
#
#   
#
###



# sample of all nouns 
# sample of animate nouns
# subtract 

# sample of control nouns 

#-- the construction of the database has been moved to 00-merge text resources 

#   Import dataset with all nouns in Italian 
#   
# The dataset has been obtained by merging word frequencies collected from Itwac (Baroni et al., 2009) 
#   to a list of morphologically tagged words (Morph-it!, Zanchetta & Baroni, 2005). 
# please refer to 00_EMS_Merge_text_resources_v1_0_0.R for the merging

setwd(wd_1)
datallnouns_imported=read.csv("all_nouns_tagged_ITA.csv")

# subtract nouns from the animate sample
datallnouns=datallnouns_imported[!datallnouns_imported$Form %in% dat_anim$Form, ]

# create a column with 'base' form (lemma without suffix)
datallnouns$base <-gsub('.{1}$', '_', datallnouns$lemma)
datallnouns$base

#
#   Split across INFLECTIONAL FEATURES - All nouns
#

# object with all Fp
alln_fp= datallnouns[datallnouns$POS=="NOUN-F:p", ]
summary(alln_fp)
alln_fp$POS=as.factor(as.character(alln_fp$POS))

# object with all Fs
alln_fs= datallnouns[datallnouns$POS=="NOUN-F:s", ]
summary(alln_fs)
str(alln_fs$FREQ)
alln_fs$POS=as.factor(as.character(alln_fs$POS))

# object with all Mp
alln_mp= datallnouns[datallnouns$POS=="NOUN-M:p", ]
summary(alln_mp)
alln_mp$POS=as.factor(as.character(alln_mp$POS))

# object with all Ms
alln_ms= datallnouns[datallnouns$POS=="NOUN-M:s", ]
summary(alln_ms)
alln_ms$POS=as.factor(as.character(alln_ms$POS))

# count: frequency observed in all inflection (token)
# sum partials 
tokenfreq_alln_fs=sum(alln_fs$Freq)
tokenfreq_alln_fp=sum(alln_fp$Freq)
tokenfreq_alln_ms=sum(alln_ms$Freq)
tokenfreq_alln_mp=sum(alln_mp$Freq)

#check with total sum
tokenfreq_total_alln=sum(datallnouns$Freq)
tokenfreq_alln_fs+tokenfreq_alln_fp+tokenfreq_alln_ms+tokenfreq_alln_mp==tokenfreq_total_alln

#
# ENTROPY - Features  All nouns
#

# entropy - type - allnouns
freq_types_all=table(datallnouns$POS)/length(datallnouns$POS)
entropy_type_alln=-sum(freq_types_all*log2(freq_types_all))
entropy_type_alln=round(entropy_type_alln, 3)

# entropy - token - allnouns
freq_tokens_alln=c(tokenfreq_alln_fp, tokenfreq_alln_fs,tokenfreq_alln_mp, tokenfreq_alln_ms)/tokenfreq_total_alln
entropy_token_alln= -sum(freq_tokens_alln*log2(freq_tokens_alln))
entropy_token_alln=round(entropy_token_alln, 3)  



# Graph: proportion of types in each feature: all nouns #--------------- mygraph_prop_allnouns_types  

library(treemap)

prop_allnouns_types=as.data.frame(freq_types_all)
prop_allnounscols=c("POS", "Freq")
colnames(prop_allnouns_types)<- prop_allnounscols
prop_allnouns_types$Freq=round(prop_allnouns_types$Freq, 3)



#prop_allnouns_types$label <- paste(prop_allnouns_types$POS, prop_allnouns_types$Freq, sep = "\n")
mygraph_prop_allnouns_types %<a-% {
prop_allnouns_types$label <- paste(legendcontent, " ", prop_allnouns_types$Freq)
treemap(prop_allnouns_types,
        
        # data
        index=c("label"),
        vSize="Freq",
        type="index",
        
        # Main
        title="All nouns - Types",
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
mygraph_prop_allnouns_types



# Graph: proportion of tokens in each feature: all nouns #--------------- mygraph_prop_allnouns_tokens 
prop_allnouns_token=as.data.frame(freq_tokens_alln)
prop_allnouns_token$Freq=prop_allnouns_token$freq_tokens_alln
prop_allnouns_token$POS=legendcontent
prop_allnouns_token$freq_tokens_alln=NULL

prop_allnouns_token$Freq=round(prop_allnouns_token$Freq, 3)

mygraph_prop_allnouns_tokens %<a-% {
#prop_allnouns_token$label <- paste(prop_allnouns_token$POS, prop_allnouns_token$Freq, sep = "\n")
prop_allnouns_token$label <- paste(legendcontent, " ", prop_allnouns_token$Freq)
treemap(prop_allnouns_token,
        
        # data
        index=c("label"),
        vSize="Freq",
        type="index",
        
        # Main
        title="All nouns - Tokens",
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
mygraph_prop_allnouns_tokens


#
# density 
#

# token frequency - density - all nouns #--------------- mygraph_dens_allnouns 
ticks=ticks_g1

mygraph_dens_allnouns %<a-% {
plot(density(alln_fp$logtoken), 
     col=col_fp, 
     lwd=myfavlwd, 
     lty=myfavlty_fp,
     main="All nouns", 
     xlab="Token frequency of occurrence (log)", 
     ylim=mymostcommon_ylim, xlim=mymostcommon_xlim, 
     axes=myaxesset)
lightaxes
lines(density(alln_fs$logtoken), col=col_fs, lwd=myfavlwd, lty=myfavlty_fs)
lines(density(alln_mp$logtoken), col=col_mp, lwd=myfavlwd, lty=myfavlty_mp)
lines(density(alln_ms$logtoken), col=col_ms, lwd=myfavlwd, lty=myfavlty_ms) 
legend("topright", inset=myfavlegendinset,# title="Inflection",
       # c("Fem. Plur.","Fem. Sing.","Masc. Plur.", "Masc. Sing."), fill=c("blue4","cyan3", "firebrick3","darkgoldenrod1"), bty="n", cex=0.8)
       legend=legendcontent, 
       fill=pal_01, 
       bty="n", 
       cex=myfavlegendcex)
}
mygraph_dens_allnouns



###
#                                   Entropy of Morphological Systems    - EMS 01     
#
#           4 - C O N T R O L   S A M P L E    N O U N S 
#
#   
#
###

#build sample
summary(datallnouns)

#draw sample from nouns 
#set a seed to have the selection replicated 
myseed=1

set.seed(myseed)
matched_control_fp <- alln_fp[sample(1:nrow(alln_fp), 45, replace=FALSE),]
summary(matched_control_fp)

set.seed(myseed)
matched_control_fs <- alln_fs[sample(1:nrow(alln_fs), 45, replace=FALSE),]
summary(matched_control_fs)

set.seed(myseed)
matched_control_mp <- alln_mp[sample(1:nrow(alln_mp), 45, replace=FALSE),]
summary(matched_control_mp)

set.seed(myseed)
matched_control_ms <- alln_ms[sample(1:nrow(alln_ms), 45, replace=FALSE),]
summary(matched_control_ms)

# distance of sampled control nouns from all nouns, within each feature 
ks.test(alln_fp$logtoken, matched_control_fp$logtoken)
ks.test(alln_fs$logtoken, matched_control_fs$logtoken)
ks.test(alln_mp$logtoken, matched_control_mp$logtoken)
ks.test(alln_ms$logtoken, matched_control_ms$logtoken)




# match the extracted plural forms to the respective singular forms
matched_femm_sg= merge(matched_control_fs, alln_fp, by.x="lemma", by.y = "lemma")
matched_femm_sg=datallnouns[match(matched_femm_sg$Form.y, datallnouns$Form), ]
dim(matched_femm_sg)
summary(matched_femm_sg)

matched_masc_sg= merge(matched_control_ms, alln_mp, by.x="lemma", by.y = "lemma")
matched_masc_sg=datallnouns[match(matched_masc_sg$Form.y, datallnouns$Form), ]
dim(matched_masc_sg)
summary(matched_masc_sg)

# match the extracted plural forms to the respective singular forms
matched_femm_pl= merge(matched_control_fp, alln_fs, by.x="lemma", by.y = "lemma")
matched_femm_pl=datallnouns[match(matched_femm_pl$Form.y, datallnouns$Form), ]
dim(matched_femm_pl)
summary(matched_femm_pl)

matched_masc_pl= merge(matched_control_mp, alln_ms, by.x="lemma", by.y = "lemma")
matched_masc_pl=datallnouns[match(matched_masc_pl$Form.y, datallnouns$Form), ]
dim(matched_masc_pl)
summary(matched_masc_pl)


#merge into df with all nouns 
matched_control=rbind(matched_control_fp, matched_control_fs, matched_control_mp, matched_control_ms)
matched_control$POS=as.factor(as.character(matched_control$POS))

summary(matched_control)


matched_singandplur=merge(matched_control, datallnouns, by.x="lemma", by.y = "lemma")
full_contsamp=datallnouns[match(matched_singandplur$Form.y, datallnouns$Form), ]
dim(full_contsamp)
summary(full_contsamp)
length(unique(full_contsamp$Form))
head(full_contsamp)

#remove possible duplicates (nouns randomly selected twice)
library(tidyverse)
full_contsamp$notunique_forms=duplicated(full_contsamp$Form)
fcs2=full_contsamp[which(full_contsamp$notunique_forms==T), ]
summary(fcs2)

summary(full_contsamp)


full_contsamp$POS=as.factor(as.character(full_contsamp$POS))
full_contsamp$nountype=rep("control")
str(full_contsamp$Form)
table(full_contsamp$POS)
full_contsamp$notunique_forms=NULL
#full_contsamp$X=NULL

control_fp= full_contsamp[full_contsamp$POS=="NOUN-F:p", ]
control_fs= full_contsamp[full_contsamp$POS=="NOUN-F:s", ]
control_mp= full_contsamp[full_contsamp$POS=="NOUN-M:p", ]
control_ms= full_contsamp[full_contsamp$POS=="NOUN-M:s", ]

#compare distribution of sample to distribution of whole set - line 519 for the updated distributions

# distance of sampled control nouns from all nouns, within each feature 
ks.test(alln_fp$logtoken, control_fp$logtoken)
ks.test(alln_fs$logtoken, control_fs$logtoken)
ks.test(alln_mp$logtoken, control_mp$logtoken)
ks.test(alln_ms$logtoken, control_ms$logtoken)


# distribution of nouns in the control sample #--------------- mygraph_controlsample_features 

mygraph_controlsample_features %<a-% { 
  
  par(mfrow=c(2,2))
  plot(density(alln_fp$logtoken), col=col_fp, lwd=myfavlwd, ylim=mymostcommon_ylim,
       main="",
       axes=myaxesset)
  lightaxes 
  lines(density(control_fp$logtoken), col=col_fp, lwd=myfavlwd, lty=myfavlty_cont )
  legend("topleft", legend = "Fem. \nPlur.", text.col=col_fp, bty="n")
  
  xpd=T
  legend("topright", inset=-0.1, 
         c("All Nouns","Control Sample"), lwd=1.9, lty=c(myfavlty_ans, myfavlty_cont), 
         bty="n", cex=0.8, ncol = 1)
  xpd=F
  
  plot(density(alln_fs$logtoken), col=col_fs, lwd=myfavlwd, ylim=mymostcommon_ylim,
       main="",
       axes=myaxesset)
  lightaxes
  lines(density(control_fs$logtoken), col=col_fs, lwd=myfavlwd, lty=myfavlty_cont)
  legend("topleft", legend = "Fem. \nSing.", text.col=col_fs, bty="n")
  
  plot(density(alln_mp$logtoken), col=col_mp, lwd=myfavlwd, ylim=mymostcommon_ylim, 
       main="",
       axes=myaxesset)
  lightaxes
  lines(density(control_mp$logtoken), col=col_mp, lwd=myfavlwd, lty=myfavlty_cont )
  legend("topleft", legend = "Masc. \nPlur.", text.col=col_mp, bty="n")
  
  plot(density(alln_ms$logtoken), col=col_ms, lwd=myfavlwd, ylim=mymostcommon_ylim,
       main="",
       axes=myaxesset)
  lightaxes
  lines(density(control_ms$logtoken), col=col_ms, lwd=myfavlwd, lty=myfavlty_cont )
  title("Frequency of nouns in the All nouns vs. the Control Sample", line = -1.1, outer = TRUE)
  legend("topleft", legend = "Masc. \nSing.", text.col=col_ms, bty="n")
  }
mygraph_controlsample_features


# another way to visualize the same comparison 
# distribution of nouns in the control sample #--------------- mygraph_controlsample_features_superimposed

#plot graph with all the features of the control sample
par(mfrow=c(1,1))
mygraph_controlsample_features_superimposed %<a-% { 
  plot(density(control_fp$logtoken), 
       ylim=mymostcommon_ylim, 
       xlim=mymostcommon_xlim, 
       col= col_fp, 
       lwd=myfavlwd,
       lty=myfavlty_cont,
       main="Control Sample", xlab="Token frequency of occurrence (log)", 
       axes=myaxesset)
  lightaxes
  lines(density(control_fs$logtoken), col=col_fs, lwd=myfavlwd, lty=myfavlty_cont)
  lines(density(control_mp$logtoken), col=col_mp, lwd=myfavlwd, lty=myfavlty_cont)
  lines(density(control_ms$logtoken), col=col_ms, lwd=myfavlwd, lty=myfavlty_cont)
  legend("topright", inset=.02, bty= "n", #title="Inflection",
         legend = legendcontent, fill=pal_01,  cex=0.8)
  
}
mygraph_controlsample_features_superimposed

# call plot of token frequency -density - all nouns to compare 
mygraph_dens_allnouns


# entropy in the control sample

# count: frequency observed in control sample (type)
typefreq_cont=table(full_contsamp$POS)/length(full_contsamp$POS)
entropy_type_cont= -(sum((typefreq_cont)*log2(typefreq_cont)))
entropy_type_cont=round(entropy_type_cont, 3)

# count: frequency observed in control sample (token)
# sum partials 
tokenfreq_cont_fp=sum(control_fp$Freq)
tokenfreq_cont_fs=sum(control_fs$Freq)
tokenfreq_cont_mp=sum(control_mp$Freq)
tokenfreq_cont_ms=sum(control_ms$Freq)

#check with total sum
tokenfreq_total_cont=sum(full_contsamp$Freq)
tokenfreq_cont_fp+tokenfreq_cont_fs+tokenfreq_cont_mp+tokenfreq_cont_ms==tokenfreq_total_cont

# entropy: token - control sample
freq_tokens_cont=c(tokenfreq_cont_fp, 
                   tokenfreq_cont_fs, 
                   tokenfreq_cont_mp, 
                   tokenfreq_cont_ms)/tokenfreq_total_cont
entropy_token_cont= -sum(freq_tokens_cont*log2(freq_tokens_cont))
entropy_token_cont=round(entropy_token_cont, 3)  




###
#                                   Entropy of Morphological Systems    - EMS 01     
#
#           5 -  C O M P A R E   D I S T R I B U T I O N S  
#
#   
#
###

#
#  sum up of distributions  
#

# allnouns - type -
count_allnouns_type=c(length(alln_fp$Form), length(alln_fs$Form), length(alln_mp$Form), length(alln_ms$Form))  
probs_allnouns_type=round(count_allnouns_type/length(datallnouns$Form),3)

# allnouns - token - 
count_allnouns_token=c(tokenfreq_alln_fp, tokenfreq_alln_fs,tokenfreq_alln_mp, tokenfreq_alln_ms)
probs_allnouns_token=round((count_allnouns_token/tokenfreq_total_alln), 3)

# animate nouns - type -
count_animnouns_type=c(length(anim_fp$Form), length(anim_fs$Form), length(anim_mp$Form), length(anim_ms$Form))
probs_animnouns_type= round(count_animnouns_type/length(dat_anim$Form),3)

# animate nouns - token -
count_animnouns_token=c(tokenfreq_anim_fp, tokenfreq_anim_fs, tokenfreq_anim_ms, tokenfreq_anim_mp)
probs_animnouns_token=round((count_animnouns_token/tokenfreq_total_anim), 3)

# control sample - type
count_contsamp_type=c(length(control_fp$Form), length(control_fs$Form), length(control_mp$Form), length(control_ms$Form))
probs_contsamp_type=round(count_contsamp_type/length(full_contsamp$Form), 3)

# control sample - token
count_contsamp_token=c(tokenfreq_cont_fp, tokenfreq_cont_fs, tokenfreq_cont_mp, tokenfreq_cont_ms)
probs_contsamp_token=round((count_contsamp_token/tokenfreq_total_cont),3)

# discrete uniform distribution
freqs_uni=round(freqs_uni, 3) 
count_freqs_uni=freqs_uni*univec_length

# create a df collecting the raw counts of all samples
count_tab_samples= rbind(count_freqs_uni,
                         count_allnouns_type, count_allnouns_token,
                         count_animnouns_type, count_animnouns_token, 
                         count_contsamp_type, count_contsamp_token)
count_tab_samples=as.data.frame(count_tab_samples)
colnames(count_tab_samples)= legendcontent

# create a df collecting the probability distributions of all samples
probs_tab_samples=rbind(freqs_uni,
      probs_allnouns_type, probs_allnouns_token,
      probs_animnouns_type, probs_animnouns_token, 
      probs_contsamp_type, probs_contsamp_token 
      )
probs_tab_samples=as.data.frame(probs_tab_samples)
colnames(probs_tab_samples)=legendcontent

#
#  entropy in different distributions
#

# uniform - token
entropy_uni
# all nouns - type
entropy_type_alln
# all nouns - token
entropy_token_alln
# animate - type
entropy_type_anim
# animate - token
entropy_token_anim
# control_sample_type
entropy_type_cont
# control_sample_token
entropy_token_cont

# create a df collecting the entropy of all distibutions
entropy_b=rbind(entropy_uni, 
                entropy_type_alln, 
                entropy_token_alln, 
                entropy_type_anim, 
                entropy_token_anim,  
                entropy_type_cont, 
                entropy_token_cont)
colnames(entropy_b)= "value"

# visualize results 
count_tab_samples
probs_tab_samples
entropy_b


#
#  Kullback-Leibler Divergence
#

# compare distr of all nouns to reference 
# type
LaplacesDemon::KLD(freqs_uni, probs_allnouns_type)
# token
LaplacesDemon::KLD(freqs_uni, probs_allnouns_token)


# compare distr of animate nouns to reference 
# type
LaplacesDemon::KLD(freqs_uni, probs_animnouns_type)
# token
LaplacesDemon::KLD(freqs_uni, probs_animnouns_token)




###
#           O U T P U T S                       
#
#             
#
###


# Graphs
# generate a vector collecting the name of the graphs to plot
complete_graphlist_EMS01=ls()[grep(("mygraph"), ls())]


# control sample list

setwd(wd_1)
write.csv(full_contsamp, "control_sample_nouns_ITA.csv")

