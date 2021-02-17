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


# EMS COMPACT
#
# v. Compact 1.0.0
# 
# https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Compact


#
# this script contains the analysis of the EMS study:

# 1) the distribution of nouns across inflectional features is compared 
# in all nouns and in the sample of animate nouns vs the maximum entropy of a reference distribution
# 2) the context entropy of nouns across inflectional features is compared 
# in the sample of animate nouns and in a control sample randomly drawn from all noun

# careful! this script does not contain the preprocessing of text files and the calculation of context entropy
# please refer to the scripts of the EMS -FUll version for preprocessing : 
# https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Full



###
#                                   Entropy of Morphological Systems    - EMS Compact   
#
#           S E T   I N P U T S  
#
#   
#
###

# required packages: pryr, stringr, car, treemap, LaplacesDemon, kolmim, multcomp, tidyverse

library(pryr)

## input required: graphical parameters 

# palette
col_fp = "#00008B" # "blue4" 
col_fs = "#00B2EE" # "deepskyblue2" 
col_mp = "#CD2626" # "firebrick3"  
col_ms = "#FF8C00" # "darkorange"  
pal_01=c(col_fp, col_fs, col_mp, col_ms)

col_ref= "#008B45" # "springgreen4"
col_borders= "#FFFFFF" # "white"

myfavlinescol1="#4A4A4A" #"grey29"

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
myfavlty_ref=6

# line density and angle
myfavdensity=75
myfavlineangle=45
#

# some common features of graphics axes
ticks_h1=c(seq.int(1:4))
ticks_g1=c(seq.int(0,16,2))
mymostcommon_xlim=c(0, 16)
mymostcommon_ylim=c(0, 0.4)


# plots with box
#myaxesset=T
#rm(lightaxes)
#lightaxes=NULL

# plots with axes on left and bottom positions
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

# number of decimal places for rounded numbers
roundnum=3

## input directories 

# this is the directory where this code is stored:
wd_code="C:\\Users\\FF\\Documents\\Analisi varie\\Inflectional Entropy ITA\\Animacy and Morphology 0_0_1\\compact_v"
setwd(wd_code)

# this is the subdirectory of wd_code where the data are stored
# input data consist in list with all nouns, list with animate nouns
wd_in=paste0(wd_code, "\\wd_in")

# this is the subdirectory of wd_code to save graphics to
wd_graphs=paste0(wd_code, "\\Graphics") 
#dir.create(wd_graphs)



###
#                                   Entropy of Morphological Systems    - EMS Compact
#
#             I M P O R T   D A T A 
#
#   
#
###

setwd(wd_in)

# Import all nouns
datallnouns_imported=read.csv("all_nouns_tagged_ITA.csv", sep=",", T)

# Import animate nouns
dat_anim= read.csv("Animate_sample_H_ITA.csv", T)

# subtract the animate sample from all nouns
datallnouns=datallnouns_imported[!datallnouns_imported$Form %in% dat_anim$Form, ]

# Import control nouns
dat_cont= read.csv("Control_sample_H_ITA.csv", T)




###
#                                   Entropy of Morphological Systems    - EMS Compact     
#     I:
#         C O M P A R E 
#           
#                   N O U N S 
#
#                         D I S T R I B U T I O N S
#
#
###



###                               Entropy of Morphological Systems    - EMS Compact  
#                                                                       I: Compare Nouns Distributions
#
#           1 - R E F E R E N C E   D I S T R I B U T I O N S
#
#   
#
###

# generate uniform distribution, 4 discrete values
# size: approx the size of corpus = tokenfreq_total_alln
univec_length= 10000
uni_values=seq(1:5)
minval=min(uni_values)
maxval=max(uni_values)

uni_d=floor(runif(univec_length, minval, maxval))
str(uni_d)

ticks=c(minval:maxval)
histlim_uni=(((1/maxval)*univec_length)+((1/10)*univec_length))
hist(uni_d, ylim=c(0,histlim_uni),
     border = col_borders,
     main = "Uniform Categorial Distribution",
     #col=col_ref, density = myfavdensity, angle = myfavlineangle,
     axes=myaxesset, 
)
lightaxes

uni_d=as.character(uni_d)

# entropy of the discrete uniform distribution
freqs_uni =table(uni_d)/length(uni_d)
entropy_uni= -sum(freqs_uni * log2(freqs_uni))
entropy_uni=round(entropy_uni, roundnum)
entropy_uni
refline=1/length(unique(uni_d))

freqs_uni4=as.data.frame(freqs_uni)
barplot(freqs_uni4$Freq, 
        border = col_borders,
        #col=col_ref, density = myfavdensity, angle = myfavlineangle,
        ylim = mymostcommon_ylim, 
        main = "Uniform Categorial Distribution"
)
abline(h=refline, col= col_ref, lty=myfavlty_ref, lwd=myfavlwd)
par(mfrow=c(1,1))



###                               Entropy of Morphological Systems    - EMS Compact  
#                                                                       I: Compare Nouns Distributions                                  Entropy of Morphological Systems    - EMS Compact     
#
#           2 - A L L    N O U N S 
#
#   
#
###


#   Import dataset with all nouns in Italian 
#   
# The dataset has been obtained by merging word frequencies collected from Itwac (Baroni et al., 2009) 
#   to a list of morphologically tagged words (Morph-it!, Zanchetta & Baroni, 2005). 
# please refer to the first script of the  Full version for the merging


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
entropy_type_alln=round(entropy_type_alln, roundnum)

# entropy - token - allnouns
freq_tokens_alln=c(tokenfreq_alln_fp, tokenfreq_alln_fs,tokenfreq_alln_mp, tokenfreq_alln_ms)/tokenfreq_total_alln
entropy_token_alln= -sum(freq_tokens_alln*log2(freq_tokens_alln))
entropy_token_alln=round(entropy_token_alln, roundnum)  



# Graph: proportion of types in each feature: all nouns #--------------- mygraph_prop_allnouns_types  

library(treemap)

prop_allnouns_types=as.data.frame(freq_types_all)
prop_allnounscols=c("POS", "Freq")
colnames(prop_allnouns_types)<- prop_allnounscols
prop_allnouns_types$Freq=round(prop_allnouns_types$Freq, roundnum)



#prop_allnouns_types$label <- paste(prop_allnouns_types$POS, prop_allnouns_types$Freq, sep = "\n")
mygraph_prop_allnouns_types %<a-% {
  prop_allnouns_types$label <- paste(legendcontent, " ", prop_allnouns_types$Freq)
  treemap(prop_allnouns_types,
          index=c("label"),
          vSize="Freq",
          type="index",
          title="All nouns - Types",
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
mygraph_prop_allnouns_types



# Graph: proportion of tokens in each feature: all nouns #--------------- mygraph_prop_allnouns_tokens 
prop_allnouns_token=as.data.frame(freq_tokens_alln)
prop_allnouns_token$Freq=prop_allnouns_token$freq_tokens_alln
prop_allnouns_token$POS=legendcontent
prop_allnouns_token$freq_tokens_alln=NULL

prop_allnouns_token$Freq=round(prop_allnouns_token$Freq, roundnum)

mygraph_prop_allnouns_tokens %<a-% {
  #prop_allnouns_token$label <- paste(prop_allnouns_token$POS, prop_allnouns_token$Freq, sep = "\n")
  prop_allnouns_token$label <- paste(legendcontent, " ", prop_allnouns_token$Freq)
  treemap(prop_allnouns_token,
          
          # data
          index=c("label"),
          vSize="Freq",
          type="index",
          title="All nouns - Tokens",
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



###                               Entropy of Morphological Systems    - EMS Compact  
#                                                                       I: Compare Nouns Distributions                                  Entropy of Morphological Systems    - EMS Compact    
#
#           3 - A N I M A T E    N O U N S 
#
#                                   
#
###


summary(dat_anim)
table(dat_anim$base)
summary(table(dat_anim$base)==4)


#remove possible duplicates 
library(tidyverse)
dat_anim$notunique_forms=duplicated(dat_anim$Form)
animdup=dat_anim[which(dat_anim$notunique_forms==T), ]
summary(animdup)
dat_anim=dat_anim[which(dat_anim$notunique_forms==F), ]
dat_anim$notunique_forms=NULL

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
entropy_type_anim=round(entropy_type_anim, roundnum)

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
entropy_token_anim=round(entropy_token_anim, roundnum)  


# Graph: proportion of types in each feature: animate nouns #--------------- mygraph_prop_animnouns_types  

prop_animnouns_types=as.data.frame(typefreq_anim)
prop_cols=c("POS", "Freq")
colnames(prop_animnouns_types)<- prop_cols
prop_animnouns_types$Freq=round(prop_animnouns_types$Freq, roundnum)



#prop_animnouns_types$label <- paste(prop_animnouns_types$POS, prop_animnouns_types$Freq, sep = "\n")
mygraph_prop_animnouns_types %<a-% {
  prop_animnouns_types$label <- paste(legendcontent, " ", prop_animnouns_types$Freq)
  treemap(prop_animnouns_types,
          index=c("label"),
          vSize="Freq",
          type="index",
          title="Animate nouns - Types",
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
          index=c("label"),
          vSize="Freq",
          type="index",
          title="Animate nouns - Tokens",
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



###                               Entropy of Morphological Systems    - EMS Compact  
#                                                                       Compare Nouns Distributions                                   Entropy of Morphological Systems    - EMS Compact     
#
#           4 - C O N T R O L   S A M P L E    N O U N S 
#
#   
#
###

full_contsamp=dat_cont

control_fp= full_contsamp[full_contsamp$POS=="NOUN-F:p", ]
control_fs= full_contsamp[full_contsamp$POS=="NOUN-F:s", ]
control_mp= full_contsamp[full_contsamp$POS=="NOUN-M:p", ]
control_ms= full_contsamp[full_contsamp$POS=="NOUN-M:s", ]


# distance of sampled control nouns from all nouns, within each feature 

wilcox.test(alln_fp$logtoken, control_fp$logtoken)
wilcox.test(alln_fs$logtoken, control_fs$logtoken)
wilcox.test(alln_mp$logtoken, control_mp$logtoken)
wilcox.test(alln_ms$logtoken, control_ms$logtoken)



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
entropy_type_cont=round(entropy_type_cont, roundnum)

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
entropy_token_cont=round(entropy_token_cont, roundnum)  




###                               Entropy of Morphological Systems    - EMS Compact  
#                                                                       I: Compare Nouns Distributions
#
#           5 -  C O M P A R E   D I S T R I B U T I O N S  
#
#
###


#
#  Token frequency across features
#

# animate sample
shapiro.test(anim_fp$logtoken)
shapiro.test(anim_fs$logtoken)
shapiro.test(anim_mp$logtoken)
shapiro.test(anim_ms$logtoken)

# quantile-quantile plots
library(car)
mygraph_qq_cont_nouns%<a-%{
  par(mfrow=c(2,2))
  qqPlot(anim_fp$logtoken, pch=myfavpch, col=col_fp, col.lines = myfavlinescol1, id=F, main = "Control Sample \n Feminine Singular")
  qqPlot(anim_fs$logtoken, pch=myfavpch, col=col_fs, col.lines = myfavlinescol1, id=F, main = "Control Sample \n Feminine Plural")
  qqPlot(anim_mp$logtoken, pch=myfavpch, col=col_mp, col.lines = myfavlinescol1, id=F, main = "Control Sample \n Masculine Singular")
  qqPlot(anim_ms$logtoken, pch=myfavpch, col=col_ms, col.lines = myfavlinescol1, id=F, main = "Control Sample \n Masculine Plural")
}
par(mfrow=c(1,1))
mygraph_qq_cont_nouns

mygraph_qq_anim_nouns%<a-%{
  par(mfrow=c(2,2))
  qqPlot(alln_fp$logtoken, pch=myfavpch, col=col_fp, col.lines = myfavlinescol1, id=F, main = "Animate nouns \n Feminine Singular")
  qqPlot(alln_fs$logtoken, pch=myfavpch, col=col_fs, col.lines = myfavlinescol1, id=F, main = "Animate nouns \n Feminine Plural")
  qqPlot(alln_mp$logtoken, pch=myfavpch, col=col_mp, col.lines = myfavlinescol1, id=F, main = "Animate nouns \n Masculine Singular")
  qqPlot(alln_ms$logtoken, pch=myfavpch, col=col_ms, col.lines = myfavlinescol1, id=F, main = "Animate nouns \n Masculine Plural")
}
par(mfrow=c(1,1))
mygraph_qq_anim_nouns


# distance within features, between samples
ks.test(control_fp$logtoken, anim_fp$logtoken)
ks.test(control_fs$logtoken, anim_fs$logtoken)
ks.test(control_mp$logtoken, anim_mp$logtoken)
ks.test(control_ms$logtoken, anim_ms$logtoken)


# frequency: median
#

medtoken_alln_fp=median(alln_fp$logtoken)
medtoken_alln_fs=median(alln_fs$logtoken)
medtoken_alln_mp=median(alln_mp$logtoken)
medtoken_alln_ms=median(alln_ms$logtoken)

medtoken_anim_fp=median(anim_fp$logtoken)
medtoken_anim_fs=median(anim_fs$logtoken)
medtoken_anim_mp=median(anim_mp$logtoken)
medtoken_anim_ms=median(anim_ms$logtoken)

medtoken_cont_fp=median(control_fp$logtoken)
medtoken_cont_fs=median(control_fs$logtoken)
medtoken_cont_mp=median(control_mp$logtoken)
medtoken_cont_ms=median(control_ms$logtoken)


median_logtoken= rbind(medtoken_alln_fp,
                       medtoken_alln_fs,
                       medtoken_alln_mp, 
                       medtoken_alln_ms, 
                       medtoken_anim_fp, 
                       medtoken_anim_fs,
                       medtoken_anim_mp,
                       medtoken_anim_ms, 
                       medtoken_cont_fp,
                       medtoken_cont_fs,
                       medtoken_cont_mp,
                       medtoken_cont_ms)

colnames(median_logtoken)= "median_token_frequency"
median_logtoken=round(median_logtoken, roundnum)


# frequency: sd
#

sdtoken_alln_fp=sd(alln_fp$logtoken)
sdtoken_alln_fs=sd(alln_fs$logtoken)
sdtoken_alln_mp=sd(alln_mp$logtoken)
sdtoken_alln_ms=sd(alln_ms$logtoken)

sdtoken_anim_fp=sd(anim_fp$logtoken)
sdtoken_anim_fs=sd(anim_fs$logtoken)
sdtoken_anim_mp=sd(anim_mp$logtoken)
sdtoken_anim_ms=sd(anim_ms$logtoken)

sdtoken_cont_fp=sd(control_fp$logtoken)
sdtoken_cont_fs=sd(control_fs$logtoken)
sdtoken_cont_mp=sd(control_mp$logtoken)
sdtoken_cont_ms=sd(control_ms$logtoken)


sd_logtoken= rbind(sdtoken_alln_fp,
                   sdtoken_alln_fs,
                   sdtoken_alln_mp, 
                   sdtoken_alln_ms, 
                   sdtoken_anim_fp, 
                   sdtoken_anim_fs,
                   sdtoken_anim_mp,
                   sdtoken_anim_ms, 
                   sdtoken_cont_fp,
                   sdtoken_cont_fs,
                   sdtoken_cont_mp,
                   sdtoken_cont_ms)

colnames(sd_logtoken)= "sd_token_frequency"
sd_logtoken=round(sd_logtoken, roundnum)


# Df with median and sd 
#
logtoken_distr=cbind.data.frame(median_logtoken, sd_logtoken)
logtoken_distr$feature=str_sub((rownames(logtoken_distr)), -7, -1)
rownames(logtoken_distr)=NULL

#
#  Summary of distributions  
#

# allnouns - type -
count_allnouns_type=c(length(alln_fp$Form), length(alln_fs$Form), length(alln_mp$Form), length(alln_ms$Form))  
probs_allnouns_type=round(count_allnouns_type/length(datallnouns$Form), roundnum)

# allnouns - token - 
count_allnouns_token=c(tokenfreq_alln_fp, tokenfreq_alln_fs,tokenfreq_alln_mp, tokenfreq_alln_ms)
probs_allnouns_token=round((count_allnouns_token/tokenfreq_total_alln), roundnum)

# animate nouns - type -
count_animnouns_type=c(length(anim_fp$Form), length(anim_fs$Form), length(anim_mp$Form), length(anim_ms$Form))
probs_animnouns_type= round(count_animnouns_type/length(dat_anim$Form), roundnum)

# animate nouns - token -
count_animnouns_token=c(tokenfreq_anim_fp, tokenfreq_anim_fs, tokenfreq_anim_ms, tokenfreq_anim_mp)
probs_animnouns_token=round((count_animnouns_token/tokenfreq_total_anim), roundnum)

# control sample - type
count_contsamp_type=c(length(control_fp$Form), length(control_fs$Form), length(control_mp$Form), length(control_ms$Form))
probs_contsamp_type=round(count_contsamp_type/length(full_contsamp$Form), roundnum)

# control sample - token
count_contsamp_token=c(tokenfreq_cont_fp, tokenfreq_cont_fs, tokenfreq_cont_mp, tokenfreq_cont_ms)
probs_contsamp_token=round((count_contsamp_token/tokenfreq_total_cont), roundnum)

# discrete uniform distribution
freqs_uni=round(freqs_uni, roundnum) 
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
colnames(entropy_b)= "entropy"

# visualize results 
logtoken_distr
count_tab_samples
probs_tab_samples
entropy_b


#
#  Kullback-Leibler Divergence
#

# compare distr of all nouns to reference 
# type
distalln__type_ent=LaplacesDemon::KLD(freqs_uni, probs_allnouns_type)
# token
distalln_token_ent=LaplacesDemon::KLD(freqs_uni, probs_allnouns_token)


# compare distr of animate nouns to reference 
# type
distanim_type_ent=LaplacesDemon::KLD(freqs_uni, probs_animnouns_type)
# token
distanim_token_ent=LaplacesDemon::KLD(freqs_uni, probs_animnouns_token)

#collect distance in a df
collected_kld=cbind(distalln__type_ent$intrinsic.discrepancy,
                    distalln_token_ent$intrinsic.discrepancy,
                    distanim_type_ent$intrinsic.discrepancy,
                    distanim_token_ent$intrinsic.discrepancy
)
colnames(collected_kld)=list("allnouns_type", "allnouns_token", "animnouns_type", "animnouns_token")



###
#                                   Entropy of Morphological Systems    - EMS Compact     
#     II:
#         C O M P A R E 
#           
#                   C O N T E X T  
#
#                           E N T R O P Y
#
#
###


# bind anim and control df into a single df
dat_nouns=rbind.data.frame(dat_cont, dat_anim)


library(kolmim)
library(multcomp)
library(car)

# Check for normality (Shapiro-Wilk)

# control sample
shapiro.test(control_fp$entropy)
shapiro.test(control_fs$entropy)
shapiro.test(control_mp$entropy)
shapiro.test(control_ms$entropy)

# quantile-quantile plots

mygraph_qq_cont_nouns%<a-%{
  par(mfrow=c(2,2))
  qqPlot(control_fp$entropy, pch=myfavpch, col=col_fp, col.lines = myfavlinescol1, id=F, main = "Control Sample \n Feminine Singular")
  qqPlot(control_fs$entropy, pch=myfavpch, col=col_fs, col.lines = myfavlinescol1, id=F, main = "Control Sample \n Feminine Plural")
  qqPlot(control_mp$entropy, pch=myfavpch, col=col_mp, col.lines = myfavlinescol1, id=F, main = "Control Sample \n Masculine Singular")
  qqPlot(control_ms$entropy, pch=myfavpch, col=col_ms, col.lines = myfavlinescol1, id=F, main = "Control Sample \n Masculine Plural")
}
par(mfrow=c(1,1))
mygraph_qq_cont_nouns


# animate sample
shapiro.test(anim_fp$entropy)
shapiro.test(anim_fs$entropy)
shapiro.test(anim_mp$entropy)
shapiro.test(anim_ms$entropy)

mygraph_qq_anim_nouns%<a-%{
  par(mfrow=c(2,2))
  qqPlot(anim_fp$entropy, pch=myfavpch, col=col_fp, col.lines = myfavlinescol1, id=F, main = "Animate nouns \n Feminine Singular")
  qqPlot(anim_fs$entropy, pch=myfavpch, col=col_fs, col.lines = myfavlinescol1, id=F, main = "Animate nouns \n Feminine Plural")
  qqPlot(anim_mp$entropy, pch=myfavpch, col=col_mp, col.lines = myfavlinescol1, id=F, main = "Animate nouns \n Masculine Singular")
  qqPlot(anim_ms$entropy, pch=myfavpch, col=col_ms, col.lines = myfavlinescol1, id=F, main = "Animate nouns \n Masculine Plural")
}
par(mfrow=c(1,1))
mygraph_qq_anim_nouns


# distance within features, between samples, pairwise
fp_c_ent_dist= ks.test(control_fp$entropy, anim_fp$entropy)
fs_c_ent_dist= ks.test(control_fs$entropy, anim_fs$entropy)
mp_c_ent_dist= ks.test(control_mp$entropy, anim_mp$entropy)
ms_c_ent_dist= ks.test(control_ms$entropy, anim_ms$entropy)

#
# Tukey HSD
#
model_H1=aov(entropy~POS*nountype, data=dat_nouns)
HSD_1= TukeyHSD(model_H1, ordered=F)
HSD_1df=as.data.frame(HSD_1$`POS:nountype`)

HSD_1df$p_adj=HSD_1df$`p adj`
HSD_1df
HSD_1df_NS=HSD_1df[HSD_1df$p_adj>.05, ]
HSD_1df_NS
HSD_1df_SV=HSD_1df[HSD_1df$p_adj<.05, ]
HSD_1df_SV



# median context entropy 
#

med_c_ent_cont_fp=median(control_fp$entropy)
med_c_ent_cont_fs=median(control_fs$entropy)
med_c_ent_cont_mp=median(control_mp$entropy)
med_c_ent_cont_ms=median(control_ms$entropy)

med_c_ent_anim_fp=median(anim_fp$entropy)
med_c_ent_anim_fs=median(anim_fs$entropy)
med_c_ent_anim_mp=median(anim_mp$entropy)
med_c_ent_anim_ms=median(anim_ms$entropy)

median_c_ent= rbind(med_c_ent_cont_fp, 
                    med_c_ent_cont_fs,
                    med_c_ent_cont_mp,
                    med_c_ent_cont_ms,
                    med_c_ent_anim_fp,
                    med_c_ent_anim_fs,
                    med_c_ent_anim_mp, 
                    med_c_ent_anim_ms
)

colnames(median_c_ent)= "median_context_entropy"
median_c_ent=round(median_c_ent, roundnum)


# frequency: sd
#
sd_c_ent_cont_fp=sd(control_fp$entropy)
sd_c_ent_cont_fs=sd(control_fs$entropy)
sd_c_ent_cont_mp=sd(control_mp$entropy)
sd_c_ent_cont_ms=sd(control_ms$entropy)

sd_c_ent_anim_fp=sd(anim_fp$entropy)
sd_c_ent_anim_fs=sd(anim_fs$entropy)
sd_c_ent_anim_mp=sd(anim_mp$entropy)
sd_c_ent_anim_ms=sd(anim_ms$entropy)



sd_c_ent= rbind( sd_c_ent_cont_fp,
                 sd_c_ent_cont_fs,
                 sd_c_ent_cont_mp,
                 sd_c_ent_cont_ms,
                 sd_c_ent_anim_fp, 
                 sd_c_ent_anim_fs,
                 sd_c_ent_anim_mp,
                 sd_c_ent_anim_ms) 

colnames(sd_c_ent)= "sd_context_entropy"
sd_c_ent=round(sd_c_ent, roundnum)


# Df with median and sd 
#
c_ent_distr=cbind.data.frame(median_c_ent, sd_c_ent)
c_ent_distr$feature=str_sub((rownames(c_ent_distr)), -7, -1)
rownames(c_ent_distr)=NULL



# context entropy: density graph: control sample #--------------- mygraph_dens_cont_contextentropy
par(mfrow=c(1,1))
mygraph_dens_cont_contextentropy  %<a-% { 
  plot(density(control_fp$entropy), 
       xlim=c(0,16),
       ylim=c(0, 0.5), 
       col=col_fp, 
       lwd=myfavlwd, 
       lty=myfavlty_fp,
       xlab="Context entropy (bits)", 
       main ="Control nouns - Context Entropy", 
       axes=myaxesset)
  lightaxes
  lines(density(control_fs$entropy), lwd=myfavlwd, col=col_fs, lty= myfavlty_fs)
  lines(density(control_mp$entropy), lwd=myfavlwd, col=col_mp, lty= myfavlty_mp)
  lines(density(control_ms$entropy), lwd=myfavlwd, col=col_ms, lty= myfavlty_ms)
  legend("topleft", inset=.02,# title="Features", 
         legend = legendcontent,
         fill=pal_01, ncol=1,  cex=0.8, bty = "n")
}
mygraph_dens_cont_contextentropy


# context entropy: density graph: animate nouns sample #--------------- mygraph_dens_cont_contextentropy

mygraph_dens_anim_contextentropy%<a-% { 
  plot(density(anim_fp$entropy), 
       xlim=c(0,16),
       ylim=c(0, 0.5),
       col="blue4",
       lwd=2.5, 
       lty=myfavlty_fp,
       xlab="Context entropy (bits)", 
       main ="Animate nouns - Context Entropy", 
       axes=myaxesset)
  lightaxes
  lines(density(anim_fs$entropy), lwd=myfavlwd, col=col_fs, lty= myfavlty_fs)
  lines(density(anim_mp$entropy), lwd=myfavlwd, col=col_mp, lty= myfavlty_mp)
  lines(density(anim_ms$entropy), lwd=myfavlwd, col=col_ms, lty= myfavlty_ms)
  legend("topleft", inset=.02,# title="Features", 
         legend = legendcontent,
         fill=pal_01, ncol=1,  cex=0.8, bty = "n")
}
mygraph_dens_anim_contextentropy




###                               Entropy of Morphological Systems    - EMS Compact  
#                                                                       
#
#           S U M   U P  
#
#
###

#
#  Sum up of obtained metrics 
# 

# I: Compare Nouns Distributions

# median, sd o frequency
logtoken_distr

# probability distributions, entropy
count_tab_samples
probs_tab_samples
entropy_b

# kld
distalln__type_ent
distalln_token_ent
distanim_type_ent
distanim_token_ent

collected_kld

# II: Compare Context Entropy
c_ent_distr
fp_c_ent_dist
fs_c_ent_dist
mp_c_ent_dist
ms_c_ent_dist

# dataframe with collected entropy measures (anim and control)
as.data.frame(t(merge(logtoken_distr, c_ent_distr)))



###
#                                   Entropy of Morphological Systems    - EMS Compact     
#             
#           
#                   P R I N T  
#
#                           G R A P H S
#
#
###

setwd(wd_graphs)

# this script prints hi res graphs for EMS analysis (Compact version) 
# please run the whole EMS - Compact script prior to use this code

# this code is derived from Graphresize, a general script to print/resize graphs, available at
# https://github.com/franfranz/Graphs_and_Pics_Toytools/blob/main/Graph_resize_1_0_2.R


#----
#   Index  


#--------------- mygraph_controlsample_features              
#--------------- mygraph_controlsample_features_superimposed
#--------------- mygraph_dens_allnouns                       
#--------------- mygraph_dens_anim_contextentropy          
#--------------- mygraph_dens_animnouns                     
#--------------- mygraph_dens_cont_contextentropy          
#--------------- mygraph_prop_allnouns_tokens                
#--------------- mygraph_prop_allnouns_types               
#--------------- mygraph_prop_animnouns_token                
#--------------- mygraph_prop_animnouns_types              
#--------------- mygraph_prop_disp_nouns_types               
#--------------- mygraph_qq_anim_nouns                      
#--------------- mygraph_qq_cont_nouns

complete_graphlist=ls()[grep(("mygraph"), ls())]


# and generate a vector collecting the name of the graphs to plot
graphlist=complete_graphlist

# choose the plots you would like to print/save
#graphlist=complete_graphlist[c(1,3)]


# INPUT REQUIRED: choose graphical settings:
#
# size of output images: uncomment your preference 

#imagesize= "small" # gset1, for small, low-quality portable images
imagesize= "medium" # gset2, average 
#imagesize= "big" # gset3 is high-res raster image (for poster printing)

g_type="cairo" 
g_units="px" 

# gset 1 
g1_width=600 
g1_height=600 
g1_pointsize=12 
g1_res=100
rescom1=png
resext_1=".png"

# gset 2 
g2_width=1200 
g2_height=1200 
g2_pointsize=12 
g2_res=200
rescom2=jpeg
resext_2=".jpeg"

# gset 3 
g3_width=2400 
g3_height=2400 
g3_pointsize=10 
g3_res=800
rescom3=tiff
resext_3=".tiff"

if (imagesize=="small"){
  g_width=  g1_width
  g_height= g1_height 
  g_pointsize= g1_pointsize 
  g_res= g1_res
  rescom=rescom1
  resext=resext_1
} else if (imagesize=="medium") {
  g_width=  g2_width
  g_height= g2_height 
  g_pointsize= g2_pointsize 
  g_res= g2_res
  rescom=rescom2
  resext=resext_2
} else if (imagesize=="big") {
  g_width=  g3_width
  g_height= g3_height 
  g_pointsize= g3_pointsize 
  g_res= g3_res
  rescom=rescom3
  resext=resext_3
} else {
  print("please select image size - line 1170-1172")
}


# save all graphs as images
for (eachgraph in graphlist) {
  thename=as.character(eachgraph)
  thefilename=paste0(thename, resext)
  rescom(filename=thefilename, 
         type=g_type, 
         units=g_units, 
         width=g_width, 
         height=g_height, 
         pointsize=g_pointsize, 
         res=g_res
  )
  eval(str2lang(eachgraph))
  dev.off()
}

# save one of the graphs: "mygraph2"
# thename="mygraph2"
# thefilename=paste0(thename, resext)
# rescom(filename=thefilename, 
#        type=g_type, 
#        units=g_units, 
#        width=g_width, 
#        height=g_height, 
#        pointsize=g_pointsize, 
#        res=g_res
# )
# mygraph2
# dev.off()
# 







