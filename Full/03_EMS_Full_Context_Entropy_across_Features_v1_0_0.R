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


# 03 - Context Entropy of Features
#
# v. FULL 1.0.0
# 
# https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Full


# this is the fourth script of the EMS analysis series   
# EMS Full version: EMS-00, EMS-01, EMS-02, EMS-03, Graph-EMS 

# in this script, the context entropy is compared 
# in the sample of animate nouns vs control nouns
# 


###
#                                   Entropy of Morphological Systems    - EMS 03   
#
#           S E T   I N P U T S  
#
#   
#
###

# required packages: pryr, LaplacesDemon, kolmim, stringr, car, multcomp

library(pryr)

## input required: graphical parameters 

# palette
col_fp = "#00008B" # "blue4" 
col_fs = "#00B2EE" # "deepskyblue2" 
col_mp = "#CD2626" # "firebrick3"  
col_ms = "#FF8C00" # "darkorange"  
pal_01=c(col_fp, col_fs, col_mp, col_ms)

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


## input directories 

# wd_code is the directory where this code is stored:
wd_code="C:\\Users\\FF\\Documents\\Analisi varie\\Inflectional Entropy ITA\\Animacy and Morphology 0_0_1"
setwd(wd_code)

# wd_3 is the subdirectory of wd_code where the input data are stored
wd_3=paste0(wd_code, "\\wd3")


###
#                                   Entropy of Morphological Systems    - EMS 03     
#
#           1 -  I M P O R T   D A T A 
#
#   
#
###

setwd(wd_3)

# Import control nouns
#dat_cont= read.csv("Control_sample_H_ITA.csv", T)
dat_cont= read.csv("Control_sample_H_ITA.csv", T)

# Import animate nouns
#dat_anim= read.csv("Animate_sample_H_ITA.csv", T)
dat_anim= read.csv("Animate_sample_H_ITA.csv", T)
dat_anim$logtoken=log(dat_anim$FREQ)

#
#   Split across INFLECTIONAL FEATURES - Control nouns
#

# control Fem_plur
control_fp= dat_cont[dat_cont$POS=="NOUN-F:p", ]
summary(control_fp)
control_fp$POS=as.factor(as.character(control_fp$POS))

# control Fem_sing
control_fs= dat_cont[dat_cont$POS=="NOUN-F:s", ]
summary(control_fs)
control_fs$POS=as.factor(as.character(control_fs$POS))

# control Masc_plur
control_mp= dat_cont[dat_cont$POS=="NOUN-M:p", ]
summary(control_mp)
control_mp$POS=as.factor(as.character(control_mp$POS))

# control Masc_sing
control_ms= dat_cont[dat_cont$POS=="NOUN-M:s", ]
summary(control_ms)
control_ms$POS=as.factor(as.character(control_ms$POS))

#
#   Split across INFLECTIONAL FEATURES - Animate nouns
#

# anim Fem plur
anim_fp= dat_anim[dat_anim$POS=="NOUN-F:p", ]
summary(anim_fp)
anim_fp$POS=as.factor(as.character(anim_fp$POS))
length(anim_fp$POS)

# anim Fem sing
anim_fs= dat_anim[dat_anim$POS=="NOUN-F:s", ]
summary(anim_fs)
anim_fs$POS=as.factor(as.character(anim_fs$POS))
length(anim_fs$POS)

# anim Masc plur
anim_mp= dat_anim[dat_anim$POS=="NOUN-M:p", ]
summary(anim_mp)
anim_mp$POS=as.factor(as.character(anim_mp$POS))
length(anim_mp$POS)

# anim Masc sing
anim_ms= dat_anim[dat_anim$POS=="NOUN-M:s", ]
summary(anim_ms)
anim_ms$POS=as.factor(as.character(anim_ms$POS))
length(anim_ms$POS)


# bind anim and control df into a single df
dat_nouns=rbind.data.frame(dat_cont, dat_anim)



###
#                                   Entropy of Morphological Systems    - EMS 03     
#
#           2 -  C O M P A R E   D I S T R I B U T I O N S  
#
#   
#
###
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
HSD_1df=as.data.frame(tthsd1$`POS:nountype`)

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



# context entropy: density graph: control sample #--------------- mygraph_dens_cont_contextentropy
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


#
#  Sum up of obtained metrics 
#

median_c_ent
sd_c_ent
fp_c_ent_dist
fs_c_ent_dist
mp_c_ent_dist
ms_c_ent_dist




# Graphs
# generate a vector collecting the name of the graphs to plot
complete_graphlist_EMS03=ls()[grep(("mygraph03"), ls())]





