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


# 02 - Compute Context Entropy 
#
# v 1.0.0
# 
# https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Full


# this is the second of the series of analysis scripts for the
# EMS entropy in Morphological Systems 
#

# in this script, the entropy of the text string surrounding the nouns is measured 
# the text files have been downloaded from https://cqpweb.lancs.ac.uk/itwac/
# using the cqp tools for query and download

###
#                                   Entropy of Morphological Systems    - EMS 02   
#
#           S E T   I N P U T S  
#
###

# required packages: readr, tools, stringr

library(readr)
library(tools)
library(stringr)

# number of decimal places for rounded numbers
roundnum=3

## input directories 

# wd_code is the directory where this code is stored:
wd_code="C:\\Users\\FF\\Documents\\Analisi varie\\Inflectional Entropy ITA\\Animacy and Morphology 0_0_1"
setwd(wd_code)

# wd_1 and wd_2 are the subdirectories of wd_code where the input data are stored
#
# input data in wd1 consists in the csvs collecting the nouns in the animate and control samples 
wd_1=paste0(wd_code, "\\wd1")
# input data in wd2 consist in texts containing the target nouns from animate and control sample
wd_2=paste0(wd_code, "\\wd2")

# wd_3 is the subdirectory of wd_code where the output data are stored
wd_3=paste0(wd_code, "\\wd3")



#
# Import animate nouns

setwd(wd_1)
anim_nouns=read.csv("animate_nouns_tagged_ITA.csv", sep=",", T)

anim_nouns$nountype=rep("animate")
anim_nouns$X=NULL
# column with log frequency
anim_nouns$logtoken=log(anim_nouns$Freq)

# column  with base 
anim_nouns$base=gsub('.{1}$', '_', anim_nouns$lemma)

# all of the nouns have 4 forms 
table(anim_nouns$base)
summary(table(anim_nouns$base)==4)

#
# Import control nouns

#control_nouns=read.csv("contsamp2.csv", T)
control_nouns=read.csv("control_sample_tagged_ITA.csv")
control_nouns$X=NULL

#
# Proceed to calculate Context entropy 
# on Itwac text files downloaded from CQPweb https://cqpweb.lancs.ac.uk/itwac/
# 

# set input directory
setwd(wd_2)

# get file names 
file.names=dir()[grep("10.txt$", dir())]

# create an empty object to create a df
H_anim_nouns=NULL
H_control_nouns=NULL

for(myfile in file.names){
  #assign the name of the target word from file name
  thename=tools::file_path_sans_ext(myfile)
  #this takes away the 10_10 from the file name, when present
  thename= gsub("10_10", "", thename)
  
  #
  # Preprocessing
  #
  
  # careful: this is for txt files from Itwac 
  # files from other corpora may have different features (punctuation, separators etc.)
  #
  
  # text is imported as a long character string
  dat0= read_file(myfile)
  # the target noun is substituted 
  dat0= gsub(thename, "myword", dat0)
  
  # separate strings using tabs
  dat= strsplit(dat0, "\t")
  # remove punctuation
  imported_text = gsub("[[:punct:]]", "", dat)
  # remove residual double bars
  imported_text = gsub("\\\\", "", imported_text)
  # break into single words
  split_text = strsplit(imported_text, " ")
  split_text = unlist(split_text)
  
  # create an object for analysis on function words
  # split_text2=split_text
  
  #
  # Metrics for Lexical words 
  # 
  
  # lexical words
  # collect all words tagged as nouns, case-sensitive. tags in Itwac are like: boscoNOUN
  txt2= split_text[grepl("NOUN$", split_text, ignore.case = F)]
  #add underscore before tag --> bosco_NOUN 
  txt2= gsub("NOUN$", "_NOUN$", txt2)
  
  # adverbs
  txt3= split_text[grepl("ADV$", split_text,  ignore.case = F)]
  txt3= gsub("ADV$", "_ADV$", txt3)
  
  # adjectives
  txt4= split_text[grepl("ADJ$", split_text,  ignore.case = F)]
  txt4= gsub("ADJ$", "_ADJ$", txt4)
  
  # verbs. careful: remember the "$" operator to signal the end of a string, or it will collect words with "VER" in the middle ("DIVERSO")
  txt5= split_text[grepl("VER$", split_text,  ignore.case = F)]
  txt5= gsub("VER$", "_VER$", txt5)
  
  #concatenate vector with these words
  txt6= c(txt2, txt3, txt4,txt5)
  #lowercase everything to avoid false double hits (Bosco, bosco, BOSCO)
  txt6= tolower(txt6)
  #discard target word
  txt7= txt6[!grepl("myword", txt6)]
  
  clean_text=txt7
  #summary(clean_text)
  
  #total token frequency
  total_token= length(clean_text)
  #total unique types 
  word_types= unique(clean_text)
  #total number of types
  total_types= length(word_types)

  # type-token ratio of contexts - lexical words
  ttr=(total_types/total_token)*100
  typetoken_ratio=round(ttr,roundnum)
  
  # context entropy-  lexical words
  freq_types<-table(clean_text)/length(clean_text)
  entropy =-sum(freq_types*log2(freq_types))	
  entropy=round(entropy, roundnum)
  
  
  #
  # Metrics for Function words 
  #
  # these measures are not used in the present study - uncomment if you like to use them for your analysis
  # 
  # # proceed as above but on words tagged as functional
  # # take anything tagged as article, conjunction, determiner, negation, wh. Other function words (auxiliary, pronouns...) have not been included due to messy tagging
  # txtFUN02=split_text2[grepl("ART$", split_text2,  ignore.case = F)]
  # txtFUN03=split_text2[grepl("CON$", split_text2,  ignore.case = F)]
  # txtFUN04=split_text2[grepl("DET$", split_text2,  ignore.case = F)]
  # txtFUN05=split_text2[grepl("NEG$", split_text2,  ignore.case = F)]
  # txtFUN06=split_text2[grepl("WH$", split_text2,  ignore.case = F)]
  # txtFUN07=c(txtFUN02,txtFUN03,txtFUN04,txtFUN05,txtFUN06)
  # 
  # # discard residual lexical words
  # txtFUN08=txtFUN07[!grepl("NOUN$", txtFUN07, ignore.case = F)]
  # txtFUN09=txtFUN08[!grepl("ADJ$", txtFUN08, ignore.case = F)]
  # txtFUN10=txtFUN09[!grepl("ADV$", txtFUN09, ignore.case = F)]
  # txtFUN11=txtFUN10[!grepl("VER$", txtFUN10, ignore.case = F)]
  # 
  # # insert underscore before POS tag
  # txtFUN12 <- gsub("ART$", "_ART$", txtFUN11, ignore.case = F)
  # txtFUN13 <- gsub("CON$", "_CON$", txtFUN12, ignore.case = F)
  # txtFUN14 <- gsub("DET$", "_DET$", txtFUN13, ignore.case = F)
  # txtFUN15 <- gsub("NEG$", "_NEG$", txtFUN14, ignore.case = F)
  # txtFUN16 <- gsub("WH$", "_WH$", txtFUN15, ignore.case = F)
  # 
  # # concatenate all function words
  # split_textFUN=c(txtFUN12,txtFUN13,txtFUN14,txtFUN15,txtFUN16)
  # 
  # # lowercase everything to avoid false double hits (Questo, QUESTO, questo)
  # split_textFUN=tolower(split_textFUN) #avoid fake double hits
  # 
  # total_tokenFUN  = length(split_textFUN)
  # word_typesFUN = unique(split_textFUN)
  # total_typesFUN = length(word_typesFUN)
  # 
  # # type-token ratio of contexts - function words
  # ttrFUN=(total_typesFUN/total_tokenFUN)*100
  # typetoken_ratioFUN=round(ttrFUN,roundnum)
  # 
  # # context entropy: functional words
  # freq_typesFUN= table(split_textFUN)/length(split_textFUN)
  # entropyFUN =-sum(freq_typesFUN*log2(freq_typesFUN))	
  # entropyFUN=round(entropyFUN, roundnum)
  # 
  
  
  # 
  # Metrics for all words (lex+fun)
  #
  # these measures are not used in the present study - uncomment if you like to include them 
  
  
  # same procedure as above
  # careful. Itwac is messy, so this is not on all tagged words but on the sum of the lexical and function words prevoiusly collected. Some function words may therefore be missing
  # 
  # # concatenate string of characters with function words + string of characters with lexical words
  # txtALL=c(txtFUN16, txt7)
  # 
  # split_textALL=txtALL
  # 
  # # lowercase to avoid fake double hits
  # split_textALL=tolower(split_textALL) 
  # 
  # total_tokenALL  = length(split_textALL)
  # word_typesALL = unique(split_textALL)
  # total_typesALL = length(word_typesALL)
  # 
  # # total_typesALL/total_tokenALL
  # ttrALL=(total_typesALL/total_tokenALL)*100
  # typetoken_ratioALL=round(ttrALL,roundnum)
  # 
  # # entropy all words
  # freq_typesALL=table(split_textALL)/length(split_textALL)
  # entropyALL =-sum(freq_typesALL*log2(freq_typesALL))	
  # entropyALL=round(entropyALL, roundnum)
  # 
  
  # create a data frame with all the selected variables 
  # some of the available variables are not included in the final analysis - uncomment if needed
  myword_data=cbind(thename, 
                    #total_types, 
                    #total_token, 
                    typetoken_ratio, 
                    entropy #, 
                    #total_typesFUN, 
                    #total_tokenFUN, 
                    #typetoken_ratioFUN, 
                    #entropyFUN, 
                    #typetoken_ratioALL, 
                    #entropyALL
                    )
  #row.names(myword_data) <- thename
  
  if( thename %in% anim_nouns$Form== T){
  temp1=myword_data
  H_anim_nouns=rbind(temp1, H_anim_nouns)  
    
  } else if (thename %in% control_nouns$Form==T){
  temp2=myword_data
  H_control_nouns=rbind(temp2, H_control_nouns)  
  }
  
}  
  

#
# Outputs
#
#


setwd(wd_3)

# merge a dataframe with data for animate nouns
anim_nouns_H=merge(anim_nouns, H_anim_nouns, by.x="Form", by.y="thename")
write.csv(anim_nouns_H, "Animate_sample_H_ITA.csv")

# merge a dataframe with data for control nouns
control_nouns_H=merge(control_nouns, H_control_nouns, by.x="Form", by.y="thename")
write.csv(control_nouns_H, "Control_sample_H_ITA.csv")

setwd(codewd)


