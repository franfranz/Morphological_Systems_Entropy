# Morphological Systems Entropy: Nominal Inflection
Code and data for the study "Entropy of morphological systems is modulated by functional and semantic properties" by F. Franzon (@franfranz) and C. Zanini (@chzani)  - [Article](https://doi.org/10.1080/09296174.2022.2063501) - [Preprint](https://psyarxiv.com/qyd4g).

## Contents

### [Compact version](https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Compact)
This version includes the final dataset used in the study and the code for analysis and graph printing. 

#### Code 
* [EMS Compact](https://github.com/franfranz/Morphological_Systems_Entropy/blob/main/Compact/EMS_Compact_v1_3_1.R)

#### Data 
Datasets of ll nouns, animate sample, control sample [(wd_in)](https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Compact/wd_in)


### [Full version](https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Full)
This version includes the code for analysis and the code to preprocess the data and obtain the dataset analyzed in the study. 

#### Code
* [00 EMS Full - Merge Text Resources](https://github.com/franfranz/Morphological_Systems_Entropy/blob/main/Full/00_EMS_Full_Merge_text_resources_v1_0_0.R)
* [01 EMS Full - Compare Noun distributions](https://github.com/franfranz/Morphological_Systems_Entropy/blob/main/Full/01_EMS_Compare_Noun_Distributions_v1_0_0.R)
* [02 EMS Full - Compute Context Entropy](https://github.com/franfranz/Morphological_Systems_Entropy/blob/main/Full/02_EMS_Full_Compute_Context_Entropy_v1_0_0.R)
* [03 EMS Full - Context Entropy across Features](https://github.com/franfranz/Morphological_Systems_Entropy/blob/main/Full/03_EMS_Full_Context_Entropy_across_Features_v1_0_0.R)
* [04 EMS Full - Print Graphs](https://github.com/franfranz/Morphological_Systems_Entropy/blob/main/Full/04_EMS_Full_Print_graphs_v1_0_0.R)

#### Data 
* Language Resources used in the study (from Itwac and Morph-it![1;2]) [(wd0)](https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Full/wd0).
* Merged dataset with all nouns, animate sample, control sample [(wd1)](https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Full/wd1).
* Text files collecting the 10-word-windows surrounding the words in the animate and control sample, collected from the Itwac corpus [(wd2)](https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Full/wd2). Please note that this folder only contains a mini sample of nouns. The full dataset is available on request
* Datasets of animate sample and control sample, with Context Entropy measures [(wd3)](https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Full/wd3).

[1] Baroni, M., Bernardini, S., Ferraresi, A., and Zanchetta, E. (2009). The wacky wide web: a collectionof very large linguistically processed web-crawled corpora.Language resources and evaluation,43(3):209–226; [2] Zanchetta, E. and Baroni, M. (2005).  Morph-it!  a free corpus-based morphological resource for theitalian language.Corpus Linguistics, 1(1):2005
