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


# 04 EMS - Print graphs
#
# v. FULL 1.0.0
# 
# https://github.com/franfranz/Morphological_Systems_Entropy/tree/main/Full

# this script prints hi res graphs for the EMS script series (Full version) 
# please run EMS scripts prior to use this code

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


#
# INPUT REQUIRED: paths
# 
#

# this is the directory where the code for analysis is stored:
wd_code="PATH"
setwd(wd_code)

# this is the subdirectory of wd_code to save graphics to
wd_graphs=paste0(wd_code, "\\Graphics") 
setwd(wd_graphs)


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
  print("please select image size - line 65-67")
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
