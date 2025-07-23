setwd("/Users/carla/Dropbox (UFL)/WORK/PAPERS/2022 WNV LONG/MNS/0824_tree with ref all/new_Tree/")

library(ggtree)
library(ape)
library(ggplot2)
library(colorspace)
library(Biostrings)
library(phytools)
library(treeio)
library(dplyr)
library(readr)
library(tidyr)
library(reshape2)

#### DROP TIP OUTLIERS ####
ml_tree <- read.tree("WNV-ALL-0824-filtered.westremoved.tree")
ml_tree2 = drop.tip(ml_tree, tip = c("452-HB_S2944_146", 
                                     "452-MB_S1877_323", 
                                     "452-TH_S1580_346", 
                                     "456-CL_S2436_3", 
                                     "AF196835.2_WNV_NY99-flamingo382-99_1999_NewYork_Flamingo", 
                                     "AF260967.1_WNV_NY99-eqhs_1999_NewYork_Equine", 
                                     "AY185913.1_WNV_362_2002_Texas_BlueJay", 
                                     "GQ379161.1_WNV_ArEq003_2006_Argentina_Equine", 
                                     "MH643887.1_WNV_BeAn854747_2018_Brazil_Equine", 
                                     "JX503096.1_UNVERIFIED_", 
                                     "AF404757.1_WNV-Italy-1998_Italy_Equine", 
                                     "MT863559.1_WNV-Akela/France/2015_2015_Lineage1_France_Equine", 
                                     "KM052152.1_WNV-349/77_1977_Lineage2_South-Africa_Equine", 
                                     "MT863561.1_WNV-7025/France/2018_Lineage2_France_Accipiter_gentilis", 
                                     "KJ958922.1_WNV-T2_2011_Turkey_Equine", 
                                     "AY185906.1_WNV_113_2002_Texas_BlueJay", 
                                     "AY185911.1_WNV_V1151_2002_Texas_Mosquito", 
                                     "DQ164186.1_WNV", 
                                     "DQ164193.1_WNV", 
                                     "AY185907.1_WNV_114_2002_Texas_BlueJay" , 
                                     "AY185910.1_WNV_135_2002_Texas_Crow" , 
                                     "AY185909.1_WNV_123_2002_Texas_Hawk",
                                     "DQ164187.1_WNV", "DQ164195.1_WNV", 
                                     "NC_009942.1" 
                                     )) 

ape::write.tree(ml_tree2, file='WNV-ALL-0824-filtered.westremoved.droptip.tree')

#ml_tree$tip.label[ml_tree$tip.label == "AF196835.2_WNV_NY99-flamingo382-99_1999_NewYork_Flamingo"] <- "AF196835.2"
#ml_tree$tip.label[ml_tree$tip.label == "AF260967.1_WNV_NY99-eqhs_1999_NewYork_Equine" ] <- "AF260967.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "AY185913.1_WNV_362_2002_Texas_BlueJay" ] <- "AY185913.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "GQ379161.1_WNV_ArEq003_2006_Argentina_Equine" ] <- "GQ379161.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "MH643887.1_WNV_BeAn854747_2018_Brazil_Equine" ] <- "MH643887.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "JX503096.1_UNVERIFIED_" ] <- "JX503096.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "DQ164187.1_WNV" ] <- "DQ164187.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "AF404757.1_WNV-Italy-1998_Italy_Equine" ] <- "AF404757.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "MT863559.1_WNV-Akela/France/2015_2015_Lineage1_France_Equine" ] <- "MT863559.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "KM052152.1_WNV-349/77_1977_Lineage2_South-Africa_Equine" ] <- "KM052152.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "MT863561.1_WNV-7025/France/2018_Lineage2_France_Accipiter_gentilis" ] <- "MT863561.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "KJ958922.1_WNV-T2_2011_Turkey_Equine" ] <- "KJ958922.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "AY185906.1_WNV_113_2002_Texas_BlueJay" ] <- "AY185906.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "AY185911.1_WNV_V1151_2002_Texas_Mosquito" ] <- "AY185911.1_"
# ml_tree2$tip.label[ml_tree2$tip.label == "DQ164186.1_WNV" ] <- "DQ164186.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "DQ164193.1_WNV" ] <- "DQ164193.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "AY185907.1_WNV_114_2002_Texas_BlueJay" ] <- "AY185907.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "AY185910.1_WNV_135_2002_Texas_Crow" ] <- "AY185910.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "DQ164195.1_WNV" ] <- "DQ164195.1"
# ml_tree2$tip.label[ml_tree2$tip.label == "AY185909.1_WNV_123_2002_Texas_Hawk" ] <- "AY185909.1"
#ml_tree$tip.label[ml_tree$tip.label == "" ] <- ""
#ape::write.tree(ml_tree2, file='WNV-ALL-0824-filtered.droptipremoved.tree')




# extract from a bigger dataset
library(dplyr)
# bigger dataset
mydata<-read.csv("WNV-All-Envelope_Complete-Genome_Experimental_Clinical_Equine_Selected(2)_Country-Origin2.csv",header=TRUE, sep = ",")
length(ml_tree2$tip.label) #2771
sum(ml_tree2$tip.label %in% mydata$Name) #2771
#ml_tree2$tip.label[!ml_tree2$tip.label%in% mydata$Name] # "DQ164187.1_WNV" "DQ164195.1_WNV" "NC_009942.1" 

# my list to subset 
mydata_subset = mydata[mydata$Name %in% ml_tree2$tip.label,]
mydups = mydata_subset[duplicated(mydata_subset$Name), "Name"]
View(mydata_subset[mydata_subset$Name %in% mydups, ])
mydata_subset = mydata_subset[!duplicated(mydata_subset$Name),]

mydata_subset = mydata_subset[order(mydata_subset$Name), ] 
nrow(mydata_subset)
all.equal(mydata_subset$Name, ml_tree2$tip.label) #mismatches
library(phytools) #midpoint
mid_ml_tree2= midpoint_root(ml_tree2)
#matching dataframe to tree tips order
mydata_subset$Name2 = factor(mydata_subset$Name, levels=mid_ml_tree2$tip.label)
mydata_subset = mydata_subset[order(mydata_subset$Name2),]
all.equal(mydata_subset$Name, mid_ml_tree2$tip.label) #TRUE
#match rownames 
rownames(mydata_subset) = mydata_subset$Name
all.equal(mydata_subset$Name, mid_ml_tree2$tip.label)#TRUE

# reclassify df regions/countries 
df = mydata_subset
class(df$Country.of.Origins)
df$Country.of.Origins = gsub(
  pattern = "Central Africa", replacement = "Central African Republic", x = df$Country.of.Origins
)
df$Country.of.Origins = gsub(
  pattern = "Czech", replacement = "Czech Republic", x = df$Country.of.Origins
)

library(countrycode)
##### data
#df <- read.csv2(file = "WNV-ALL-0824-filtered.droptip-meta-filtered.csv", header = TRUE, sep = ",")
df <- df %>%
  mutate(Region = countrycode(Country.of.Origins, origin = "country.name", destination = "region"))
#change "United States - this study" 
df[df$Country.of.Origins == "United States - this study", "Region"] = "United States - this study"
unique(df$Region)
write_csv(df, file = "meta_region.csv")




# #reorder metadata heatmap to match tree:
# all(p$data$label %in% df2$Name) #FALSE
# # re-organize and re-order
# num_tips <- length(p$data$label)
# print(num_tips)
# #find entries in df e not in tree
# missing_in_tree <- setdiff(df2$Name, p$data$label)
# print(missing_in_tree)
# # filter df
# df_filtered <- df2 %>%
#   filter(!(Name %in% missing_in_tree))
# #re-check
# missing_in_tree2 <- setdiff(df_filtered$Name, p$data$label)
# all(df_filtered$Name %in% p$data$label) #TRUE



# getting regions for heatmap
unique(df$Region)
color_region <- c("Latin America & Caribbean" = "#f57251",
                  "East Asia & Pacific"  =  "#5ab94a",
                  "Europe & Central Asia"= "#d2b659",
                  "Sub-Saharan Africa"  = "#397e82",
                  "Middle East & North Africa" ="#5b6cd4",
                  "South Asia" = "#557742",
                  "North America" = "#913934", 
                  "United States - this study"= "#e012c8")

# plotting
p = ggtree(mid_ml_tree2, ladderize = TRUE)
plot(p)
# load new scale color for plotting heatmap
# library(ggnewscale)
# p2<- p + new_scale_fill()
# plot(p2)

#mapping heatmap
p2heat = gheatmap(p, df[,4, drop=FALSE], 
         offset = 0.00000000000008, width=0.2,
         font.size = 4,
         family = "",
         hjust = 0.05,
         color=rgb(1, 1, 1, alpha=0.0001)) +
  scale_fill_manual(values=color_region) #+ theme(legend.position = "bottom")
p2heat

#save
ggsave(file="WNV.pdf", plot = last_plot(), width=6, height=6, dpi = 300)

##### plotting subclade #####

df2<-read.csv("meta_region_thisstudy.csv",header=TRUE, sep = ",")
length(df2$Name) #2792
ml_tree3 <- read.tree("WNV-ALL-0824-filtered.westremoved.droptip_subclade.tree")
ml_tree3 = midpoint_root(ml_tree3)
plot(ml_tree3)
length(ml_tree3$tip.label) #2240
df_sub = df2[df2$Name %in% ml_tree3$tip.label,] 
nrow(df_sub) #2251 there are now duplicates
df_sub = df_sub[!duplicated(df_sub$Name),]
nrow(df_sub) #2240
write_csv(df_sub, file = "meta_region_sub.csv")
### MATCH data with tree labels: factor, levels, rownames
#ordering with tree
df_sub$Name2 = factor(df_sub$Name, levels=ml_tree3$tip.label)
df_sub = df_sub[order(df_sub$Name2), ]
all.equal(df_sub$Name, ml_tree3$tip.label) #TRUE
#match rownames#
rownames(df_sub) = df_sub$Name
all.equal(df_sub$Name, ml_tree3$tip.label) #TRUE


unique(df_sub$Region)
color_region <- c("Latin America & Caribbean" = "#f57251",
                  "East Asia & Pacific"  =  "#5ab94a",
                  "Europe & Central Asia"= "#d2b659",
                  "Sub-Saharan Africa"  = "#397e82",
                  "Middle East & North Africa" ="#5b6cd4",
                  "South Asia" = "#557742",
                  "North America" = "#913934", 
                  "United States - this study"= "#e012c8")

p = ggtree(ml_tree3, ladderize = TRUE)
p2heat = gheatmap(p, df_sub[,3, drop=FALSE], 
                  offset = 0.00000000000008, width=0.2,
                  font.size = 4,
                  family = "",
                  hjust = 0.05,
                  colnames = FALSE, 
                  # colnames_angle = 30,
                  # colnames_position = 'top', 
                  color=rgb(1, 1, 1, alpha=0.0001)) + 
  scale_fill_manual(values=color_region)
p2heat

ggsave(file="WNV_subclade.pdf", plot = last_plot(), width=6, height=6, dpi = 300)

ggsave(filename= "WNV_subclade.tiff", width = 6, height = 6, device='tiff', dpi=600)




