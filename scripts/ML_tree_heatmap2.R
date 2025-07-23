setwd("/Users/mdmamunmonir/MAC_DRIVE/DOC/Manuscripts/MS_hospital_data/Treetime/test.treetime")

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

beast_tree <- read.beast("BD_1991_2024_clean_beast.tree")
print(as.phylo(beast_tree), printlen=3)
get.fields(beast_tree)
get.data(beast_tree)
beast_tree[, c("node", "height")]
names(beast_tree[1,])

x <- as_tibble(beast_tree)
x
class(x)
#write.csv(as.matrix(x), file="beast_tree_data.csv")
as.phylo(x)

##############
to_drop<-c("BD_1971_ERR025385","BD_1979_ERR025383",
           "BD_1987_ERR018176","BD_2002_ERR018121",
           "BD_2017_DRR335690","BD_2019_SRR14297548",
           "BD_2019_SRR14297551","BD_2019_SRR14297552",
           "BD_2019_SRR14297680","BD_2020_BFS822",
           "BD_2022_DRR395013")
beast_tree_reduced<- drop.tip(beast_tree, to_drop)
metadata_beast<-as_tibble(read.csv("metadata_beast_tree.csv",header=TRUE, sep = ","))

x<-as_tibble(beast_tree_reduced)
metadata_beast_subset = metadata_beast[metadata_beast$label %in% x$label,]

y <- full_join(x, metadata_beast_subset, by = 'label')
y
View(y)

as.treedata(y)
y %>% as.treedata %>% as_tibble

child(y, 458)
parent(y, 2)
offspring(y, 458)
ancestor(y, 2)
MRCA(y, 2, 3)
child(beast_tree_reduced, 458)

#write.csv(as.matrix(y), file="reduced_tree_data.csv")

#######################
tree <- beast_tree_reduced@phylo
tree$node.label  # internal node labels, if available

library(ape)
library(ggtree)

# Example: get MRCA of selected tips
BD_1.2a_selected_tips <- c("BD_2019_BFS809","BD_2019_BFS810","BD_2019_BFS811",
                   "BD_2019_BFS813","BD_2019_SRR14297547","BD_2019_SRR14297549",
                   "BD_2019_SRR14297550","BD_2019_SRR14297557","BD_2019_SRR14297558",
                   "BD_2019_SRR14297682","BD_2019_SRR14297684","BD_2019_SRR14297689",
                   "BD_2020_BFS814","BD_2020_BFS815","BD_2020_BFS816",
                   "BD_2020_BFS817","BD_2020_BFS818","BD_2020_BFS819","BD_2020_BFS820",
                   "BD_2020_BFS821","BD_2020_BFS823","BD_2020_BFS824","BD_2020_BFS825",
                   "BD_2020_BFS827","BD_2020_BFS828","BD_2020_BFS829","BD_2020_BFS831",
                   "BD_2020_BFS832","BD_2020_BFS833","BD_2020_BFS835","BD_2020_BFS837",
                   "BD_2020_BFS838","BD_2020_BFS839","BD_2020_BFS851","BD_2020_BFS852",
                   "BD_2020_BFS858","BD_2020_BFS859","BD_2020_BFS860","BD_2020_BFS861",
                   "BD_2020_BFS862","BD_2020_BFS863","BD_2020_BFS864","BD_2020_BFS886",
                   "BD_2020_BFS887","BD_2020_BFS888","BD_2020_BFS890","BD_2020_BFS892",
                   "BD_2020_BFS898","BD_2020_BFS902","BD_2021_BFS1030","BD_2021_BFS1031",
                   "BD_2021_BFS1032","BD_2021_BFS1033","BD_2021_BFS779","BD_2021_BFS780",
                   "BD_2021_BFS781","BD_2021_BFS782","BD_2021_BFS783","BD_2021_BFS784",
                   "BD_2021_BFS785","BD_2021_BFS787","BD_2021_BFS788","BD_2021_BFS789",
                   "BD_2021_BFS790","BD_2021_BFS791","BD_2021_BFS792","BD_2021_BFS793",
                   "BD_2021_BFS794","BD_2021_BFS795","BD_2021_BFS796","BD_2021_BFS797",
                   "BD_2021_BFS798","BD_2021_BFS799","BD_2021_BFS800","BD_2021_BFS801",
                   "BD_2021_BFS802","BD_2021_BFS803","BD_2021_BFS804",
                   "BD_2021_BFS805","BD_2021_BFS806","BD_2021_BFS807","BD_2021_BFS808",
                   "BD_2021_BFS840","BD_2021_BFS841","BD_2021_BFS842","BD_2021_BFS843",
                   "BD_2021_BFS844","BD_2021_BFS845","BD_2021_BFS846","BD_2021_BFS847",
                   "BD_2021_BFS848","BD_2021_BFS849","BD_2021_BFS850","BD_2021_BFS853",
                   "BD_2021_BFS854","BD_2021_BFS855","BD_2021_BFS856","BD_2021_BFS857",
                   "BD_2021_BFS865","BD_2021_BFS866","BD_2021_BFS867","BD_2021_BFS868",
                   "BD_2021_BFS869","BD_2021_BFS872","BD_2021_BFS873","BD_2021_BFS877",
                   "BD_2021_BFS880","BD_2021_BFS881","BD_2021_BFS882","BD_2021_BFS883","BD_2021_BFS885","BD_2021_BFS904","BD_2021_BFS938","BD_2021_BFS947","BD_2021_BFS948","BD_2022_BFS1012","BD_2022_BFS1018","BD_2022_BFS1037","BD_2022_BFS1038","BD_2022_BFS1041","BD_2022_BFS1043","BD_2022_BFS1047","BD_2022_BFS1048","BD_2022_BFS1057","BD_2022_BFS1061","BD_2022_DRR394994","BD_2022_DRR394995","BD_2022_DRR394996","BD_2022_DRR394999","BD_2022_DRR395000","BD_2022_DRR395002","BD_2022_DRR395003","BD_2022_DRR395004","BD_2022_DRR395005","BD_2022_DRR395006","BD_2022_DRR395007","BD_2022_DRR395009","BD_2022_DRR395015")  # Replace with your real tip labels

# For ape/ggtree trees
BD_1.2a_mrca_node <- getMRCA(beast_tree_reduced@phylo, BD_1.2a_selected_tips)
print(BD_1.2a_mrca_node)

BD_1.2b_selected_tips <-c("BD_2019_BFS812","BD_2019_SRR14297553","BD_2019_SRR14297554","BD_2019_SRR14297555","BD_2019_SRR14297556","BD_2019_SRR14297681","BD_2019_SRR14297685","BD_2019_SRR14297686","BD_2019_SRR14297687","BD_2020_BFS826","BD_2020_BFS830","BD_2020_BFS834","BD_2020_BFS836","BD_2020_BFS889","BD_2020_BFS901","BD_2021_BFS1011","BD_2021_BFS1034","BD_2021_BFS1056","BD_2021_BFS786","BD_2022_BFS1014","BD_2022_BFS1015","BD_2022_BFS1016","BD_2022_BFS1017","BD_2022_BFS1019","BD_2022_BFS1020","BD_2022_BFS1021","BD_2022_BFS1022","BD_2022_BFS1023","BD_2022_BFS1024","BD_2022_BFS1029","BD_2022_BFS1035","BD_2022_BFS1045","BD_2022_BFS1049","BD_2022_BFS1052","BD_2022_BFS1053","BD_2022_BFS1059","BD_2022_BFS1060","BD_2022_BFS1063","BD_2022_BFS1065","BD_2022_BFS1146","BD_2022_BFS1148","BD_2022_BFS1149","BD_2022_BFS1150","BD_2022_BFS1151","BD_2022_BFS1152","BD_2022_BFS1170","BD_2022_DRR394997","BD_2022_DRR395001","BD_2022_DRR395008","BD_2022_DRR395010","BD_2022_DRR395011","BD_2022_DRR395012","BD_2022_DRR395014","BD_2023_BFS1112","BD_2023_BFS1114","BD_2023_BFS1116","BD_2023_BFS1117","BD_2023_BFS1119","BD_2023_BFS1121","BD_2023_BFS1125","BD_2023_BFS1128","BD_2023_BFS1130","BD_2023_BFS1131","BD_2023_BFS1133","BD_2023_BFS1135","BD_2023_BFS1137","BD_2023_BFS1138","BD_2023_BFS1139","BD_2023_BFS1140","BD_2023_BFS1142","BD_2023_BFS1143","BD_2023_BFS1153","BD_2023_BFS1154","BD_2023_BFS1155","BD_2023_BFS1156","BD_2023_BFS1157","BD_2023_BFS1158","BD_2023_BFS1159","BD_2023_BFS1160","BD_2023_BFS1162","BD_2023_BFS1163","BD_2023_BFS1165","BD_2023_BFS1166","BD_2023_BFS1174","BD_2024_Sept26-49","BD_2024_Sept26-50","BD_2024_Sept26-51","BD_2024_Sept26-52","BD_2024_Sept26-53","BD_2024_Sept26-54","BD_2024_Sept26-55","BD_2024_Sept26-56","BD_2024_Sept26-57","BD_2024_Sept26-58","BD_2024_Sept26-59")

BD_1.2b_mrca_node <- getMRCA(beast_tree_reduced@phylo, BD_1.2b_selected_tips)
print(BD_1.2b_mrca_node)

#p <- ggtree(beast_tree_reduced, mrsd = "2024-09-27")
#p_flipped <- flip(p, 551, 688)
#p_flipped

#############################

#p1<-ggtree(beast_tree_reduced, mrsd="2024-09-27") + theme_tree2() + 
#  geom_tiplab(as_ylab=TRUE, color='firebrick')

#p2<-ggtree(beast_tree_reduced, branch.length='rate') + theme_tree2() +
 # labs(caption="Substitution rate")

#d <- data.frame(node=c(456, 455), type=c("A", "B"))
#p<-ggtree(beast_tree_reduced, mrsd="2024-09-27") + geom_hilight(data=d, aes(node=node, fill=type),
#                            type = "roundrect")+ theme_tree2()
#p

data_lineage<-data.frame(metadata_beast_subset$Lineage)

rownames(data_lineage)<-metadata_beast_subset$label
colnames(data_lineage)<-c("Lineage")

data_ctxB<-data.frame(metadata_beast_subset$ctxB_in_silico)
rownames(data_ctxB)<-metadata_beast_subset$label
colnames(data_ctxB)<-c("ctxB")

data_PLE<-data.frame(metadata_beast_subset$PLE)
rownames(data_PLE)<-metadata_beast_subset$label
colnames(data_PLE)<-c("PLE")

data_VSP2<-data.frame(metadata_beast_subset$VSP2_types)
rownames(data_VSP2)<-metadata_beast_subset$label
colnames(data_VSP2)<-c("VSP2")

data_VSP2$VSP2<-factor(data_VSP2$VSP2, labels = c("wild", "sporadic", "Var1", "Var-2", "Var2*", "Var-3", "Var3*",        
                                                  "Var-4", "Var4*", "Var-5", "Var6", "Var6*"))


##################################
library(ggnewscale)
library(ggtree)
library(ggplot2)
library(treeio)

# Attach metadata manually before plotting
# This converts the treedata to a ggtree-compatible dataframe
tree_data <- ggtree(beast_tree_reduced, mrsd = "2024-09-27")
tree_df <- tree_data$data

# Merge lineage data
tree_df <- merge(tree_df, data_lineage, by.x = "label", by.y = "row.names", all.x = TRUE)

# Step 1: Basic plot without adding heatmap
lineage_colors <- c(
  "Ref" = "#3c3c3c",   
  "BD-0" = "#3385ca" ,   
  "BD-1"  = "#0dfc24" , 
  "BD-1.1" = "#8cbd35" ,
  "BD-1.2" = "#e79035" ,
  "BD-1.2a" = "#efc132",
  "BD-1.2b" = "#c17b9f" ,
  "BD-2"   = "#d2666d"  )

p <- ggtree(beast_tree_reduced, mrsd = "2024-09-27") +
  geom_tippoint(data = tree_df, aes(x = x, y = y, color = Lineage), size = 0.5) +
  theme_tree2() +
  scale_x_continuous(
    name = "Year",
    breaks = seq(1955, 2025, by = 5),  # Custom tick marks every 5 years
    labels = seq(1955, 2025, by = 5)
  )+
  xlab("Year") +scale_color_manual(values = lineage_colors) +
  guides(color = guide_legend(title = "Lineage"))

######################################
# Step 2: Add heatmaps
######################################

# Step 2: Add Lineage heatmap with custom fill colors
p1 <- gheatmap(
  p, data_lineage,
  offset = 0, width = 0.05,
  colnames = FALSE,
  colnames_offset_y = -9,
  legend_title = "Lineage"
) +
  scale_fill_manual(
    values = lineage_colors,
    name = "Lineage"  # sets legend title
  ) +
  scale_x_ggtree() +  # Align x-axis with ggtree
  scale_y_continuous(expand = c(0, 0.3))


# Step 3: Add ctxB heatmap
p2 <- p1 + new_scale_fill()

  p3 <- gheatmap(
    p2, data_ctxB,
    offset = 4, width = 0.05,
    colnames = FALSE,
    colnames_offset_y = -9,
    legend_title = "ctxB"
  ) +
    scale_x_ggtree() +  # Align x-axis with ggtree
    scale_y_continuous(expand = c(0, 0.3))
  
  # Step 4: Add PLE heatmap
  p4 <- p3 + new_scale_fill()
  p5 <- gheatmap(
    p4, data_PLE,
    offset = 8, width = 0.05,
    colnames = FALSE,
    colnames_offset_y = -9,
    legend_title = "PLE"
  )
  # Step 5: Add VSP2 heatmap
  p6<-p5 + new_scale_fill()

  p7 <- gheatmap(
    p6, data_VSP2,
    offset = 12, width = 0.05,
    colnames = FALSE,
    colnames_offset_y = -9,
    legend_title = "VSP2"
  )  
  # Show final plot
p7

tiff(filename = "Beast_tree.tiff",
     width = 10, height = 10, units = "in", res = 300)
p7
dev.off()


#################
library(ggtree)
library(ggnewscale)  # for new_scale_fill()

# Step 1: Plot the tree with tip points (lineage color)
p <- ggtree(beast_tree_reduced, mrsd = "2024-09-27") +
  geom_tippoint(data = tree_df, aes(x = x, y = y, color = Lineage), size = 0.5) +
  theme_tree2() +
  scale_x_continuous(
    name = "Year",
    breaks = seq(1955, 2025, by = 5),
    labels = seq(1955, 2025, by = 5)
  ) +
  xlab("Year") +
  scale_color_manual(values = lineage_colors) +
  guides(color = guide_legend(title = "Lineage"))

# Step 2: Add Lineage heatmap
p1 <- gheatmap(
  p, data_lineage,
  offset = 0, width = 0.05,
  colnames = FALSE,
  colnames_offset_y = -9,
  legend_title = "Lineage"
) +
  scale_fill_manual(values = lineage_colors, name = "Lineage")

# Step 3: Add ctxB heatmap
p2 <- p1 + new_scale_fill()
p3 <- gheatmap(
  p2, data_ctxB,
  offset = 0.06, width = 0.05,
  colnames = FALSE,
  colnames_offset_y = -9,
  legend_title = "ctxB"
) +
  scale_fill_manual(values = ctxB_colors, name = "ctxB")

# Step 4: Add PLE heatmap
p4 <- p3 + new_scale_fill()
p5 <- gheatmap(
  p4, data_PLE,
  offset = 0.12, width = 0.05,
  colnames = FALSE,
  colnames_offset_y = -9,
  legend_title = "PLE"
) +
  scale_fill_manual(values = PLE_colors, name = "PLE")

# Step 5: Add VSP2 heatmap
p6 <- p5 + new_scale_fill()
p7 <- gheatmap(
  p6, data_VSP2,
  offset = 0.18, width = 0.05,
  colnames = FALSE,
  colnames_offset_y = -9,
  legend_title = "VSP2"
) +
  scale_fill_manual(values = VSP2_colors, name = "VSP2") +
  scale_x_ggtree() +
  scale_y_continuous(expand = c(0, 0.3))

# Show final plot
p7







