setwd("/Users/mdmamunmonir/MAC_DRIVE/DOC/Manuscripts/MS_hospital_data")

library(tidyverse)
data<-read.csv("harmonized_data2.csv", header=TRUE)

unique(data$patha)
cholera_pathogen<-c("VC O139","VC non 1","VC Eltor ogawa","VC Eltor inaba","VC classical ogawa","VC classical inaba")

cholera_positive<-unique(c(which(data$patha%in%cholera_pathogen),
                           which(data$pathb%in%cholera_pathogen),
                           which(data$pathc%in%cholera_pathogen)))


cholera<-rep(0, length(data[,1]))
cholera[cholera_positive]<- 1
data$cholera<-cholera

cholera_data<-data[cholera_positive,]
write.csv(cholera_data, file="cholera_data.csv")

type<-NULL

for(i in 1:length(cholera_data[,1])){
  pathogen<-c(cholera_data[i,]$patha,cholera_data[i,]$pathb, cholera_data[i,]$pathc)
  vibrio<-pathogen[which(pathogen%in%cholera_pathogen)]
  if(length(vibrio)>1){
    vibrio<-vibrio[1]
  }
  type<-c(type,vibrio)
}

cholera_data$DOVM<-factor(cholera_data$DOVM, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec"))
cholera_data$cholera<-type
table(cholera_data$cholera)
table(cholera_data$DOVY,cholera_data$cholera)
table(cholera_data$DOVY,cholera_data$DOVM)
table(cholera_data$DOVYM,cholera_data$DOVD)

##############
# disease seviarity parameters
#temp<-as.matrix(table(cholera_data$DOVY,cholera_data$TEMP))
diadur<-as.matrix(table(cholera_data$DOVY,cholera_data$DIADUR))
diadur<-diadur[,-1]
colnames(diadur)<-c("Duration <1 day", "Duration >15 day","Duration 1-3 day","Duration 10-12 day","Duration 12-14 day","Duration 4-6 day","Duration 7-9 day")

diadur<-(diadur/rowSums(diadur))*100
r_diadur<-cbind(diadur[,1],diadur[,3], diadur[,6],diadur[,7], diadur[,4],diadur[,5],diadur[,2])
colnames(r_diadur)<-c("Duration <1 day","Duration 1-3 day", "Duration 4-6 day","Duration 7-9 day","Duration 10-12 day","Duration 12-14 day","Duration >15 day")

constol<-as.matrix(table(cholera_data$DOVY,cholera_data$CONSTOL))
colnames(constol)
constol<-constol[,-c(1)]
constol<-(constol/rowSums(constol))*100
colnames(constol)<-c("Stool_contains_Blood","Stool_contains_Mu+bl", "Stool_contains_Mucus", "Stool_contains_None")
constol<-constol[,-c(4)]

r_constol<-cbind(constol[,3],constol[,1],constol[,2])
colnames(r_constol)<-c("Stool_contains_Mucus", "Stool_contains_Blood","Stool_contains_Mu+bl")

NUMSTOOL<-table(cholera_data$DOVY,cholera_data$NUMSTOOL)
colnames(NUMSTOOL)
NUMSTOOL<-NUMSTOOL[,-c(1,2)]
NUMSTOOL<-(NUMSTOOL/rowSums(NUMSTOOL))*100

colnames(NUMSTOOL)<-c("11-15 times in 24hrs", "16-20 times in 24hrs",  "21+ times in 24hrs", "3-5 times in 24hrs", "6-10 times in 24hrs")

r_NUMSTOOL<-cbind(NUMSTOOL[,4],NUMSTOOL[,5], NUMSTOOL[,1], NUMSTOOL[,2], NUMSTOOL[,3])
colnames(r_NUMSTOOL)<-c("3-5 times in 24hrs", "6-10 times in 24hrs","11-15 times in 24hrs", "16-20 times in 24hrs",  "21+ times in 24hrs")

ABDPAIN<-table(cholera_data$DOVY,cholera_data$ABDPAIN)
colnames(ABDPAIN)
ABDPAIN<-ABDPAIN[,-c(1,2)]
ABDPAIN<-(ABDPAIN/rowSums(ABDPAIN))*100

colnames(ABDPAIN)<-c("No_abdpain", "abdpain")

#Vomit<-table(cholera_data$DOVY,cholera_data$VOMIT)
#COUDIA<-table(cholera_data$DOVY,cholera_data$COUDIA)

severity_data<-cbind(r_diadur,r_constol,r_NUMSTOOL,ABDPAIN)
severity_data<-severity_data[-c(1:12,28:29),]

row_dist <- dist(severity_data, method = "euclidean")
col_dist <- dist(t(severity_data), method = "euclidean")

row_cluster <- hclust(row_dist, method = "complete")
col_cluster <- hclust(col_dist, method = "complete")
?tiff

heatmap(severity_data,  Colv = as.dendrogram(col_cluster), scale = "row",margins = c(10,5))
heatmap(severity_data, Rowv = as.dendrogram(row_cluster), Colv = NA, scale = "row",margins = c(10,5))
heatmap(severity_data, Rowv = as.dendrogram(row_cluster), Colv = as.dendrogram(col_cluster), scale = "row",margins = c(10,5))


tiff(filename = "test.tiff",width = 10, height = 10, units = "in",res = 300)
heatmap(severity_data, Rowv = as.dendrogram(row_cluster), Colv = NA, scale = "row",margins = c(10,5))
dev.off()

# Load the ComplexHeatmap package
library(ComplexHeatmap)


Heatmap(severity_data, 
        name = "Expression",                        # Title for the color legend
        col = colorRampPalette(c("white", "red"))(50),  # Color scale
        row_title = "Factor",                        # Title for rows
        column_title = "Year",                   # Title for columns
        cluster_rows = as.dendrogram(row_cluster),  # Row clustering dendrogram
        cluster_columns = as.dendrogram(col_cluster) # Column clustering dendrogram
)

z<-data%>% group_by(DOVY,DOVM, AREA) %>% summarise(total=sum(cholera))
print(z, n=90)

unique(z$AREA)

area<-c("R/A","V/A","basti","common H/A","others")
year<-c(1996:2024)
Months<-c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")

reordered_area_data<-NULL

for (i in 1:length(year))
{
  Date_info<-NULL
  for(j in 1:length(Months)){
    Total<-NULL
    Date_info<-c(year[i],Months[j])
    for (k in 1:length(area)) {
      data_match=NULL
      area_match=NULL
      
      date_match<-intersect(which(z$DOVY==year[i]),which(z$DOVM ==Months[j]))
      area_match<-intersect(date_match, which(z$AREA==area[k]))
      
      if(length(area_match)==1)
      {
        total_k<-z$total[area_match]
      }else{total_k=0}
      
      Total<-c(Total,total_k)
    }
   date_total<-c(Date_info, Total)
   reordered_area_data<-rbind(reordered_area_data,date_total)
  }
}

colnames(reordered_area_data)<-c("DOVY","DOVM","R/A","V/A","basti","common H/A","others")

write.csv(reordered_area_data, file="cholera_cases_area.csv")

z1<-read.csv("cholera_cases_area.csv", header=TRUE)

p1 <- ggplot(z1, aes(x=AREA, y=total, fill=DOVM)) + 
  geom_boxplot() +
  facet_wrap(~areacommonH.A, ncol = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

z1$DOVY<-as.factor(z1$DOVY)
p1 <- ggplot(z1, aes(x=DOVY, y=total, fill=DOVY)) + 
  geom_boxplot() +
  facet_wrap(~AREA, ncol = 10)+
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p1+theme(axis.text=element_text(size=14),
         axis.title=element_text(size=16,face="bold"))


plot(table(cholera_data$patha))

data2<-read.csv("monthly_cases_2008_2022_cholera.csv", header=TRUE)

locations<-as.vector(colnames(data2)[-c(1,2)])

cases_loc<-NULL

for (i in 3:93)
{
	cases_loc<-c(cases_loc,data2[,i])
}

data <- data.frame(
	Location = rep(locations, each = 180),
	Year = rep(rep(2008:2022, each = 12), times = 91),
	Month = rep(1:12, times = 1365),
	Cases = cases_loc
)

write.csv(data, file="data_cholera.csv")

#######
# Open refined sorted data for locations in Dhaka city
#######

library(ggplot2)

# Read the CSV file
data <- read.csv("data_cholera_dhaka.csv", header = TRUE)

data$Location <- factor(data$Location, levels = unique(data$Location))

# Create a ggplot object for bubble plots
p <- ggplot(data, aes(x = factor(Month, levels = 1:12, labels = month.abb), y = Location, size = Cases, color = Location))

#ggplot(data, aes(x = factor(Month, levels = 1:12, labels = month.abb), y = Location, size = Cases >1, color = Location))
# Create the bubble plot
p <- p + geom_point() +
	
	# Create separate plots for each year
	facet_wrap(~Year, scales = "free_y", ncol = 1) +
	
	# Customize the plot
	labs(title = "Monthly Cases by Location",
			 x = "Month",
			 y = "Location",
			 size = "Cases") +
	scale_size_continuous(range = c(-1, 12)) +  # Adjust the size range
	
	# Save the plot to a TIFF file
	tiff("cholera_dhaka.tiff", width=10, height=250, units = "in", compression = "lzw", res = 100)

print(p)

dev.off()

########################
# Total cases in the years
########################
data3 <- read.csv("Total_cases_years_cholera_Dhaka.csv", header = TRUE)

locations<-as.vector(colnames(data3)[-c(1)])

cases_loc<-NULL

for (i in 2:51)
{
  cases_loc<-c(cases_loc,data3[,i])
}

data <- data.frame(
  Location = rep(locations, each = 29),
  Year = rep(rep(1996:2024, each = 1), times = 50),
  Cases = cases_loc
)

#write.csv(data, file="data_cholera.csv")
data$Location <- factor(data$Location, levels = unique(data$Location))
# Create a ggplot object for bubble plots
p <- ggplot(data, aes(x = factor(Year, levels = 1996:2024), y = Location, size = Cases, color = Location))

p <- p + geom_point() +
  # Customize the plot
  labs(title = "Yearly Cases by Location",
       x = "Years",
       y = "Location",
       size = "Cases") +
  scale_size_continuous(range = c(-1, 12)) +  # Adjust the size range
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=13)) +
  theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13))+
# Save the plot to a TIFF file
  tiff("cholera_dhaka_years2.tiff", width=15, height=15, units = "in", compression = "lzw", res = 300)

print(p)

dev.off()

###################################
# Plot pathogen data

data4 <- read.csv("pathogen_serotype_data.csv", header = TRUE)

p <- ggplot(data4, aes(x = pathogen, y = factor(year, levels = 2024:1996), size = Cases, color = pathogen))

p <- p + geom_point() +
  # Customize the plot
  labs(title = "Yearly Cases by Location",
       x = "Pathogen",
       y = "Years",
       size = "Cases") +
  scale_size_continuous(range = c(-1, 12)) +  # Adjust the size range
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=13)) +
  theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13)) +
  scale_x_discrete(labels=c("VC Eltor inaba"="Inaba","VC Eltor ogawa"="Ogawa", "VC non 1"="NOVC","VC O139" ="O139"))

tiff("cholera_pathogen.tiff", width=5, height=10, units = "in", compression = "lzw", res = 300)

print(p)

dev.off()

#################################
setwd("/Users/mdmamunmonir/MAC_DRIVE/DOC/Manuscripts/MS_hospital_data")

library(ggplot2)
data5<- read.csv("circulating_strains.csv", header=TRUE)

multi_types<-data5[which(data5$Labels=="multi types"),]
single_dominant<-data5[-c(which(data5$Labels=="multi types")),]

test<-wilcox.test(x=multi_types$Cholera.case, y=single_dominant$Cholera.case,  paired = FALSE, alternative = "two.sided")

P1 <- ggplot(data5, aes(x=Labels, y=Cholera.case)) +
  geom_boxplot(fill=c("blue","red")) +
  labs(title= 'Cholera cases correponding to dominance of different lineages', x= ' ', y= '', tag = "") +
  geom_point()
df1 <- data.frame(a = c(1, 1,2 ,2), b = c(700, 710, 710, 700))
P1<- P1 + geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 720, label = "p-value = 0.0009435", size = 4)

P1


library(ggpubr)
p1 <- ggboxplot(data5, x = "Label1", y = "Cholera.case",
               color = "Label1", palette = "jco",
               add = "jitter")
#  Add p-value
p1<-p1 + stat_compare_means()

p2 <- ggboxplot(data5[-1,], x = "Label2", y = "Cholera.case",
                color = "Label2", palette = "jco",
                add = "jitter")
#  Add p-value
p2<-p2 + stat_compare_means()

tiff(filename = "cholera_case_lineages5.tiff",
     width = 6, height = 8, units = "in", res = 300)
(p1 / p2)
dev.off()

##################
library(tidyverse)

setwd("/Users/mdmamunmonir/MAC_DRIVE/DOC/Manuscripts/MS_hospital_data")
data<-read.csv("/Users/monirulmemlab/Research_data/Hospital_data_icddrb/cholera_data_area.csv",header=TRUE)

data$dovy<-as.factor(data$dovy)

data2<-data %>% group_by(dovy) %>% group_by(prloc_up2)

z<-data%>% group_by(dovy, prloc_up2, areacommonH.A) %>% summarise(total=sum(disease))
print(z, n=90)

z1<- z %>% filter(areacommonH.A==c("basti"))

write.csv(z,file="test.csv")

z1$prloc_up2 <- factor(z1$prloc_up2,
                       levels = c('vph.shr','vnu.shr'),ordered = TRUE)

bp <- ggplot(z1, aes(x=prloc_up2, y=total, group=prloc_up2)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
bp

df2$variable <- factor(df2$variable,
                       levels = c('vph.shr','vnu.shr'),ordered = TRUE)


z2<-z %>% filter(areacommonH.A==c("commonH/A")) 
bp1 <- ggplot(z2, aes(x=prloc_up2, y=total, group=prloc_up2)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
bp1


bp + facet_grid(. ~ prloc_up2)

write.csv(z, file="cholera_cases_area.csv")

###########################

setwd("/Users/mdmamunmonir/MAC_DRIVE/DOC/Manuscripts/MS_hospital_data")
data<-read.csv("dhaka_cholera_cases_area.csv",header=TRUE)

data$PRLOC <- factor(data$PRLOC, levels = unique(data$PRLOC))
data$AREA <- factor(data$AREA, levels = c("basti", "common H/A", "V/A", "R/A", "others"))

library(ggplot2)
p1 <- ggplot(data, aes(x=PRLOC, y=TOTAL_CASES, fill=AREA)) + 
  geom_boxplot() +
  facet_wrap(~AREA, ncol = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p1+theme(axis.text=element_text(size=14),
         axis.title=element_text(size=16,face="bold"))

p1 <- ggplot(data, aes(x=AREA, y=TOTAL_CASES, fill=AREA))+
  geom_boxplot()+
  facet_wrap(~YEAR_GROUP, ncol = 1)

ggplot(data, aes(x=AREA, y=TOTAL_CASES, fill=AREA))+
  geom_violin(trim=FALSE)+
  facet_wrap(~YEAR_GROUP, ncol = 1)

data2<-read.csv("dhaka_cholera_cases_area_year_groups.csv",header=TRUE)

ggplot(data2, aes(x=AREA, y=TOTAL_CASES, fill=AREA))+
  geom_violin(trim=FALSE)+
  facet_wrap(~YEAR_GROUP, ncol = 1)

p<-ggplot(data2, aes(x=AREA, y=TOTAL_CASES, fill=YEAR_GROUP)) +
  geom_boxplot(position=position_dodge(1))
p

library(ggpubr)

basti<-data2[which(data2$AREA=="basti"),]
commonHA<-data2[which(data2$AREA=="common H/A"),]
RA<-data2[which(data2$AREA=="R/A"),]
VA<-data2[which(data2$AREA=="V/A"),]



p1 <- ggboxplot(basti, x = "YEAR_GROUP", y = "TOTAL_CASES",
                color = "YEAR_GROUP", palette = "jco",
                add = "jitter")
#  Add p-value
p1<-p1 + stat_compare_means()

p2 <- ggboxplot(commonHA, x = "YEAR_GROUP", y = "TOTAL_CASES",
                color = "YEAR_GROUP", palette = "jco",
                add = "jitter")
#  Add p-value
p2<-p2 + stat_compare_means()

p3 <- ggboxplot(RA, x = "YEAR_GROUP", y = "TOTAL_CASES",
                color = "YEAR_GROUP", palette = "jco",
                add = "jitter")
#  Add p-value
p3<-p3 + stat_compare_means()

p4 <- ggboxplot(VA, x = "YEAR_GROUP", y = "TOTAL_CASES",
                color = "YEAR_GROUP", palette = "jco",
                add = "jitter")
#  Add p-value
p4<-p4 + stat_compare_means()


tiff("cholera_dhaka_years_area_Year_groups2.tiff", width=12, height=3, units = "in", res = 300)

(p1|p2|p4|p3)

dev.off()

##################
library(tidyverse)

setwd("/Users/monirulmemlab/Research_data/Hospital_data_icddrb")
data<-read.csv("/Users/monirulmemlab/Research_data/Hospital_data_icddrb/cholera_data_area.csv",header=TRUE)


data$dovy<-as.factor(data$dovy)
data$dovm<-as.factor(data$dovm)

data2<-data %>% group_by(dovy) %>% group_by(prloc_up2)

z<-data%>% group_by(dovy, dovm, prloc_up2) %>% summarise(total=sum(disease))
print(z, n=90)

#z1<- z %>% filter(areacommonH.A==c("basti"))

write.csv(z,file="test_month.csv")

#################----
data<-read.csv("monthly_cholera_cases_dhaka.csv",header=TRUE)

data$prloc_dhaka <- factor(data$prloc_dhaka, levels = unique(data$prloc_dhaka))
data$dovm<- factor(data$dovm, levels = 1:12 )

library(ggplot2)
p1 <- ggplot(data, aes(x=dovm, y=total_cases, fill=dovm)) + 
  geom_boxplot() +
  facet_wrap(~prloc_dhaka, ncol = 10)+
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p1+theme(axis.text=element_text(size=14),
         axis.title=element_text(size=16,face="bold"))


tiff("cholera_dhaka_months_each_loc2.tiff", width=15, height=8, units = "in", compression = "lzw", res = 300)

print(p1)

dev.off()

######----
data<-read.csv("cholera_cases_monthly.csv",header=TRUE)
data$dovm<- factor(data$dovm, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") )
p2 <- ggplot(data, aes(x=dovm, y=total_cholera_cases, fill=dovm)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2+theme(axis.text=element_text(size=14),
         axis.title=element_text(size=16,face="bold"))

data_1996_2000<-data[which(data$dovy<2001),]
data_2001_2004<-data[intersect(which(data$dovy<2005),which(data$dovy>2000)),]
data_2005_2008<-data[intersect(which(data$dovy<2009),which(data$dovy>2004)),]
data_2009_2012<-data[intersect(which(data$dovy<2013),which(data$dovy>2008)),]
data_2013_2016<-data[intersect(which(data$dovy<2017),which(data$dovy>2012)),]
data_2017_2020<-data[intersect(which(data$dovy<2021),which(data$dovy>2016)),]
data_2021_2023<-data[intersect(which(data$dovy<2024),which(data$dovy>2020)),]


p_1 <- ggplot(data_1996_2000, aes(x=dovm, y=total_cholera_cases, fill=dovm)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_1+theme(axis.text=element_text(size=14),
         axis.title=element_text(size=16,face="bold"))


p_2 <- ggplot(data_2001_2004, aes(x=dovm, y=total_cholera_cases, fill=dovm)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_2+theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"))

p_3 <- ggplot(data_2005_2008, aes(x=dovm, y=total_cholera_cases, fill=dovm)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_3+theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"))

p_4 <- ggplot(data_2009_2012, aes(x=dovm, y=total_cholera_cases, fill=dovm)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_4+theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"))

p_5 <- ggplot(data_2013_2016, aes(x=dovm, y=total_cholera_cases, fill=dovm)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_5+theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"))


p_6 <- ggplot(data_2017_2020, aes(x=dovm, y=total_cholera_cases, fill=dovm)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_6+theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"))


p_7 <- ggplot(data_2021_2023, aes(x=dovm, y=total_cholera_cases, fill=dovm)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_7+theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"))

library(ggpubr)

plot<- ggarrange(p_1,p_2,p_3,p_4,p_5,p_6,p_7, ncol=4, nrow=2, common.legend = TRUE,legend="bottom")

annotate_figure(plot, top = text_grob("Dive depths (m)", 
                                      color = "red", face = "bold", size = 14))


data_1996_2010<-data[which(data$dovy<2011),]
data_2011_2023<-data[intersect(which(data$dovy<2024),which(data$dovy>2010)),]


p_8 <- ggplot(data_1996_2010, aes(x=dovm, y=total_cholera_cases, fill=dovm)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_8+theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"))


p_9 <- ggplot(data_2011_2023, aes(x=dovm, y=total_cholera_cases, fill=dovm)) + 
  geom_boxplot() +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_9+theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"))


plot1<- ggarrange(p_8,p_9, ncol=1, nrow=2, common.legend = TRUE,legend="bottom")

tiff("cholera_cases_seasonality.tiff", width=4, height=8, units = "in", compression = "lzw", res = 300)

plot1

dev.off()

########

tiff("cholera_dhaka_months_total.tiff", width=5, height=5, units = "in", compression = "lzw", res = 300)
print(p2)

dev.off()


############################################
# Association analysis SNPs indels


setwd("/Users/mdmamunmonir/MAC_DRIVE/DOC/Manuscripts/MS_hospital_data")

snp_data<-read.csv("SNP_indels.csv",header=TRUE)
#fisher.test(snp_data[,2],snp_data[,3])
SNP_INDELS<-snp_data[,-c(1,2)]

P_value<-rep("NA",length(SNP_INDELS[1,]))

for(i in 1:length(SNP_INDELS[1,])){
  snp<-SNP_INDELS[,i]
  lineage<-snp_data[,2]
  if(length(unique(snp))>1){
    test<-fisher.test(as.factor(snp),as.factor(lineage),simulate.p.value=TRUE)
    P_value[i]<-test$p.value
  }
}

write.csv(cbind(colnames(SNP_INDELS), P_value), file="SNP_INDEL_ASSOC.csv")


#####################################################
# Summary table
setwd("/Users/mdmamunmonir/MAC_DRIVE/DOC/Manuscripts/MS_hospital_data")
data_cholera<-read.csv("cholera_data.csv", header=T)

data_96_10<-data_cholera[which(data_cholera$DOVY<2011),]
data_11_24<-data_cholera[-which(data_cholera$DOVY<2011),]

library(dplyr)

df=data_cholera
# Assume your data frame is called `df` and age is in a column named `age`
df <- df %>%
  mutate(age_group = case_when(
    agey >= 0 & agey <= 4   ~ "Infants (0-4 years)",
    agey >= 5 & agey <= 14  ~ "Children (5-14 years)",
    agey >= 15 & agey <= 49 ~ "Adults (15-49 years)",
    agey >= 50             ~ "Older Adults (50+)",
    TRUE                  ~ NA_character_   # For missing or invalid age values
  ))

df <- df %>%
  mutate(year_groups = case_when(
    DOVY >= 1996 & DOVY <= 2010 ~ "years (1996-2010)",
    DOVY >= 2011 & DOVY <= 2024 ~ "years (2011-2024)",
    TRUE ~ NA_character_
  ))


df %>%
  count(year_groups, age_group) %>%
  group_by(year_groups) %>%
  mutate(percent_within_year_groups = round(100 * n / sum(n), 1)) %>%
  ungroup()


df %>%
  count(year_groups, sex) %>%
  group_by(year_groups) %>%
  mutate(percent_within_year_groups = round(100 * n / sum(n), 1)) %>%
  ungroup()


df %>%
  group_by(year_groups, ) %>%
  summarise(
    count = n(),
    percent = round(100 * n() / nrow(df), 1),
    median_age = median(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE),
    .groups = "drop"
  )








