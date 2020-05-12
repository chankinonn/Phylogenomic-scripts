############## Calculate average BS of genetrees ##################

library(dplyr)
library(ggplot2)
library(viridisLite)

setwd("~/Dropbox/ongoing_projects/hylarana/rfdist/species_trees/exon")
files<-list.files(".")
tree.files<-files[grep("_astral", files)]

## Get gene tree folders
tree.folder<-"~/Dropbox/ongoing_projects/hylarana2/rfdist/genetrees/exon"
setwd(tree.folder)
files<-list.dirs(".", full.names = F)
tree.dirs<-files[files != ""]

bs.list<-vector("list", 0)
for (i in 1:length(tree.dirs)){
  setwd("~/Dropbox/ongoing_projects/hylarana2/rfdist/species_trees/exon")
  #load in the data
  dataset.name<-gsub("_astral.tre", "", tree.files[i])
  
  #Go to tree folder
  setwd(paste0(tree.folder, "/IQTrees_", dataset.name))
  temp.trees<-list.files(".")
  temp.trees<-temp.trees[grep(".treefile", temp.trees)]
  
  #read in and combine all trees
  temp.list<-c()
  for (j in 1:length(temp.trees)){
    gene.tree<-read.tree(temp.trees[j])
    temp.bs <- mean(as.numeric(gene.tree$node.label), na.rm=T)  ### calculate mean bs for each gene tree
    temp.list<-append(temp.list, temp.bs)
  }#end j loop
  #Saves data
  print(paste0(dataset.name, " finished!"))
  add.list<-list(temp.list)
  names(add.list)<-paste0(dataset.name)
  bs.list<-append(bs.list, add.list)
}#end dataset loop

bs.data<-bs.list
names(bs.list)

############
### PLOT ###
############
###### CHECK SEQUENCE DATASETS IN bs.data #########

miss50 <-data.frame(bs.data[[1]])### Make sure dataset matches the right list in bs.data!!! ##
colnames(miss50) <- "Bootstrap" ## Create column name
miss50$Data <- rep("miss50",nrow(miss50)) # populate new column

miss75 <-data.frame(bs.data[[2]])
colnames(miss75) <- "Bootstrap"
miss75$Data <- rep("miss75",nrow(miss75)) # make new column 

miss95 <-data.frame(bs.data[[3]])
colnames(miss95) <- "Bootstrap"
miss95$Data <- rep("miss95",nrow(miss95)) # make new column 

unfiltered <-data.frame(bs.data[[4]])
colnames(unfiltered) <- "Bootstrap"
unfiltered$Data <- rep("unfiltered",nrow(unfiltered)) # make new column 

PIS50 <-data.frame(bs.data[[5]])
colnames(PIS50) <- "Bootstrap"
PIS50$Data <- rep("PIS50",nrow(PIS50)) # make new column 

PIS75 <-data.frame(bs.data[[6]])
colnames(PIS75) <- "Bootstrap"
PIS75$Data <- rep("PIS75",nrow(PIS75)) # make new column 

PIS95 <-data.frame(bs.data[[7]])
colnames(PIS95) <- "Bootstrap"
PIS95$Data <- rep("PIS95",nrow(PIS95)) # make new column 

combined <- rbind(miss50, miss75, miss95, unfiltered, PIS50, PIS75, PIS95)
write.csv(combined, "~/Dropbox/ongoing_projects/hylarana2/bootstrap/combined_intron.csv")

#### For uce ###
PIS50 <-data.frame(bs.data[[1]])
colnames(PIS50) <- "Bootstrap"
PIS50$Data <- rep("PIS50",nrow(PIS50)) # make new column 

PIS75 <-data.frame(bs.data[[2]])
colnames(PIS75) <- "Bootstrap"
PIS75$Data <- rep("PIS75",nrow(PIS75)) # make new column 

miss50 <-data.frame(bs.data[[3]])### Make sure dataset matches the right list in bs.data!!! ##
colnames(miss50) <- "Bootstrap" ## Create column name
miss50$Data <- rep("miss50",nrow(miss50)) # populate new column

miss75 <-data.frame(bs.data[[4]])
colnames(miss75) <- "Bootstrap"
miss75$Data <- rep("miss75",nrow(miss75)) # make new column 

miss95 <-data.frame(bs.data[[5]])
colnames(miss95) <- "Bootstrap"
miss95$Data <- rep("miss95",nrow(miss95)) # make new column 

unfiltered <-data.frame(bs.data[[6]])
colnames(unfiltered) <- "Bootstrap"
unfiltered$Data <- rep("unfiltered",nrow(unfiltered)) # make new column 

combined <- rbind(miss50, miss75, miss95, unfiltered, PIS50, PIS75)
write.csv(combined, "~/Dropbox/ongoing_projects/hylarana2/bootstrap/combined_uce.csv")


#### Read separate Bootstrap tables

setwd("~/Dropbox/ongoing_projects/hylarana2/bootstrap/")
intron <- read.csv("combined_intron.csv")
intron$Dataset <- rep("Intron",nrow(intron)) # make new column 

exon <- read.csv("combined_exon.csv")
exon$Dataset <- rep("Exon",nrow(exon)) # make new column 

EC <- read.csv("combined_EC.csv")
EC$Dataset <- rep("Exons-combined",nrow(EC)) # make new column 

uce <- read.csv("combined_uce.csv")
uce$Dataset <- rep("UCE",nrow(uce)) # make new column 

combined_all <- rbind(exon, EC, intron, uce)
write.csv(combined_all, "combined_all_Bootstrap.csv")

combined_all <- read.csv("combined_all_Bootstrap.csv")

### Calculate group means for plotting
df <- combined_all %>%
  group_by(Data, Dataset) %>%
  summarise(meanBS=mean(Bootstrap))


### Plot ###

ggplot(combined_all, aes(Bootstrap, color=Data,fill=Data)) +
  #theme_classic() +
  geom_density(alpha=0.85) +
  geom_vline(data=df, aes(xintercept=meanBS, color=Data), linetype="dashed") +
  #scale_colour_manual(values=mycol, name="Dataset") +
  #scale_fill_manual(values=mycol, name="Dataset") +
  scale_fill_viridis(discrete=TRUE, option="D") +
  scale_color_viridis(discrete=TRUE, option="D") +
  ylab("Density") +
  xlab("Average Bootstrap") +
  facet_wrap(~Dataset, ncol=4) +
  theme(text = element_text(size=12)) +
  theme(legend.position = "none")
