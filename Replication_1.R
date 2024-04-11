##
#
# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
##
##########################
#getwd()
#setwd("/Data (APSR)")
#rm(list = ls())

# Load Packages 
library(tidyr); library(dplyr); library(data.table);
library(NetSwan); library(networktools); library(tidyverse);
library(ggpubr); library(gridExtra);
library(igraph); library(extrafont); library(ggplot2); library(ggraph);
library(tidygraph)
library(stargazer)
#############
install.packages(c("NetSwam", "extrafront", "tidygraph", "Rtools"))
library("tidygraph")
library("Rtools")
#
#Load Data
rep_dataset <- read.csv("C:/NewGithubFolder/StatsII_Spring2024/Temporary_Replication_Idi/Data APSR/Data/bol_census_elections.csv")
head(rep_dataset)
dim(rep_dataset) # Rows/Observations=335; Columns/Variables=6
names(rep_dataset) #"X", "Departamento" ; "Provincia" ; "Municipio" ; "PobRuralProp2001" ; "mas_per2002"  
str(rep_dataset)
####
table(rep_dataset$Departamento)
# Answer:
#Beni Chuquisaca  Cochabamba     La Paz      Oruro      Pando     Potosi  Santa Cruz     Tarija 
#19         28         47         84          35         15         40         56          11 
#load("/Data/network.Rdata")

# This graph includes alliances and excludes interactions with political parties and/or the government

##
#set.seed(1)

### Remove vertices disconnected from main organizational network 
# (or, in the case of 'others', not constitutive of an actual organization)

g3 <- delete_vertices(g3, "CODES")
g3 <- delete_vertices(g3, "Unión Juvenil Cruceñista")
g3 <- delete_vertices(g3, "Nacion Camba")
g3 <- delete_vertices(g3, "others")


#Key descriptive statistics
vcount(g3)
ecount(g3)
mean_distance(g3, directed=F)

# Key centrality metrics
deg1 <- as.data.frame(degree(g3))
deg2 <- as.data.frame(eigen_centrality(g3))
deg3 <- as.data.frame(betweenness(g3))


#  TABLE 2: Origin Networks' Centrality Measures

deg_all <- cbind(deg1, deg2$vector, deg3) # Combines results

colnames(deg_all)[1] <- "Degree"
colnames(deg_all)[2] <- "Eigenvector"
colnames(deg_all)[3] <- "Betweenness"

deg_tbl2 <- deg_all[c("Coordinadora","FDUTC–LP “TK”", "FSUTCC"),] #Gets measures for three organizations of interest

stargazer(deg_tbl2,
          type='latex', style='apsr',summary=F,
          flip=T,
          font.size="large",
          column.sep.width = "10pt") 


# FIGURE 3:Network Environment of Indigenous Parties’ Origin Networks

ecol_coor <- rep("gray90", ecount(g3)) 
ecol_coor[incident(g3, "Coordinadora", mode="all")] <- "black"  #direct edges connecting Coordinadora to network in black, else grey
vcol_coor <- rep("white", vcount(g3))
vcol_coor[V(g3)$label=="Coordinadora"] <- "black" 

ecol_cocha <- rep("gray90", ecount(g3))
ecol_cocha[incident(g3, "FSUTCC", mode="all")] <- "black" #direct edges connecting FSUTCC to network in black, else grey
vcol_cocha <- rep("white", vcount(g3))
vcol_cocha[V(g3)$label=="FSUTCC"] <- "black"

ecol_lapaz <- rep("gray90", ecount(g3))
ecol_lapaz[incident(g3, "FDUTC–LP “TK”", mode="all")] <- "black" #direct edges connecting FDUTC-LP'TK' to network in black, else grey
vcol_lapaz <- rep("white", vcount(g3))
vcol_lapaz[V(g3)$label=="FDUTC–LP “TK”"] <- "black"


#Figure 3a
f3a <- ggraph(g3, layout="nicely")+
  geom_edge_fan(aes(edge_color=ecol_coor, alpha=.5))+
  scale_edge_color_manual(values=c("black", "gray88"))+
  geom_node_point(aes(fill = vcol_coor), shape = 21, color="black")+
  scale_fill_manual(values = c("white","grey"))+
  geom_node_text(aes(label = name), family = "Times New Roman",size=3.5, repel = TRUE) +
  ggtitle("(a) Coordinadora") +
  theme_graph() +
  theme(legend.position = "none",
        plot.title = element_text(size = 11, face = "bold", family="Times New Roman"),
        plot.margin=margin(1,0,0,0,'cm'))


#Figure 3b
f3b <- ggraph(g3, layout="nicely")+
  geom_edge_fan(aes(edge_color=ecol_lapaz, alpha=.5))+
  scale_edge_color_manual(values=c("black", "gray88"))+
  geom_node_point(aes(fill = vcol_lapaz), shape = 21, color="black")+
  scale_fill_manual(values = c("white","grey"))+
  geom_node_text(aes(label = name), family = "Times New Roman",size=3.5, repel = TRUE) +
  ggtitle("(b) FDUTCP-LP 'TK'") +
  theme_graph() +
  theme(legend.position = "none",
        plot.title = element_text(size = 11, face = "bold", family="Times New Roman"),
        plot.margin=margin(1,0,0,0,'cm'))


#Figure 3c
f3c <- ggraph(g3, layout="nicely")+
  geom_edge_fan(aes(edge_color=ecol_cocha, alpha=.5))+
  scale_edge_color_manual(values=c("black", "gray88"))+
  geom_node_point(aes(fill = vcol_cocha), shape = 21, color="black")+
  scale_fill_manual(values = c("white","grey"))+
  geom_node_text(aes(label = name), family = "Times New Roman", size=3.5, repel = TRUE) +
  ggtitle("(c) FSUTCC") +
  theme_graph() +
  theme(legend.position = "none",
        plot.title = element_text(size = 11, face = "bold", family="Times New Roman"),
        plot.margin=margin(1,0,0,0,'cm'))

f3_comb <- ggarrange(f3a, f3b, f3c, nrow=1, ncol=3)

ggsave(f3_comb, filename = '../Data (APSR)/Figures/Fig3.tiff', width = 18, height = 10, device='tiff', dpi=1000)



### FIGURE 4: Organizational network and CSUTCB Ties

## Figure 4a
f4a <- ggraph(g3, layout="fr")+
  geom_edge_link(alpha=.2, edge_color="grey20") +
  geom_node_point(aes(fill = key_nodes), size=degree(g3)*.2, shape = 21, color="black")+
  scale_fill_manual(values = c("white","grey"))+
  geom_node_text(aes(filter = key_nodes=="key", label = name), repel = TRUE) +
  ggtitle("(a) Key Organizational Nodes") +
  theme_graph() +
  theme(legend.position = "none",
        plot.title = element_text(size = 11, face = "bold", family="Times New Roman"),
        plot.margin=margin(1,0,0,0,'cm'))


#Figure 4b
inc.edges_csutcb <- incident(g3, V(g3)[label=="CSUTCB"], mode="all")

ecol_csutcb <- rep("gray90", ecount(g3))
ecol_csutcb[inc.edges_csutcb] <- "black"
vcol_csutcb <- rep("white", vcount(g3))
vcol_csutcb[V(g3)$label=="CSUTCB"] <- "black"

f4b <- ggraph(g3, layout="nicely")+
  geom_edge_fan(aes(edge_color=ecol_csutcb, alpha=.5))+
  scale_edge_color_manual(values=c("black", "gray88"))+
  geom_node_point(aes(fill = vcol_csutcb), shape = 21, color="black")+
  scale_fill_manual(values = c("white","grey"))+
  geom_node_text(aes(label = name), family = "Times New Roman", size=3.5, repel = TRUE) +
  ggtitle("(b) CSUTCB Ties") +
  theme_graph() +
  theme(legend.position = "none",
        plot.title = element_text(size = 11, face = "bold", family="Times New Roman"),
        plot.margin=margin(1,0,0,0,'cm'))

f4_comb <- ggarrange(f4a, f4b, nrow=1, ncol=2)

ggsave(f4_comb, filename = '../Data (APSR)/Figures/Fig4.tiff', width = 8, height = 5, device='tiff', dpi=1000)




### FIGURE 5: Organizational Nodes' Bridge Centrality

cw<- cluster_walktrap(g3) #finds densely connected subgraphs (communities) via random walks

br<- bridge(g3, cw) # calculates bridge centrality measures

# Prepare data on bridge centrality for graphs
br_str <- as.data.frame(br$`Bridge Strength`)
br_betw <- as.data.frame(br$`Bridge Betweenness`)
br_clos <- as.data.frame(br$`Bridge Closeness`)

bridge <- cbind(br_str, br_betw, br_clos)

colnames(bridge)[1] <- "Strength"
colnames(bridge)[2] <- "Betweenness"
colnames(bridge)[3] <- "Closeness"
bridge$Organization <- row.names(bridge)

f5a <- ggplot(aes(x=Strength, y=reorder(Organization, -desc(Strength))), data=bridge)+
  geom_line(group=1)+
  geom_point() +
  labs(x='Bridge Strength', y='Organization') +
  theme_linedraw()+
  theme(legend.position = "none",
        text=element_text(family="Times New Roman"),
        axis.title = element_text(size=10, face="bold"))

f5b <- ggplot(aes(x=Betweenness, y=reorder(Organization, -desc(Betweenness))), data=bridge)+
  geom_line(group=1)+
  geom_point() +
  labs(x='Bridge Betweenness', y='') + 
  theme_linedraw()+
  theme(legend.position = "none",
        text=element_text(family="Times New Roman"),
        axis.title = element_text(size=10, face="bold"))

f5c <- ggplot(aes(x=Closeness, y=reorder(Organization, -desc(Closeness))), data=bridge)+
  geom_line(group=1)+
  geom_point() +
  labs(x='Bridge Closeness', y='', face="bold")+
  theme_linedraw()+
  theme(legend.position = "none",
        text=element_text(family="Times New Roman"),
        axis.title = element_text(size=10, face="bold"))


f5_comb <- ggarrange(f5a, f5b, f5c, ncol=3, nrow=1)

ggsave(f5_comb, filename = '../Data (APSR)/Figures/Fig5.tiff', width = 10, height = 8, device='tiff', dpi=1000)



# FIGURE 7: Attack Tolerance of Organizational Network

f7r <-swan_combinatory(g3,10) #calculates network vulnerability to attacks
f7r <- cbind(f7r, V(g3)$name)
f7r <- as.data.frame(f7r)


# Prepare data for plotting
colnames(f7r)[1] <- "nodes_removed"
colnames(f7r)[2] <- "Betweenness"
colnames(f7r)[3] <- "Degree"
colnames(f7r)[4] <- "Cascading"
colnames(f7r)[5] <- "Random"
colnames(f7r)[6] <- "Organization"


attack <- gather(f7r, key = measure, value = Rate, 
                 c("Betweenness", "Degree", "Cascading", "Random"))

f7 <- ggplot(attack, aes(x=as.numeric(nodes_removed), y = as.numeric(Rate), group = measure, shape=measure, linetype = measure)) + 
  geom_point(size=1.7)+
  geom_line()+
  labs(x='Fraction of nodes removed', y='Connectivity loss') +
  scale_shape_manual(values=c(20,4,0,1))+
  theme_linedraw()+
  theme(text=element_text(family="Times New Roman"),
        axis.title = element_text(size=10, face="bold"), 
        legend.title=element_blank(),
        legend.position = c(1, 0),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.box.background = element_rect(color="black"))


ggsave(f7, filename = '../Data (APSR)/Figures/Fig7.tiff', width = 8, height = 5, device='tiff', dpi=1000)
#####################
