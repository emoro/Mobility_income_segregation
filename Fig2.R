## Code to reproduce Figure 1 of
## "Mobility patterns are a large factor in experienced income segregation in large US cities"
## by Esteban Moro et al.
## Date: April 13, 2021
## Author: Esteban Moro

## libraries
library(latex2exp)
library(ggplot2)
library(data.table)
library(ggthemes)
library(patchwork)

## visualization settings
source("visualization_settings.R")

## load data
places <- fread("./data/places.csv.gz")
taxonomy <- fread("./data/taxonomy.csv.gz",sep=",")


## preprocess 
#compute averages by category
table_cats <- places[,.(nplaces=.N,nvisits=mean(nvisits),nuusers=mean(nusers),
                        avg_dist=sum(nvisits*avg_dist)/sum(nvisits),
                        avg_inc_seg=mean(income_seg)),.(cat)][order(cat)]

#lets put the taxonomy in the table of cats
table_cats <- merge(table_cats,taxonomy,by=c("cat"))

#select only certain taxonomy categories for the visualization
table_cats[,tax:=ifelse(Taxonomy %in% c("Transportation","Grocery","Education","Work",
                                        "Health","Arts / Museum","Religious"),
                        Taxonomy, "Other")]

#plot
tt <- table_cats[nplaces>200]
g <- ggplot() + 
  geom_point(data=tt[tax=="Other"],
             aes(x=avg_dist,y=avg_inc_seg,size=sqrt(nplaces)),
             pch=21,
             color="darkgray",
             fill="gray") + 
  geom_point(data=tt[tax!="Other"],
             aes(x=avg_dist,y=avg_inc_seg,size=sqrt(nplaces),
             fill=factor(tax)),
             pch=21,
             color="black") + 
  theme_Publication() + 
  scale_fill_Publication(name="Taxonomy")+ 
  scale_size_continuous(name="#Places",range=c(1,10),
                        breaks=sqrt(c(10,500,1000,5000,10000,50000)),
                        labels=c(10,500,1000,5000,10000,50000))+
  guides(fill = guide_legend(override.aes = list(size=5)))+
  scale_x_log10(limits=c(7.5,33))+
  scale_y_continuous(limits=c(0.28,0.63))+
  geom_hline(yintercept=mean(places$income_seg),linetype=2)+
  xlab("Accessibility (average distance from home, in km.)")+
  ylab("Average place income segregation by category")
g
