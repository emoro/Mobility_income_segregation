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
users <- rbindlist(lapply(Sys.glob("./data/users_*.csv.gz"),fread))

## preprocess 
#normalize place data by metro area
places[,avg_income_seg_cbsa:=mean(income_seg),.(cbsa)]
#add name of metro area
places[,name:=names[match(cbsa,msas)]]


#plots
g1 <- ggplot(users)+  
  geom_density(aes(x=shuffled_income_seg),col=color_low,fill=color_low,alpha=0.3) +
  geom_density(aes(x=income_seg),fill=color_high,col=color_high,alpha=0.8) + 
  scale_y_sqrt(breaks=c(0,1,5,10,15,20))+
  theme_Publication()+xlab("User Income Segregation")+ylab("Density")

g2 <- ggplot(places) + 
  geom_density(aes(x=income_seg_shuffled),col=color_low,fill=color_low,alpha=0.3) +
  geom_density(aes(x=income_seg),fill=color_high,col=color_high,alpha=0.8) +
  theme_Publication()+xlab("Place income segregation")+ylab("Density")

g3 <- ggplot(places,aes(x=income_seg/avg_income_seg_cbsa,col=factor(name))) + 
  geom_line(stat="density") + theme_Publication()+ 
  scale_colour_tableau(palette = "Classic Green-Orange 12", name ="City") + 
  xlab("Place income segregation (normalized)") + 
  guides(colour = guide_legend(override.aes = list(size=3)))

gf <- g1 + g2 + g3 + plot_layout(widths=c(3.5,3.5,4))

gf