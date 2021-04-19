## Code to reproduce Figure 3 of
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
users <- rbindlist(lapply(Sys.glob("./data/users_*.csv.gz"),fread))
users <- na.omit(users)

#plots
#sigmas density
gsigmas <- ggplot(users,aes(x=sigmas)) + 
  geom_density(fill="gray",col="gray",bw=0.01) + 
  theme_Publication() + 
  scale_x_continuous(limits=c(0,1),expand=c(0.005,0.005),breaks = c(0,0.5,1))+
  scale_y_continuous(expand=c(0.005,0.005),limits=c(0,5.5))+ 
  geom_vline(xintercept=mean(users$sigmas),col="darkred",linetype=2)+
  xlab("sigma_s")+ylab("")+
  theme(plot.margin = unit(c(0.15,0.15,0.15,0.15), "cm"))

#sigmap density
gsigmap <- ggplot(users,aes(x=sigmap)) +  
  geom_density(fill="gray",col="gray",bw=0.01) + 
  theme_Publication()+  scale_x_continuous(limits=c(0,1),breaks = c(0,0.5,1))+
  scale_y_continuous(limits=c(0,2),breaks = c(0,1,2)) + 
  geom_vline(xintercept=mean(users$sigmap),col="darkred",linetype=2) + 
  xlab("sigma_p")+ylab("")+ 
  theme(plot.margin = unit(c(0.05,0.05,0.05,0.05), "cm"))

#user income segregation density plot
p1 <- plotdensfunction(users$sigmas,users$sigmap,users$income_seg,0.03,10,0.09,0.8) + 
  theme_bw() + 
  theme(panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),panel.grid = element_blank())+  
  scale_x_continuous(limits=c(0,1),expand=c(0.005,0.005),breaks = seq(0,1,0.2))+
  scale_y_continuous(expand=c(0.005,0.005),breaks=seq(0,1,0.2),limits=c(0,1))+
  theme(panel.border = element_rect(color="black",size=rel(1.6))) + ggtitle("User income segregation (Data)") + 
  xlab("sigma_s") + ylab("sigma_p")

gsigmap + gsigmas + p1 + plot_layout(widths=c(2.5,2.5,4))
