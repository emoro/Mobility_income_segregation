## Code to reproduce Figure 4 of
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

get_quant <- function(x){
  as.numeric(cut(x,breaks=quantile(x,probs=seq(0,1,length.out=5)),include.lowest=T))
}
users[,quant_sigmap:=get_quant(sigmap)]
users[,quant_sigmas:=get_quant(sigmas)]


#1. Places
var_places <- c("Exhibit","Movie Theater","Science Museum","Theme Park",
                "Dim Sum","Ramen","Tapas","Assisted Living",
                "Automotive","Indie Movies","Accessories",
                "Lingerie","Hockey","Factory","Warehouse")

#1.1 Places and sigmap
vars <- melt(users[,c("quant_sigmap",var_places),with=F],id.vars = "quant_sigmap")
vars[,mean_var:=mean(value),.(variable)]
vars_group <- vars[,.(nn=.N,mn=mean(value),
                      perc=(mean(value)-mean(mean_var))/mean(mean_var)),
                   .(quant_sigmap,variable)]
gsigmap_places <- ggplot(vars_group,aes(x=variable,y=quant_sigmap)) + 
  geom_tile(aes(fill=perc*100),col="grey") + 
  scale_fill_gradientn(colours = palette_chosen,limits=c(-120,120))+
  theme_hc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  theme(legend.position = "none",
        axis.text.x=element_blank(),axis.title.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  ylab(TeX('$\\sigma_p$ quantile'))+
  ggtitle("Fraction of time spent in category")

#1.2 Places and sigmas
vars <- melt(users[,c("quant_sigmas",var_places),with=F],id.vars = "quant_sigmas")
vars[,mean_var:=mean(value),.(variable)]
vars_group <- vars[,.(nn=.N,mn=mean(value),
                      perc=(mean(value)-mean(mean_var))/mean(mean_var)),
                   .(quant_sigmas,variable)]
gsigmas_places <- ggplot(vars_group,aes(x=variable,y=quant_sigmas)) + 
  geom_tile(aes(fill=perc*100),col="grey") + 
  scale_fill_gradientn(name= "% deviation from the mean",colours = palette_chosen,limits=c(-120,120))+
  theme_hc() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  ylab(TeX('$\\sigma_s$ quantile'))+
  ggtitle("Fraction of time spent in category")

#2.Census
var_census <- c("9-12th grade","Bachelor or more","Employed","White","Black",
                "Trans. Car","Trans. Public","Median income","Poverty Ratio")

#2.1 Census and sigmap
vars <- melt(users[,c("quant_sigmap",var_census),with=F],id.vars = "quant_sigmap")
vars[,mean_var:=mean(value),.(variable)]
vars_group <- vars[,.(nn=.N,mn=mean(value),
                      perc=(mean(value)-mean(mean_var))/mean(mean_var)),
                   .(quant_sigmap,variable)]

gsigmap_census <- ggplot(vars_group,aes(x=variable,y=quant_sigmap)) + 
  geom_tile(aes(fill=perc*100),col="grey") + 
  scale_fill_gradientn(colours = palette_chosen,limits=c(-62,62))+
  theme_hc() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  theme(legend.position = "none",axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  ylab(TeX('$\\sigma_p$ quantile'))+  
  ggtitle("Fraction of population")

#2.1 Census and sigmas
vars <- melt(users[,c("quant_sigmas",var_census),with=F],id.vars = "quant_sigmas")
vars[,mean_var:=mean(value),.(variable)]
vars_group <- vars[,.(nn=.N,mn=mean(value),
                      perc=(mean(value)-mean(mean_var))/mean(mean_var)),
                   .(quant_sigmas,variable)]

gsigmas_census <- ggplot(vars_group,aes(x=variable,y=quant_sigmas)) + 
  geom_tile(aes(fill=perc*100),col="grey") + 
  scale_fill_gradientn(name= "% deviation from the mean",colours = palette_chosen,limits=c(-62,62))+
  theme_hc() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  ylab(TeX('$\\sigma_s$ quantile'))+  
  ggtitle("Fraction of population")

require(patchwork)
(gsigmap_places + gsigmap_census +
    gsigmas_places + gsigmas_census)+plot_layout(widths=c(17,9),height=c(2,2))