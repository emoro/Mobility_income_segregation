
## ggplot themes and scales
theme_Publication <- function(base_size=14, base_family="Helvetica Neue") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
    + theme(plot.title = element_text(size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            axis.ticks.length = unit(-1.4, "mm"),
            axis.text.x = element_text(margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
            axis.text.y = element_text(margin = unit(c(t = 0, r = 2.5, b = 0, l = 0), "mm")),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "right",
            #            legend.direction = "horizontal",
            #            legend.key.size= unit(0.2, "cm"),
            legend.margin = margin(t=0,unit="cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

#colors used
colores <- c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")

color_high <- "#543005"
color_mid <- "#f5f5f5"
color_mid_high <- "#f6e8c3"
color_mid_low <- "#c7eae5"
color_low <- "#003c30"
color_other <- "#dfc27d"
col_vis <- rev(c("#EA3727","#FD8C30","#F6E331","#6DCFE1","#3B3FAD"))
palette_chosen <- c(color_low,color_low,color_mid_low,color_mid,color_mid_high,color_high,color_high)


#names for the CBSAS
msas <- c(14460,16980,19100,19820,31080,33100,35620,37980,41860,42660,47900)
names <- c("Boston","Chicago","Dallas","Detroit","Los Angeles","Miami","New York","Philadelphia","San Francisco","Seattle","Washington")

#2D plot
plotdensfunction <- function(x,y,z,bin,thres,min=NA,max=NA,...){
  xyz <- data.table(x,y,z)
  cc <- xyz[,.(n=length(z),mz=mean(z,na.rm=T)),.(xp=floor(x/bin),yp=floor(y/bin))]
  cc$xp <- cc$xp*bin
  cc$yp <- cc$yp*bin
  cc <- cc[cc$n>=thres,]
  colores <- col_vis
  colores <- rev(c(color_high,"#bf812d",color_mid_high,color_mid_low,"#35978f",color_low))
  if(is.na(min)) min=min(cc$mz)
  if(is.na(max)) max=max(cc$mz)
  myBreaks <- round((seq(sqrt(min),sqrt(max),length.out = 10))^2,2)
  p<- ggplot(cc, aes(xp,yp)) + geom_tile(aes(fill=mz)) + scale_fill_gradientn(name="",colours = colores,breaks=myBreaks,labels=format(myBreaks,digits=2,scientific=F),limits=c(min,max),trans="sqrt") +labs(x="",y="")+theme_bw()+guides(fill=guide_colorbar(barwidth=0.5,barheight=10))
  p
}
