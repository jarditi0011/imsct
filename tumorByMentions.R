#tabling
source("IMSCT/imsct-quickload.r")
library(reshape2)
library(ggplot2)


confreqs<-genesonly[13:18]
confreqs<-sapply(confreqs,function(x){sum(x)})
names(confreqs)<-c("Review", "Clinical Standard", 
                   "Clinical Supplemental", "Basic Research", 
                   "Population Studies", 
                   "Clinical Algo/ML")

disfreqs<-table(unlist(str_split(genesonly$Diseases, ",|;")))
tumors<-names(disfreqs)
names(disfreqs)<-c("Astrocytoma", "Diffuse Midline Glioma", "Ependymoma", 
                   "Glioblastoma", "Other Glioma", "High-Grade Glioma", "Low-Grade Glioma", 
                   "Oligodendroglioma", "Oligoastrocytoma")

countments<-function(tumor){
  apply(genesonly[str_detect(genesonly$Diseases, tumor), 13:18], 2, function(x) sum(x, na.rm=T))
}

tbl<-data.frame(disfreqs) #count number of times each chromosomal region appears
colnames(tbl)<-c("Tumor", "Total Mentioned Genes")
ments<-t(data.frame(sapply(tumors, countments, simplify=F, USE.NAMES = T)))
rownames(ments)<-c("Astrocytoma", "Diffuse Midline Glioma", "Ependymoma", 
                   "Glioblastoma", "Other Glioma", "High-Grade Glioma", "Low-Grade Glioma", 
                   "Oligodendroglioma", "Oligoastrocytoma")
colnames(ments)<-c("Review", "Clinical Standard", 
                   "Clinical Supplemental", "Basic Research", 
                   "Population Studies", 
                   "Clinical Algo/ML")
big<-cbind(tbl,ments)
big<-big[order(big$`Total Mentioned Genes`, decreasing = T),]

test<-melt(big, "Tumor")
names(test)<-c("Tumor", "Key", "Count")
Value<-apply(test,1,function(x){if(x[2]=="Total Mentioned Genes"){"Genes"}else{"Context"}})
test<-cbind(test, Value)
test$Tumor<-factor(test$Tumor, levels=unique(test$Tumor))

colors=c('#000000', '#A03F3F', '#A0843F', '#3FA049', '#3F87A0', '#4F3FA0', '#A03FA0')

{
  p<-ggplot(data=test, aes(x=Value, y=Count, colour=Key, fill=Key)) +
    geom_bar(stat="identity", width=1, position="stack", linewidth=0, colour=NA) + 
    labs(title="Tumor Markers and their Research Contexts") +
    facet_grid(. ~ Tumor, switch="x") + 
    scale_fill_manual(values=colors) +
    scale_y_continuous(breaks=seq(0,24,2)*15, minor_breaks = c(seq(0,24,2)*15-10, seq(0,24,2)*15-20)) +
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          strip.text.x = element_text(angle = 90), 
          panel.spacing = unit(0, "lines"), 
          panel.grid.major.y  = element_line(colour = '#BEBEBE'), 
          panel.grid.minor.y  = element_line(colour = '#BEBEBE'), 
          panel.border = element_rect(colour = "#BEBEBE", fill=NA, linewidth=0.25), 
          legend.title = element_blank())
  p
}
ggsave(filename=paste("tumorByContext.jpeg"), 
       plot=p,
       device ="jpeg")
