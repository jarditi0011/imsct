#muts by region

library(reshape2)
library(ggplot2)
annot5<-read.table("annot_file4.txt")
cents<-read.table("centromeres.txt")
chrs<-unique(cents$V2)

getarm<-function(gene){
  #print(gene)
  where<-which(annot5$V1==gene)
  gchr<-annot5$V2[where]
  gcentp<-min(cents$V4[cents$V2==gchr])
  gcentq<-max(cents$V4[cents$V2==gchr])
  gend<-max(annot5$V4[where], annot5$V3[where])
  pq<-if(gend<gcentp){"p"} else
    if (gend>gcentq){"q"} else {
      print(paste("Inspect:", gene))
      "p"
    }
  pq
}

go2<-genesonly
go2$Fusion<-str_detect(go2$genelesion, "FUSION;NFUSION|NFUSION")
go2$Methylation<-str_detect(go2$genelesion, "METHYLATION;NMETHYLATION")
go2$Mutation<-str_detect(go2$genelesion, "YES|NO|YES;NO|YES; NO")

countmuts<-function(region){
  objs<-unlist(str_split(region, "(?=.$)"))
  genes<-unique(annot5$V1[annot5$V2==objs[1] & annot5$V6==objs[2]])
  df<-go2[go2$Gene %in% genes, c(23:25)]
  df<-sapply(df, function(x) sum(x, na.rm=T))
  df
}

annot5[5]<-NA
annot5<-dplyr::distinct(annot5) # each gene appears exactly once in this processed object
annot5[6]<-sapply(annot5$V1, getarm)

tbl<-data.frame(table(paste(annot5$V2, annot5$V6, sep = ""))) #count number of times each chromosomal region appears
tbl<-tbl[order(tbl$Var1),]
colnames(tbl)<-c("Region", "Total Mentioned Genes")
ments<-data.frame(t(sapply(unique(paste0(annot5$V2, annot5$V6)), countmuts)))
ments<-ments[order(rownames(ments)),]
big<-cbind(tbl,ments)
View(big)

myorder1<-paste(c(1:22,"X"),"p", sep="")
myorder2<-paste(c(1:22,"X"),"q", sep="")
myorder<-paste("chr", c(rbind(myorder1,myorder2)),sep="")
myorderx<-intersect(myorder,big$Region)
myindex<-match(big$Region,myorderx) #order chromosomes properly
big<-big[order(myindex),]
test<-melt(big, "Region")
names(test)<-c("Region", "Lesion", "Count")
Value<-apply(test,1,function(x){if(x[2]=="Total Mentioned Genes"){"Genes"}else{"Lesion"}})
test<-cbind(test, Value)
test$Region<-factor(test$Region, levels=myorderx)

{
  p<-ggplot(data=test, aes(x=Value, y=Count, colour=Lesion, fill=Lesion)) +
    geom_bar(stat="identity", width=1, position="stack", linewidth=0, colour=NA) + 
    labs(title="Lesions of Gene Markers in each Chromosomal Region") +
    facet_grid(. ~ Region, switch="x") + 
    scale_fill_manual(values=c('#000000','#A03F3F', '#3FA049',  '#4F3FA0')) +
    scale_y_continuous(breaks=c(10,20,30,40,50), minor_breaks = c(5,15,25,35,45,55)) +
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

ggsave("lesionsbyregion.jpeg", device="jpeg")
