# tx factrs
source(paste0(getwd(), "/IMSCT/imsct-quickload.r"))
library(enrichR)
library(ggplot2)
library(KEGGREST)
library(stringr)
library(xlsx)
mydbs <-"Panther_2016"

enr<-enrichr(genesonly$Gene, mydbs)
enr<-enr$`Panther_2016`
enr<-enr[-c(8,19),]
enr$Term<-str_remove_all(enr$Term, "Homo.*")
enr$Term<-str_remove_all(enr$Term, "\\d.$")
enr$Term<-str_remove_all(enr$Term, "map ST")

plotEnrich(enr[1:7,], showTerms=26,title="Gene Ontology of IMSCT Markers", xlab="")
ggsave("panther_imsct_7.jpg", device="jpeg")
#saved a 5.33 x 2.14 in image
plot(-log(enr$Adjusted.P.value), 
     main="P_adj for Gene Ontology Categories", 
     sub = "ordered matching gene_ontology.xlsx", 
     ylab = "P_adj", 
     xlab = "Gene Ontological Category")
ggsave("panther_padj.jpg", device="jpeg")

write.xlsx(x=data.frame(Transcription_Factor=enr$Term, 
                        P_adj=enr$Adjusted.P.value,
                        Genes=str_replace_all(enr$Genes, ";", ",")),
           file="panther_imsct.xlsx", row.names=F)
