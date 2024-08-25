# tx factrs
source(paste0(getwd(), "/IMSCT/imsct-quickload.r"))
library(enrichR)
library(ggplot2)
library(KEGGREST)
library(stringr)
library(readxl)
mydbs <-"ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X"

enr<-enrichr(genesonly$Gene, mydbs)
enr<-enr$`ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X`
enr$Term<-str_remove_all(enr$Term, "ENCODE|CHEA")
enr<-enr[-c(8,19),]
plotEnrich(enr, showTerms=10,title="Transcription Factors and Regulators \n of IMSCT markers", xlab="")

ggsave("tf_imsct.jpg", device="jpeg")
# saved a 4.68 x 2.62 in image 

plot(-log(enr$Adjusted.P.value), 
     main="P_adj for TF Categories", 
     sub = "ordered matching tf_imsct.xlsx", 
     ylab = "P_adj", 
     xlab = "TF Category")
ggsave("tf_padj2.jpg", device="jpeg")

write.xlsx(x=data.frame(Transcription_Factor=enr$Term, Genes=str_replace_all(enr$Genes, ";", ",")),
          file="tf_imsct2.xlsx", row.names=F)
