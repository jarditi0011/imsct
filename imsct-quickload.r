setwd("D:/Rwd/IMSCT")

library(stringr)

### helper functions
dfadd<-function(col){
  if (all(is.na(col))) {
    NA
  } else if (all(is.numeric(col), na.rm = TRUE)) {
    sum(col)
  } else if (all(is.character(col), na.rm = TRUE)) {
    temp<-paste(col, sep=";")
    temp2<-unique(unlist(str_split(temp, ";")))
    str_flatten(temp2, collapse=";")
  }
}

#combine fields row-wise: if strings, append entries, if numbers, add entries
rowcombine<-function(df){
  as.data.frame(t(sapply(df, dfadd)))
}

###################################################################
##### construct correctly edited table of references ##############
###################################################################
library(readxl)
imsct<-read_xlsx("IMSCT.xlsx", 2)
imsct<-imsct[1:195, !str_detect(colnames(imsct), "NA.")]
imsct[imsct=="NA"]<-NA
imsct[13:23][is.na(imsct[13:23])]<-0
cols<-colnames(imsct)
#cols
#View(imsct)

#table(imsct$SUM.total.papers.)
#hist(imsct$SUM.total.papers., 28)

#processing proteins into genes
imsct[which(imsct=="synaptophysin", arr.ind=TRUE)]<-"SYP"
imsct[which(imsct=="VIMENTIN", arr.ind=TRUE)]<-"VIM"
imsct[which(imsct=="HIST1H3B", arr.ind=TRUE)]<-"H3C2"
imsct[which(imsct=="HIST1H3C", arr.ind=TRUE)]<-"H3C3"

#Merlin protein, an entry under gene, is encoded by the gene NF2, which has a separate entry
merlin<-imsct[which(imsct$Gene=="Merlin"),]
imsct[which(imsct=="NF2", arr.ind=TRUE)[1], 14]<-imsct[which(imsct=="NF2", arr.ind=TRUE)[1], 14]+merlin[14]
imsct[which(imsct=="NF2", arr.ind=TRUE)[1], 15]<-imsct[which(imsct=="NF2", arr.ind=TRUE)[1], 15]+merlin[15]
imsct[which(imsct=="NF2", arr.ind=TRUE)[1], 21]<-imsct[which(imsct=="NF2", arr.ind=TRUE)[1], 21]+merlin[21]
imsct[which(imsct=="NF2", arr.ind=TRUE)[1], 22]<-imsct[which(imsct=="NF2", arr.ind=TRUE)[1], 22]+merlin[22]
imsct<-imsct[-which(imsct$Gene=="Merlin"),]
imsct[which(imsct=="PDGF-B", arr.ind=TRUE)]<-"PDGFB"
imsct[which(imsct=="Desmin", arr.ind=TRUE)]<-"DES"
imsct[which(imsct=="Calretinin", arr.ind=TRUE)]<-"CALB2"
imsct[which(imsct=="caldesmon", arr.ind=TRUE)]<-"CALD1"
imsct[which(imsct=="Chromatogranin", arr.ind=TRUE)]<-"CHGA" #"chromatogranin does not exist on the internet but "chromogranin" was so I used that here
imsct[which(imsct=="FXIIIa", arr.ind=TRUE)]<-"F13A1" # factor XIII is a heterodimer of 2 A peptides and 2 B peptides; activated form is only the A peptides so I included that gene as the replacement
imsct[which(imsct=="PDL-1", arr.ind=TRUE)]<-"CD247"
imsct[which(imsct=="Reticulin", arr.ind=TRUE)]<-"COL3A1"
imsct[which(imsct=="HMB-45", arr.ind=TRUE)]<-"PMEL"
#inhibin is a stain for a multi-gene protein so I am leaving it in
imsct[which(imsct=="NESTIN", arr.ind=TRUE)]<-"NES"
imsct[which(imsct=="BCRP/ABCG2", arr.ind=TRUE)]<-"ABCG2"
imsct[which(imsct=="EMMPRIN", arr.ind=TRUE)]<-"BSG"
imsct[which(imsct=="III‐β‐tubulin", arr.ind=TRUE)]<-"TUBB3"
imsct[which(imsct=="LINK-PINT", arr.ind=TRUE)]<-"LINC-PINT"
imsct[which(imsct=="KI67", arr.ind=TRUE)]<-"MKI67"
imsct[which(imsct=="EMA", arr.ind=TRUE)]<-"MUC1"
imsct[which(imsct=="H3F3A", arr.ind=TRUE)]<-"H3-3A"
imsct[which(imsct=="NEUN", arr.ind=TRUE)]<-"RBFOX3"
imsct[which(imsct=="CD31", arr.ind=TRUE)]<-"PECAM1"
imsct[which(imsct=="SMARCB1", arr.ind=TRUE), 6]<-imsct[which(imsct=="INI1", arr.ind=TRUE),6]
imsct[which(imsct=="SMARCB1", arr.ind=TRUE), 7]<-imsct[which(imsct=="INI1", arr.ind=TRUE),7]
imsct[which(imsct=="SMARCB1", arr.ind=TRUE), 15]<-imsct[which(imsct=="INI1", arr.ind=TRUE),15]
imsct[which(imsct=="SMARCB1", arr.ind=TRUE), 21]<-imsct[which(imsct=="INI1", arr.ind=TRUE),21]
imsct[which(imsct=="SMARCB1", arr.ind=TRUE), 22]<-imsct[which(imsct=="INI1", arr.ind=TRUE),22]
imsct<-imsct[-which(imsct$Gene=="INI1"),]
imsct<-imsct[-which(imsct$Gene=="ERCCA"),] # this is a marker tested for chemo response in our study
imsct[which(imsct=="NG2", arr.ind=TRUE)]<-"CSPG4"
imsct[which(imsct=="GPR112", arr.ind=TRUE)]<-"ADGRG4"
imsct[which(imsct=="CD56", arr.ind=TRUE)]<-"NCAM1"
imsct[which(imsct=="NSE", arr.ind=TRUE)]<-"ENO2"
imsct[which(imsct=="SMA", arr.ind=TRUE)]<-"ACTA2"
imsct[which(imsct=="RB", arr.ind=TRUE)]<-"RB1"
imsct[which(imsct=="GATSL2", arr.ind=TRUE)]<-"CASTOR2"
imsct[which(imsct=="INT2", arr.ind=TRUE)]<-"FGF3"
imsct[which(imsct=="HERK3", arr.ind=TRUE)]<-"MAPK6"
imsct[which(imsct=="KIAA1199", arr.ind=TRUE)]<-"CEMIP"
imsct[which(imsct=="MEK", arr.ind=TRUE)]<-"MAP2K1"
#imsct[which(imsct=="ZFTA", arr.ind=TRUE)]<-"C11orf95" # ZFTA is the correct name but I'm too lazy to download a new genome

#removing exons
imsct[which(imsct=="MN1ex1", arr.ind=TRUE)]<-"MN1"
imsct[which(imsct=="BEND2ex7", arr.ind=TRUE)]<-"BEND2"

#pancytokeratin is an antibody to 30 genes for different keratins
#cytokeratin is also listed as a separate category both from pancytokeratin and from CK
CKs<-imsct[which(
  imsct$Gene=="CK"|imsct$Gene=="Pancytokeratin"|imsct$Gene=="Cytokeratin"
),]
CKs<-rowcombine(CKs)
imsct<-imsct[-c(44,66,75),]
imsct[nrow(imsct)+1,]<-CKs

#fixing disease typos
imsct$Diseases<-str_replace_all(imsct$Diseases, " DMG", "DMG")
imsct$Diseases<-str_replace_all(imsct$Diseases, " GBM", "GBM")
imsct$Diseases<-str_replace_all(imsct$Diseases, "ASTROOLIGO", "OLIGOASTRO")
imsct$Diseases<-str_replace_all(imsct$Diseases, "ATRO", "ASTRO")
imsct$Diseases<-str_replace_all(imsct$Diseases, " ASTRO", "ASTRO")
imsct$Diseases<-str_replace_all(imsct$Diseases, " DMG", "DMG")

#ont<-write_ontology(imsct$Gene, "IMSCT Genes")
imsct[13:23]<-sapply(imsct[13:23], as.numeric)
imsct[13]<-imsct[13]+imsct[14]
imsct<-imsct[-14]
#colnames(imsct)[c(13:18,20)]<-c("Review", "Clinical \n Standard", 
#                               "Clinical \n Supplemental", "Basic \n Research", 
#                               "Population \n Studies", 
#                               "Clinical \n Algo/ML", "Total")
colnames(imsct)<-c("Gene", "unkexpchng", "genelesion", "expchng", "loss", 
                   "marker.ab.det", "Diseases", "fuspart", "chromosomal", 
                   "IgDisAbs", "pottrt", "our.study", "Review", 
                   "Clinical \n Standard", "Clinical \n Supplemental", 
                   "Basic \n Research", "Population \n Studies", 
                   "Clinical \n Algo/ML", "ClinTrial", "Total", 
                   "ptspos", "ptsneg")

#imsct$genelesion<-str_replace_all()

families<-imsct[c(which(str_detect(imsct$Gene, "[a-z]")),
                  which(imsct$Gene=="S100"), # a family of proteins
                  which(imsct$Gene=="NFP"), #NeuroFilamentProtein
                  which(imsct$Gene=="PHH3"), # PHosphorylation of Histone 3
                  which(imsct$Gene=="FBXO"), # A family of proteins whhich are often fused
                  which(imsct$Gene=="AL160314.2"), # AL160314 is a sequence from the beginning of chr14, the most updated sequence is .7
                  which(imsct$Gene=="AC092164.1") # similar  #
),] 
genesonly<-imsct[which(imsct$Gene %in% setdiff(imsct$Gene, families$Gene)),]
genesonly<-genesonly[order(genesonly$Gene),]

#enrichR recognizes ZFTA but not C11orf95
#genesonly[which(genesonly=="ZFTA", arr.ind=TRUE)]<-"C11orf95"
#write.table(genesonly$Gene, file="imsct_genes2.txt", col.names = FALSE, row.names = FALSE)
#genesonly[which(genesonly=="C11orf95", arr.ind=TRUE)]<-"ZFTA"

disfreqs<-table(unlist(str_split(imsct$Diseases, ",|;")))

names(disfreqs)<-c("Astrocytoma", "Diffuse\nMidline\nGlioma", "Ependymoma", 
                   "Glioblastoma", "Other\nGlioma", "High-Grade \n Glioma", "Low-Grade \n Glioma", 
                   "Oligodendro- \n glioma", "Oligoastro- \n cytoma")
disfreqs<-disfreqs[order(disfreqs, decreasing = TRUE)]
confreqs<-genesonly[c(13:18,20)]
confreqs<-unlist(lapply(confreqs,function(x){sum(x)}))
#confreqs[1]<-confreqs[1]+confreqs[2]
#confreqs<-confreqs[-2]


names(confreqs)<-c("Review", "Clinical \n Standard", 
                   "Clinical \n Supplemental", "Basic \n Research", 
                   "Population \n Studies", 
                   "Clinical \n Algo/ML", "Total")
