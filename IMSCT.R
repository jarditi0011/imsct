### biblopgraphic work for IMSCT

# This bracket clusters all the setup editing together 
# And is separately implemented for sourcing in the file "imsct-quickload.r"
{
setwd("E:/Rwd/IMSCT")

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
library(xlsx)
imsct<-read.xlsx("IMSCT.xlsx", 2)
imsct<-imsct[1:195, !str_detect(colnames(imsct), "NA.")]
imsct[imsct=="NA"]<-NA
imsct[13:23][which(is.na(imsct[13:23]), arr.ind = TRUE)]<-0
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
genesonly[which(genesonly=="ZFTA", arr.ind=TRUE)]<-"C11orf95"
write.table(genesonly$Gene, file="imsct_genes2.txt", col.names = FALSE, row.names = FALSE)
genesonly[which(genesonly=="C11orf95", arr.ind=TRUE)]<-"ZFTA"

disfreqs<-table(unlist(str_split(genesonly$Diseases, ",|;")))

names(disfreqs)<-c("Astrocytoma", "Diffuse\nMidline\nGlioma", "Ependymoma", 
                   "Glioblastoma", "Other\nGlioma", "High-Grade \n Glioma", "Low-Grade \n Glioma", 
                   "Oligodendro- \n glioma", "Oligoastrocytoma \n (outdated diag.)")
disfreqs<-disfreqs[order(disfreqs, decreasing = TRUE)]
consums<-genesonly[c(13:18,20)]
consums<-unlist(lapply(confreqs,function(x){sum(x)}))
#confreqs[1]<-confreqs[1]+confreqs[2]
#confreqs<-confreqs[-2]


names(confreqs)<-c("Review", "Clinical \n Standard", 
                   "Clinical \n Supplemental", "Basic \n Research", 
                   "Population \n Studies", 
                   "Clinical \n Algo/ML", "Total")
}

barplot(confreqs, main="Research Contexts of Single-Gene Markers of IMSCTs", 
        ylab = "Number of Genes", 
        sub=" * Excludes large-scale chromosomal aberrations and tests across gene families")

## ^ these need to be redone in ggplot

p<-ggplot(data=data.frame(disfreqs), aes(Var1,Freq)) + 
  geom_bar(stat="identity") + 
  labs(title="Single-Gene IMSCT Markers Researched in Different Diseases", 
       y="Number of Genes", x=NULL) +
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5) +
  theme_minimal()
plot(p)
ggsave(filename="imsct_diseases.jpeg", 
       plot=p,
       device ="jpeg", 
       width=11, 
       height=7, 
       units = "in")

##############################################################################
##### gene set enrichment ####################################################
##############################################################################


library(enrichR)
library(ggplot2)
library(KEGGREST)
mydbs <- c("GO_Biological_Process_2023", 
           "GO_Molecular_Function_2023", 
           "GO_Cellular_Component_2023", 
           "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X"
           )

setwd("E:/Rwd/IMSCT/20240229_plots")
#mydbs<-mydbs[8]
enr<-enrichr(genesonly$Gene, mydbs)
enr<-enr[]
for(dfnum in 1:length(enr)){
  print(names(enr)[dfnum])
  p<-plotEnrich(enr[[dfnum]], showTerms=20, numChar=50, title = names(enr)[dfnum])
  print("hello")
  ggsave(filename=paste(names(enr)[dfnum], ".jpeg", sep=""), 
         plot=p,
         device ="jpeg", 
         width=8, 
         height=6, 
         units = "in")
  
  write.csv(enr[[dfnum]], file=paste(names(enr)[dfnum], ".csv", sep=""), quote=FALSE)
}

mydbs<-"KEGG_2021_Human"
enr<-enrichr(genesonly$Gene, mydbs)
enr<-enr$KEGG_2021_Human
enr<-enr[str_detect(enr$Term, "athway|metab|senes"),]
enr<-enr[enr$Adjusted.P.value<0.1,]
kegg<-keggList("pathway", "hsa")
keggids<-names(kegg[str_detect(kegg, str_flatten(enr$Term, collapse="|"))])

p<-plotEnrich(enr, showTerms=20, numChar=50, title = "KEGG Signalling Pathways (2021)")
ggsave(filename="kegg_genes.jpeg", 
       plot=p,
       device ="jpeg", 
       width=8, 
       height=6, 
       units = "in")
write.csv(enr, "kegg_genes.csv", quote=F)

##################################################################
##### Pathview for KEGG pathways #################################
##################################################################

library(pathview)

#setwd("E:/Rwd/IMSCT/20240220_pv")
genesums<-as.numeric(genesonly$SUM.total.papers.)
#plot(genesums)
#fac<-0.64
#genesums<-fac*genesums/max(genesums)+(1-fac)
#plot(genesums)
names(genesums)<-genesonly$Gene
discrete<-list(gene=FALSE, cpd=FALSE)
limit<-list(gene = 27, cpd = 1)
dirs<-list(gene=TRUE, cpd=TRUE)
pv<-pathview(gene.data=genesums, 
             pathway.id = keggids[1], 
             kegg.native = F, 
             out.suffix = "",
             gene.idtype = "SYMBOL", 
             plot.col.key = T, 
             discrete = discrete, 
             limit = limit, 
             both.dirs = dirs)

##################################################################
##### Building ChromoMap files ###################################
##################################################################

library(rtracklayer)
genes<-readGFF("imsct_genes.refGene.gtf")
genes<-genes[!str_detect(genes$seqid, "_"),]
genes[,1]<-sapply(genes[,1],as.character)
colnames(genes)[1]<-"seqid"
genes[which(genes=="C11orf95", arr.ind=TRUE)]<-"ZFTA"
#genesu<-cbind(index=1:730, genes)
genesu<-data.frame(t(sapply(unique(genes$gene_id), function(x){
  rows<-genes$gene_id==x
  g<-genes[rows,]
  mn<-min(g$start)
  mx<-max(g$end)
  g<-g[1,]
  g[1,4]<-mn
  g[1,5]<-mx
  g
})))
#genesu<-data.frame(t(genesu),stringsAsFactors = F)
genesu<-as.data.frame(sapply(genesu, unlist))
#colnames(genesu)<-colnames(genes)
genesu<-genesu[order(genesu$gene_id),]
counts<-sapply(genesu$gene_id, function(x){imsct$SUM.total.papers.[imsct$Gene==x]})
genesonly<-genesonly[!str_detect(genesonly$Gene, 
                                str_flatten(setdiff(genesonly$Gene, 
                                                    genesu$gene_id), 
                                            collapse="|")),]
annot_file5<-data.frame(genesu[9],
                        genesu[1], 
                        genesu[4],
                        genesu[5],
                        counts)
write.table(annot_file5, "annot_file5.txt", quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)

fus<-imsct[!is.na(imsct$FUSION.PARTNER),]
ones<-1:length(fus[1])

link_data<-cbind(fus[1],
                 ones,
                 fus[8],
                 ones)
one<-list()
two<-list()
repeats<-list()
for(i in 1:length(fus[,1])){
  new1<-fus[i,1]
  new2<-fus[i,8]
  seen1<-str_detect(two, new1)
  seen2<-str_detect(one, new2)
  if(any(seen1&seen2)){
    repeats<-append(repeats,i)
  } else{
    one<-append(one, new1)
    two<-append(two, new2)
  }
}

link_data<-link_data[-unlist(repeats),]
colnames(link_data)<-NULL
row.names(link_data)<-NULL
## NB can't seem to get links working properly due to some bizarre type error in chromomap


confreqs<-genesonly[c(13:19)]
confreqs[1]<-confreqs[1]+confreqs[2]
confreqs<-confreqs[-2]
names(confreqs)<-c("Rev", "CSt", 
                   "CSp", "Res", 
                   "Pop", 
                   "Alg")
chrs<-paste("chr", unlist(lapply(genesu$seqid, as.character)), sep="")
genesdata<-cbind(genesu[9],
                 genesu[1], 
                 genesu[4],
                 genesu[5], 
                 confreqs)
dvs<-str_flatten(colnames(genesdata)[1:4],collapse=" + ")
ivs<-str_flatten(colnames(genesdata)[5:10],collapse=" + ")
form<-paste(dvs, ivs, sep=" ~ ")
library(Hmisc)
genesdata<-meltData(formula(form), data=genesdata)
genesdata<-genesdata[genesdata$value!=0,]
genesdata<-genesdata[,c(1,2,3,4,6,5)]
annot_file4<-apply(genesdata, 2, as.character)
write.table(annot_file4, "annot_file4.txt", quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)




chrleng<-read.table("chrlengs.txt")
chrleng<-chrleng[1:2]
colnames(chrleng)<-c("Chr", "Length")
ogorder<-order(chrleng$Chr)
chrleng$Chr<-lapply(chrleng$Chr, function(x) str_glue("chr", x))
chrleng<-chrleng[order(unlist(chrleng$Chr)),]

cents<-read.table("centromeres.txt")

chrs<-unique(cents$V2)
mins<-lapply(chrs, function(chr){
  chrtable<-cents[cents$V2==chr,]
  min(chrtable[3])
})
chrleng$Centromere<-mins
cents<-data.frame(chr=chrs, centromerestart=unlist(mins))

ones<-data.frame(rep(1, length(chrleng$Chr)))
chr_file<-cbind(chrleng[1], ones, chrleng[2:3])
chr_file<-apply(chr_file, 2, as.character)
chr_file<-chr_file[order(as.numeric(str_remove(chrleng$Chr, "chr"))),]
write.table(chr_file, "chr_file.txt", quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)



annot_file<-cbind(genes$transcript_id, 
                  unlist(chrs), 
                  genes$start, 
                  genes$end, 
                  genes$gene_id)
annot_file2<-cbind(genesu$gene_id, 
                   unlist(chrs), 
                   genesu$start, 
                   genesu$end, 
                   counts)
annot_file2<-apply(annot_file2, 2, as.character)
write.table(annot_file2, "annot_file3.txt", quote=FALSE, sep='\t', row.names = FALSE, col.names = FALSE)

###################################################################
##### Using chromoMap #############################################
###################################################################

library(chromoMap)
#chromoMap("chr_file.txt", "annot_file2.txt", labels=T)
chromoMap("chr_file.txt", annot5,#"annot_file5.txt", 
          labels=T, 
          segment_annotation = F,
          export.options = T,           
          label_angle = -65,
          chr_length = 8,
          chr_width = 20,
          canvas_width = 1000, 
          #data_based_color_map = T, 
          #data_type = "numeric",
          #data_colors = list(c("gray","red")),
          #show.links = T,
          #loci_links = as.data.frame(link_data),
          #links.colors = "blue",
          #plots="bar",
          #plot_filter=list(c("col","byCategory")),
          #ch2D.colors=c("black", "red4", "orange4", "green4", "blue4", "purple4"),
          #heat_map=F, 
          #title="IMSCT genes by research context",
          #plot_height=50,
          legend=T,
          lg_y=1000,
          plot.legend.labels = "Literature Mentions"
)

library(shiny)
# Define UI for application that draws chromoMap
ui <- fluidPage(
  
  # Application title
  #titlePanel("IMSCT genes"),
  
  # you can use GUI controls for your chromoMap
  sidebarLayout(
    sidebarPanel(
      #some code
    ),
    
    # Show a plot of the generated distribution
    mainPanel=mainPanel(
      chromoMapOutput("myChromoMap")
    )
  )
)

# Define server logic required to draw chromoMap
server <- function(input, output) {
  
  output$myChromoMap <- renderChromoMap({
    chromoMap("chr_file.txt", "annot_file5.txt", 
              labels=T, 
              segment_annotation = F,
              export.options = T,           
              label_angle = -65,
              chr_length = 8,
              chr_width = 20,
              canvas_width = 1000, 
              data_based_color_map = T, 
              data_type = "numeric",
              data_colors = list(c("gray","red")),
              #plots="bar",
              #plot_filter=list(c("col","byCategory")),
              #ch2D.colors=c("black", "red4", "orange4", "green4", "blue4", "purple4"),
              #heat_map=F, 
              #title="IMSCT genes by research context",
              #plot_height=50,
              #show.links = T,
              #loci_links = link_data,
              #links.colors = "blue",
              legend=T,
              lg_y=1000,
              plot.legend.labels = "Literature Mentions"
              )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

saveRDS(genesu, "genesu")
saveRDS(imsct, "imsct_proc")
saveRDS(genesonly, "genesonly")
saveRDS(link_data, "link_data")

###############################################################
######## tables of interesting categories #####################
###############################################################

factors<-unique(str_replace_all(str_replace_all(unlist(str_split(imsct$Diseases, ",|;")), " ", ""), "ATRO", "ASTRO"))[1:10]
tbl<-sapply(factors, function(factor) {sum(str_detect(imsct$Diseases, factor), na.rm=TRUE)})
write.table(tbl, "diseases.tsv", sep = "\t")
pie(tbl)

factors<-unique(str_replace_all(str_replace_all(unlist(str_split(imsct$Mutation..YES.NO.FUSION.NFUSION.METHYLATION.NMETHYLATION., ",|;")), " ", ""), "ATRO", "ASTRO"))[-1]
tbl<-sapply(factors, function(factor) {sum(str_detect(imsct$Mutation..YES.NO.FUSION.NFUSION.METHYLATION.NMETHYLATION., factor), na.rm=TRUE)})
write.table(tbl, "muts.tsv", sep = "\t")
pie(tbl)

factors<-unique(str_replace_all(str_replace_all(unlist(str_split(imsct$INC..INC.EXP.AMP..vs..DEC..DEC.EXP....or.NINC..NDEC, ",|;")), " ", ""), "ATRO", "ASTRO"))[-1]
tbl<-sapply(factors, function(factor) {sum(str_detect(imsct$INC..INC.EXP.AMP..vs..DEC..DEC.EXP....or.NINC..NDEC, factor), na.rm=TRUE)})
write.table(tbl, "exp.tsv", sep = "\t")
pie(tbl)

factors<-unique(str_replace_all(str_replace_all(unlist(str_split(imsct$DEL.LOH.LOF..chromosomal.monosomy..YES.NO., ",|;")), " ", ""), "ATRO", "ASTRO"))[-1]
tbl<-sapply(factors, function(factor) {sum(str_detect(imsct$DEL.LOH.LOF..chromosomal.monosomy..YES.NO., factor), na.rm=TRUE)})
write.table(tbl, "muts2.tsv", sep = "\t")
pie(tbl)

factors<-unique(str_replace_all(str_replace_all(unlist(str_split(imsct$Detected.as.MARKER.ANTIBODY..YES.NO., ",|;")), " ", ""), "ATRO", "ASTRO"))[-3]
tbl<-sapply(factors, function(factor) {sum(str_detect(imsct$Detected.as.MARKER.ANTIBODY..YES.NO., factor), na.rm=TRUE)})
write.table(tbl, "marker-ab.tsv", sep = "\t")
pie(tbl)






#############################################################################
##### for 2+ mentions only ##################################################
#############################################################################

imsct_2<-imsct[which(imsct$SUM.total.papers.>1),]
imsct_2$Gene

#processing proteins into genes
imsct_2[which(imsct_2=="synaptophysin", arr.ind=TRUE)]<-"SYP"
imsct_2[which(imsct_2=="VIMENTIN", arr.ind=TRUE)]<-"VIM"
imsct_2[which(imsct_2=="HIST1H3B", arr.ind=TRUE)]<-"H3C2"
imsct_2[which(imsct_2=="HIST1H3C", arr.ind=TRUE)]<-"H3C3"
#Merlin protein, an entry under gene, is encoded by the gene NF2, which has a separate entry
merlin<-imsct_2[which(imsct_2$Gene=="Merlin"),]
imsct_2[which(imsct_2=="NF2", arr.ind=TRUE)[1], 14]<-imsct_2[which(imsct_2=="NF2", arr.ind=TRUE)[1], 14]+merlin[14]
imsct_2[which(imsct_2=="NF2", arr.ind=TRUE)[1], 15]<-imsct_2[which(imsct_2=="NF2", arr.ind=TRUE)[1], 15]+merlin[15]
imsct_2[which(imsct_2=="NF2", arr.ind=TRUE)[1], 21]<-imsct_2[which(imsct_2=="NF2", arr.ind=TRUE)[1], 21]+merlin[21]
imsct_2[which(imsct_2=="NF2", arr.ind=TRUE)[1], 22]<-imsct_2[which(imsct_2=="NF2", arr.ind=TRUE)[1], 22]+merlin[22]
imsct_2[which(imsct_2=="PDGF-B", arr.ind=TRUE)]<-"PDGFB"




imsct_2<-imsct_2[-which(str_detect(imsct_2$Gene, "[a-z]+")),]
imsct_2[is.na(imsct_2)]<-0
View(imsct_2)
imsct_2$SD<-apply(imsct_2[14:20], 1, sd)
imsct_2$variance<-apply(imsct_2[14:20], 1, var)


