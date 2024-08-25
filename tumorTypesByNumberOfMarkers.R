#Fig 1. Types of intramedullary spinal cord tumors, 
#ranked by total number (%) of markers mentioned. 
source("imsct-quickload.r")
library(ggplot2)

disfreqs<-table(unlist(str_split(imsct$Diseases, ",|;")))

names(disfreqs)<-c("Astrocytoma", "Diffuse\nMidline\nGlioma", "Ependymoma", 
                   "Glioblastoma", "Other\nGlioma", "High-Grade \n Glioma", "Low-Grade \n Glioma", 
                   "Oligo-\ndendroglioma", "Oligo-\nastrocytoma")

disfreqs<-disfreqs[order(disfreqs, decreasing = TRUE)]

title<-"Tumor Types by Number of Markers"
p<-ggplot(data=data.frame(x=names(disfreqs), disfreqs), aes(x=reorder(x,-disfreqs),disfreqs)) + 
  geom_bar(stat="identity") + 
  labs(title=title, 
       y="Number of Markers", x=NULL) +
  geom_text(aes(label=paste0(disfreqs, " (", floor(disfreqs/190*100), "%)")), vjust=-0.3, size=3.5) +
  theme_minimal()

#p
ggsave(filename=paste(str_replace_all(title, " ", "_"),".jpeg"), 
       plot=p,
       device ="jpeg", 
       width=9, 
       height=7, 
       units = "in")
