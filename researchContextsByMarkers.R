#Research contexts of intramedullary spine tumor markers.
source("imsct-quickload.r")
library(ggplot2)

confreqs<-imsct[c(13:18)]
confreqs<-unlist(lapply(confreqs,function(x){sum(x)}))
#confreqs[1]<-confreqs[1]+confreqs[2]
#confreqs<-confreqs[-2]


names(confreqs)<-c("Review", "Clinical \n Standard", 
                   "Clinical \n Supplemental", "Basic \n Research", 
                   "Population \n Studies", 
                   "Clinical \n Algo/ML")

title<-"Research Contexts of IMSCT Markers"
p<-ggplot(data=data.frame(x=names(confreqs), confreqs), aes(x=reorder(x,-confreqs),confreqs)) + 
  geom_bar(stat="identity") + 
  labs(title=title, 
       y="Number of Mentions", x=NULL) +
  geom_text(aes(label=paste0(confreqs, " (", floor(confreqs/sum(confreqs)*100), "%)")), vjust=-0.3, size=3.5) +
  theme_minimal()

p
ggsave(filename=paste(str_replace_all(title, " ", "_"),".jpeg"), 
       plot=p,
       device ="jpeg", 
       width=7, 
       height=7, 
       units = "in")

astro<-imsct[str_detect(imsct$Diseases, "ASTRO"),]

confreqs2<-astro[c(13:18)]
confreqs2<-unlist(lapply(confreqs2,function(x){sum(x, na.rm = TRUE)}))
#confreqs[1]<-confreqs[1]+confreqs[2]
#confreqs<-confreqs[-2]


names(confreqs2)<-c("Review", "Clinical \n Standard", 
                   "Clinical \n Supplemental", "Basic \n Research", 
                   "Population \n Studies", 
                   "Clinical \n Algo/ML")

title<-"Research Contexts of Astrocytoma Markers"
p<-ggplot(data=data.frame(x=names(confreqs2), confreqs2), aes(x=reorder(x,-confreqs2),confreqs2)) + 
  geom_bar(stat="identity") + 
  labs(title=title, 
       y="Number of Mentions", x=NULL) +
  geom_text(aes(label=paste0(confreqs2, " (", floor(confreqs2/sum(confreqs2)*100), "%)")), vjust=-0.3, size=3.5) +
  theme_minimal()

p
ggsave(filename=paste(str_replace_all(title, " ", "_"),".jpeg"), 
       plot=p,
       device ="jpeg", 
       width=7, 
       height=7, 
       units = "in")
