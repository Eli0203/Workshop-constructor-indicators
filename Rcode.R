##############################################################################################################################
######################WORKSHOP VULNERABILITY#################################################################################
###BY: Eliana Vallejo
####install packages
##########################################################################################
list.of.packages <- c("reshape2", "ggplot2","plyr","dplyr","ggcorrplot","sp","FactoMineR","devtools","factoextra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
####LOAD PACKAGES
library(reshape2);library(plyr);library(dplyr)   ##packages for data management
library(ggplot2);library(ggcorrplot);library(sp) ##packages for graphs
library(FactoMineR);library(factoextra)  ##packages for factorial techniques metodologies
##DATA SET
datos<-read.csv("clipboard",header=T,sep="\t")
###PREPARING DATA
datos<-datos[-which(datos$District%in%"All Rwanda"),]
rownames(datos)<-datos$District
datos$poverty<-datos$poverty..+datos$Extreme.poverty..
datos$Extreme.poverty..<-NULL
datos$poverty..<-NULL
###descriptive data
datos%>%melt( vars.id="District")%>%data.frame(.)%>%ggplot(aes(x=District,y=value,fill=District))+geom_bar(stat="identity")+ facet_wrap(~variable,scales="free_x")+ coord_flip() +guides(fill=F)
ggcorrplot(cor(datos[-1]), hc.order = TRUE, type = "upper",   ggtheme = ggplot2::theme_gray,colors = c("#E46726", "white","#6D9EC1" ))
#################################
#######Factorial technique
res.pca = PCA(datos[,-1], scale.unit=TRUE, ncp=5, graph=F)
res.pca$eig
res.pca$var$coord;res.pca$var$contrib
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10, 
            ggtheme= theme(axis.text.y = element_text(size=16,hjust=.5,vjust=0,face="plain", angle = 90),
                       axis.text.x= element_text(size=18,hjust=0.5,angle=90)))
                 
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE)
datos$coord1<-res.pca$ind$coord[,1]  ###vulnerability indicators
###rescaled
datos$ind<-((datos$coord1-min(datos$coord1))/(max(datos$coord1)-min(datos$coord1)))*100
colnames(datos)[1]<- "NAME_2" 
###########################graphs
map<-readRDS("C:/Users/evallejo/Downloads/RWA_adm2.rds")
datos<-merge(datos,map@data,by="NAME_2");map@data<-datos
l1 <- list("sp.text", coordinates(map), map@data$NAME_2) 
spplot(map,zcol="ind",main="index",colorkey=list(at =seq(0,100,10),space="right",cex=1.5),col.regions= colorRampPalette(c("#8c510a","#bf812d","#dfc27d","#f6e8c3","#80cdc1","#018571"))(18),sp.layout=l1)
