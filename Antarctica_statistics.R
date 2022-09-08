##Environment Preparation
#setwd(".")
#dir.create("./Antarctica")
#setwd("./Antarctica")
#dir.create("Data")
#dir.create("Results")
#dir.create("Figures")

##Data Preparation
install.packages("readr")
library(readr)
data<-read.csv("Data/all_data.csv",sep=";")
str(data)
data$WATER<-as.factor(data$WATER)
data$HUMAN<-as.factor(data$HUMAN)
data$BIOLOGY<-as.factor(data$BIOLOGY)

##Data exploration
str(data)
summary(data)

##Descriptive statistics of the variables
install.packages("psych")
library(psych) 
onlyvariables<-data[,5:38]
descriptive<-describe(onlyvariables)

install.packages("openxlsx")
library(openxlsx)
write.xlsx(descriptive,"./Results/descriptive.xlsx")


##Pairwise correlations - using Spearman
install.packages("corrplot")
library(corrplot)


data_corr<-cor(data[,3:38],method=c('spearman'))
write.csv(data_corr,"./Results/correlation.csv")


newmat<-data.matrix(data[,3:38])
p_values<-cor.mtest(newmat, conf.level=0.95,method='spearman',exact=FALSE)
corr_plot<-corrplot.mixed(data_corr_spearman,tl.cex = 0.6,tl.srt=45,cl.ratio=0.1,  number.cex=0.6, order='original' ,p.mat=p_values_spearman$p, insig='blank', win.asp=1)



#3Network plot of the correlations
install.packages("qgraph")
library(qgraph)

groups<-qgraph(data_corr,graph='cor',layout='spring',threshold=0.6, label.cex=2,usePCH=FALSE)


##Principal Components Analysis (PCA) - only with organics
CECs<-read.csv("data/CECs.csv",sep=";")
#First we evaluate the variance of the variables
variance_CECs<-round(apply(CECs[,2:9], 2, var),3)
variance_CECs

#As variance differ a lot among variables, data were scaled (mean centered and divided by standard deviation)
CECs_scaled<-prcomp(CECs[,2:9],center=TRUE,scale=TRUE)
summary(CECs_scaled)
plot(CECs_scaled,type="l")

#Data for scores and loadings plots - done with Origin 2017 software
write.csv(CECs_scaled$x,"./results/PCA_Scores.csv")
write.csv(CECs_scaled$rotation,"./results/PCA_loadings.csv")

#Elaborate general PCA biplots
install.packages("factoextra")
library(ggplot2)
library(factoextra)


#Biplots with only the variables along the first four PCs combined 
fviz_pca_var(CECs_scaled, axes=c(1,2), col.var ="contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = FALSE, xlab="PC1 (59%)", ylab="PC2 (25%)",geom.var = c("point", "text"),labelsize=6,pointsize=3)+theme(text = element_text(size = 18),axis.title = element_text(size = 18),axis.text = element_text(size = 18))
fviz_pca_var(CECs_scaled, axes=c(3,4), col.var ="contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = FALSE,  xlab="PC3 (7%)", ylab="PC4 (5%)",geom.var = c("point", "text"),labelsize=6,pointsize=3)+theme(text = element_text(size = 18),axis.title = element_text(size = 18),axis.text = element_text(size = 18))
fviz_pca_var(CECs_scaled, axes=c(1,3), col.var ="contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = FALSE, xlab="PC1 (59%)", ylab="PC3 (7%)",geom.var = c("point", "text"),labelsize=6,pointsize=3)+theme(text = element_text(size = 18),axis.title = element_text(size = 18),axis.text = element_text(size = 18))
fviz_pca_var(CECs_scaled, axes=c(1,4), col.var ="contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = FALSE, xlab="PC1 (59%)", ylab="PC4 (5%)",geom.var = c("point", "text"),labelsize=6,pointsize=3)+theme(text = element_text(size = 18),axis.title = element_text(size = 18),axis.text = element_text(size = 18))
fviz_pca_var(CECs_scaled, axes=c(2,3), col.var ="contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = FALSE, xlab="PC2 (25%)", ylab="PC3 (7%)",geom.var = c("point", "text"),labelsize=6,pointsize=3)+theme(text = element_text(size = 18),axis.title = element_text(size = 18),axis.text = element_text(size = 18))
fviz_pca_var(CECs_scaled, axes=c(2,4), col.var ="contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = FALSE, xlab="PC2 (25%)", ylab="PC4 (5%)",geom.var = c("point", "text"),labelsize=6,pointsize=3)+theme(text = element_text(size = 18),axis.title = element_text(size = 18),axis.text = element_text(size = 18))


#Biplots with only samples along the first four PCs combined
fviz_pca_ind(CECs_scaled, axes=c(1,2), col.ind = "blue", repel = FALSE,geom = c("point", "text"),labelsize = 6,pointsize=3,xlab="PC1 (59%)", ylab="PC2 (25%)")+theme(text = element_text(size = 18),axis.title = element_text(size = 18),axis.text = element_text(size = 18))
fviz_pca_ind(CECs_scaled, axes=c(3,4), col.ind = "blue", repel = FALSE,geom = c("point", "text"),labelsize = 6,pointsize=3,xlab="PC3 (7%)", ylab="PC4 (5%)")+theme(text = element_text(size = 18),axis.title = element_text(size = 18),axis.text = element_text(size = 18))
fviz_pca_ind(CECs_scaled, axes=c(1,3), col.ind = "blue", repel = FALSE,geom = c("point", "text"),labelsize = 6,pointsize=3,xlab="PC1 (59%)", ylab="PC3 (7%)")+theme(text = element_text(size = 18),axis.title = element_text(size = 18),axis.text = element_text(size = 18))
fviz_pca_ind(CECs_scaled, axes=c(1,4), col.ind = "blue", repel = FALSE,geom = c("point", "text"),labelsize = 6,pointsize=3,xlab="PC1 (59%)", ylab="PC4 (5%)")+theme(text = element_text(size = 18),axis.title = element_text(size = 18),axis.text = element_text(size = 18))
fviz_pca_ind(CECs_scaled, axes=c(2,3), col.ind = "blue", repel = FALSE,geom = c("point", "text"),labelsize = 6,pointsize=3,xlab="PC2 (25%)", ylab="PC3 (7%)")+theme(text = element_text(size = 18),axis.title = element_text(size = 18),axis.text = element_text(size = 18))
fviz_pca_ind(CECs_scaled, axes=c(2,4), col.ind = "blue", repel = FALSE,geom = c("point", "text"),labelsize = 6,pointsize=3,xlab="PC2 (25%)", ylab="PC4 (5%)")+theme(text = element_text(size = 18),axis.title = element_text(size = 18),axis.text = element_text(size = 18))

#Biplots with variables and samples along the first four PCs combined
fviz_pca_biplot(CECs_scaled, axes=c(1,2), col.ind = "blue", repel = FALSE,geom = c("point", "text"),col.var ="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),labelsize = 4,pointsize=1,xlab="PC1 (59%)", ylab="PC2 (25%)")+theme(text = element_text(size = 15),axis.title = element_text(size = 15),axis.text = element_text(size = 15))
fviz_pca_biplot(CECs_scaled, axes=c(3,4), col.ind = "blue", repel = FALSE,geom = c("point", "text"),col.var ="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),labelsize = 4,pointsize=1,xlab="PC3 (7%)", ylab="PC4 (5%)")+theme(text = element_text(size = 15),axis.title = element_text(size = 15),axis.text = element_text(size = 15))
fviz_pca_biplot(CECs_scaled, axes=c(1,3), col.ind = "blue", repel = FALSE,geom = c("point", "text"),col.var ="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),labelsize = 4,pointsize=1,xlab="PC1 (59%)", ylab="PC3 (7%)")+theme(text = element_text(size = 15),axis.title = element_text(size = 15),axis.text = element_text(size = 15))
fviz_pca_biplot(CECs_scaled, axes=c(1,4), col.ind = "blue", repel = FALSE,geom = c("point", "text"),col.var ="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),labelsize = 4,pointsize=1,xlab="PC1 (59%)", ylab="PC4 (5%)")+theme(text = element_text(size = 15),axis.title = element_text(size = 15),axis.text = element_text(size = 15))
fviz_pca_biplot(CECs_scaled, axes=c(2,3), col.ind = "blue", repel = FALSE,geom = c("point", "text"),col.var ="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),labelsize = 4,pointsize=1,xlab="PC2 (25%)", ylab="PC3 (7%)")+theme(text = element_text(size = 15),axis.title = element_text(size = 15),axis.text = element_text(size = 15))
fviz_pca_biplot(CECs_scaled, axes=c(2,4), col.ind = "blue", repel = FALSE,geom = c("point", "text"),col.var ="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),labelsize = 4,pointsize=1,xlab="PC2 (25%)", ylab="PC4 (5%)")+theme(text = element_text(size = 15),axis.title = element_text(size = 15),axis.text = element_text(size = 15))



