library(tidyverse)
data<-read.csv("Rhinophylla.csv", sep=",")

data[data$bat=="Rhinophylla alethina",]

prop<-c()
for (i in 1:nrow(data)){
  prop[i]<-data$N[i]/sum(data$N[data$Site==data$Site[i]])
}

data$prop<-prop

batnames<-unique(data[,c("bat", "Site", "Ref")])$bat
data

data<-data[-which(data$Fam=="Unknown"),]
data$batcode<-paste0(data$bat, data$Site)

rhin.mat<-reshape(data[,c("Fam","batcode","prop")], direction="wide", idvar= "Fam", timevar="batcode")
row.names(rhin.mat)<-rhin.mat$Fam
rhin.mat<-rhin.mat[,-1]
rhin.mat[is.na(rhin.mat)]<-0
rhin.mat<-as.matrix(rhin.mat)
inorder<-colnames(rhin.mat)
colnames(rhin.mat)<-batnames


rhin.mat2<-rhin.mat
rhin.mat2[rhin.mat2 >0]<-1 

v<-rowSums(rhin.mat2)
sorted <- order(-v, names(v))
rhin.mat<-rhin.mat[sorted,]


library(bipartite)

png(filename="Fig 2.png", width=5200, height=3600, res=600)
par(las=1, mar=c(4,8,4,1))

plotmatrix(rhin.mat, 
           plot_labels=FALSE,
           binary=FALSE, base_color="#6B9814")

axis(1, at= 1:31, cex.axis=0.8, mgp=c(2,0,0), tick=FALSE)

mtext(text=c(expression(italic("R. alethina"), 
                          italic("R. fischerae"),
                          italic("R. pumilio"))),
      side=1, at = c(1.2,5, 17),line=2, cex=0.9)


axis(2, at = 1:nrow(rhin.mat),
     labels =rownames(rhin.mat)[length(rownames(rhin.mat)):1],cex.axis=0.9,
     mgp=c(2,0.5,0), tick=FALSE)

N<-c(31,"*",
     3 ,9,8,"*","*",
     3,53,35,38,6,8,5,12,5,17,24,4,1,"*",
     3,"*",5,17,"*")
axis(3, at=1:ncol(rhin.mat),labels=N, tick=F, las=1, cex.axis=0.8,mgp=c(2,0,0))
abline(v=c(2.5, 7.5), lty=2)
segments(x0=c(1,3,8), x1=c(2,7,26), y0=-1, y1=-1, xpd=TRUE)
segments(x0=c(1,2,3,7,8,26), x1=c(1,2,3,7,8,26), y0=rep(-1,6), y1=rep(-0.5,6), xpd=TRUE)
segments(x0=0.5, x1=26.5, y0=22, y1=22, xpd=TRUE)
segments(x0=c(0.5,26.5), x1=c(0.5,26.5), y0=c(22,22), y1=c(21.5,21.5), xpd=TRUE)
text(x=12, y= 22.5, labels=expression(bold("Consumption events")), xpd=TRUE, col="#6B9814")
dev.off()
