


if(require(FactoMineR) == FALSE){
  install.packages("FactoMineR")
  library(FactoMineR)
}


Pizza<-read.table(file="Pizza.csv",header=TRUE, sep=",")
Pizza[2]<-NULL
PCA.Pizza<-PCA(Pizza[1:8],quali.sup = c(1),scale.unit =TRUE )

View(PCA.Pizza$eig)

View(PCA.Pizza$var$contrib)

