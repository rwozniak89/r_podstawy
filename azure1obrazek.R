
if(require(jpeg) == FALSE){
  install.packages("jpeg")
  library(jpeg)
}

if(require(FactoMineR) == FALSE){
  install.packages("FactoMineR")
  library(FactoMineR)
}

photo <- readJPEG("zz.jpg")

photo <- photo[,,1]+photo[,,2]+photo[,,3]

photo <- photo/max(photo)
w<-PCA(photo, scale.unit = TRUE, ncp = 28, graph = FALSE)
p <- reconst(w)
pw<-writeJPEG(p)
image(p)

View(w$eig)