library(reshape2)
library(jpeg)
library(AzureML)



img <- readJPEG('flaga.jpg')
dn<-dim(img)
dn
img<-round(img*255.0)
img<-melt(img)



img<-dcast(img,Var1+Var2~Var3)



name<-c("X","Y","R","G","B")



for (i in 1:length(name)) {
  names(img)[i] <- name[i]
  
}



w<-rgb(img$R,img$G,img$B,maxColorValue = 255)
w


dim(w) = c(dn[2],dn[1])
#dim(w) = c(659,616)
#dim(p) = c(dn[2],dn[1])
library(grid)
grid.raster(w)




#write.table(img,"img.csv",row.names = FALSE, dec = ".", sep = ",", quote = FALSE)



ws <- workspace(id = "3f8beceb3cb0437fac1e938eebc65a38", auth = "76MM5KZhmv3QNNuK5PvAgWT54kwQefdZqOzFXKAfgbR+Bjobu1+HcUC1YrETWRaAUI2iTZfWHoBQHtAl+XybqA==")



upload.dataset(img, ws, "www")