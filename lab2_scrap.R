
# install.packages( c("RSelenium","seleniumPipes","dplyr") )
# install.packages( "stringr" )
# install.packages( "gtools" )
# install.packages( "rvest" )
library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)
library(rvest)
# ?remoteDr
remDr <- remoteDr(remoteServerAddr = "http://localhost",
                  port = 4444,
                  browserName = "chrome",
                  newSession = TRUE
                  )

remDr%>% go("https://www.otodom.pl/sprzedaz/mieszkanie")


wektorLinkow <-c()

for(i in 1:2){
  newUrl<- paste0("https://www.otodom.pl/sprzedaz/mieszkanie/?page=",i)
  remDr%>%go(newUrl)
  elems<-remDr%>%findElements(using = "tag name", "h3")
  
  for ( j in 1:length(elems)){
    #  print(elems[[j]])%>%getElementText()
    e<-findElementsFromElement(elems[[j]],using = "tag name", "a")
    if(length(e)>0){
      link<-e[[1]]%>%getElementAttribute("href")
      print(link)
      wektorLinkow<-c(wektorLinkow,link)
    }
  }
}


wektorLinkowU<-wektorLinkow%>%unique()


zrobWiersz<-function(w,wektorLinkowU,remDr){
  remDr%>%go(wektorLinkowU[w])
  el<- remDr%>%findElement(using = "class name","css-srd1q3")
  cena<-NA
  cena<-el%>%getElementText()
  szczegoly<- remDr%>%findElements("class name", "css-18h1kfv")
  listaSzczegolyOpis<-c()
  listaSzczegolyWartosci<-c()
  for( i in 1: length(szczegoly)){
    listaSzczegolyOpis<- c(listaSzczegolyOpis,szczegoly[[i]]%>%findElementsFromElement("class name","css-o4i8bk"))
    listaSzczegolyWartosci<- c(listaSzczegolyWartosci,szczegoly[[i]]%>%findElementsFromElement("class name","css-1ytkscc"))
  }
  nazwyKolumn<- lapply(listaSzczegolyOpis,getElementText) %>% str_replace_all(":","")
  wartosci<- unlist(lapply(listaSzczegolyWartosci,getElementText))
  
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  df1<- cbind(cena,df1)
  df1
}

mieszkania<-NULL
for(w in 1:length(wektorLinkowU) ){
  skip<-FALSE
  tryCatch(
    df1<-zrobWiersz(w=w,wektorLinkowU,remDr = remDr ),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(mieszkania)){
    mieszkania<-df1
  }else{
    mieszkania<-smartbind(mieszkania,df1)
  }
}


#inny sposb na pobieranie własicowsci mieskzan

zrobWierszRvest<-function(w,wektorLinkow,remDr){
  newUrl<-wektorLinkow[w]
  page<-read_html(newUrl)
  cena<-html_node(page,".css-srd1q3")%>%html_text()
  v<- page%>% xml_find_all('/html/body/div[1]/main/div/div[3]/div[1]/*/*')%>%html_attr("title")%>%na.omit()
  
  indexy <-seq(1,length(v),1)
  nazwyKolumn <- v[indexy%%2==1]
  wartosci <- v[indexy%%2==0]
  df1<- data.frame (matrix(wartosci,nrow = 1,ncol=length(wartosci)) )
  print(df1)
  names(df1) <- nazwyKolumn
  df1<-cbind(cena,df1)
}

mieszkania2<-NULL
for(w in 1:length(wektorLinkowU) ){
  skip<-FALSE
  tryCatch(
    df1<-zrobWierszRvest(w=w,wektorLinkowU,remDr = remDr ),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(mieszkania2)){
    mieszkania2<-df1
  }else{
    mieszkania2<-smartbind(mieszkania2,df1)
  }
}

#### inny sposóB na pobieranie linkow


wektorLinkow<-c()
for(i in 1:100){
  newUrl<- paste0("https://www.otodom.pl/sprzedaz/mieszkanie/?page=",i)
  page<-read_html(newUrl)
  result<-page%>%html_nodes(xpath='/html/body/div[3]/main/section[2]/div/div/div[1]/div/article[*]/div[1]/header/h3/a')
  #print(result)
  wektorLinkow<-c(wektorLinkow,xml_attr(result,"href"))
}
wektorLinkowU<-wektorLinkow%>%unique()


mieszkania3<-NULL
liczbaLinkow<-length(wektorLinkowU)
for(w in 1: liczbaLinkow ){
  print(paste0(w," / ",liczbaLinkow ) )
  skip<-FALSE
  tryCatch(
    df1<-zrobWierszRvest(w,wektorLinkowU,remDr ),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(mieszkania3)){
    mieszkania3<-df1
  }else{
    mieszkania3<-smartbind(mieszkania3,df1)
  }
}


