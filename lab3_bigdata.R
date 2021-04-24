install.packages("DBI")
library(DBI)
install.packages("RSQLite")
library(RSQLite)
install.packages("RPostgres")
library(RPostgres)
install.packages("rstudioapi")
library(rstudioapi)

install.packages("odbc")
library(odbc)

install.packages("tidyverse")
library(tidyverse)

# do MS SQL https://db.rstudio.com/databases/microsoft-sql-server/

konta <- read.csv("konta.csv")


lengthOfFile<- function(filepath,systemLinuxUnix=FALSE, systemWindwos=FALSE){
  #if(.Platform$OS.type == "unix" )
  if ( systemLinuxUnix){
    l <- try(system(paste("wc -l",filepath),intern=TRUE))
    l<-strsplit(l,split=" ")
    l<-as.numeric(l[[1]])
    l
  }
  else if(systemWindwos){
    l <- try(system(paste('find /v /c "" ',filepath),intern=TRUE))
    l<-strsplit(l,split=" ")
    #l<-as.numeric(l[[1]])
    l
  }
  else{
    l<-length(count.fields(filepath))
    l
  }
}

lengthOfFile("konta.csv", systemWindwos=TRUE)
lengthOfFile("konta.csv")

srednia<- function(filepath,columnname,size,sep=",",header=TRUE){
  suma<-0
  counter<-0
  fileConnection<- file(description = filepath, open = "r")
  data<-read.table(fileConnection,nrows=size,header=header,fill=TRUE,sep=sep)
  columnNames<-names(data)
  
  repeat{
    if(nrow(data)==0 ){
      close(fileConnection)
      break
    }
    data<-na.omit(data)
    counter<-counter+ nrow(data)
    suma<- suma+sum(data[[columnname]])
    data<-read.table(fileConnection,nrows=size,col.names = columnNames,fill=TRUE,sep=sep)
  }
  suma/counter
}

srednia("konta.csv", columnname = "saldo", size = lengthOfFile("konta.csv"))
mean(konta[["saldo"]])

readToBase<-function(filepath,dbpath,tablename,size,sep=",",header=TRUE,delete=TRUE){
  ap<-!delete
  ov<-delete
  fileConnection<- file(description = filepath, open = "r")
  data<-read.table(fileConnection,nrows=size,header=header,fill=TRUE,sep=sep)
  columnNames<-names(data)
  dbConn<-dbConnect(SQLite(),dbpath)
  dbWriteTable(conn = dbConn,name=tablename,data,append=ap,overwrite=ov)
  repeat{
    
    if(nrow(data)==0){
      close(fileConnection)
      dbDisconnect(dbConn)
      break
    }
    data<-read.table(fileConnection,nrows=size,col.names = columnNames,fill=TRUE,sep=sep)
    dbWriteTable(conn = dbConn,name=tablename,data,append=TRUE,overwrite=FALSE)
  }
  
}

readToBase("konta.csv", "konta.sqlite", "konta", size = lengthOfFile("konta.csv") )

con<-dbConnect(SQLite(),"konta.sqlite")
dbGetQuery(con,"SELECT COUNT(*) FROM konta")
dbDisconnect(con)



conMSSQL <- DBI::dbConnect(odbc::odbc(), 
                      Driver = "ODBC Driver 17 for SQL Server", 
                      Server = "(LocalDB)\\MSSQLLocalDB", 
                      Database = "atest1"
                      )
dbGetInfo(conMSSQL)
dbGetQuery(conMSSQL,"SELECT * FROM tabelapkb")
dbDisconnect(conMSSQL)

connectMe<-function(typ=Postgres(),dbname="ulvimbvm",host="tai.db.elephantsql.com",user="ulvimbvm", pass="NBN9Vp6JoWZMfYvu04hc36k7LFm-TeoN"){
  con<-dbConnect(typ,
                 dbname=dbname,
                 host=host,
                 user=user,
                 password=pass) #askForPassword("database pass"))
}

conn<-connectMe()
dbGetInfo(conn)
#dbDisconnect(conn)

readToBaseWithDbConn<-function(filepath,dbConn,tablename,size,sep=",",header=TRUE,delete=TRUE){
  ap<-!delete
  ov<-delete
  fileConnection<- file(description = filepath, open = "r")
  data<-read.table(fileConnection,nrows=size,header=header,fill=TRUE,sep=sep)
  columnNames<-names(data)
  dbWriteTable(conn = dbConn,name=tablename,data,append=ap,overwrite=ov)
  repeat{
    
    if(nrow(data)==0){
      close(fileConnection)
      dbDisconnect(dbConn)
      break
      
    }
    data<-read.table(fileConnection,nrows=size,col.names = columnNames,fill=TRUE,sep=sep)
    dbWriteTable(conn = dbConn,name=tablename,data,append=TRUE,overwrite=FALSE)
  }
}
conn<-connectMe()
readToBaseWithDbConn("konta.csv", conn, "konta", size = lengthOfFile("konta.csv") )
conn<-connectMe()
readToBaseWithDbConn("pjatk_su.csv", conn, "pjatk_su", size = lengthOfFile("pjatk_su.csv") )


readToBaseWithDbConn("pjatk_su.csv", conMSSQL, "pjatk_su", size = lengthOfFile("pjatk_su.csv") )

conn<-connectMe()
dbListTables(conn)
dbDisconnect(conn)

dbListTables(conMSSQL)

readToBase("pjatk_su.csv", "pjatk_su.sqlite", "pjatk_su", size = lengthOfFile("pjatk_su.csv") )

con<-dbConnect(SQLite(),"pjatk_su.sqlite")
dbGetQuery(con,"SELECT COUNT(*) FROM pjatk_su")
dbDisconnect(con)



library(tidyverse)

suicidesFromFile <-read.csv("pjatk_su.csv")
object.size(suicidesFromFile)
conn<-connectMe()
suicideTable <- tbl(conn, "pjatk_su")
class(suicideTable)
suicideTable%>%select(country,year,age,generation)
object.size(suicideTable)
#dbDisconnect(conn)
tabelaR<- suicideTable%>%select(everything())%>%collect()
class(tabelaR)
object.size(tabelaR)

ggplot(data=suicideTable) + geom_bar(aes(x=country)) + coord_flip()
ggplot(data=suicideTable) + geom_bar(aes(x=year)) + coord_flip()