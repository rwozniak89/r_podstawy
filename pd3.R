#Zadania:

####################################
#1.Utworz funkcje: rankAccount <- function(dataFrame,colName,groupName,valueSort,num) 
#ktora bedzie zwracala dla danej tabeli(dataFrame) n wierszy posiadajace najwieksze 
#wartosci(sortowanie po kolumnie valueSort) 
#dla wybranej grupy(konkretna wartosc komorki , np. "NAUCZYCIEL) z kolumny(colName) 
#np. occupation-zawod. 
# num to ile ma zwrócić jak top, limit


df_konta1 <- read.csv(file = 'konta.csv')
#View(konta1)
class(df_konta1)
object.size(df_konta1)

rankAccount <- function(dataFrame,colName,groupName, valueSort,num) {
  df <- dataFrame
  #sprawdzanie kolumn
  if (colName %in% colnames(df)) { print(paste0(colName, " jest kolumna!"))
  } else { print(paste0(colName, " nie ma takiej kolumny!"))
    stop("BŁĄD - nie ma takiej kolumny") }
  if(valueSort %in% colnames(df)) { print(paste0(valueSort, " jest kolumna!"))
  } else { print(paste0(valueSort, " nie ma takiej kolumny!")) 
    stop("BŁĄD - nie ma takiej kolumny") }
  
  wynik_filtorwania <- df[df[colName] == groupName,]
  wynik_sortowania <- wynik_filtorwania[order(-wynik_filtorwania[valueSort]),]
  
  #sprawdzanie limitu, czy nie przekracza
  wynik_limitu <- NA
  liczba_wierszy <- nrow(wynik_sortowania)
  if(num < liczba_wierszy) { wynik_limitu <- head(wynik_sortowania, num)
  print(paste0("Dobry znak! - wskazana wartośc ", num, " jest mniejsza niż liczba wierszy ", liczba_wierszy)) 
  } else {
    wynik_limitu <- wynik_sortowania
    print(paste0("wskazana wartośc ", num, " przekracza liczbe wierszy ", liczba_wierszy, "dlatego max ograniczono")) 
  }
  wynik_limitu
}

colName <- "occupation"
groupName <- "NAUCZYCIEL"
valueSort = "saldo"
num<- 10

wynik_zad3_1 <- rankAccount(df_konta1, colName, groupName, valueSort, num)
wynik_zad3_1

##########################################
#2.Tak jak w 1 tylko z uzyciem datachunku. 
#przyklad naglowka:


rankAccountBigDatatoChunkV2 <- function(filename , size, colName, groupName, valueSort, num){
  header<- TRUE
  sep=","
  counter<-0
  nrows_size = 1
  print(paste0("rozmiar przeszukiwanych wierszy danych -linijka po linijce to:", size ))
  fileConnection<- file(description = filename, open = "r")
  data<-read.table(fileConnection,nrows=nrows_size,header=header,fill=TRUE,sep=sep)
  if(data1[,colName] != groupName)
  {
    data <- data[0,]
    safe_trigger = 1
  }
  
  
  #print(paste0("data: ", data))
  #columnNames<-names(data)
  #print(paste0("columnNames: ", columnNames))
  #View(data)
  #print(class(data))
  #print(object.size(data))
  repeat{
    if((safe_trigger == 0 & nrow(data)==0) | counter >= size){
      close(fileConnection)
      break
    }
    #print(paste0("nrow(data): ", nrow(data)))
    #print(paste0("counter: ", counter))
    data1<-read.table(fileConnection,nrows=nrows_size,col.names = columnNames,fill=TRUE,sep=sep)
    #print(paste0("data1: ", data1))
    #print(class(data1))
    #columnNames<-names(data1)
    #print(paste0("columnNames: ", columnNames))
    nro_data1 = nrow(data1)
    if(nro_data1 >0)
    {
      if(data1[,colName] == groupName)
      {
        nro = nrow(data)
        if(nro < num)
        {
          data <- rbind(data, data1)
        }
        else
        {
          min1 = (data1[,valueSort])
          minData = min(data[,valueSort])
          #print(paste0("2min data: ", minData," min data1: ", min1, ' len data:', nro, " num:", num ))
          if(min1 > minData)
          {
            data <- data[data[,valueSort] != minData, ]
            data <- rbind(data, data1)
          }
        }
        safe_trigger = 0
      }
    }
    
    counter<-counter +  nrows_size
    #if(counter %% 1000 == 0) print(paste0("counter:", counter))
  }
  #len = length(data)
  #nro = nrow(data)
  #print(paste0(' len data:', len, " num:", num, " nro:", nro ))
  wynik_sortowania <- data[order(-data[valueSort]),]
  wynik_sortowania
}

filename = "konta.csv"
size = 3000
colName <- "occupation"
groupName <- "NAUCZYCIEL"
valueSort = "saldo"
num<- 10

wynik_zad3_2v2 <- rankAccountBigDatatoChunkV2(filename, size, colName, groupName, valueSort, num)
wynik_zad3_2v2


#3.SPRAWIDZIC CZY DA SIE ZROBIC TO SAMO W zapytaniu SQL dla takich wartosci jak: tabelaZbazyDanych,occupation, nauczyciel, saldo
#install.packages("odbc")
library(odbc)
#install.packages("RPostgres")
library(RPostgres)
# przygotowanie
# połączenie i zapis do bazy danych tabeli z csv
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

###############
#część własciwa
#odczyt z bazy danych

rankAccountWithDB <- function(tabelaZbazyDanych ,colName, groupName, valueSort, num){
  query <- paste0("SELECT * FROM ", tabelaZbazyDanych, " where ", colName, " = '", groupName, "'")
  query_order <- paste(query, " order by ", valueSort, " desc")
  query_limit <- paste(query_order, " limit ", num)
  wynik <- dbGetQuery(conn, query_limit)
  wynik
}

conn<-connectMe()
#dbListTables(conn) T test tabeli
#dbGetQuery(conn,"SELECT * FROM konta ORDER BY saldo Limit 10") # test prostego zapytania

tabelaZbazyDanych = "konta"
colName <- "occupation"
groupName <- "NAUCZYCIEL"
valueSort = "saldo"
num<- 10

wynik_zad3_3 <- rankAccountWithDB(tabelaZbazyDanych ,colName, groupName, valueSort, num)
wynik_zad3_3