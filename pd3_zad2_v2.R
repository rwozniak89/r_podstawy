
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
size = 5000
colName <- "occupation"
groupName <- "NAUCZYCIEL"
valueSort = "saldo"
num<- 10

wynik_zad3_2v2 <- rankAccountBigDatatoChunkV2(filename, size, colName, groupName, valueSort, num)
wynik_zad3_2v2
