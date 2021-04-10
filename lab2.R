
install.packages( c("RSelenium","seleniumPipes","dplyr") )

library(RSelenium)
library(seleniumPipes)
library(dplyr)

# ?remoteDr
remDr <- remoteDr(remoteServerAddr = "http://localhost",
                  port = 4444,
                  browserName = "chrome",
                  newSession = TRUE
                  )

remDr%>% go("https://www.otodom.pl/sprzedaz/mieszkanie")