#w AzureML dla Two-Class Neutral Network -> hidden Layer Sepcification -> custom network definition
#input Data auto;
#hidden H1 [10] from Data all;
#hidden H2 [8] from H1 all;
#hidden H3 [6] from H2 all;
#output Result auto from H3 all;
# szczegóły składni "net... https://docs.microsoft.com/en-us/azure/machine-learning/studio-module-reference/two-class-neural-network#bkmk_Customizing

library(AzureML)
ws <- workspace(id = "3f8beceb3cb0437fac1e938eebc65a38", auth = "76MM5KZhmv3QNNuK5PvAgWT54kwQefdZqOzFXKAfgbR+Bjobu1+HcUC1YrETWRaAUI2iTZfWHoBQHtAl+XybqA==")


library(lme4)
set.seed(1)
train <- sleepstudy[sample(nrow(sleepstudy), 120),]
m <- lm(Reaction ~ Days + Subject, data = train)



# Deine a prediction function to publish based on the model:
sleepyPredict <- function(newdata){
  predict(m, newdata=newdata)
}

## Probolem z zip??? Należy dodać zmienną środowiskową uzytkownka (albo też systemową) na  "C:\Rtools\bin\zip"albo tak jak niżej
#Rtools.bin="C:\\Rtools\\bin\\zip"
#sys.path = Sys.getenv("PATH")
#if (Sys.which("zip") == "" ) {
#  system(paste("setx PATH \"", Rtools.bin, ";", sys.path, "\"", sep = ""))
#}
#Sys.which("zip")

publishWebService(ws, fun = sleepyPredict, name="WebService3",
                  inputSchema = sleepstudy,
                  data.frame=TRUE)