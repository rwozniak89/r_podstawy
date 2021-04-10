

# biblioteki uuid miniCRAN base64enc dodac przez packages -> installa 
# poterm pobrac https://cran.r-project.org/src/contrib/Archive/AzureML/ i tez zainstalwoać z zipa...
# 
# z https://studio.azureml.net/Home/ViewWorkspaceCached/3f8beceb3cb0437fac1e938eebc65a38#Workspace/Settings/Name
# Workspace ID
# 3f8beceb3cb0437fac1e938eebc65a38
# z https://studio.azureml.net/Home/ViewWorkspaceCached/3f8beceb3cb0437fac1e938eebc65a38#Workspace/Settings/AuthTokens
# Primary Authorization Token
# 76MM5KZhmv3QNNuK5PvAgWT54kwQefdZqOzFXKAfgbR+Bjobu1+HcUC1YrETWRaAUI2iTZfWHoBQHtAl+XybqA==


library(AzureML)

help("AzureML")

ws <- workspace(id = "3f8beceb3cb0437fac1e938eebc65a38", auth = "76MM5KZhmv3QNNuK5PvAgWT54kwQefdZqOzFXKAfgbR+Bjobu1+HcUC1YrETWRaAUI2iTZfWHoBQHtAl+XybqA==")


upload.dataset(photo,ws,"Zdjecie")

#datasets(ws, filter = c("all", "my datasets", "samples"))


#upload.dataset(airquality, ws, "airquality")

#head(datasets(ws))

#delete.datasets(ws, "airquality")