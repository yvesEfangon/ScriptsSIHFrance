
#Transformation du fichier des communes de France téléchargé sur 
#https://www.data.gouv.fr/fr/datasets/kml-des-communes-y-compris-dom-tom/

getFileContent <- function(filename=''){

  file_path    <- paste("2013", filename,sep = '/')
  
  
  content <- read.csv(file = file_path, na.strings = c(''),sep = ',', header = TRUE,colClasses = c("factor","factor","factor") )
  
  content
}


kmlToJson <- function(){
  library(jsonlite)
  library(geojsonR)
  library(rlist)
  library(rgeos)
  library(rgdal)
  library(geosphere)
  
  initGeo<- TO_GeoJson$new()
  options(digits = 20)
  
  fcontent <- getFileContent("Metropo.csv")
  fcontent$KML <- gsub("<Polygon> <outerBoundaryIs> <LinearRing> <coordinates>","",fcontent$KML)
  fcontent$KML <- gsub("</coordinates> </LinearRing> </outerBoundaryIs> </Polygon>","",fcontent$KML)
  fcontent$KML <- trimws(fcontent$KML)
 
  
  fcontent$KML <- strsplit(fcontent$KML," ")
  
  nbRows  <- nrow(fcontent)
  myjson  <- data.frame(city_code=character(nbRows),city = character(nbRows),
                        geometry = as.character(nbRows), area=as.double(nbRows), stringsAsFactors = FALSE)
 
  for(i in 1:nbRows){
    
    
    coords <- unlist(fcontent$KML[i])
    coordinates <- list()
    forArea <- c()
    
    for(j in 1:length(coords)){
      l01 <- coords[j]
      
      if(l01==""){
        next
      }
      
      
      l01 <- unlist(strsplit(l01,","))
     
      coordinates[[length(coordinates)+1]] <- c(as.numeric(l01[1]), as.numeric(l01[2]))
      forArea <- rbind(forArea,c(as.numeric(l01[1]), as.numeric(l01[2])))
    }
    
    if(length(coordinates)<=0){
      next
    }
    
   
    coordinates <- as.list(coordinates)
    myjson$city_code[i] <- as.character(fcontent$Code.commune[i])
    myjson$city[i] <- as.character(fcontent$Nom.Commune[i])
    myjson$geometry[i] <-initGeo$Polygon(list(coordinates), stringify = TRUE)
    myjson$area[i] <- areaPolygon(forArea)
  }
  
  jsonTosave <- jsonlite::toJSON(myjson)
  write(jsonTosave,'Metropo.json')
}
