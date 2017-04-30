pollutantmean <- function(directory = "specdata", pollutant, id = 1:332){
        #set working directory to make it easier to get to data
        setwd(paste0("~/R-Practice/", directory))
        
        #Clear aggregate variable
        allData <-NULL
        #start looping for operations since multiple csv files
        
        for(x in id){
                #adjust filename into 3 digit format
                if (x<10){
                        fixid <- paste0("00",x,sep="")
                }else if (x<100){
                        fixid <- paste0("0",x,sep="")
                }else {fixid <- x}
                #create filename using variables
                fileName <- paste(fixid,".csv",sep="")   
                
                #Get the data for each file name into a variable
                DataCopy<- read.csv(fileName)
                
                #Take only the pollutant column into temp variable
                y <- DataCopy[,pollutant]
                
                #Create an aggregate vector which is all observations from all files
                allData <- append(allData, y)
        
         
                }
        #Get Mean of the aggregate vector and exclude NA data
        print(mean(allData,na.rm=TRUE))
        
        #Set the working directory back to what it was for GTS comp
        setwd("~/R-Practice/")           
}
#need to adjust id to 3 digits