complete <-function(directory, id = 1:332){
        
        #set working directory to make it easier to get to data
        setwd(paste0("~/R-Practice/", directory))
        
        #Clear aggregate variable
        idList <-NULL
        completeList <-NULL
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
                
                #count number of complete cases in this ID
                numComp <- sum(complete.cases(DataCopy))
                

                idList <- append(idList,x)
                completeList <- append(completeList, numComp)}
                
               
        #Show Answer
        allCases <-data.frame(idList, completeList)
        names(allCases) <- c("id","nobs")       
        print(allCases)
        
        #Set the working directory back to what it was for GTS comp
        setwd("~/R-Practice/")    
}