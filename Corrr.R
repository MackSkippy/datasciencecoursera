corr <- function(directory, threshold = 0){
        #take all csv which meet threshold, then run correlation between sulfate and
        #nitrate columns 
        
        allCases <- NULL
        
        complete <-function(directory, id = 1:332, allCases){
                
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
                corAll = NULL
                for(n in 1:332){
                        
                        if(allCases[n,"nobs"]>threshold)
                                
                                #adjust filename into 3 digit format
                                if (n<10){
                                        fixidat <- paste0("00",n,sep="")
                                }else if (n<100){
                                        fixidat <- paste0("0",n,sep="")
                                }else {fixidat <- n}
                        #create filename using variables
                        fileNamea <- paste(fixidat,".csv",sep="")   
                        #print(fileNamea)
                        corData <-read.csv(fileNamea)
                        good <-complete.cases(corData)
                        #print(corData)
                        corAll <-c(corAll,corData[good,])
                       
                        
                }
                
                print(cor(corAll$nitrate,corAll$sulfate))
                
                
                #Set the working directory back to what it was for GTS comp
                setwd("~/R-Practice/")    
        }
        
        
        
        #create report with all items
        complete(directory)
        
        #Find id with nobs GT threshold
        # thresh <- allCases$nobs > threshold
        #print(thresh)
        #read CSV with those id
        #correlate nitrate and sulfate from this group
        
}