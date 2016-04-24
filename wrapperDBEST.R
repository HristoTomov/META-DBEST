wrapperDBEST <- function(x, stackDates) {
        
        if(any(is.na(x))) { ## filter NA
                
                return(c(NA, NA, NA, NA, NA, NA, NA))                
        } else {
                
                DBEST.results <- tryCatch({
                        
                        ndvi.pixel <- zoo(x,stackDates, frequency=24)
                        ndvi.pixel.monthly <- aggregate(ndvi.pixel, as.yearmon(index(ndvi.pixel)),"mean")
                        
                        OutputDBEST=DBEST(ndvi.pixel.monthly, 
                                          seasonality=12,
                                          data.type="cyclical", 
                                          algorithm="change detection", 
                                          breakpoints.no=1, 
                                          first.level.shift=0.1, 
                                          second.level.shift=0.2, 
                                          duration=24, 
                                          distance.threshold="default", 
                                          alpha=0.05)
                        
                        TrendSegments <- OutputDBEST$SegmentNo
                        StartYear <- as.numeric(format( as.Date(time(ndvi.pixel.monthly)[OutputDBEST$Start]), "%Y"))
                        Duration <- OutputDBEST$Duration
                        EndYear <- as.numeric(format(
                                as.Date(time(ndvi.pixel.monthly)[
                                        OutputDBEST$End]), "%Y"))
                        Change <- OutputDBEST$Change
                        ChangeType <- OutputDBEST$ChangeType
                        Significance <- OutputDBEST$Significance
                        return(c(TrendSegments,StartYear,Duration,EndYear,Change,ChangeType,Significance))

                        }, error=function(e){
                                
                                return(c(NA, NA, NA, NA, NA, NA, NA))
                                
                        }) ## end try catch
                return(DBEST.results)
        }
}

