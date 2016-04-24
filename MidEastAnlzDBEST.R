library(zoo)
library(sp)
library(raster)

rasterOptions(progress="text")

# Config section

region.countries <- list (  "BHR", ## Bahrain
                        "CYP", ## Cyprus
                        "EGY", ## Egypt
                        "IRN", ## Iran
                        "IRQ", ## Iraq
                        "ISR", ## Israel
                        "JOR", ## Jordan
                        "KWT", ## Kuwait
                        "LBN", ## Lebanon
                        "OMN", ## Oman
                        "PSE", ## Palestine
                        "QAT", ## Qatar
                        "SAU", ## Saudi Arabia
                        "SYR", ## Syria
                        "TUR", ## Turkey
                        "ARE", ## United Arab Emirates
                        "YEM") ## Yemen

Output.DIR <- "/PATH/TO/Outputs/"
DBEST.DIR <- "/PATH/TO/DBEST/"
MidEastAnlz.DIR <- "/PATH/TO/METADBEST/"
data.DIR <- "/PATH/TO/ndvi3g_81_11/"

data.date.start <- as.yearmon('1982-01')
data.date.end <-  as.yearmon('2010-12')

# End Config

# Load DBEST
source(paste(DBEST.DIR,"inv.R",sep=""))
source(paste(DBEST.DIR,"pinv.R",sep=""))
source(paste(DBEST.DIR,"mldivide.R",sep=""))
source(paste(DBEST.DIR,"DETREND_SJ.R",sep=""))
source(paste(DBEST.DIR,"Pvalue_ith_coeff.R",sep=""))
source(paste(DBEST.DIR,"Point_to_line.R",sep=""))
source(paste(DBEST.DIR,"BIC_BP.R",sep=""))
source(paste(DBEST.DIR,"DBEST_timeseries_BP.R",sep=""))
source(paste(DBEST.DIR,"DBEST_timeseries_Threshold.R",sep=""))
source(paste(DBEST.DIR,"Compute_epsilon.R",sep=""))
source(paste(DBEST.DIR,"f_level_shift.R",sep=""))
source(paste(DBEST.DIR,"Trend_local_change.R",sep=""))
source(paste(DBEST.DIR,"DBEST.R",sep=""))

# Load MidEastAnlz files
source(paste(MidEastAnlz.DIR,"readNDVI3g.R",sep=""))
source(paste(MidEastAnlz.DIR,"loadRegion.R",sep=""))
source(paste(MidEastAnlz.DIR,"wrapperDBEST.R",sep=""))
source(paste(MidEastAnlz.DIR,"plotMaps.R",sep=""))

region.ts <- loadRegion(region.countries,data.date.start,data.date.end,data.DIR)

NDVI3g.dates <- getZ(region.ts)
message("Processing dataset...","\n",appendLF=FALSE)
flush.console()
trends.characteristics <- calc(region.ts, fun=function(x) {
        wrapperDBEST(x,stackDates=NDVI3g.dates)}, forceapply=TRUE)
names(trends.characteristics) <- c("TrendSegments","StartYear","Duration",
                                   "EndYear","Change","ChangeType",
                                   "Significance")

# Output GTiff
output.files <- paste(Output.DIR, format(data.date.start, format="%Y%b"), 
                      format(data.date.end, format="%Y%b"), "." , 
                      names(trends.characteristics), "." , "DBEST", sep="")
writeRaster(trends.characteristics, filename=output.files, bylayer=TRUE, format="GTiff", overwrite=TRUE)

# Output Statistic
output.statfile <- paste(Output.DIR, format(data.date.start, format="%Y%b"), 
                         format(data.date.end, format="%Y%b"), "." , "DBEST.txt", sep="")
out<-capture.output(freq(trends.characteristics))
cat(out,file=output.statfile,sep="\n")

# Output Maps
region.GADM <- lapply(region.countries ,function(x) raster::getData('GADM', country=x, level=0))
region.set.IDs <- function(sp,i) spChFIDs(sp,paste(i,row.names(sp@data), sep="."))
region.SPDF  <- do.call(rbind,mapply(region.set.IDs,region.GADM, seq_along(region.GADM)))

# Classify insignificant pixels
rcl.insignificant <- trends.characteristics$Significance - 1
rcl.insignificant <- rcl.insignificant * -9999

# TrendSegments
rcl.TrendSegments <- trends.characteristics$TrendSegments + 
        rcl.insignificant
classes.TrendSegments <- c(0, 20, 1,  20, 40, 2, 40, 60, 3, 60, 100, 4, 100, Inf, 5)
classes.TrendSegments <- matrix(classes.TrendSegments, ncol=3, byrow=TRUE)
rcl.TrendSegments <- reclassify(rcl.TrendSegments, classes.TrendSegments)

plotMaps(Output.DIR, output.name = "TrendSegments",region.SPDF, 
         map.raster=rcl.TrendSegments,
         map.title=paste("Number of Trend Segments", 
                         format(data.date.start, format="%Y%b"), 
                         format(data.date.end, format="%Y%b", sep=" ")), 
         colors=c("#e60000", "#feff72","#ffbfbf","#73b2ff", "#b2b2b2"),
         legend.cols = 3, 
         legend.lables=c("bellow 20","20 - 40","40 - 60","above 60", "Insignificant")
)

rcl.StartYear <- trends.characteristics$StartYear + rcl.insignificant
classes.StartYear <- c(1981, 1988, 1,  1988, 1995, 2, 1995, 2002, 3, 2002, 2010, 4, 2010, Inf, 5)
classes.StartYear <- matrix(classes.StartYear, ncol=3, byrow=TRUE)
rcl.StartYear <- reclassify(rcl.StartYear, classes.StartYear)

plotMaps(Output.DIR, output.name = "StartYear",region.SPDF,
         map.raster=rcl.StartYear,
         map.title=paste("Start Year of Change", 
                         format(data.date.start, format="%Y%b"), 
                         format(data.date.end, format="%Y%b", sep=" ")), 
         colors=c( "#fe0000", "#ffc701","#b7ff8f","#33c2fe", "#b2b2b2"),
         legend.cols = 3, 
         legend.lables=c("1982 - 1988","1989 - 1995","1996 - 2002","2003 - 2010", "Insignificant")
)

# Duration
rcl.Duration <- trends.characteristics$Duration + rcl.insignificant
classes.Duration <- c(0, 1, 1, 1, 12, 2, 12, 24, 3, 24, 36,4, 36, 100, 5, 
                      100, Inf, 6)
classes.Duration <- matrix(classes.Duration, ncol=3, byrow=TRUE)
rcl.Duration <- reclassify(trends.characteristics$Duration, 
                           classes.Duration)

plotMaps(Output.DIR, output.name = "Duration",region.SPDF, 
         map.raster=rcl.Duration,
         map.title=paste("Duration of Change (months)", 
                         format(data.date.start, format="%Y%b"), 
                         format(data.date.end, format="%Y%b", sep=" ")), 
         colors=c( "#e60000", "#feff72","#ffbfbf","#73b2ff", "#b2b2b2"),
         legend.cols = 3, 
         legend.lables=c("1","2 - 12","12 - 24","above 24","Insignificant")
)

# EndYear
rcl.EndYear <- trends.characteristics$EndYear + rcl.insignificant
classes.EndYear <- c(1981, 1988, 1,  1988, 1995, 2, 1995, 2002, 3, 2002, 2010, 4, 2010, Inf, 5)
classes.EndYear <- matrix(classes.EndYear, ncol=3, byrow=TRUE)
rcl.EndYear <- reclassify(rcl.EndYear, classes.EndYear)

plotMaps(Output.DIR, output.name = "EndYear",region.SPDF, 
         map.raster=rcl.EndYear,
         map.title=paste("End Year of Change", 
                         format(data.date.start, format="%Y%b"), 
                         format(data.date.end, format="%Y%b", sep=" ")), 
         colors=c( "#fe0000", "#ffc701","#b7ff8f","#33c2fe", "#b2b2b2"),
         legend.cols = 3, 
         legend.lables=c("1982 - 1988","1989 - 1995","1996 - 2002", "2003 - 2010", "Insignificant")
)

# Change
rcl.Change <- trends.characteristics$Change + rcl.insignificant
classes.Change <- c(-0.3, -0.2, 1,  -0.2, -0.1, 2, -0.1, 0, 3, 0, 0.1, 4, 
                    0.1, 0.2, 5, 0.2, 0.3, 6, 0.3, 0.4, 7, 0.4, 0.5, 8, 
                    0.5, 1, 9, 1, Inf, 10)
classes.Change <- matrix(classes.Change, ncol=3, byrow=TRUE)
rcl.Change <- reclassify(rcl.Change, classes.Change)

plotMaps(Output.DIR, output.name = "Change",region.SPDF, 
         map.raster=rcl.Change,
         map.title=paste("Magnitude of Change", 
                         format(data.date.start, format="%Y%b"), 
                         format(data.date.end, format="%Y%b", sep=" ")), 
         colors=c( "#ff2100", "#ff7f7e","#ffedbf","#e9febf","#d6e700",
                   "#8bb402","#d0ff72","#4a8a00","#00734c", "#b2b2b2"),
         legend.cols = 2, 
         legend.lables=c("-0.3 - -0.2","-0.2 - -0.1", "-0.1 - 0", 
                         "0 - 0.1", "0.1 - 0.2", "0.2 - 0.3", "0.3 - 0.4", 
                         "0.4 - 0.5", "0.5 - 1", "Insignificant")
)

# ChangeType
rcl.ChangeType <- trends.characteristics$ChangeType + rcl.insignificant
rcl.ChangeType <- reclassify(rcl.ChangeType, c(0,1,1,1,2,2,2,Inf,3))

plotMaps(Output.DIR, output.name = "ChangeType",region.SPDF, 
         map.raster=rcl.ChangeType,
         map.title=paste("Change Type", 
                         format(data.date.start, format="%Y%b"), 
                         format(data.date.end, format="%Y%b", sep=" ")), 
         colors=c( "#bfe9ff", "#e60000", "#b2b2b2"),
         legend.cols = 2, 
         legend.lables=c("Non-Abrupt", "Abrupt", "Insignificant")
)
