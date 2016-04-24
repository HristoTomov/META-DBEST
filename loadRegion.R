loadRegion <- function(region.countries,data.date.start,
                       data.date.end,data.path) {
        
        message("Downloading countries borders...","\n",appendLF=FALSE)
        flush.console()
        
        # Load region countries data from GADM.org
        region.GADM <- lapply(region.countries ,function(x) 
                raster::getData('GADM', country=x, level=0))
        
        message("Merging the selected countries...","\n",appendLF=FALSE)
        flush.console()
        
        # Change polygons IDs to unique values
        region.set.IDs <- function(sp,i) spChFIDs(sp,
                                  paste(i,row.names(sp@data),sep="."))
        
        # Combine SpatialPolygonDataFrame objects
        region.SPDF  <- do.call(rbind,mapply(region.set.IDs,
                                     region.GADM,seq_along(region.GADM)))
        
        # Create an empty template
        region.r.ext <- extent(c(-180, 180, -90 , 90))
        region.r.res <- 1/12 ## resolution from GIMMS 3g specification
        region.r.tpl <- raster(ext = region.r.ext, 
                               resolution = region.r.res)
        
        # Rasterize SPDF polygons to raster with 0 value
        region.r <- rasterize(region.SPDF, region.r.tpl, field=0)
        
        #
        # Load GIMMS NDVI 3g data
        #
        
        # Define a raster stack for the NDVI time-series
        region.ts <- stack()
        
        # GIMMS NDVI 3g grid parameters
        data.samples <- 2160 ## or cols
        data.lines <- 4320 ## or rows
        data.size   <- data.samples*data.lines ## number of items
        data.type <- 2 ## 2-byte short
        data.byte.order <- 1 ## 0 for little endian / 1 for big endian
        
        # Check what type of endian the reader should use
        ieee <- if(.Platform$endian=="big") 1 else 0 
        ## check if this machine use big endian
        data.endian <- if(ieee==data.byte.order | data.byte.order<0)
                .Platform$endian else "swap"
        
        # Generate dates for the period
        data.dates <- as.yearmon(seq(as.numeric(data.date.start),
                                     as.numeric(data.date.end), 1/24))
        data.dates <- c(data.dates, data.date.end) 
        ## Append one more measurement (two per month)
        
        # Format according to GIMMS file naming 
        data.names <- format(data.dates, format="%y%b")
        data.names <- tolower(data.names) 
        ## months to lower case
        
        # Create two periods (a/b) for each month
        data.names[1:length(data.names) %% 2 == 1] <- paste(data.names[1:length(data.names) %% 2 == 1], "15a", sep = "")
        data.names[1:length(data.names) %% 2 == 0] <- paste(data.names[1:length(data.names) %% 2 == 0], "15b", sep = "")
        
        # Create a list of files 
        file.patern <- paste("geo", data.names, ".*-VI3g", sep = "")
        data.files <- lapply(file.patern, function(x) {list.files(
                data.path, pattern = x, full.names = TRUE)[1]})
        
        message("Loading NDVI3g data...","\n",appendLF=FALSE)
        flush.console()
        
        pb <- txtProgressBar(min = 0, max = length(data.files), style = 3)
        
        region.ts <- stack(sapply(seq(data.files), function(i) {
                setTxtProgressBar(pb, i)
                flush.console()
                readNDVI3g(data.file=data.files[[i]], data.size, data.type, 
                           data.endian, data.samples, data.lines, 
                           region.r, region.SPDF)
        }))
        
        close(pb)

        # Name the layers
        names(region.ts) <- data.names
        
        # Set layers' dates
        data.dates <- as.Date(as.yearmon(data.dates))
        data.dates[1:length(data.dates) %% 2 == 0] <- data.dates[1:length(data.dates) %% 2 == 0]  + 15
        region.ts <- setZ(region.ts, data.dates)        
}
