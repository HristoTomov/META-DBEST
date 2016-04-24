plotMaps <- function(output.path, output.name, countries.SPDF,
                     countries.bg = "white", countries.border = "gray70",
                     map.raster, map.title, scalebar.dist=1000,
                     northarrow.size=3,northarrow.topoffset=1, colors, 
                     legend.cols = 2, legend.lables, pdfGrid = c(5,2), 
                     region.ts) {
        
        pdf(paste(output.path, paste(output.name, "pdf", sep="."),
                                                                sep="/")) 
        
        par.old <- par(no.readonly = TRUE)
        on.exit(par(par.old))
        par(mar=c(4, 4, 4, 5))
        
        plot(countries.SPDF, lwd=0.2, border=countries.bg, 
             col=countries.bg, 
             xlim=c(xmin(map.raster) + 1, xmax(map.raster) - 1), 
             ylim=c(ymin(map.raster) + 1, ymax(map.raster) - 1),axes = F)
        
        degAxis(1, col = 'white', col.ticks = 'black', cex.axis = 0.95, font = 3, family = 'serif')
        degAxis(2, col = 'white', col.ticks = 'black', cex.axis = 0.95, font = 3, family = 'serif')
        box()
        plot(map.raster, legend = FALSE, col=colors, axes=F, add=TRUE)
        invisible()
        
        grid(lwd = 1.8)
        text(xmin(map.raster) + 0.5, ymin(map.raster) - 3, 
                "Projection: Geographic\nCoordinate System:
                WGS 1984\nData Source: GIMMS3g/GADM",
                 adj=c(0,0), cex=0.7, col="gray18")
        
        scalebar(scalebar.dist, xy = c(xmin(map.raster) + 0.5,
                ymin(map.raster)), lonlat = TRUE, 
                label = paste(scalebar.dist, "km"), lwd=2)
        
        colors <- c(colors,"#ffffff")
        legend.lables <- c(legend.lables,"Masked")
        legend("bottomright",  legend = legend.lables, 
               fill = colors,ncol=legend.cols,cex = 0.8)

        north.coord <- c(xmin(map.raster) + northarrow.topoffset, ymax(map.raster) - northarrow.topoffset)
        SpatialPolygonsRescale(layout.north.arrow(1), offset = north.coord, scale = northarrow.size, plot.grid=F)
        plot(countries.SPDF, lwd=0.8, border="gray70", add=TRUE, axes = F)
        mtext(side=3, line=1, map.title, cex=1.2)
        mtext(side=1, "Longitude", line=2.5, cex=0.8)
        mtext(side=2, "Latitude", line=2.5, cex=0.8)
        dev.off() 
}
