# rasterx = Full_indices_H[[1]]
# rastery = Full_indices_I[[1]]
# color.matrix = col.matrix
# nquantiles = 4
# xlab =""
# ylab = ""
# pngname = paste0("./Maps/Map_legends/",name_code,"_Bivmap_legend.png" )

add_bivar_legend <- function(rasterx, rastery, color.matrix, nquantiles, xlab, ylab, pngname, scaledeci)
{
  
  seqx <- rasterx@data@values
  minx <- min(seqx, na.rm = T)
  maxx <- max(seqx, na.rm = T)
  footx <- (maxx - minx) / nquantiles
  
  seqx <- seq(minx, maxx, footx)
  
  seqy <- rastery@data@values
  miny <- min(seqy, na.rm = T)
  maxy <- max(seqy, na.rm = T)
  footy <- (maxy - miny) / nquantiles
  
  seqy <- seq(miny, maxy, footy)
  
  # par( mar= c(6.5,7.5,11.5,22.5) )
  png(filename = pngname, width = 400, height = 400, units = "px", pointsize = 12,
      bg = "transparent")
  par(mar=c(5,6,4,1)+.1)  
  plot(seqx, seqy, frame.plot = F, xlab = xlab, ylab = ylab, cex.lab = 2.5, cex = 0, axes= FALSE)
  for (i in 1:(length(seqy)))
  {
    if(i == 1) {next}
    for(j in 1:(length(seqx)))
    {
      if (j==1) {next}
      seqj <- seq(seqx[j-1], seqx[j], length.out = 100)
      for(k in 1:length(seqj))
      {
        col.temp <- col.matrix[i , j]
        graphics::points( rep(seqj[k], length(seqj)) ,seq(seqy[i-1], seqy[i], length.out = 100), pch = 15, 
                          col = col.temp, cex = 1 )
      }
    }
    
  } 
  axis(side = 1, at = seqx, labels = round(seqx, scaledeci), cex.axis = 2, col = "black", lwd = 3, font = 2)
  axis(side = 2, at = seqy, labels = round(seqy, scaledeci), cex.axis = 2, col = "black", lwd = 3, font = 2) 
  dev.off()
}