# rasterx <- Full_indices_H[[1]]
# rastery <- Full_indices_I[[1]]
# colormatrix <- col.matrix
# nquantiles <- 4


biv_map_modified <- function (rasterx, rastery, colormatrix, nquantiles = 10) 
{
  rastery@data@values[is.na(rastery@data@values) & is.na(rasterx@data@values) == FALSE] <- 0
  
  
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
  
  
  quanmean <- raster::getValues(rasterx)
  temp <- data.frame(quanmean, quantile = rep(NA, length(quanmean)))
  
  for(i in 1:nquantiles)
  {
 temp$quantile[ temp$quanmean >= seqx[i] & temp$quanmean <= seqx[i+1]] <- i+1
  }
  
  r1 <- temp
  
  quantr <- data.frame(r1[, 2])
  
  quanvar <- raster::getValues(rastery)
  temp <- data.frame(quanvar, quantile = rep(NA, length(quanvar)))
  
  for(i in 1:nquantiles)
  {
    temp$quantile[ temp$quanvar >= seqy[i] & temp$quanvar <= seqy[i+1]] <- i+1
  }
  
  r2 <- temp
  
  quantr2 <- data.frame(r2[, 2])
 
  col.matrix2 <- colormatrix
  cn <- unique(c(colormatrix))
  for (i in 1:length(col.matrix2)) {
    ifelse(is.na(col.matrix2[i]), col.matrix2[i] <- 1, col.matrix2[i] <- which(col.matrix2[i] == 
                                                                                 cn)[1])
  }
  cols <- numeric(length(quantr[, 1]))
  for (i in 1:length(quantr[, 1])) {
    a <- quantr[i, 1]
    b <- quantr2[i, 1]

    
    cols[i] <- as.numeric(col.matrix2[b, a])
    
  }
  
  

  cols[r1$quanmean == 0 & r2$quanvar == 0]  <- as.numeric(length(cn)+1)
  
  r <- rasterx
  r[1:length(r)] <- cols
  
  # plot(r,
  #      frame.plot=F,
  #      axes=F,
  #      box=F,
  #      add=F,
  #      legend=F,
  #      col=as.vector(col.matrix_v))
  # 
  
  return(r)
}

