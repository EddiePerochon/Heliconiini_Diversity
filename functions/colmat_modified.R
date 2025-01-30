# valuesx <- Full_indices_H[[1]]
# valuesy <- Full_indices_I[[1]]

colmat_modif <- function (nquantiles = 10, upperleft = "blue", upperright = "red", 
          bottomleft = "grey", bottomright = "yellow", xlab = "x label", 
          ylab = "y label", valuesx = NA, valuesy = NA) 
{
  seqx <- valuesx@data@values
  minx <- min(seqx, na.rm = T)
  maxx <- max(seqx, na.rm = T)
  footx <- (maxx - minx) / nquantiles
  
  seqx <- seq(minx, maxx, footx)
  
  seqy <- valuesy@data@values
  miny <- min(seqy, na.rm = T)
  maxy <- max(seqy, na.rm = T)
  footy <- (maxy - miny) / nquantiles
  
  seqy <- seq(miny, maxy, footy)
  
  my.data <- seq(0, 1, 0.01)
  my.class <- classInt::classIntervals(my.data, n = nquantiles, 
                                       style = "quantile")
  my.pal.1 <- classInt::findColours(my.class, c(upperleft, 
                                                bottomleft))
  my.pal.2 <- classInt::findColours(my.class, c(upperright, 
                                                bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  for (i in 1:101) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102 - i, ] <- classInt::findColours(my.class, 
                                                   my.col)
  }
  col.matrix_big <- col.matrix
  plot(seqx, seqy, frame.plot = F, xlab = xlab, ylab = ylab, cex.lab = 1.3, cex = 0, axes= F)
  axis(side = 1, at = seqx, labels = round(seqx, 2), cex.axis = 1)
  axis(side = 2, at = seqy, labels = round(seqy, 2), cex.axis = 1)
  par(new=TRUE)

  
  plot(c(1, 1), pch = 19, col = my.pal.1, cex = 0.5, xlim = c(0,1), ylim = c(0, 1), frame.plot = F, xlab = xlab, ylab = ylab, cex.lab = 1.3, axes = F)
  for (i in 1:101) {
    col.temp <- col.matrix[i - 1, ]
    graphics::points(my.data, rep((i - 1)/100, 101), pch = 15, 
                     col = col.temp, cex = 1 )

  }
  
  
 
  

  seqs <- seq(0, 100, (100/nquantiles))
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
  
}
