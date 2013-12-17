 effect.stars.plot <- function(coefs, names, labels, nrow, radius, col.fill = "gray90", 
                               cex.names = 1, cex.labels = 0.8, col.circle = "black", 
                               lwd.circle = 1, lty.circle = "longdash"){
   
  # how many categories
  n.kat <- ncol(coefs)
  
  # how many variables
  n.var <- nrow(coefs)
  
  # how many columns are need for the stars
  ncol <- ceiling(n.var/nrow)
  
  # how many empty boxes are needed
  n.nulls <- ncol*nrow - n.var
  
  # create layout for stars
  locations <- matrix(c(rep(1:ncol,nrow),rep(nrow:1,each=ncol)),ncol=2)[1:n.var,]
  
  
  # cosinus and sinus for angles to the categories
  cosis <- c()
  sinis <- c()
  alts <- 0:(n.kat - 1)
  angle <- alts * ((360/n.kat) * pi)/180
  cosis <- cos(angle)
  sinis <- sin(angle)
  
  # locations of ray ends, relative to center
  x.locs <- t(coefs)*cosis
  y.locs <- t(coefs)*sinis
  
  # shrinkage factor: so, that locations of labels are at 
  # maximum 0.42 from center
  factor <- 0.42/(max(abs((t(coefs)+0.3)*cosis),abs((t(coefs)+0.3)*sinis)))
  
  # locations of variable/star names
  names.locs <- locations 
  names.locs[,2] <- names.locs[,2] + 0.48
  
  # locations of all labels
  labels.x.locs <- (t(coefs)+0.3)*cosis*factor + rep(locations[,1],each=n.kat)
  labels.y.locs <- (t(coefs)+0.3)*sinis*factor + rep(locations[,2],each=n.kat)
  
  # set pars for plot
  par(xpd=TRUE)
  
  # plot stars and circles
  stars(coefs, scale = FALSE, draw.segments = FALSE, len = factor, 
        flip.labels = FALSE, labels=NULL, main = "", xpd=TRUE, locations=locations)
  
  symbols(locations[,1],locations[,2], circles = rep(factor,n.var), 
          add = TRUE, inches = FALSE, fg = col.circle, bg =col.fill, lwd = lwd.circle,
  lty = lty.circle)
  
  stars(coefs, scale = FALSE, draw.segments = FALSE, len = factor, 
        flip.labels = FALSE, labels=NULL, main = "", xpd=TRUE,locations=locations, add=TRUE)
  
  # add variable/star names
  text(names.locs[,1],names.locs[,2], labels = names, cex = cex.names)
  
  # add labels
  for(i in 1:n.var){
  text(labels.x.locs[,i],labels.y.locs[,i],labels =labels[i,],cex=cex.labels)
  }
    
}