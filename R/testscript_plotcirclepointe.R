x <- 3
y <- 4

n <- 12
r <- 4


plot(1,1,pch=1, width = 10)

circle <- function(xv,yv,r){
  x <- sapply(xv, function(x0){
    return(c(x0+r*sin(0:300/150*pi)))
  })
  
  y <- sapply(yv, function(y0){
    return(c(y0+r*cos(0:300/150*pi)))
  })
  
  return(list(x=unlist(x),y=unlist(y)))
}

circle(x,y,r)

plot(circle(1:4,1:4,r), type="l")
grid()

vec <- seq(from=0, by=2*pi/n, length.out = n)
lon <- c(x+r*sin(vec))
lat <- c(y+r*cos(vec))

points(lon,lat, col="red")

library(sp)

n <- 70
RDIST <- 0.3
r <- RDIST/8

x <- runif(n)
y <- runif(n)
l <- 1:n

cpoints <- data.frame(x=x, y=y, label=l)
#coordinates(cpoints) <- ~x + y
plot(cpoints[,1:2])

rcs <- round(cpoints[,1:2]/RDIST)*RDIST;rcs
rcpairs <- as.factor(paste(rcs[,1], rcs[,2]))
overlaps <- levels(rcpairs)[table(rcpairs) > 1]

shifted_points <- cpoints
opoints <- data.frame()

plot(circle(x,y,0.1), pch=".", col="grey", xlim=c(0,1),ylim=c(0,1))
sapply(overlaps, function(overlap){
  #overlap <- overlaps[1]
  #find the ids of overlapping points
  point_ids <- which(rcpairs == overlap)
  x0 <- mean(shifted_points[point_ids, 1]) # approximate center of the coordinates
  y0 <- mean(shifted_points[point_ids, 2])
  points(x0, y0, col="green")
  opoints <<- rbind(opoints, data.frame(x0,y0))
  n <- length(point_ids) #number of points to be shifted
  vec <- 2*pi/(3*n)+seq(from=0, by=2*pi/n, length.out = n)
  r <- max(n/3,1)*r
  xc <- c(x0+r*sin(vec))
  yc <- c(y0+r*cos(vec))
  #print(point_ids)
  pold <- shifted_points[point_ids, c(1,2)] 
  arcs <- mapply(function(xp, yp){
    message(xp-x0," ",yp-y0)
    arc <- atan2(xp-x0, yp-y0)*360/(2*pi)
    if(arc<0)
      arc <- arc + 360
   # text(xp,yp, labels = round(arc))
    arc
  },  xp=pold[,1],
      yp=pold[,2]
  )
  
  shifted_points[point_ids[order(arcs)], c(1,2)] <<- data.frame(x=xc, y=yc)
  return(invisible())
})

par(mfrow=c(1,2))
plot(circle(x,y,0.02), pch=".", col="grey", xlim=c(0,1),ylim=c(0,1))
grid()
text(x,y, labels=l,cex=0.8)
points(opoints,col="green")

#plot(shifted_points[,1:2], col="red")
plot(circle(shifted_points[,1], shifted_points[,2],0.02), pch=".", col="red", xlim=c(0,1),ylim=c(0,1))
text(shifted_points[,1:2], labels=l, cex=0.8)
grid()
points(opoints, col="green")
#abline(v=seq(0, round(max(cpoints[,1])*RDIST)/RDIST  ,by = RDIST))
#abline(h=seq(0, round(max(cpoints[,2])*RDIST)/RDIST  ,by = RDIST))
par(mfrow=c(1,1))

