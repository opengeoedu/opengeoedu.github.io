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

n <- 30
RDIST <- 1
r <- RDIST/6

x <- runif(n)
y <- runif(n)
l <- 1:n

cpoints <- data.frame(x=x, y=y, label=l)
#coordinates(cpoints) <- ~x + y
plot(cpoints[,1:2])

rcs <- round(cpoints/RDIST)*RDIST;rcs
rcpairs <- as.factor(paste(rcs[,1], rcs[,2]))
overlaps <- levels(rcpairs)[table(rcpairs) > 1]

shifted_points <- cpoints
sapply(overlaps, function(overlap){
  overlap <- overlaps[1]
  #find the ids of overlapping points
  point_ids <- which(rcpairs == overlap)
  x0 <- mean(shifted_points[point_ids, 1]) # approximate center of the coordinates
  y0 <- mean(shifted_points[point_ids, 2])
  message(x0," ",y0)
  #cat(x0, "- ",y0, "\n")
  n <- length(point_ids) #number of points to be shifted
  vec <- seq(from=0, by=2*pi/n, length.out = n)
  xc <- c(x0+r*sin(vec))
  yc <- c(y0+r*cos(vec))
  #print(point_ids)
  pold <- shifted_points[point_ids, c(1,2)] 
  pnew <- data.frame(x=xc, y=yc)
  
  sapply(pold())
  
  
  
  shifted_points[point_ids, c(1,2)] <<- data.frame(x=xc, y=yc)
  return(invisible())
})

par(mfrow=c(1,2))
plot(circle(x,y,0.1), pch=".", col="grey")
text(x,y, labels=l,cex=0.8)

#plot(shifted_points[,1:2], col="red")
plot(circle(shifted_points[,1], shifted_points[,2],0.05), pch=".", col="red")
text(shifted_points[,1:2], labels=l, cex=0.8)
grid()
abline(v=seq(0, round(max(cpoints[,1])*RDIST)/RDIST  ,by = RDIST))
abline(h=seq(0, round(max(cpoints[,2])*RDIST)/RDIST  ,by = RDIST))
par(mfrow=c(1,1))
