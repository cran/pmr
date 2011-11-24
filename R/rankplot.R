rankplot <- function(dset, trans=FALSE){
require(graphics)
nitem <- ncol(dset)-1
test <- matrix(data = 0, nrow = factorial(nitem), ncol = nitem, byrow = TRUE)
temp1 <- 1:nitem
i <- 1

if (nitem<3 || nitem>4){
message("Only ranking data with 3 or 4 items is supported.")
}

## generate a list of all possible rankings
for (j in 1:(nitem^nitem-1)){
temp1[1] <- nitem - j%%nitem
temp2 <- j - j%%nitem
for (k in nitem:2){
temp1[k] <- nitem - temp2%/%(nitem^(k-1))
temp2 <- temp2 - (nitem-temp1[k])*(nitem^(k-1))
}
temp2 <- 0
for (l in 1:nitem){
for (m in 1:nitem){
if (temp1[l] == temp1[m] && l != m){
temp2 <- 1
}
}
}
if (temp2 == 0){
for (p in 1:nitem){
test[i,p] = temp1[p]
}
i <- i+1
}
}


n <- rep(0,factorial(nitem))
for (j in 1:factorial(nitem)){
for (k in 1:nrow(dset)){
temp_ind <- 0
for (l in 1:nitem){
if (test[j,l] != dset[k,l]) {temp_ind <- temp_ind + 1}
}
if (temp_ind == 0) {n[j] <- dset[k,nitem+1]}
}
}
test2 <- cbind(test, n)

if (nitem==3){
label <- c(123,132,312,321,231,213)
freq <- rep(0,6)
for (i in (1:6)){
for (j in (1:6)){
if(test2[i,1]*100+test2[i,2]*10+test2[i,3] == label[j]){
freq[j] <- test2[i,4]
}
}
}
x = c(0,1,1.5,1,0,-0.5)
y = c(0,0,-0.866,-1.732,-1.732,-0.866)
symbols(x,y,circle=freq^0.5/sum(freq^0.5)/2,inches=FALSE)
text(x,y,labels=label)
polygon(x,y)
}

if (nitem==4){
plot(c(0,3), c(0,-3), xlab=" ", ylab=" ", type="n")
# draw the labels and the circles
label <- rep(0,24)
freq <- rep(0,24)
for (i in (1:24)){
label[i] <- test2[i,1]*1000+test2[i,2]*100+test2[i,3]*10+test2[i,4]
}
for (i in (1:24)){
for (j in (1:24)){
if(test2[i,1]*1000+test2[i,2]*100+test2[i,3]*10+test2[i,4] == label[j]){
freq[j] <- test2[i,5]
}
}
}

if (trans==FALSE){
x <- c(0.76,1.26,0.55,1.51,0.77,1.24,1.15,1.61,0.44,2.2,0.64,1.98,1.27,2.23,1,2.55,1.53,2.15,1.88,2.38,1.66,2.72,1.98,2.56)
y <- c(-1.02,-0.56,-1.93,-0.24,-1.7,-0.4,-1.43,-1,-1.22,-0.4,-0.95,-0.58,-2.04,-1.15,-2.33,-0.88,-1.93,-1.36,-2.23,-1.77,-2.55,-1.55,-2.37,-1.83)
}
else {
x <- c(1,1.66,0.55,1.98,0.77,1.53,1.27,1.88,0.44,2.56,0.64,2.15,1.15,2.38,0.76,2.72,1.24,1.98,1.61,2.23,1.26,2.55,1.51,2.2)
y <- c(-2.33,-2.55,-1.93,-2.37,-1.70,-1.93,-2.04,-2.23,-1.22,-1.83,-0.95,-1.36,-1.43,-1.77,-1.02,-1.55,-0.4,-0.58,-1,-1.15,-0.56,-0.88,-0.24,-0.4)
}
symbols(x,y,circle=freq^0.5/sum(freq^0.5)/2,inches=FALSE)
text(x,y+0.1,labels=label)

# draw the truncated octahedron
Ax = c(0.64,0.77,1.53,2.15,1.98,1.24)
Ay = c(-0.95,-1.7,-1.93,-1.36,-0.58,-0.4)
polygon(Ax,Ay)
Dx = c(1.15,1.27,1.88,2.38,2.23,1.61)
Dy = c(-1.43,-2.04,-2.23,-1.77,-1.15,-1)
polygon(Dx,Dy,lty=3)
Ex = c(1.98,2.15,2.56,2.72,2.55,2.2)
Ey = c(-0.58,-1.36,-1.83,-1.55,-0.88,-0.4)
polygon(Ex,Ey)
Fx = c(0.55,1,1.66,1.98,1.53,0.77)
Fy = c(-1.93,-2.33,-2.55,-2.37,-1.93,-1.7)
polygon(Fx,Fy)
Gx = c(1.66,1.98,2.56,2.72)
Gy = c(-2.55,-2.37,-1.83,-1.55)
lines(Gx,Gy)
Hx = c(0.55,0.44,0.64,1.24,1.51,2.2)
Hy = c(-1.93,-1.22,-0.95,-0.4,-0.24,-0.4)
lines(Hx,Hy)
Ix = c(0.44,0.76,1.26,1.51)
Iy = c(-1.22,-1.02,-0.56,-0.24)
lines(Ix,Iy,lty=3)
Jx = c(1.26,1.61)
Jy = c(-0.56,-1)
lines(Jx,Jy,lty=3)
Kx = c(0.76,1.15)
Ky = c(-1.02,-1.43)
lines(Kx,Ky,lty=3)
Lx = c(1,1.27)
Ly = c(-2.33,-2.04)
lines(Lx,Ly,lty=3)
Mx = c(1.66,1.88)
My = c(-2.55,-2.23)
lines(Mx,My,lty=3)
Nx = c(2.23,2.55)
Ny = c(-1.15,-0.88)
lines(Nx,Ny,lty=3)
Ox = c(2.38,2.72)
Oy = c(-1.77,-1.55)
lines(Ox,Oy,lty=3)

}

}