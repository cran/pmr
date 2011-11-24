mdpref <- function(dset,rank.vector=FALSE){

nitem <- ncol(dset)-1
nobs <- sum(dset[,nitem+1])
X <- matrix(data = 0, nrow = nobs, ncol = nitem, byrow = TRUE)

# enlarge ranking dataset
temp <- 1
for (i in 1:nrow(dset)){
if (dset[i,nitem+1] > 0){
for (j in 1:dset[i,nitem+1]){
for (k in 1:nitem){
X[temp,k] <- dset[i,k]
}
temp <- temp + 1
}
}
}

# reverse coding
for (i in 1:nrow(X)){
for (j in 1:nitem){
X[i,j] <- nitem-X[i,j]+1
}
}

# standardize
temp_mean <- rep(mean(X[1,]),nobs)
temp_sd <- rep(sd(X[1,]),nobs)

for (i in 1:nrow(X)){
for (j in 1:nitem){
X[i,j] <- (-X[i,j] + temp_mean[i])/temp_sd[i]
}
}

# multidimensional preference
md <- svd(X, nu=2, nv=2)
D <- matrix(data = 0, nrow = 2, ncol = 2, byrow = TRUE)
for (i in 1:2){
D[i,i] <- md$d[i]
}

# plot item graph
A <- md$u*(nitem-1)^0.5
B <- md$v %*% D/(nitem-1)^0.5
label <- 1:nitem
plot(B, col="blue", xlab="Dimension 1", ylab="Dimension 2", type="n")
text(B[,1],B[,2],labels=label)

# draw rank vector
A <- -A
d1 <- max(B[,1])/max(A[,1])
d2 <- max(B[,2])/max(A[,2])
d3 <- min(B[,1])/min(A[,1])
d4 <- min(B[,2])/min(A[,2])
mind <- min(d1,d2,d3,d4)
A1 <- A * mind
if (rank.vector==TRUE){
for (i in 1:nrow(A1)){
testx <- c(0,A1[i,1])
testy <- c(0,A1[i,2])
lines(testx, testy)
}
}

coord <- matrix(data = 0, nrow = nrow(dset), ncol = nitem+3, byrow = TRUE)
# write output ranking
for (i in 1:nrow(dset)){
for (j in 1:(nitem+1)){
coord[i,j] <- dset[i,j]
}
}

temp <- 2
while (coord[temp,(nitem+1)] == 0){
temp <- temp + 1
}

coord[1,(nitem+2)] <- A1[1,1]
coord[1,(nitem+3)] <- A1[1,2]
while(temp < nrow(dset)){
for (i in 2:nrow(A1)){
if (round(A1[(i-1),1],digits=5) != round(A1[i,1],digits=5) && round(A1[(i-1),2],digits=5) != round(A1[i,2],digits=5)){
coord[temp,(nitem+2)] <- A1[i,1]
coord[temp,(nitem+3)] <- A1[i,2]
temp <- temp + 1
if (temp < nrow(dset)){
while (coord[temp,(nitem+1)] == 0){
temp <- temp + 1
}
}
}
}
}

lst <- list(item=B, ranking=coord, explain=(D[1,1]+D[2,2])/sum(md$d))
return(lst)

}