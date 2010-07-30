destat <- function(dset){

nitem <- ncol(dset)-1

meanrank <- rep(1,nitem)
paircom <- matrix(data = 0, nrow = nitem, ncol = nitem, byrow = TRUE)
marginal <- matrix(data = 0, nrow = nitem, ncol = nitem, byrow = TRUE)

## compute mean rank
meanrank <- mean(dset[1:nitem])

## compute pair comparison
for (i in 1:nrow(dset)){
for (j in 1:nitem){
for (k in 1:nitem){
if (dset[i,j] < dset[i,k]) {paircom[j,k] <- paircom[j,k] + dset[i,nitem+1]}
}
}
}

## compute marginal matrix
for (i in 1:nrow(dset)){
for (j in 1:nitem){
marginal[j,dset[i,j]] <- marginal[j,dset[i,j]] + dset[i,nitem+1]
}
}

lst <- list(mean.rank=meanrank, pair=paircom, mar=marginal)
return(lst)
}