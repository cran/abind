library(abind)
# unlike cbind or rbind
abind(x=1:4,y=5:8)
# like cbind
abind(x=1:4,y=5:8,along=2)                   
abind(x=1:4,matrix(5:20,nrow=4),along=2)
abind(1:4,matrix(5:20,nrow=4),along=2)
# like rbind
abind(x=1:4,matrix(5:20,nrow=4),along=1)
abind(1:4,matrix(5:20,nrow=4),along=1)
# different default dimnames:
abind(x=1:4,matrix(5:20,nrow=4),along=1)
abind(x=1:4,matrix(5:20,nrow=4),along=1,force.array=FALSE)
# concatenates two vectors:
abind(x=1:4,y=5:8)
abind(x=c(a=1,b=2),y=3:4)
abind(x=c(a=1,b=2),y=c(c=3,d=4))
# simulate rbind with row vectors in three ways:
# (1) easiest way: insert new dimension before 1 (use any number less than 1 for along)
abind(x=1:4,y=5:8,along=0.5)
abind(x=c(a=1,b=2),y=c(c=3,d=4), along=0) # with names
abind(x=c(a=1,b=2),y=c(c=3,d=4), along=0, use.first.dimnames=TRUE)
# (2) permute the result:
aperm(abind(1:4,5:8,along=2),c(2,1))
# different default dimnames:
aperm(abind(1:4,5:8,along=2),c(2,1))
# (3) convert arguments to row vectors
abind(matrix(1:4,nrow=1),matrix(5:8,nrow=1),along=1)
# bind two matrices, 5 possible values for along
abind(x=matrix(1:16,nrow=4),y=matrix(17:32,nrow=4),along=1)
abind(x=matrix(1:16,nrow=4),y=matrix(17:32,nrow=4),along=2)
abind(x=matrix(1:16,nrow=4),y=matrix(17:32,nrow=4),along=3)
abind(x=matrix(1:16,nrow=4),y=matrix(17:32,nrow=4),along=0.5)
abind(x=matrix(1:16,nrow=4),y=matrix(17:32,nrow=4),along=1.5)
# examples with three matrices
cc <- as.data.frame(matrix(25:36,nrow=3))
aa <- matrix(1:12,nrow=3,dimnames=list(letters[1:3],LETTERS[1:4]))
abind(a=aa, cc, matrix(25:36,3,4), along=0, use.first.dimnames=TRUE)
abind(a=aa, cc, matrix(25:36,3,4), along=1, use.first.dimnames=TRUE)
abind(a=aa, cc, matrix(25:36,3,4), along=1.1, use.first.dimnames=TRUE)
abind(a=aa, cc, matrix(25:36,3,4), along=2)
abind(a=aa, cc, matrix(25:36,3,4), along=2, use.first.dimnames=TRUE)
abind(a=aa, cc, matrix(25:36,3,4), along=3, use.first.dimnames=TRUE)
abind(a=aa, cc, matrix(25:36,3,4), along=3, use.anon.names=FALSE, use.first.dimnames=TRUE)
abind(a=aa, cc, dd=matrix(25:36,3,4), along=1.1, use.first.dimnames=TRUE)
x1 <- array(1:8,dim=c(2,2,2),dimnames=list(letters[6:7],letters[1:2],letters[24:25]))
x1
# test that we get dimnames correctly when we need to expand dimensions
x2.1 <- array(11:14,dim=c(2,2),dimnames=list(letters[1:2],letters[24:25]))
x2.2 <- array(11:14,dim=c(2,2),dimnames=list(letters[6:7],letters[24:25]))
x2.3 <- array(11:14,dim=c(2,2),dimnames=list(letters[6:7],letters[1:2]))
abind(x1, h=x2.1, along=1)
abind(x1, c=x2.2, along=2)
abind(x1, z=x2.3, along=3)

