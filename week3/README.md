### Sample:Output

> B <- matrix(c(1,2,3,4),2,2)
> B1 <- makeCacheMatrix(B)
> cacheSolve(B1)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(B1)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5