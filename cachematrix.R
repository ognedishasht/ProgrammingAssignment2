## In this file the two required functions are defined:
##
## makeCacheMatrix(m)
##      The argument m must be a square matrix.
##      The function returns a SpecialMatrix object to which m is coerced
##      (see below the description of the class "SpecialMatrix").
##
## cacheSolve(sm, ...)
##      The argument sm must be a square matrix. If sm is an object of class
##      "matrix", cacheSolve() returns its inverse calculated by a call to
##      solve(). Therefore, solve() is called every time when the cacheSolve()
##      is invoked. However, if sm is an object of class "SpecialMatrix", 
##      cacheSolve() uses an overloaded function which calls solve() only when
##      necessary. If the inverse has been calculated before, a cached value is
##      fetched and returned which is much faster for large matrices.
##      The additional ... arguments of cacheSolve() are passed to solve()
##      if the latter function is to be called. 
##
## Class "SpecialMatrix" is a subclass of "matrix". A SpecialMatrix object can
## be used instead of any "matrix" object. However, this subclass introduces new
## behavior providing caching capability.
##
## The definition of "SpecialMatrix" follows:

setClass("SpecialMatrix",
         contains = "matrix",
         slots = c(get_last = "function", get_inverse = "function",
                   set_last = "function", set_inverse = "function")
)

## The slots store functions used to access the cached values - see below 
## the description of the function as.SpecialMatrix().
##
## The function as.SpecialMatrix() returns a SpecialMatrix object to which its 
## argument is coerced. The object has a cache composed of two data members:
##      "inverse" contains the last calculated inverse matrix
##      "last" is the matrix whose inverse is "inverse", i.e., the content of
## the object at the time when the cache was last updated.
## There are also four methods:
##      "get_l" and "get_i" return "last" and "inverse" correspondingly;
##      "set_l" and "set_i" assign their arguments to "last" and "inverse"
## correspondingly.
## Pointers to these methods are stored in the slots of the object created.

as.SpecialMatrix <- function(m) {
        if (!is.matrix(m))
                stop("argument must be a matrix")
        if (nrow(m) != ncol(m))
                stop("argument must be a square matrix")
        last <- NULL
        inverse <- NULL
        get_l <- function() last
        get_i <- function() inverse
        set_l <- function(m) last <<- m
        set_i <- function(m) inverse <<- m
        new("SpecialMatrix", m, 
            get_last = get_l, get_inverse = get_i, 
            set_last = set_l, set_inverse = set_i)
}

## The overloaded solve.SpecialMatrix tries to fetch a cached inverse if the
## current value of the matrix coincides with the matrix cached in "last".
## Otherwise, the standard function "solve" is invoked. Anyway, this function
## creates and returns a SpecialMatrix object with initialized cache. The
## object contains the inverse of the matrix.
## The additional ... arguments of solve.SpecialMatrix() are passed to solve()
## if the latter function is to be called.

solve.SpecialMatrix <- function(sm, ...) {
        m <- as(sm, "matrix")  # strict coercion
        # Coercion to "matrix" is needed to avoid recursion if the cache
        # cannot be used.
        if (isTRUE(all.equal(m, sm@get_last(), check.attributes = FALSE))) {
                # If the current matrix has been inverted before, use the cache
                message("Fetching cached inverse")
                inv <- sm@get_inverse()
        }
        else {
                # If the current matrix has not been inverted before, calculate
                # the inverse and cache it
                message("Calculating the inverse")
                inv <- solve(m, ...)
                sm@set_last(m)
                sm@set_inverse(inv)
        }
        # Create a SpecialMatrix object to be returned
        result <- as.SpecialMatrix(inv)
        # "result" is created with it's cache initialized: if "result" is
        # the inverse of "m" then "m" is the inverse of "result" as well
        result@set_last(inv)
        result@set_inverse(m)
        result
}


## Function "makeCacheMatrix" is just a wrapper of "as.SpecialMatrix".

makeCacheMatrix <- function(x = matrix()) {
        as.SpecialMatrix(x)
}


## Function "cacheSolve" is just a wrapper of "solve.SpecialMatrix".
## "solve" can be used too instead of "cacheSolve" on any SpecialMatrix object.
## In this case, the cached value will be used if one is available.
## Additionally, "cacheSolve" can be used instead of "cache" on any matrix and
## will return its inverse (with a warning).

cacheSolve <- function(x, ...) {
        if (!is(x, "SpecialMatrix"))
                warning("cacheSolve uses a cached inverse matrices for \
SpecialMatrix objects only.")
        solve(x, ...)
}

## /////////////// End of the assignment ////////////////////
##
## How to test the code above:
##
## 1. Let's create a matrix "m"
#  >    m <- matrix(c(5, 0, 7, 0, 0, 8, 0, 5, 2, 0, 3, 0, 0, 5, 0, 3), nrow = 4)
#  >    m
##
## 2. We can calculate it's inverse:
#  >    print(im <- solve(m))
##
## 3. Now let's create a SpecialMatrix object containing m:
#  >    print(sm <- makeCacheMatrix(m))
##    We can see that sm contains the same matrix.
##
## 4. sm is a matrix too; we can manipulate it like any other matrix:
#  >    nrow(sm)
#  >    ncol(sm)
#  >    sm[2, 2]
#  >    sm %*% im
##    (The last multiplication yields identity matrix, possibly with some
##     deviation due to rounding.)
##
## 5. Now we use cacheSolve to invert sm:
#  >    print(ism <- cacheSolve(sm))
##    We can see that the content of ism is identical to im.
##    The message says that the inverse is calculated because no cached value
##    is available yet.
##
## 6. ism is indeed the inverse of sm:
#  >    ism %*% sm
##
## 7. Let's call again cacheSolve with the same sm:
#  >    cacheSolve(sm)
##    Now the message reads that the cached inverse is fetched. The inverse is
##    not recalculated.
##
## 8. Let's calculate the inverse of the inverse:
#  >    sm2 <- cacheSolve(ism)  
##    As ism was created with ready-to-use cache, no calculation was needed.
##
## 9. sm coincides with the inverse of it's inverse:
#  >    isTRUE(all.equal(sm, sm2))
##
## 10. Now lets change sm. We can modify just one element:
#  >    sm[1, 1] = 6
##
## 11. Then, if we call cacheSolve again it will detect that the new sm is
##     different from the cached one. That is why the inverse must be computed
##     and the cache will not be used:
#  >    cacheSolve(sm)
##     (The message says it.)
##
## 12. Instead of "cacheSolve", we can use just "solve". The same result will
##     be obtained:
#  >    solve(sm)
##
## 13. Or, we can apply "cacheSolve" instead of "solve" on "normal" matrices:
#  >    cacheSolve(m)
##     (We get warning in this case that cacheSolve cannot use a cache and
##     will always calculate the inverse.)
