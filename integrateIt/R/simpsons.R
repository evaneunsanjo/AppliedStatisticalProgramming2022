
#' Simpson as S4 Class
#'
#' objects are created with the function \code{integrateIt}
#'
#' An object of classes 'Simpson' has following slots:
#'
#'\itemize{
#' \item \code{x} a vector of inputs
#' \item \code{y} a function to be integrated
#' \item \code{a} starting value of integration
#' \item \code{b} end value of the integration
#' \item \code{Rule} rule to be used in the approximation
#'\item \code{S} result of the approximation using Simpson rule
#'}
#'
#' @author Eunsan Jo
#' @rdname Simpson
#'
#' @export
setClass(Class="Simpson",
         representation = representation(
           x = "numeric",
           y = "function",
           a ="numeric",
           b = "numeric",
           Rule  = "character",
           S = "numeric"
         ),
         prototype = prototype(
           x = numeric(),
           y = function(x){return(x)},
           a = numeric(),
           b = numeric(),
           Rule  = character(),
           S = numeric()
         )
)

#' @export
setMethod("initialize", "Simpson",  function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})

#' @export
#set a validity test again heavily borrowing from lecture slides
setValidity("Simpson", function(object){

  # tests if x is of numeric vector
  test_x <- is.numeric(object@x)
  if (!testx){stop("X must be a numeric vector")}

  # tests whether x and y are of the same length
  test_length <-  length(object@x) == length(object@y)
  if (!test_length){stop("The length of x and y must be the same")}

  #tests whether a and b are in x
  test_include <- object@a %in% object@x & object@b %in% object@X
  if (!test_include){stop("a and b must be within range of x")  }
}
)


#print function
#' @export
setMethod(f = "print", signature(x = "Simpson"),
          definition = function(x){
            print(x@T)
          }
)

# devtools::document()
