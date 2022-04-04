

#' Trapezoid as S4 Class
#'
#' An object of classes 'Trapezoid' has following slots:
#'
#' @slot x a vector of inputs
#' @slot y a function to be integrated
#' @slot a starting value of integration
#' @slot b end value of the integration
#' @slot Rule rule to be used in the approximation
#' @slot T result of the approximation using Trapezoid rule
#' @slot S result of the approximation using Simpson rule
#'
#' @author Eunsan Jo
#' @rdname Trapezoid
#'
#' @export
setClass(Class="Trapezoid",
         representation = representation(
            y = "function",
            x = "numeric",
           a ="numeric",
           b = "numeric",
           Rule  = "character",
           T = "numeric"
         ),
         prototype = prototype(
          y = function(x){return(x)},
          x = numeric(),
           a = numeric(),
           b = numeric(),
           Rule  = character(),
           T = numeric()
         )
)


#' @export
setMethod("initialize", "Trapezoid",  function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})


#' @export
#set a validity test again heavily borrowing from lecture slides
setValidity("Trapezoid", function(object){

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



#' @export
#print function
setMethod(f = "print", signature(x = "Trapezoid"),
          definition = function(x){
            print(x@T)
          }
)



# devtools::document()

