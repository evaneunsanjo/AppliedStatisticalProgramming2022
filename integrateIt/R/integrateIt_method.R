#' Integration approximation using Trapezoid rule or Simpson's rule
#'
#' Generates approximation of integration using Trapezoid rule or integration rule.

#' @param x Vector of x values, numeric
#' @param y Function to be integrated, a function
#' @param a Starting value of vector x, numeric
#' @param b Ending value of vector x, numeric
#' @param Rule approximation rule in use, \code{Trapezoid} or \code{Simpson}.
#'
#' @return A list containing:
#'  \item{EstType}{Approximation rule that was used}
#'  \item{Values}{Inputted values of x and y}
#'  \item{Estimation}{Result of the approximation}
#'
#' @author Eunsan Jo
#'
#' @examples
#' x <- c(-1:3)
#' y <- 2^x
#' a <- -1
#' b <- 3
#' integrateIt(x, y, a, b, "Trapezoid")


#' @seealso \code{\link{print_method}}
#' @rdname integrateIt
#' @aliases IntegrateIt, integrateit
#' @export
#set generic method ahead of specific methods first
setGeneric(name = "integrateIt",
           def = function(x, y, a, b, Rule){
             standardGeneric("integrateIt")
             })

integrateIt<-function(x, y, a, b, Rule){

  if (Rule == "Trapezoid"){
    y <- y
    n <- length(x) - 1
    h <- (b-a) / n
    T <-  h /2 * (y[1] + sum(2*y[2:n] + y[n+1]))

    return(list("Trapezoid", x=x, y=y, output = T))

  }

  if (Rule == "Simpson"){
    y <- y
    n <- length(x) - 1
    h <- (b-a) / n
    #seq() function allows us to sum x_2,x_4,... and x_3, x_5, ... separately
    S <-  h /3 * (y[1] + 4*sum(y[seq(2,n,2)]) + 2*sum(y[3,n,2]) + y[n+1])

    return(list("Simpson", x=x, y=y, output = S))
  }
  else{
    return("Please insert a valid rule.")
  }
}



# devtools::document()
