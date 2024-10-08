#################################################################
#################################################################
#################################################################
#' @title Take the logarithm of the absolute value of a number with its sign
#' @description This function takes a numerical input \code{x}, computes the logarithm of its
#' absolute value and multiplies it by the sign of \code{x}. This ensures that the
#' result has the same sign as the input \code{x}.
#' @param x A numerical input.
#' @return The result of taking the logarithm of the absolute value of \code{x}
#' with its sign.
#' @examples
#' .logmod(10)
#' .logmod(-5)
#' @export
#' @keywords internal
.logmod <- function(x) sign(x) * log(abs(x))

#' @title Find the derivative
#' @description Find the rate of change of a function with respect to a variable
#' @export
#' @keywords internal
.deriv <- function(x, y) diff(y) / diff(x)

#' @title Middle point of x
#' @description Find the middle point of x
#' @export
#' @keywords internal
.middle_pts <- function(x) x[-1] - diff(x) / 2