#' Apply a bandpass filter (remove frequencies outside given range)
#'
#' @param x Signal
#' @param a Lower limit
#' @param b Higher limit
#'
#' @return Filtered signal
#' @export
bandpass.filter <- function(x, a, b) {
  z <- fft(x)
  if (a>1) z[seq(1, a-1)] <- 0
  if (b<length(z)) z[seq(b+1, length(z)-b-1)] <- 0
  if (a>1) z[seq(length(z)-a+1, length(z))] <- 0
  Re(fft(z, TRUE))/length(x)
}
