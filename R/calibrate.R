#written by Thomas Elliott
#University of California, Irvine
#http://thomaselliott.me



#' fuzzy set calibration
#' 
#' fsCalibrate calibrates fuzzy sets using the direct transformation method as described in Charles Ragin's
#' Redesigning Social Inquiry
#'
#' @param x a numeric vector of scores to be calibrates
#' @param thresholds a vector of length three with the thresholds to use for calibrating in the oder: fully out, crossover, full in
#'
#' @return
#' 
#' A vector with the same length of x, with the scores of x transformed into fuzzy set membership scores
#' 
#' @export
fsCalibrate<-function(x,thresholds) {
	#x = a vector of values to be fuzzified
	#thresholds = a vector of length 3 containing the threshold values in the following order: (fully out,crossover,fully in)
	
	if( length(thresholds) != 3 ) stop("You must provide three threshold values")
	dev<-x-thresholds[2]
	if ( thresholds[1] > thresholds[3] ) dev<-dev*(-1)
	us<-3/abs(thresholds[3]-thresholds[2])
	ls<-3/abs(thresholds[1]-thresholds[2])
	scal<-rep(0,length.out=length(x))
	scal[which(dev>=0)]<-us
	scal[which(dev<0)]<-ls
	prod<-dev*scal
	scores<-exp(prod)/(1+exp(prod))
	return(scores)
}