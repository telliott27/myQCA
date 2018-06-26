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



#' Print Pretty Truth Table
#' 
#' prints a pretty version of the truth table for use with knitr and rmarkdwon
#' 
#' @param truth a truthTable object
#' @param complete print the complete truth table, including empty rows?
#' 
#' @return 
#' 
#' A markdown formatted version of the truth table
#' 
#' @export
#' @importFrom magrittr %>%

pretty.tt<-function(truth,complete=FALSE) {
  tt<-truth$tt %>% dplyr::select(-PRI,-cases) %>% 
    dplyr::mutate(incl=round(as.numeric(incl),digits=3)) %>%
    dplyr::arrange(desc(incl))
  if( !complete ) {
    tt<-tt %>% dplyr::filter(n>0)
  }
  knitr::kable(tt)
}

#' Print Pretty QCA Solution
#' 
#' Prints a pretty version of the QCA solution that can be used with knitr and rmarkdown
#' 
#' @param sol a qca object
#' @param number which intermediate solution
#' 
#' @ return
#' 
#' A markdown formatted version of the truth table
#' 
#' @export
#' @importFrom magrittr %>%

pretty.qca<-function(sol, number=1) {
  # pull out the individual intermediate solution
  i.sol<-sol$i.sol[[number]]$IC
  # grab the data.frame of the individual recipe solutions
  sol.df<-i.sol$incl.cov %>% tibble::rownames_to_column("Recipe") %>% dplyr::select(-cases)
  overall.sol<-data.frame(Recipe="Overall", 
                          inclS=i.sol$sol.incl.cov["incl"],
                          PRI=i.sol$sol.incl.cov["PRI"],
                          covS=i.sol$sol.incl.cov["cov"])
  sol.df<-sol.df %>% dplyr::bind_rows(overall.sol) %>%
    dplyr::mutate_at(dplyr::vars(inclS,PRI,covS,covU), dplyr::funs(round(.,digits=3))) %>%
    dplyr::select(Recipe,Consistency=inclS,Coverage=covS,`Unique Coverage`=covU)
  options(knitr.kable.NA='')
  knitr::kable(sol.df)
}
