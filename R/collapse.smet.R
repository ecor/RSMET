NULL

#'    Coerces a \code{smet-class} object to a data frame
#' 
#' 
#' 
#' @param x first \code{\link{smet-class}} object 
#' @param y second \code{\link{smet-class}} object 
#' @param headers header attributes to  check before collapsing 
#' @export
#'
#' @return a collapsed \code{\link{smet-class}} object
#' @examples 
#' 
#' smet1 <- as.smet(system.file("examples/zugspitze1.smet",package="RSMET"))
#' smet2 <- as.smet(system.file("examples/zugspitze2.smet",package="RSMET"))
#' 
#' newsmet <- collapse.smet(x=smet1,y=smet2)
#' 
#' 
#' 
#' 

collapse.smet <- function (x,y,headers=NULL) {
	
	x <- as.smet(x) ## x and y must be smet!
	y <- as.smet(y)
	
	if (is.null(headers)) {
		
		headers <- names(x@header)
		
	}
	
	headers <- headers[headers %in% intersect(names(x@header),names(y@header))]
		
	header_values_x <- x@header[headers]
	header_values_y <- y@header[headers]
		
		
	#######
	
	if (!identical(header_values_x,header_values_y)) {
		
		warning("Collapsing SMET : header mismatch!!!")
	}
	
	out <- x 
	
	out@data <- rbind(x@data,y@data)
	
	#######
	
	
	return(out)
	
	
}