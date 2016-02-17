NULL

#'    Coerces a \code{smet-class} object to a data frame
#' 
#' 
#' 
#' @param x first \code{\link{smet-class}} object 
#' @param y second \code{\link{smet-class}} object 
#' @param headers header attributes to  check before collapsing 
#' @param date.field   field name used for date and time. Default is \code{"timestamp"}, as used for \code{SMET} format. 
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
#' newsmet_ <- collapse.smet(x=smet2,y=smet1)
#' 
#' 

collapse.smet <- function (x,y,headers=NULL,date.field="timestamp") {
	
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
		
		s <- list()
		mx <- list()
		my <- list()
		cwarn <- list()
		for (it in headers) {
			mx[[it]] <- str_trim(paste(header_values_x[[it]],collapse=" "))
			my[[it]] <- str_trim(paste(header_values_y[[it]],collapse=" "))
			s[[it]] <- sprintf("%s: %s == %s",it,mx[[it]],my[[it]]) ###%(header_values_x)
			cwarn[[it]] <- (mx[[it]]!=my[[it]])

		}

		iw <- which(unlist(cwarn))
		if (length(iw)>=1) { 
			iw <- unique(c(iw,which(headers=="station_id")))
		    m <- c("Collapsing SMET : header mismatch!!!",unlist(s[iw]))
		    m <- paste(m,sep="   ")
		
			warning(m)
		}
	}
	
	out <- x 
	
	dd <- rbind(x@data,y@data)
	
	dd <- dd[order(dd[,date.field]),]
	dd <- dd[which(!duplicated(dd[,date.field])),]
	out@data <- dd
	
	out@file <- as.character(NA)
	#######
	
	
	return(out)
	
	
}