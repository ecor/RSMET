NULL
#' "Print" and "Show" method for "smet-class" object 
#' 
#' @param x,object a \code{smet-class} object
#' @param date.field field neme used for date and time. Default is \code{"timestamp"}.
#' @param date.format format used for date and time. Default is \code{"\%Y-\%m-\%dT\%H:\%M:\%S"}.
#' @param file filename where to print the \code{smat-class} object \code{x} as an ASCII file. 
#' @param print.all logical value. If it is \code{FALSE} exceeding lines are omitted for a better human readibility. 
#' @param max.records maximum printable number of records. Default is 20 and it is activated only if \code{print.all==FALSE}. ,
#' @param ... further argumnents for \code{writeLines}
#' 
#' @rdname print
#' @method print smet
#' @aliases print 
#' 
#' 
#' 
#' @export 
#' 
#' @seealso \code{\link{smet-class}}, \code{\link{smet}},\code{\link{writeLines}}
#' 
#' @examples 
#' 
#' file <- system.file("examples/test.smet",package="RSMET")
#' sm <- smet(file)
#' print(sm)
#' 


print.smet <- function(x,
		date.field="timestamp",
		date.format="%Y-%m-%dT%H:%M:%S",
		file=NULL,
		print.all=!identical(file,NULL),
		max.records=20,
		...) {
	
	
	x@data <- x@data[,x@header$fields]
	
	
	if (date.field %in% names(x@data)) {
	
		x@data[,date.field] <- as.character(x@data[,date.field],format=date.format)
	
	}
	
	
	out <- x@signature 
	out[2] <- "[HEADER]"
	hout <- sapply(X=x@header,FUN=paste,collapse=" ")
	hout <- paste(names(hout),hout,sep=" = ")
	dout <- apply(X=x@data,FUN=paste,collapse=" ",MARGIN=1)
	
	if (print.all==FALSE) {
		
		lmax <- length(dout)
		lmax[lmax>max.records] <- max.records+1
		
		dout <- dout[1:lmax]
		
		if (lmax>max.records) {
			
			dout[lmax] <- "..."
		}
		
		
	}
	
	out <- c(out,hout,"[DATA]",dout)
	
	if (is.null(file)) {
		
		base::writeLines(out,...)
		
	} else {
		
		
		base::writeLines(out,con=file,...)
	}
	
	
	


	
	
}


NULL
#'
#' @rdname print
#' @method show smet
#' @aliases show
#' 
#' @export
setMethod("show","smet",function(object) {print(x=object)})










































