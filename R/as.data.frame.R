NULL

#'    Coerces a \code{smet-class} object to a data frame
#' 
#' 
#' 
#' @param x a \code{smet-class} object 
#' @param date.field field neme used for date and time. Default is \code{"timestamp"}.
#' @param add.header logical If \code{TRUE} it adds header scalar attributes as fields of the data frame. Default is \code{FALSE}.
#' @param use.melt logical. Default is \code{FALSE}. If it is \code{TRUE}, the result is melted with \code{\link{melt}}.
#' @param ... further arguments
#' 
#' @seealso \code{\link{smet-class}}, \code{\link{smet}},\code{\link{as.data.frame}}
#' @import methods
#' @importFrom reshape2 melt
#' @rdname as.data.frame
#' @method as.data.frame smet
#' @aliases as.data.frame 
#' @export 
#' 
#'  
#'
#' 
#' 
#' 











as.data.frame.smet <- function(x,...,date.field="timestamp",add.header=FALSE,use.melt=FALSE) {
	
	 
	
	out <- base::as.data.frame(x@data,...)
	out <- out[,x@header$fields]
	mult <- x@header$units_multiplier[x@header$fields]
	offset <- x@header$units_offset[x@header$fields]
	
	i <- which(names(out)!=date.field)
	
	temp <- out
	
	####if (length(i)==1) out <- as.data.frame(out[,i])
	
	## insert here SI.UNIT==false https://en.wikipedia.org/wiki/International_System_of_Units
	if (length(i)>1) {
		out[,i] <- t(apply(X=as.data.frame(out[,i]),FUN=function(x,mult,offset) {x*mult+offset},mult=mult[i],offset=offset[i],MARGIN=1))
	} else { 
	
		out[,i] <- out[,i]*mult[i]+offset[i]
	}
	
	header <- x@header
	header$units_multiplier <- header$units_multiplier*0+1
	header$units_offset <- header$units_offset*0+1
	
	
	attr(out,"header") <- header
	attr(out,"signature") <- x@signature
	
	
	out[out==x@header$nodata] <- NA
	
	if (add.header==TRUE) {
		
		nna <- names(out)
		
		nnam <- names(x@header)
		nnam <- nnam[!(nnam %in% c("fields","units_offset","units_multiplier","nodata"))]
		header <- x@header[nnam]
		is <- sapply(X=header,FUN=length)
		header <- header[is==1]
		header <- as.data.frame(header,stringsAsFactors=FALSE)
		
		out <- cbind(header,out)
		
		
		
	}
	
	if (use.melt==TRUE) {
		
		id <- names(out)[!(names(out) %in% x@header$fields) | names(out)==date.field]
		
		if (date.field %in% names(out)) {
			
			startd <- out[1,date.field]
			
			out[,date.field] <- as.numeric(out[,date.field]-startd,units="secs")
			
		}
		
		out <- melt(data=out,id=id,na.rm=TRUE)
		out$variable <- as.character(out$variable)
		
		if (date.field %in% names(out)) {
			
		
			
			out[,date.field] <- (out[,date.field]+startd)
			
		}
		
		
		
	}
	
	
	return(out)
	
	
	
	
	
}