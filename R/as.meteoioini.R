NULL
#### TO DO SISTEMRARE meteoioini_MULTIPLIER PER RH..



#' Coerces an object to a \code{meteoioini-class}  object
#' 
#' @param object the object to be coerced
#' 
#' @param ... further arguments
#' 
#' @rdname as.meteoioini
#' @export
#' 
#' @examples 
#' 
#' ini <- as.meteoioini("test")
#'  
#' 
#' 





as.meteoioini <- function (object=NULL,...)  {
	
	
	return(standardGeneric("as.meteoioini"))
	
}


NULL
#' 
#' 

#' 
#' @rdname as.meteoioini
#' @method as.meteoioini default
#' @aliases as.meteoioini 
#' @export


setGeneric("as.meteoioini",function (object,...)  {
	
	out <- NA
	warning("Object cannot be coerced as 'meteoioini'") 
	return(out)
	
	
	
})


NULL
#'
#' 
#' @rdname as.meteoioini
#' @method as.meteoioini character
#' @aliases as.meteoioini 
#' @export

setMethod("as.meteoioini","character",function(object,...) {
	
	if (object %in% c("test","example")) {
		
		
		object <- system.file("examples/io.ini",package="RSMET")
	}		
			
			
	if (file.exists(object)==TRUE) {
		
	##	out <- meteoioini(file=object,...)
		out <- RSMET::meteoioini(file=object,...)
		
	}	else {
		
		value <- get(object)
		out <- RSMET::as.meteoioini(value,...)
		
	}
			
	
	return(out)
	
	
})


NULL
#'
#' 
#' 
#' @rdname as.meteoioini
#' @method as.meteoioini list
#' @aliases as.meteoioini 
#' @export
#' 
#' 


setMethod("as.meteoioini","list",function(object,...) {lapply(X=object,FUN=RSMET::as.meteoioini,...)})
					   


	
	
NULL
#'
#' 
#' @rdname as.meteoioini
#' @method as.meteoioini meteoioini
#' @aliases as.meteoioini 
#' @export
#' 
#' 
	
	
	setMethod("as.meteoioini","meteoioini",function(object,...) { 
				
				
				args <- list(...)
				
				slotnames <- names(getSlots("meteoioini"))
				
				slotnames <- slotnames[slotnames %in% names(args)]
				
				for (it in slotnames) {
					
					slot(object,it) <- args[[it]]
				}
				
				
				return(object)})
	
	



































