NULL
#'
#' @param x a \code{\link{smet-class}} object

#' @param date.field   field name used for date and time. Default is \code{"timestamp"}, as used for \code{SMET} format.
#' @param by an index vector or a chararacter vector defining the time interval used for aggregation. Default is \code{c("hourly","daily","monthly","yearly")}, it can be one of these character values, in case of vectore with caracheter values. only the first element is considered. 
#' @param past logical value. If it is \code{TRUE} aggregation value is referenced to the time step before the \code{date.field} (e.g. \code{"timestamp"} instant value, otherwise it is referenced to the timestep after the \code{date.field} value.
#' @param INDEX,FUN,... further arguments for \code{\link{tapply}}

#' @details \code{FUN} can be one function or a list of functions where each function is used to aggregate each time-series variable of the \code{\link{smet-class}} object object. In case \code{FUN} is a \code{list} , each function element should be named with the respectiva variable name and all functions must have the seme arguments passed through \code{...} .  

#' @title aggregate
#' @description aggregate
#' @rdname aggregate
#' @method aggregate smet
#' @aliases aggregate 
#' @export
#' @importFrom stats aggregate
#' @importFrom lubridate seconds day<- hour<- hours minute<- month<- second<-
#' ' @ssealso \code{\lik{as.smet}},\code{\link{tapply}}
#' @examples
#' smet <- as.smet(system.file("examples/T0179.smet",package="RSMET"))
#' 
#' out <- aggregate(smet)
#' out_d <- aggregate(smet,by="daily",past=FALSE)
#' 



aggregate.smet <- function(x,date.field="timestamp",FUN=mean,INDEX=by[1],by=c("hourly","daily","monthly","yearly"),past=TRUE,...) {
	
	
	out <- x
	
	if (date.field %in% fields(x)) { 
	
		
		out <- as.data.frame(x)		
		mult <- x@header$units_multiplier
		offset  <- x@header$units_offset
		### AGGREGATE HERE 
		t_time <- out[,date.field]
		ivars <- which(names(out)!=date.field)
		tocheck <- duplicated(t_time,fromLast=FALSE) | duplicated(t_time,fromLast=TRUE)
		hasNA <- apply(X=out[,ivars],MARGIN=1,FUN=function(x){length(which(is.na(x)))})
		itocheck <- which(tocheck)
		
		if (length(itocheck)>0) {
			
			dftt <- data.frame(ic=itocheck,hasNA=hasNA[itocheck],t_time=as.character(t_time[itocheck]))
			valid_row <- tapply(X=dftt$hasNA,FUN=min,INDEX=dftt$t_time)
			dftt$valid_row <- valid_row[dftt$t_time]
			dftta <- dftt[dftt$hasNA!=dftt$valid_row,]
			out <- out[-dftta$ic,]
			t_time <- out[,date.field]
		}
		##check <- tapply(X=hasNA,INDEX=t_time,FUN=min) 		
		if (!is.list(FUN)) FUN <- list(FUN=FUN)
		
		ffields <- fields(x)
		ffields <- ffields[ffields!=date.field]
		
		if (length(FUN)!=length(ffields)) {
			
			
			
			
			
			for (itf in ffields[!(ffields %in% names(FUN))]) {
				
				
				FUN[[itf]] <- FUN[[1]]
				
				
			}
			
			FUN <- FUN[ffields]
			
		}		
		
		if (length(INDEX)==1) {
			
			
			if (past==TRUE) {
				shift <- seconds(1)
			} else {
				shift <- 0
			}
			
			t_timex <- t_time-shift
			if (INDEX=="yearly") {
				
					day(t_timex) <- 1	
					month(t_timex) <- 1
					INDEX <- "daily"
			}
			
			if (INDEX=="monthly") {
				day(t_timex) <- 1
				INDEX <- "daily"				
			}
			
			if (INDEX=="daily") {
				
				hour(t_timex) <- 0
				minute(t_timex) <- 0
				second(t_timex)  <- 0 
		###		INDEX <- t_timex
			} 
			
			if (INDEX=="hourly") {
				
				minute(t_timex) <- 0
				second(t_timex)  <- 0
				if (past==TRUE) t_timex <- t_timex+hours(1)
				
				
				
			}
			
			INDEX <- t_timex
			
			### 
			
			
			
		}
		
			
		date.format="%Y-%m-%dT%H:%M:%S"
		INDEXc <- as.character(INDEX,format=date.format)
		timestamp <- sort(unique(INDEX))
		timestamp_c <- as.character(timestamp,format=date.format)
		
		
		outn <- data.frame(timestamp=timestamp)
		names(outn) <- date.field
		
		for (it in ffields) {
				
			
			vect <- tapply(X=out[,it],FUN=FUN[[it]],INDEX=INDEXc,...)
			outn[,it] <- vect[timestamp_c]
				
				
				
		}
			
			
			##dftime$year <- 
			
			
			
			
			
			
			
		
		
		
		### AGGREGATION 
		
		
		
		
		####
		attr(outn,"header") <- attr(out,"header")
		out <- as.smet(outn,date.field=date.field,mult=mult,offset=offset)
	
	
	} else { 
	
		## DO NOTHING
		out <- x 
	}
	
	
	return(out)
	
	
}