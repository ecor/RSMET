NULL
#' Put SMET object file into a \code{meteoini-class}
#' 
#' @param ... \code{\link{smet-class}} objects
#' @param smetlist (alternative) list of \code{\link{smet-class}} objects
#' @param ini a \code{\link{meteoioini-class}} object  
#' @param smetwdir .....
#' 
#' 
#' 
#' @export 
#' 
#' @examples
#' 
#' ##  NOT YET IMPLEMENTED
#' 


putSmetIntoIni <- function(...,
		smetlist=NULL,
		ini=as.meteoioini("test"),
		smetwdir=system.file("temp",package="RSMET"),
		force.smetdir=TRUE,
		new.ini.file=NA) {
			
			
	
			if (is.null(smetlist)) {
				
				
				smetlist <- list(...)
				
				if (is.null(ini)) {
					
					
					warning("Meteoio Ini object is not espliciated! It is taken implicitly!!")
					len <- length(smetlist)
					ini <- smetlist[[len]]
					smetlist <- smetlist[-len]
					
					
				}
				
				
			}
			
			out <- ini
			
			input <- out@Input
			
			#### SMET
			cond <-  !is.null(input$METEO)
			if (cond==FALSE) {
				
				message("No METEO key value specified, automatically set as SMET")
				input$METEO <- SMET
			} 
			if (!identical(input$METEO,"SMET")) {
				
				massage("METEO key value forced to SMET!")
				input$SMET <- "SMET"
				
				
			}	
			##### SMETDIR
			
			if (append==TRUE) smetwdir <- input$METEOPATH
			
			cond <-  !is.null(input$METEOPATH)
			if (cond==FALSE) {
				
				message("No METEOPATH key value specified, automatically set as smetwdir argument")
				input$METEOPATH <- smetwdir
			} 
			if (!identical(input$METEO,"SMET")) {
				
				massage("METEO key value forced to smetwdir argument!")
				input$SMET <- smetwdir
				
				
			}	
			
			if (append==TRUE) {
				
					start <- which(stringr::str_detect(names(input),"STATION"))
					start <- length(start)
					
			} else {
				
					toremove <-  which(stringr::str_detect(names(input),"STATION"))
					input <- input[-toremove]
					start <- 0				
				
			}
			
			newstation_keys <- paste("STATION",start+1:length(smetlist),sep="")
			
			if (is.null(names(smetlist))) names(smatlist) <- newstation_keys
			
			names(newstation_keys) <- names(smetlist)
			
			for (it in names(smetlist)) {
				
				file <- paste(smetwdir,it,sep="/")
				file <- paste(file,".smet",sep="")
				input[[newstation_keys[it]]] <- it
				smetlist[[it]]@file <- file			
				
			}
			
			lapply(X=smatfiles,FUN=RSMET::print,file="internal")
			
			
			out <- as.meteoioini(out,Input=input,file=as.character(new.ini.file))
			
			
			
			
			
			
			return(out)
			
		}








