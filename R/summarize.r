#'Summarize symptoms
#'
#'This function summarizes weekly reported symptoms as acute respiratory infection (ARI) and influenza-lile illness (ILI). 
#'It makes use of the definitions for ARI and ILI provided by the European Center for Disease Control (ECDC).
#' @param x a \code{\link{flunet}} object
#' @param definitions character vector, one or more definitions to summarize symptoms 
#' @param CR_as_TRUE logical, if \code{TRUE}, answers as "can't remember" are considered as "yes", "no" otherwise. 
#' This is needed to resolve questions like "did your symptoms developed over a few hours?".
#' @param keep_bool logical, if \code{TRUE}, all intermediate, logical, variables created by the function are kept.
#' @export
#' @importFrom plyr mutate mapvalues
#' @return a \code{\link{flunet}} object. The weekly survey contains a logical column per symptom definitions.
#' @note the symptoms definitions for ARI and ILI are taken from the ECDC: \url{http://ecdc.europa.eu/en/activities/surveillance/eisn/surveillance/pages/influenza_case_definitions.aspx}
summarize_symptoms <- function(x,definitions=c("ARI_ecdc","ILI_ecdc","ILI_fever"),CR_as_TRUE=c("FALSE","TRUE"),keep_bool=c("FALSE","TRUE")){

	#checks
	if(!inherits(x,"flunet")){
		stop("x is not a flunet object")
	}
	definitions <- match.arg(definitions,several.ok=TRUE)
	CR_as_TRUE <- as.logical(match.arg(CR_as_TRUE))
	keep_bool <- as.logical(match.arg(keep_bool))

	df_weekly <- x$surveys$weekly
	if(is.null(df_weekly)){
		warnings("x doesn't contain weekly survey, no symptoms to summarize")
		return(x)
	}

	# log
	x$log$summarize_symptoms <- list("definitions"=definitions,"CR_as_TRUE"=CR_as_TRUE)

	# factor to bool
	from_values <- c("yes","no","CR")
	to_values <- c(TRUE,FALSE,CR_as_TRUE)
	df_weekly <- mutate(df_weekly,sympt_sudden_bool=as.logical(mapvalues(sympt_sudden,from_values,to_values)),fever_sudden_bool=as.logical(mapvalues(fever_sudden,from_values,to_values)))

	# tag fever_sudden_bool as FALSE when fever==FALSE & fever_sudden_bool is not answered
	df_weekly$fever_sudden_bool[is.na(df_weekly$fever_sudden_bool) & !df_weekly$fever] <- FALSE

	# summarize symptoms
	for(def in definitions){

		def_2_parse <- switch(def,
			"ARI_ecdc"="((sympt_sudden_bool | fever_sudden_bool ) & (cough | throat | breath | nose ))",
			"ILI_ecdc" = "((sympt_sudden_bool | fever_sudden_bool ) & (fever | chills | top_fever %in% c(\"38-38.9\",\"39-39.9\",\">40\") | tired | head | muscle_joint ) & (cough | throat | breath))",
			"ILI_fever" = "((sympt_sudden_bool | fever_sudden_bool) & (fever | chills | top_fever %in% c(\"38-38.9\",\"39-39.9\",\">40\")) & (tired | head | muscle_joint) & (cough | throat | breath))"		
			)

		df_weekly[[def]] <- with(df_weekly,eval(parse(text=def_2_parse)))
	}

	if(!keep_bool){
		var_bool <- c("sympt_sudden_bool","fever_sudden_bool")
		df_weekly <-subset(df_weekly,select=setdiff(names(df_weekly),var_bool))
	}

	x$surveys$weekly <- df_weekly

	return(x)
}