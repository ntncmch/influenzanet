#'Summarize symptoms
#'
#'This function summarizes weekly reported symptoms as acute respiratory infection (ARI) and influenza-lile illness (ILI). 
#'It makes use of the definitions for ARI and ILI provided by the European Center for Disease Control (ECDC).
#' @param flunet a \code{\link{flunet}} object
#' @param definitions character vector, one or more definitions to summarize symptoms 
#' @param CR_as_TRUE logical, if \code{TRUE}, answers as "can't remember" are considered as "yes", "no" otherwise. This choice is required for the questions regarding sudden onset of symptom and fever.
#' This is needed to resolve questions like "did your symptoms developed over a few hours?".
#' @param keep_bool logical, if \code{TRUE}, all intermediate, logical, variables created by the function are kept. That is the variables with CR (can't remember) after transformation accoridng to \code{CR_as_TRUE}
#' @export
#' @import plyr
#' @note If the question on sudden onset of fever is not answered (\code{fever_sudden=NA}) but the participant didn't reported fever, then it is set to \code{FALSE}.
#' @return a \code{\link{flunet}} object. The weekly survey contains a logical column per symptom definitions.
#' @note the symptoms definitions for ARI and ILI are taken from the ECDC: \url{http://ecdc.europa.eu/en/activities/surveillance/eisn/surveillance/pages/influenza_case_definitions.aspx}
summarize_symptoms <- function(flunet,definitions=c("ARI_ecdc","ILI_ecdc","ILI_fever"),CR_as_TRUE=FALSE,keep_bool=FALSE){

	
	definitions <- match.arg(definitions,several.ok=TRUE)

	if(is_survey_present(flunet,survey="weekly",warning_message="no symptoms to summarize")){
		df_weekly <- flunet$surveys$weekly
	} else {
		return(flunet)
	}

	# write info on the log
	flunet$log$summarize_symptoms <- list("definitions"=definitions,"CR_as_TRUE"=CR_as_TRUE,"keep_bool"=keep_bool)

	# factor to bool
	from_values <- c("yes","no","CR")
	to_values <- c(TRUE,FALSE,CR_as_TRUE)
	df_weekly <- mutate(df_weekly,sympt_sudden_bool=as.logical(mapvalues(sympt_sudden,from_values,to_values)),fever_sudden_bool=as.logical(mapvalues(fever_sudden,from_values,to_values)))

	# tag fever_sudden_bool as FALSE when fever==FALSE & fever_sudden_bool is not answered
	x <- with(df_weekly, which(is.na(fever_sudden_bool) & !fever))
	df_weekly$fever_sudden_bool[x] <- FALSE

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
		df_weekly <- df_weekly[setdiff(names(df_weekly),var_bool)]
	}

	flunet$surveys$weekly <- df_weekly

	return(flunet)
}


#'Summarize underlying health conditions
#'
#'This function summarizes the underlying health conditions of every participant according to \code{definitions}
#' @param flunet a \code{\link{flunet}} object
#' @param definitions character vector, one or more definitions to summarize underlying health conditions. See note below.
#' @note \code{any_UHC} stands for any underlying health conditions.
#' @export
summarize_UHC <- function(flunet, definitions=c("any_UHC")) {

	definitions <- match.arg(definitions,several.ok=TRUE)

	if(is_survey_present(flunet,survey="intake",warning_message="no underlying health conditions to summarize")){
		df_intake <- flunet$surveys$intake
	} else {
		return(flunet)
	}

	# summarize symptoms
	for(def in definitions){

		def_2_parse <- switch(def,
			"any_UHC"="(asthma | diabetes | other_respiratory | heart | kidney | immuno )"
			)

		df_intake[[def]] <- with(df_intake,eval(parse(text=def_2_parse)))
	}

	flunet$surveys$intake <- df_intake

	return(flunet)
}

#'Summarize age
#'
#'This function summarizes the age of every participant with age groups. It's mainly a wrapper aroud \code{\link[base]{cut}}.
#' @inheritParams summarize_symptoms
#' @inheritParams base::cut
#' @export
#' @import plyr
#' @examples \dontrun{
#' flunet <- summarize_age(flunet, breaks=c(0,18,45,65,Inf), labels=c("0-17", "18-44", "45-64", "65+"))
#'}
summarize_age <- function(flunet, breaks, labels = NULL, include.lowest = TRUE, right = FALSE, ordered_result = TRUE, ...) {

	if(is_survey_present(flunet,survey="intake",warning_message="no age to summarize")){
		df_intake <- flunet$surveys$intake
	} else {
		return(flunet)
	}

	df_intake <- mutate(df_intake, age_group = cut(age, breaks = breaks, labels=labels, include.lowest=include.lowest, right=right, ordered_result=ordered_result, ...))

	flunet$surveys$intake <- df_intake

	return(flunet)
}




