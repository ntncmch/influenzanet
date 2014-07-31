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
#' @import dplyr
#' @importFrom plyr mapvalues
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



#'Summarize symptom severity
#'
#'This function summarizes symptom severity by taking the highest symptom as ordered in \code{severity}.
#' @param  severity character vector, symptoms ordered by increasing severity. Each symptoms must correspond to a logical variable in the weekly survey. If the vector is (partially) named, availables names are used as the new labels for the severity.
#' @inheritParams summarize_symptoms
#' @export
#' @import dplyr
#' @seealso \code{\link{summarize_symptoms}}
#' @return a \code{\link{flunet}} object with an additional - ordered - variable \code{symptom_severity} in the weekly survey.
summarize_severity <- function(flunet, severity=c("ARI"="ARI_ecdc","ILI_no_fever"="ILI_ecdc","ILI_fever")) {

	if(is_survey_present(flunet,survey="weekly",warning_message="severity won't be summarized")){
		df_weekly <- flunet$surveys$weekly
	} else {
		return(flunet)
	}
	
	if("symptom_severity"%in%names(df_weekly)){
		warning("symptom severity is already present in the weekly survey and will be erased", call.=FALSE)
		df_weekly$symptom_severity <- NULL	
	}

	if(is.null(x <- names(severity))){
		# names equal value
		names(severity) <- severity
	} else if(any(x <- names(severity)=="")){
		x <- which(x)
		names(severity)[x] <- severity[x]
	}

	# swap names with value
	labels_severity <- names(severity)
	values_severity <- unname(severity) 

	var_ordered <- c(get_ordered_variables(df_weekly),list("symptom_severity"=labels_severity))

	# write info on the log
	flunet$log$summarize_severity <- list("severity"=severity)

	# keep only reports with symptoms in severity
	any_severity <- paste(values_severity,collapse=" | ")	
	df_summarize <- filter(df_weekly,eval(parse(text=any_severity),df_weekly))
	df_keep <- anti_join(df_weekly,df_summarize,by=names(df_weekly))

	severity_ordered <- factor(labels_severity,levels=labels_severity,ordered=TRUE)

	df_summarize$symptom_severity <- apply(df_summarize[values_severity],1,function(x) {max_na(severity_ordered[as.logical(x)],na_rm=TRUE)})
	
	df_weekly <- rbind_list(df_summarize,df_keep) %>% arrange(person_id,comp_time)

	flunet$surveys$weekly <- set_ordered_variables(df_weekly,var_ordered)

	return(flunet)
}


#'Summarize underlying health conditions
#'
#'This function summarizes the underlying health conditions of every participant according to \code{definitions}
#' @param definitions character vector, one or more definitions to summarize underlying health conditions. See note below.
#' @inheritParams summarize_symptoms
#' @note \code{any_UHC} stands for any underlying health conditions.
#' @export
summarize_UHC <- function(flunet, definitions=c("any_UHC")) {

	definitions <- match.arg(definitions,several.ok=TRUE)

	if(is_survey_present(flunet,survey="intake",warning_message="no underlying health conditions to summarize")){
		df_intake <- flunet$surveys$intake
	} else {
		return(flunet)
	}

	if(any(x <- definitions%in%names(df_intake))){
		warning(sQuote(definitions[x])," already present in the intake survey and will be erased", call.=FALSE)
		df_intake[definitions[x]] <- NULL	
	}

	# write info on the log
	flunet$log$summarize_UHC <- list("definitions"=definitions)

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

#'Summarize smoking habits
#'
#'This function summarizes the smoking habits of participants by creating new variables.
#' @param new_var character vector, new variables created to summarize smoking habits. See note below.
#' @inheritParams summarize_symptoms
#' @note The following summary variables are currently available:
#' \itemize{
#' 	\item \code{"smoke_bool"} is a logical variable that indicates whether participant is a smoker or not.
#' }
#' @export
#' @import dplyr
#' @importFrom plyr revalue
summarize_smoke <- function(flunet, new_var = c("smoke_bool")) {

	new_var <- match.arg(new_var,several.ok=TRUE)

	if(is_survey_present(flunet,survey="intake",warning_message="smoke won't be summarized")){
		df_intake <- flunet$surveys$intake
	} else {
		return(flunet)
	}

	if("smoke_bool"%in%new_var){
		df_intake <- mutate(df_intake,smoke_bool=as.logical(revalue(smoke, c(
			"no" = "FALSE", 
			"occasionally" = "TRUE",
			"<10_per_day" = "TRUE",
			">10_per_day" = "TRUE",
			"unknown" = "NA")))
		)
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
#' @import dplyr
#' @examples \dontrun{
#' flunet <- summarize_age(flunet, breaks=c(0,18,45,65,Inf), labels=c("0-17", "18-44", "45-64", "65+"))
#'}
summarize_age <- function(flunet, breaks, labels = NULL, include.lowest = TRUE, right = FALSE, ordered_result = TRUE, ...) {

	if(is_survey_present(flunet,survey="intake",warning_message="no age to summarize")){
		df_intake <- flunet$surveys$intake
	} else {
		return(flunet)
	}
	
	if("age_group"%in%names(df_intake)){
		warning("age group is already present in the intake survey and will be erased", call.=FALSE)
		df_intake$age_group <- NULL	
	}

	df_intake <- mutate(df_intake, age_group = cut(age, breaks = breaks, labels=labels, include.lowest=include.lowest, right=right, ordered_result=ordered_result, ...))

	# write info on the log
	flunet$log$summarize_age <- list("breaks"=breaks,"labels"=attributes(df_intake$age_group)$levels)

	flunet$surveys$intake <- df_intake

	return(flunet)
}




#'Compute baseline health score
#'
#'This function takes all health-score of reports without any symptom and use \code{fun_summarize} to summarize them.
#' @param  fun_summarize character. Name of the function used to compute baseline health-score. By default, the median function is used.
#' @inheritParams summarize_symptoms
#' @export
#' @import dplyr
#' @return A \code{\link{flunet}} object where the \code{intake} survey has an extra column \code{baseline_health_score}.
summarize_baseline_health_score <- function(flunet, fun_summarize=median) {

	if(!is.function(fun_summarize)){
		stop("Argument ",sQuote("fun_summarize")," is not an R function.",call.=FALSE)
	}

	if(is_survey_present(flunet,survey="weekly",warning_message="baseline health score won't be summarized")){
		df_weekly <- flunet$surveys$weekly
	} else {
		return(flunet)
	}

	if(is_survey_present(flunet,survey="intake",warning_message="baseline health score won't be summarized")){
		df_intake <- flunet$surveys$intake
	} else {
		return(flunet)
	}

	if("baseline_health_score"%in%names(df_intake)){
		warning("baseline health score is already present in the intake survey and will be erased", call.=FALSE)
		flunet$surveys$intake$baseline_health_score <- NULL	
		df_intake <- flunet$surveys$intake
	}

	# write info on the log
	flunet$log$summarize_baseline_health_score <- list("fun_summarize"=fun_summarize)

	flunet$surveys$intake <- df_weekly %>% filter(
		!is.na(health_score) & (
			(is.na(n_bout) & (is.na(still_ill) | !still_ill) & (is.na(still_off) | still_off!="yes") & is.na(cause)) |
			(!is.na(symptom_end) & symptom_end < report_date) | 
			((is.na(still_ill) | !still_ill) & (is.na(n_bout) | position_bout == length_bout))
			)
		) %>% 
	group_by(person_id) %>% 
	summarise(baseline_health_score=fun_summarize(health_score)) %>% 
	left_join(flunet$surveys$intake,.,by="person_id")

	return(flunet)
}

#'Summarize episodes of illness
#'
#'This functions summarize the weekly survey (longitudinal data) into episodes of illness. Existing variables of the weekly survey are summarized and several new ones are created (e.g. \code{symptom_duration}).
#' @inheritParams summarize_symptoms
#' @note The weekly survey must not have duplicated report dates (for a given participant). If so, this is solved using the function \code{\link{resolve_multiple_report_date}}.
#' @export
#' @import myRtoolbox dplyr
#' @return A \code{\link{flunet}} object with an additional survey \code{episode}.
summarize_episode <- function(flunet) {

	if(is_survey_present(flunet,survey="weekly",warning_message="episodes won't be summarized")){
		df_weekly <- flunet$surveys$weekly
	} else {
		return(flunet)
	}

	# check no duplicated report dates
	n_duplicates <- df_weekly %>% group_by(person_id,report_date) %>% summarize(n=n()) %>% filter(n>1) %>% nrow
	if(n_duplicates){
		warning("Duplicated report dates in weekly survey are solved using the function ",sQuote("resolve_multiple_report_date"),call.=FALSE)
		flunet <- resolve_multiple_report_date(flunet)
		df_weekly <- flunet$surveys$weekly
	}

	# summarize variables manually
	summarize_manually <- NULL

	for(var in names(df_weekly)){
		x <- switch (var, 
			report_date = c(first_report_date = "first_na(report_date)", last_report_date = "last_na(report_date)"),
			symptom_start = c(symptom_start="first_na(symptom_start)"),
			symptom_end = c(symptom_end="last_na(symptom_end)"),
			length_bout = c(length_bout = "last(length_bout)"),
			still_ill = c(still_ill = "last(still_ill)"),
			still_off = c(still_off = "last(still_off)"),
			health_score = c(median_health_score = "median(health_score,na.rm=TRUE)",min_health_score = "min(health_score,na.rm=TRUE)")
			)
		summarize_manually <- c(summarize_manually,x)		
	}

	# other variables
	var_names <- setdiff(names(df_weekly),names(summarize_manually))

	# boolean variables 
	is_bool <- vapply(df_weekly[var_names], function(x) all(is.logical(x)),logical(1))
	var_bool <- var_names[is_bool]

	# some variables should be negative when any of the other is positive (med_no, visit_no)
	var_bool_no <- var_bool[str_detect(var_bool,"\\b.+_no\\b")]
	var_bool <- setdiff(var_bool,var_bool_no)

	summarize_bool <- paste0("any_na(",var_bool,",na_rm=TRUE)")
	names(summarize_bool) <- var_bool

	summarize_bool_no <- paste0("all_na(",var_bool_no,",na_rm=TRUE)")
	names(summarize_bool_no) <- var_bool_no

	# ordered variables
	var_names <- setdiff(var_names,var_bool)

	var_ordered <- names(get_ordered_variables(df_weekly[var_names]))
	var_ordered_min <- str_detect_multi_pattern(var_ordered,c("time_visit","time_phone","AV"),value=TRUE)
	var_ordered_max <- setdiff(var_ordered,var_ordered_min)
	
	summarize_ordered_min <- paste0("min_na(",var_ordered_min,",na_rm=TRUE)")
	names(summarize_ordered_min) <- var_ordered_min
	summarize_ordered_max <- paste0("max_na(",var_ordered_max,",na_rm=TRUE)")
	names(summarize_ordered_max) <- var_ordered_max

	# all summarize
	summarize_all <- c(summarize_manually,summarize_bool,summarize_bool_no,summarize_ordered_min,summarize_ordered_max)

	# summarize
	# keep only reports belonging to a bout and put them in the right order
	df_weekly_bout <- filter(df_weekly,!is.na(n_bout)) %>% 
	arrange(person_id,report_date)

	n_episodes <- df_weekly_bout %>% select(person_id,n_bout) %>% unique %>% nrow
	n_participants <- df_weekly_bout %>% select(person_id) %>% unique %>% nrow

	message("Summarizing ",n_episodes," episodes for ",n_participants," participants")

	df_episode <- df_weekly_bout %>% group_by(person_id,n_bout)

	call_summarize <- parse(text=sprintf("summarize(df_episode,%s)",paste(paste(names(summarize_all),summarize_all,sep="="),collapse=",")))	
	df_episode <- eval(call_summarize) %>% ungroup

	# mutate
	if(all(c("symptom_start","symptom_end")%in%names(df_episode))){
		df_episode <- mutate(df_episode,symptom_duration=as.numeric(symptom_end - symptom_start))	
	}

	# perform some checks

	# all episodes with non-NA time_off must be associated with the highest change_routine ("yes + off")
	df_episode$change_routine[!is.na(df_episode$time_off)] <- last(levels(df_episode$change_routine))

	# all episodes with non-NA time_[visit,phone]_* must have [visit,phone]_* equal to TRUE
	var_time <- str_detect_multi_pattern(names(df_episode),c("time_visit","time_phone"),expression="regexp",value=TRUE)
	var_bool_time <- extract_string(var_time,"time_",2)
	mutate_bool_time <- paste0(var_bool_time," | !is.na(",var_time,")")
	names(mutate_bool_time) <- var_bool_time
	call_mutate <- parse(text=sprintf("mutate(df_episode,%s)",paste(paste(names(mutate_bool_time),mutate_bool_time,sep="="),collapse=",")))
	df_episode <-  eval(call_mutate)

	# logical variable *_no should be consistent
	# browser()
	# print(var_bool)
	mutate_bool <- NULL
	for(x in c("visit_","phone_","med_")){

		var_no <- paste0(x,"no")
		var <- setdiff(grep(x, var_bool,value=TRUE),var_no)
		# print(var)
		mutate_var <- paste0("(",paste0(var,collapse=" | "),")")
		# print(mutate_var)
		names(mutate_var) <- var_no
		mutate_bool <- c(mutate_bool,mutate_var)
	}
	# call_filter <- parse(text=sprintf("filter(df_episode,%s)",paste(paste(names(mutate_bool),mutate_bool,sep="=="),collapse=" | ")))
	# df <- eval(call_filter)	
	call_mutate <- parse(text=sprintf("mutate(df_episode,%s)",paste(paste(names(mutate_bool),mutate_bool,sep="=!"),collapse=",")))
	# print(call_mutate)
	df_episode <-  eval(call_mutate)


	flunet$surveys$episode <- df_episode

	return(flunet)
}




