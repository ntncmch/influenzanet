sample_one_baseline_health_score <- function(df_one,df_dist) {

	# df_one contain the profile of one participant + max of its health-score
	# df_dist_one contains all distribution, need to be restricted to df_one covariates

	covariates <- setdiff(names(df_dist),c("baseline_health_score","freq"))
	filter_covariate <- paste(covariates,paste0("df_one$",covariates),sep="==",collapse=" & ")
	call_filter <- parse(text=sprintf("subset(df_dist,%s & baseline_health_score >= df_one$max_health_score)",filter_covariate))	
	df_dist_one <- eval(call_filter)

	#	
	if(nrow(df_dist_one)){
		baseline_health_score <- ifelse(nrow(df_dist_one) > 1, sample(df_dist_one$baseline_health_score, 1, prob = df_dist_one$freq), df_dist_one$baseline_health_score)
	} else {
		warning("baseline at 100 for max(health)=",df_one$max_health_score,call.=FALSE)
		baseline_health_score <- 100
	}

	df_one$baseline_health_score <- baseline_health_score

	return(df_one)
}


#'Sample missing data
#'
#'These functions sample missing data values from a user provided distribution: 
#'\itemize{
#'	\item \code{sample_missing_baseline_health_score} samples a baseline health-score from the distribution truncated to the highest "ill" health-score.
#'}
#' If the distribution is conditioned on some covariates, the sampling is conditioned on the same covariates. Example of such distributions are the predicted distributions (see links below).
#' @param df_dist a \code{data.frame} containing the sampling distribution. Must contain at least two columns, one for the value of the sampled variable (with the same name as in \code{flunet} object) and the other for the probability of this value (with names \code{freq}). 
#' @inheritParams summarize_symptoms
#' @export
#' @import dplyr
#' @seealso \code{\link{predicted_distribution_symptom_duration}}, \code{\link{predicted_distribution_baseline_healthscore}}, \code{\link{empirical_distribution_symptom_duration}}, \code{\link{empirical_distribution_time_to_report}}
#' @return A \code{\link{flunet}} object with missing value replaced by sampled ones in the appropriate surveys. A warning column is added to the survey and flagged to \code{TRUE} whenever there is a sampled value. 
#' @name sample missing data
#' @aliases sample_missing_baseline_health_score
sample_missing_baseline_health_score <- function(flunet, df_dist) {

	if(is_survey_present(flunet,survey=c("intake","weekly"),warning_message="=> missing baseline health score won't be sampled")){
		df_intake <- flunet$surveys$intake
		df_weekly <- flunet$surveys$weekly
	} else {
		return(flunet)
	}

	if(is.null(df_intake$baseline_health_score)){
		stop("No baseline health-score variable in the intake survey. First run the function ",sQuote("summarize_baseline_health_score"), call.=FALSE)
	}

	# warning
	df_warning <- data.frame(name="W_sampled_baseline_health_score",description="baseline health score ",stringsAsFactors=FALSE)
	df_intake[[df_warning$name]] <- FALSE

	covariates <- setdiff(names(df_dist),c("baseline_health_score","freq"))

	# test that df_intake and df_dist have the same levels for the covariates
	levels_intake <- lapply(df_intake[covariates],function(x) {levels(as.factor(x))})
	levels_dist <- lapply(df_dist[covariates],function(x) {levels(as.factor(x))})
	if(!all(x <- unlist(Map(setequal,x=levels_intake,y=levels_dist)))){
		stop("Levels of covariate(s) ",sQuote(names(x[!x])), " in the intake survey don't match the ones of the predicted distribution.",call.=FALSE)
	}

	# select participants without baseline and all covariates available
	call_filter <- parse(text=sprintf("filter(df_intake, is.na(baseline_health_score) & %s)",paste(sprintf("!is.na(%s)",covariates),collapse=" & ")))
	df_missing_baseline <- eval(call_filter)
	df_keep <- anti_join(df_intake,df_missing_baseline,by=names(df_intake))
	# select participants with at least one health-score available and compute their max health_score
	df_max_health_score <- semi_join(df_weekly,df_missing_baseline,by="person_id") %>% 
	filter(!is.na(health_score)) %>% 
	group_by(person_id) %>% 
	summarize(max_health_score=max_na(health_score,na_rm=TRUE))

	df_missing_baseline <- inner_join(df_missing_baseline,df_max_health_score,by="person_id")

	# sample baseline
	df_sampled_baseline <- 	df_missing_baseline %>% group_by(person_id) %>% do(sample_one_baseline_health_score(.,df_dist))
	df_sampled_baseline$max_health_score <- NULL
	df_sampled_baseline[[df_warning$name]] <- TRUE

	# bind
	df_intake <- rbind_list(df_keep,df_sampled_baseline) %>% arrange(comp_time)

	flunet$surveys$intake <- df_intake

	# log
	df_warning$freq <- sum(df_intake[[df_warning$name]])
	flunet$log$missing_data$baseline_health_score <- list(sampled=df_sampled_baseline,dist=df_dist,warnings=df_warning)

	return(flunet)
}









