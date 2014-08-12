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

	# ordered variables need to be reset after each rbind_fill() and do()
	var_ordered <- get_ordered_variables(df_intake)

	# warning
	df_warning <- data.frame(name="W_sampled_baseline_health_score",description="baseline health score was sampled",stringsAsFactors=FALSE)
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

	df_2sample <- inner_join(df_missing_baseline,df_max_health_score,by="person_id")
	df_missing_baseline_no_max <- anti_join(df_missing_baseline,df_2sample,by=names(df_missing_baseline))

	# sample baseline
	df_sampled <- 	df_2sample %>% group_by(person_id) %>% do(sample_one_baseline_health_score(.,df_dist)) %>% set_ordered_variables(var_ordered)
	df_sampled$max_health_score <- NULL
	df_sampled[[df_warning$name]] <- TRUE

	# bind
	df_intake <- rbind_list(df_keep,df_sampled,df_missing_baseline_no_max) %>% arrange(comp_time) %>% set_ordered_variables(var_ordered)

	flunet$surveys$intake <- df_intake

	# log
	df_warning$freq <- sum(df_intake[[df_warning$name]])
	flunet$log$missing_data$baseline_health_score <- list(sampled=df_sampled,dist=df_dist,warnings=df_warning)

	return(flunet)
}


sample_one_missing_start_date_using_time_to_report <- function(df_one,df_dist,warning) {

	# df_one contain the profile of one participant + one episode
	# df_dist_one contains all distribution, need to be restricted to df_one covariates

	covariates <- setdiff(names(df_dist),c("person_id","n_bout","first_report_date","time_to_report_symptom_start"))

	filter_covariate <- paste(covariates,paste0("df_one$",covariates),sep="==",collapse=" & ")
	# and make sure symptom duration is not greater than symptom_end
	call_filter <- parse(text=sprintf("subset(df_dist,%s & wday(first_report_date) == wday(df_one$first_report_date) & time_to_report_symptom_start < df_one$max_time_to_report_start_date)",filter_covariate))	
	df_dist_one <- eval(call_filter)

	#	
	if(nrow(df_dist_one)){
		time_to_report <- ifelse(nrow(df_dist_one) > 1, sample(df_dist_one$time_to_report_symptom_start, 1), df_dist_one$time_to_report_symptom_start)
		df_one$symptom_start <- df_one$first_report_date - time_to_report		
		df_one[[warning]] <- TRUE
	} else {
		warning("no symptom start for: ",sQuote(df_one[c("person_id","n_bout")]),call.=FALSE)
	}


	return(df_one)
}

sample_one_missing_date_using_symptom_duration <- function(df_one,df_dist,type=c("start","end"),warning) {

	type <- match.arg(type)

	# df_one contain the profile of one participant + one episode
	# df_dist contains all distribution, need to be restricted to df_one covariates

	covariates <- setdiff(names(df_dist),c("symptom_duration","freq"))

	filter_covariate <- paste(covariates,paste0("df_one$",covariates),sep="==",collapse=" & ")
	# and make sure symptom duration is not greater than symptom_end
	call_filter <- parse(text=sprintf("subset(df_dist,%s & symptom_duration >= df_one$min_symptom_duration & symptom_duration <= df_one$max_symptom_duration)",filter_covariate))	
	df_dist_one <- eval(call_filter)

	#	
	if(nrow(df_dist_one)){
		symptom_duration <- ifelse(nrow(df_dist_one) > 1, sample(df_dist_one$symptom_duration, 1, prob = df_dist_one$freq), df_dist_one$symptom_duration)
		
		if(type=="start"){
			df_one$symptom_start <- df_one$symptom_end - symptom_duration			
		} else if(type=="end") {
			df_one$symptom_end <- df_one$symptom_start + symptom_duration
		}
		
		df_one[[warning]] <- TRUE
	} else {
		warning("minimal symptom duration is out of the bound of the distribution: ",df_one$min_symptom_duration,call.=FALSE)
	}

	return(df_one)
}

#'Sample missing symptom start/end dates
#'
#'This function sample missing symptom start and end dates using a distribution of the duration of symptoms
#' @param  symptom_duration distribution
#' @param  time_to_report distributions
#' @param  sample_warning_start vector
#' @param  sample_warning_end vector
#' @inheritParams summarize_symptoms
#' @note TODO
#' @export
#' @import dplyr
sample_missing_symptom_start_end_dates <- function(flunet, symptom_duration, time_to_report=NULL, sample_warning_start=NULL, sample_warning_end=NULL) {

	df_dist_symptom_duration <- symptom_duration
	df_dist_time_to_report <- time_to_report

	if(is_survey_present(flunet,survey=c("intake","weekly","episode"),warning_message="=> missing dates won't be sampled")){
		df_intake <- flunet$surveys$intake
		df_weekly <- flunet$surveys$weekly
		df_episode <- flunet$surveys$episode
		# join intake and episode for covariates
		df_episode_intake <- inner_join(df_episode,df_intake,by="person_id")
	} else {
		return(flunet)
	}

	# ordered variables need to be reset after each rbind_fill() and do()
	var_ordered <- get_ordered_variables(df_episode_intake)

	# warning
	df_warning <- data.frame(name=c(WSSSFSD="W_sampled_S_start_from_S_duration",WSSSFTTR="W_sampled_S_start_from_time_to_report",WSSEFSD="W_sampled_S_end_from_S_duration"),description=c("symptom start date was sampled from symptom duration distribution","symptom start date was sampled from time to report","symptom end date was sampled from symptom duration distribution"),stringsAsFactors=FALSE)
	df_episode_intake[df_warning$name] <- FALSE

	# covariates 
	covariates <- setdiff(names(df_dist_symptom_duration),c("symptom_duration","freq"))

	# test that df_episode_intake and df_dist_symptom_duration have the same levels for the covariates
	levels_episode <- lapply(df_episode_intake[covariates],function(x) {levels(as.factor(x))})
	levels_dist <- lapply(df_dist_symptom_duration[covariates],function(x) {levels(as.factor(x))})
	if(!all(x <- unlist(Map(setequal,x=levels_episode,y=levels_dist)))){
		stop("Levels of covariate(s) ",sQuote(names(x[!x])), " in the intake survey don't match the ones of the predicted distribution.",call.=FALSE)
	}

	# select episodes without (or with a warned) symptom_start
	# call_filter <- parse(text=sprintf("filter(df_episode_intake, (is.na(symptom_start) | %s) & %s)",paste(sample_warning_start,collapse=" | "),paste(sprintf("!is.na(%s)",covariates),collapse=" & ")))
	call_filter <- parse(text=sprintf("filter(df_episode_intake, is.na(symptom_start) | %s)",paste(sample_warning_start,collapse=" | ")))
	df_missing_start <- eval(call_filter)
	df_keep <- anti_join(df_episode_intake,df_missing_start,by=names(df_episode_intake))
	
	# for each episode, find the date of previous report
	df_missing_start <- select(df_missing_start,person_id,n_bout,first_report_date) %>% 
	inner_join(.,df_weekly %>% select(person_id,report_date), by="person_id") %>% 
	mutate(delta=as.numeric(first_report_date-report_date)) %>% 
	filter(delta>0) %>% 
	group_by(person_id, n_bout) %>% 
	summarize(previous_report_date=max(report_date)) %>% 
	left_join(df_missing_start,.,by=c("person_id", "n_bout"))
	

	# select those with a valid symptom end, all covariates available and compute min and max symptom durations
	call_filter <- parse(text=sprintf("filter(df_missing_start, !is.na(symptom_end) & !(%s) & %s)",paste(sample_warning_end,collapse=" | "),paste(sprintf("!is.na(%s)",covariates),collapse=" & ")))
	df_missing_start_valid_end <- eval(call_filter) %>% mutate(min_symptom_duration=as.numeric(symptom_end-first_report_date), max_symptom_duration=as.numeric(symptom_end-previous_report_date))

	# if no previous report date, put max = Inf
	tmp <- df_missing_start_valid_end$max_symptom_duration
	df_missing_start_valid_end$max_symptom_duration[is.na(tmp)] <- Inf

	my_warning <- df_warning["WSSSFSD","name"]

	df_sampled_start_valid_end <- df_missing_start_valid_end %>% group_by(person_id,n_bout) %>% do(sample_one_missing_date_using_symptom_duration(.,df_dist_symptom_duration,"start",my_warning)) %>% set_ordered_variables(var_ordered)

	# look at all those with missing start dates but no valid end date or not all covariates
	df_missing_start_keep <- anti_join(df_missing_start,df_missing_start_valid_end,by=names(df_missing_start))

	# if time_to_report is available then use it to sample start dates
	if(!is.null(time_to_report)){
		# sample missing start dates that have no valid end dates or missing covariates and compute max report date
		df_missing_start_missing_end <- df_missing_start_keep %>% mutate(max_time_to_report_start_date=as.numeric(first_report_date-previous_report_date))
		
		# if no previous report date, put max = Inf
		tmp <- df_missing_start_missing_end$max_time_to_report_start_date
		df_missing_start_missing_end$max_time_to_report_start_date[is.na(tmp)] <- Inf
		
		# remove episodes that were reported after recovery
		df_not_sampled <- filter(df_missing_start_missing_end,length_bout==1 & (!still_ill | (!is.na(symptom_end) & symptom_end<first_report_date)))
		
		df_missing_start_missing_end <- anti_join(df_missing_start_missing_end, df_not_sampled, by=names(df_missing_start_missing_end))

		my_warning <- df_warning["WSSSFTTR","name"]
		# sample missing start date using empirical distribution of time to report
		# need to reorder variables after do and rbind_list
		df_sampled_start_missing_end <-  group_by(df_missing_start_missing_end, person_id,n_bout) %>% 
		do(sample_one_missing_start_date_using_time_to_report(.,df_dist_time_to_report,my_warning)) %>% 
		set_ordered_variables(var_ordered)

		df_missing_start_keep <- rbind_list(df_sampled_start_missing_end, df_not_sampled) %>% set_ordered_variables(var_ordered)
	}

	var_ordered <- get_ordered_variables(df_sampled_start_valid_end)
	df_episode_intake <- rbind_list(df_sampled_start_valid_end, df_missing_start_keep, df_keep) %>% arrange(first_report_date) %>% set_ordered_variables(var_ordered)
	# which(sapply(df_sampled_start_valid_end,is.ordered))


	df_episode_intake <- df_episode_intake[setdiff(names(df_episode_intake),c("min_symptom_duration","max_symptom_duration","max_time_to_report_start_date","previous_report_date"))]

	# select episodes without (or with a warned) symptom_end, a valid or sampled start dates and all covariates
	call_filter <- parse(text=sprintf("filter(df_episode_intake, (is.na(symptom_end) | %s) & (!is.na(symptom_start) & (!(%s) | %s)) & %s)", paste(sample_warning_end, collapse=" | "), paste(sample_warning_start,collapse=" | "), paste(df_warning[c("WSSSFSD","WSSSFTTR"),"name"],collapse=" | "), paste(sprintf("!is.na(%s)",covariates),collapse=" & ")))
	df_missing_end <- eval(call_filter)
	df_keep <- anti_join(df_episode_intake,df_missing_end,by=names(df_episode_intake))
	
	# if W_S_start_after_S_end is in warning end and warning start, if start has been sampled, if there is no more conflict between dates and not any warning for end => do not sample end.
	if("W_S_start_after_S_end"%in%sample_warning_start & "W_S_start_after_S_end"%in%sample_warning_end){
		call_filter <- parse(text=sprintf("filter(df_missing_end, (%s) & !is.na(symptom_end) & symptom_start <= symptom_end & !(%s))", paste(df_warning[c("WSSSFSD","WSSSFTTR"),"name"],collapse=" | "), paste(setdiff(sample_warning_end,"W_S_start_after_S_end"), collapse=" | ")))
		df_not_sample <- eval(call_filter)
		df_keep <- rbind_list(df_keep,df_not_sample) %>% set_ordered_variables(var_ordered)
		df_missing_end <- anti_join(df_missing_end,df_not_sample,by=names(df_missing_end))
	}

	# participants that are still off are considered still ill if no information
	df_missing_end_still_ill <- df_missing_end %>% filter(still_ill | (is.na(still_ill) & still_off=="yes")) 
	df_missing_end_not_still_ill <- anti_join(df_missing_end,df_missing_end_still_ill,by=names(df_missing_end))

	# for each episode with still ill at the end, find the date of next report after last report (max date of end of symptoms)
	df_missing_end_still_ill <- df_missing_end_still_ill %>% 
	select(person_id, n_bout, last_report_date) %>% 
	inner_join(.,df_weekly %>% select(person_id,report_date), by="person_id") %>% 
	mutate(delta=as.numeric(report_date-last_report_date)) %>% 
	filter(delta>0) %>% 
	group_by(person_id, n_bout) %>% 
	summarize(next_report_date=min(report_date)) %>% 
	left_join(df_missing_end_still_ill,.,by=c("person_id", "n_bout")) %>% 
	mutate(min_symptom_duration=as.numeric(last_report_date - symptom_start), max_symptom_duration=as.numeric(next_report_date - symptom_start))

	# if no next report date, put max = Inf
	tmp <- df_missing_end_still_ill$max_symptom_duration
	df_missing_end_still_ill$max_symptom_duration[is.na(tmp)] <- Inf

	# for each episode that recovers at the end, find the date of report just before last report (min date of end of symptoms)
	df_missing_end_not_still_ill <- df_missing_end_not_still_ill %>% 
	select(person_id, n_bout, last_report_date) %>% 
	inner_join(.,df_weekly %>% select(person_id,report_date), by="person_id") %>% 
	mutate(delta=as.numeric(last_report_date - report_date)) %>% 
	filter(delta>0) %>% 
	group_by(person_id, n_bout) %>% 
	summarize(previous_report_date=max(report_date)) %>% 
	left_join(df_missing_end_not_still_ill,.,by=c("person_id", "n_bout")) %>% 
	mutate(min_symptom_duration=as.numeric(previous_report_date - symptom_start), max_symptom_duration=as.numeric(last_report_date - symptom_start))

	tmp <- df_missing_end_not_still_ill$min_symptom_duration
	df_missing_end_not_still_ill$min_symptom_duration[is.na(tmp) | tmp < 0] <- 0

	# bind both 
	df_missing_end <- rbind_list(df_missing_end_not_still_ill, df_missing_end_still_ill) %>% arrange(person_id, first_report_date) %>% set_ordered_variables(var_ordered)
	
	my_warning <- df_warning["WSSEFSD","name"]
	df_sampled_end <- df_missing_end %>% group_by(person_id,n_bout) %>% 
	do(sample_one_missing_date_using_symptom_duration(.,df_dist_symptom_duration,"end",my_warning)) %>% 
	set_ordered_variables(var_ordered)

	# bind
	df_episode_intake <- rbind_list(df_keep,df_sampled_end) %>% arrange(comp_time) %>% set_ordered_variables(var_ordered)

	flunet$surveys$episode <- df_episode_intake[c(names(df_episode),df_warning$name)]
	
	# log
	df_warning$freq <- sapply(df_episode_intake[df_warning$name],sum)
	
	flunet$log$missing_data$symptom_start_end_dates <- list(dist=list(symptom_duration=symptom_duration, time_to_report=time_to_report), sample_warning_date=list(start=sample_warning_start, end=sample_warning_end),warnings=df_warning)

	return(flunet)
}








