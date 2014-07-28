#'Compute empirical distributions
#'
#'These functions compute several empirical distributions: 
#'\itemize{
#'	\item \code{empirical_distribution_symptom_duration} returns the empirical distribution of the time between symptom start and symptom end dates.
#'	\item \code{empirical_distribution_time_to_report} returns the empirical distribution of the time between the symptom start or end dates (as specified by the \code{what} argument) and the report date.
#'}
#'These distribution are computed from a subset of the \code{episode} survey. If this survey is not present, it will be created from the weekly survey using the function \code{\link{summarize_episode}}.
#'Subsetting is made via the \code{subset} and/or \code{remove_warnings} arguments. See argument description below.
#'Finally, covariates to be returned are specified with \code{covariates}. See example below.
#' @param  subset character. Logical expression indicating reports to keep: missing values are taken as false. If present, only episode of illness that verify \code{subset} will be processed. This is mainly to set conditions on the warnings to remove unreliable episodes. See \code{remove_warnings} for an alternative way to subset the weekly survey.
#' @param  remove_warnings character vector. Names of the warnings to exclude. 
#' @param  covariates character vector of covariates to keep.
#' @inheritParams summarize_symptoms
#' @name empirical distribution
#' @aliases empirical_distribution_symptom_duration
#' @export
#' @import dplyr
#' @return A \code{\link{flunet}} object with the empirical distributions stored as list of 3 elements in \code{flunet$log$empirical_distributions}. Each list contains
#'\itemize{
#'	\item \code{subset} the logical condition used for subsetting the \code{episode} survey. Made from \code{subset} and \code{remove_warnings} arguments.
#'	\item \code{covariates} copy of the \code{covariates} argument.
#'	\item \code{distribution} a \code{data.frame} containing the empirical distribution.
#'}
#' @examples \dontrun{
#' # symptom duration
#' bad_warnings <- c("W_S_start_too_far", "W_S_start_before_previous_report", "W_S_start_after_S_end", "W_S_start_wrong","W_S_end_too_far", "W_S_end_before_previous_report", "W_S_end_wrong")		
#' covariates <- "symptom_severity"
#' flunet <- empirical_distribution_symptom_duration(flunet, remove_warnings=bad_warnings, covariates=covariates)
#' # time to report symptom start
#' bad_warnings <- c("W_S_start_too_far", "W_S_start_before_previous_report", "W_S_start_after_S_end", "W_S_start_wrong")
#' covariates <- c("symptom_severity","first_report_date")
#' flunet <- empirical_distribution_time_to_report(flunet, what="symptom_start", remove_warnings=bad_warnings, covariates=covariates)
#' # symptom duration
#' bad_warnings <- c("W_S_end_too_far", "W_S_end_before_previous_report", "W_S_start_after_S_end", "W_S_end_wrong")
#' covariates <- c("symptom_severity","last_report_date")
#' flunet <- empirical_distribution_time_to_report(flunet, what="symptom_end", remove_warnings=bad_warnings, covariates=covariates)
#'}
empirical_distribution_symptom_duration <- function(flunet, subset=NULL, remove_warnings=NULL, covariates=NULL) {

	if(is_survey_present(flunet,survey="episode",warning_message=paste(sQuote("summarize_episode"),"is run first"))){
		df_episode <- flunet$surveys$episode
	} else {
		flunet <- summarize_episode(flunet)
		df_episode <- flunet$surveys$episode
	}

	# edit subset with remove_warnings
	if(!is.null(remove_warnings)){
		subset_warnings <- paste0("!(",paste(remove_warnings,collapse=" | "),")")
		subset <- paste(subset,subset_warnings,collapse=" & ")
	}

	# keep only bout with valid date of symptom onset and end and within subset
	df_subset <- filter(df_episode,(!is.na(symptom_start) & !is.na(symptom_end) & eval(parse(text=subset),df_episode))) %>% mutate(symptom_duration=symptom_end - symptom_start)
	
	# keep only interesting variables
	df_subset <- df_subset[c("person_id","n_bout",intersect(names(df_subset),covariates),"symptom_duration")]

	flunet$log$empirical_distributions$symptom_duration <- list(subset=subset,covariates=covariates,distribution=df_subset)

	return(flunet)
}


#' @name empirical distribution
#' @param  what character. Specify what date is reported.
#' @export
#' @importFrom plyr rename
#' @aliases empirical_distribution_time_to_report
empirical_distribution_time_to_report <- function(flunet, what=c("symptom_start","symptom_end"), subset=NULL, remove_warnings=NULL, covariates=NULL) {

	what <- match.arg(what)

	if(is_survey_present(flunet,survey="episode",warning_message=paste(sQuote("summarize_episode"),"is run first"))){
		df_episode <- flunet$surveys$episode
	} else {
		flunet <- summarize_episode(flunet)
		df_episode <- flunet$surveys$episode
	}

	# edit subset with remove_warnings
	if(!is.null(remove_warnings)){
		subset_warnings <- paste0("!(",paste(remove_warnings,collapse=" | "),")")
		# subset_warnings <- paste(remove_warnings,collapse=" | ")
		subset <- paste(subset,subset_warnings,collapse=" & ")
	}

	# keep only bout with valid date of symptom onset and end and within subset
	parse_filter <- sprintf("!is.na(%s)",what)
	report_date <- switch (what, 
		symptom_start = "first_report_date",
		symptom_end = "last_report_date"
		)
	parse_mutate <- sprintf("time_to_report_%s = %s - %s",what,report_date,what)
	
	new_var <- paste("time_to_report",what,sep="_")

	call <- substitute(
		df_episode %>% 
		subset(!is.na(what) & eval(parse(text=subset))) %>% 
		mutate(toto = report_date - what) %>% 
		rename(c("toto"=new_var))
		,list(what=as.name(what),report_date=as.name(report_date),new_var=new_var)
		)
	df_subset <- eval(call)

	# keep only interesting variables
	df_subset <- df_subset[c("person_id","n_bout",intersect(names(df_subset),covariates),new_var)]

	list_dist <- list(list(subset=subset,covariates=covariates,distribution=df_subset))

	flunet$log$empirical_distributions[new_var] <- list_dist

	return(flunet)

}


#'Compute predicted distribution
#'
#'This function use a linear mixed effect (lme) model to adjust the symptom duration to several covariates (specified by the \code{form} argument) and returns the fitted distribution (using predictive confidence intervals) for each combination of the covariate levels.
#' @param  remove_outliers logical, if \code{TRUE} then a first lme is run and used to eliminate the outliers before running a second lme. Default to \code{FALSE}.
#' @inheritParams summarize_symptoms
#' @inheritParams lme_boxcox
#' @note The symptom duration is normalised using a boxcox transformation. 
#' @export
#' @importFrom car bcPower
#' @importFrom plyr ddply
#' @seealso \code{\link{lme_boxcox}}
#' @return toto
predicted_distribution_symptom_duration <- function(flunet, form="symptom_duration~symptom_severity+age_group", heteroscedasticity="age_group", range_response=c(0, 20), remove_outliers= FALSE, plot = FALSE) {

	# check empirical distribution
	if(is.null(flunet$log$empirical_distributions$symptom_duration$distribution)){
		stop("Empirical distribution need to be computed first. Have a look at the function ",sQuote("empirical_distribution_symptom_duration"), call.=FALSE)
	}

	df_intake <- flunet$surveys$intake
	if(nrow(df_intake)!=n_distinct(df_intake$person_id)){
		# TODO: roll up (ask Seb), think how to deal with that in nlme
		warning("Force one profile per participant using the function ",sQuote("resolve_multiple_profiles"), call.=FALSE)
		flunet <- resolve_multiple_profiles(flunet)
		df_intake <- flunet$surveys$intake
	}

	# bind with intake survey
	offset <- 0.1
	df_dist <- flunet$log$empirical_distributions$symptom_duration$distribution
	df_reg <- inner_join(df_intake,df_dist,by="person_id") %>% mutate(symptom_duration=as.numeric(symptom_duration)+offset, person_id=as.numeric(as.factor(person_id)), symptom_severity=factor(symptom_severity, ordered=FALSE))

	ans <- lme_boxcox(df_reg, form=form, heteroscedasticity=heteroscedasticity, range_response=range_response, predict_all = TRUE, norm_test=TRUE, plot = FALSE)

	if(remove_outliers){
		df_reg <- ans$data %>% filter(pearson_residuals<2)
		ans <- lme_boxcox(df_reg, form=form, heteroscedasticity=heteroscedasticity, range_response=range_response, predict_all = TRUE, norm_test=TRUE, plot = FALSE)
	}
	
	# predicted distribution
	my_formula <- as.formula(form)
	response <- as.character(my_formula[[2]])
	explanatory <- setdiff(all.vars(my_formula), response)

	x <- 0:max(ans$data$symptom_duration - offset)
	x_trans_up <- bcPower(x+offset+1, ans$bc_coef)
	x_trans <- bcPower(x+offset, ans$bc_coef)

	df_dist <- ddply(ans$prediction,explanatory,function(df){	
		pred <- bcPower(df$prediction, ans$bc_coef)
		px <- pnorm((x_trans_up-pred)/df$SE2)-pnorm((x_trans-pred)/df$SE2)
		return(data.frame(symptom_duration=x,freq=px/sum(px)))
	})

	# plot distribution
	gdf <- df_dist %>% mutate(symptom_severity=parsed_names(symptom_severity),age_group=parsed_names(age_group))
	p <- ggplot(gdf)+facet_grid(age_group~symptom_severity,labeller=label_parsed)
	p <- p + geom_bar(aes(x=symptom_duration,y=freq),stat="identity")
	p <- p + ylab("Probability") + xlab("Symptom duration (in days)")
	p <- p + theme_bw()
	if(plot){
		print(p)		
	}

	return(list(reg=ans,dist=df_dist,plot=p))

}