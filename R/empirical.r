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


# time to report vs wday for S_start, S_end. dateuration of symptom (by age and type of symptom). baseline heatl score.
compute_empirical_distribution_for_missing_data <- function(
	df_data, 
	df_profile,
	symptom_ordered = c("ARI_ecdc", "ILI_ecdc", "ILI_fever"),
	W_S_start = c("W_S_start_too_far", "W_S_start_before_previous_report", "W_S_start_after_S_end", "W_S_start_wrong"),
	W_S_end = c("W_S_end_too_far", "W_S_end_before_previous_report", "W_S_start_after_S_end", "W_S_end_wrong"),
	S_duration_predictors=c("symptom","age_group"),
	S_duration_max=30,
	plot_check = FALSE,
	dir_plot = "./pdf/data_processing",
	remove_outlier=FALSE,
	fun_summary_baseline=fun_summary_baseline,
	...) {


	symptom_ordered <- ordered(symptom_ordered)

	########
	#time between first report and date of symptom onset, by week-day of report
	tmp <- subset(df_data, !is.na(symptom_start) & position_bout == 1)
	ind <- which(apply(tmp[, W_S_start], 1, any))
	df_data_S_start_ok <- mutate(tmp[-ind, ], wday_report = wday(report_date, label = TRUE,abbr=FALSE), variable = "time_to_report_S_start", value = as.numeric(report_date - symptom_start))
	df_time_to_report <- df_data_S_start_ok[c("person_id", "wday_report", "variable", "value")]

	#time between last report and date of symptom end, by wday of report
	tmp <- subset(df_data, !is.na(symptom_end) & position_bout == length_bout)
	ind <- which(apply(tmp[, W_S_end], 1, any))
	df_data_S_end_ok <- mutate(tmp[-ind, ], wday_report = wday(report_date, label = TRUE,abbr=FALSE), variable = "time_to_report_S_end", value = as.numeric(report_date - symptom_end))
	df_time_to_report <- rbind(df_time_to_report, df_data_S_end_ok[c("person_id", "wday_report", "variable", "value")])

	#smoothing
	df_dist_time_to_report_smooth <- ddply(df_time_to_report,c("wday_report", "variable"),function(df){
		x <- df$value
		dx <- density(x,from=0,to=max(x),n=max(x)+1,...)
		dx$y <- dx$y/sum(dx$y)
		return(data.frame(value=dx$x,freq=dx$y*nrow(df)))
	},.progress="text")
	df_dist_time_to_report_smooth$type <- "density"

	#empirical distribution:
	df_dist_time_to_report_emp <- count(df_time_to_report, vars = c("wday_report", "variable", "value"))
	df_dist_time_to_report_emp$type <- "empirical"

	#interpolated distribution
	df_dist_time_to_report_interp <- ddply(df_dist_time_to_report_emp,c("wday_report", "variable"),function(df){
		x <- df$value
		y <- df$freq
		tmp <- approx(x,y,xout=0:max(x))
		df2 <- data.frame(value=tmp$x,freq=tmp$y)
		return(df2)
	},.progress="text")
	df_dist_time_to_report_interp$type <- "interpolate"

	df_dist_time_to_report <- do.call("rbind",list(df_dist_time_to_report_emp, df_dist_time_to_report_smooth, df_dist_time_to_report_interp))

	##########
	#compute empirical distribution of the duration of symptoms
	df_S_start_end_ok <- join(df_data_S_start_ok[c("person_id", "n_bout", "symptom_start")], df_data_S_end_ok[c("person_id", "n_bout", "symptom_end")], by = c("person_id", "n_bout"), type = "inner")
	df_S_start_end_ok <- mutate(df_S_start_end_ok, S_duration = as.numeric(symptom_end - symptom_start))

	#find type of symptom of each episode
	var_symptom <- as.character(symptom_ordered)
	df_data_match <- match_df(df_data, df_S_start_end_ok, on = c("person_id", "n_bout"))

	df_S_start_end_ok <- ddply(df_S_start_end_ok, c("person_id", "n_bout"), function(df) {
		tmp <- match_df(df_data_match, df, on = c("person_id", "n_bout"))
		df[var_symptom] <- vapply(tmp[, var_symptom], function(x) any_na_rm(x), FUN.VALUE = c(TRUE))
		return(df)
	}, .progress = "text")

	df_S_start_end_ok$symptom <- ordered(apply(df_S_start_end_ok[, var_symptom], 1, function(x) max(symptom_ordered[as.logical(x)])))
	df_S_start_end_ok <- df_S_start_end_ok[setdiff(names(df_S_start_end_ok), var_symptom)]

	df_S_duration <- df_S_start_end_ok[c("person_id","n_bout", "symptom", "S_duration")]
	df_S_duration <- subset(df_S_duration, S_duration < S_duration_max)
	df_S_duration <- join(df_profile,df_S_duration,type="right",by="person_id")

	df_reg <- mutate(df_S_duration,S_duration2=S_duration+0.1,person_id_num=as.numeric(as.factor(person_id)),smoke_bool=smoke_as_logical(smoke))


	if(0){
		df_outlier <- lme_regression(df_reg, formula = "S_duration2~symptom+age_group+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 20, test_dist = FALSE, CI_interval="confidence",suffix= paste0("full_info_SDmax=",S_duration_max),plot_outlier_TS=FALSE,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group",use_facet_grid=TRUE,facet_formula="gender~smoke_bool") 
		df_remove <- match_df(df_reg, df_outlier,on=c("person_id","n_bout"))
		df_reg_wo <- diff_df(df_reg, df_remove)
		df_outlier_wo <- lme_regression(df_reg_wo, formula = "S_duration2~symptom+age_group+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 20, test_dist = FALSE, CI_interval="confidence",suffix= "full_info_wo",plot_outlier_TS=FALSE,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group",use_facet_grid=TRUE,facet_formula="gender~smoke_bool") 
		df_outlier_wo_2 <- lme_regression(df_reg_wo, formula = "S_duration2~symptom+age_group+smoke_bool+is_risk+gender",transformation = "boxcox", max_response = 20, test_dist = FALSE, CI_interval="confidence",suffix= "full_info_wo",plot_outlier_TS=FALSE,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group",use_facet_grid=TRUE,facet_formula="gender~smoke_bool") 
		df_outlier_wo_3 <- lme_regression(df_reg_wo, formula = "S_duration2~symptom+age_group+smoke_bool+gender",transformation = "boxcox", max_response = 20, test_dist = FALSE, CI_interval="confidence",suffix= "full_info_wo",plot_outlier_TS=FALSE,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group",use_facet_grid=TRUE,facet_formula="gender~smoke_bool") 
		df_outlier_wo_4 <- lme_regression(df_reg_wo, formula = "S_duration2~symptom+age_group+smoke_bool",transformation = "boxcox", max_response = 20, test_dist = FALSE, CI_interval="confidence",suffix= "full_info_wo",plot_outlier_TS=FALSE,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group",use_facet_wrap=TRUE,facet_formula="~smoke_bool") 
		df_outlier_wo_5 <- lme_regression(df_reg_wo, formula = "S_duration2~symptom+age_group",transformation = "boxcox", max_response = 20, test_dist = FALSE, CI_interval="prediction",suffix= "full_info_wo",plot_outlier_TS=FALSE,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group") 
	}
	df_reg_age <- mutate(df_reg,age_group2=revalue(age_group,c("0-17"="0-64","18-44"="0-64","45-64"="0-64")))
	#df_reg_wo_age <- mutate(df_reg_wo,age_group2=revalue(age_group,c("0-17"="0-64","18-44"="0-64","45-64"="0-64")))
	#df_outlier_wo_6 <- lme_regression(df_reg_wo_age, formula = "S_duration2~symptom+age_group2",transformation = "boxcox", max_response = 20, test_dist = TRUE, CI_interval="prediction",suffix= "full_info_wo",plot_outlier_TS=FALSE,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group2") 
	#tmp <- lme_regression(df_reg_wo_age, formula = "S_duration2~symptom+age_group2",transformation = "boxcox", max_response = 20, test_dist = TRUE, CI_interval="prediction",suffix= "full_info_wo",plot_outlier_TS=FALSE,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group2",return_outliers=FALSE) 
	tmp <- lme_regression(df_reg_age, formula = "S_duration2~symptom+age_group2",transformation = "boxcox", max_response = 20, test_dist = FALSE, CI_interval="prediction",suffix= paste0("full_info_SDmax=",S_duration_max),plot_outlier_TS=FALSE,df_data= df_data,df_profile=df_profile,x_var="symptom",x_lab="symptom severity",fill_var="age_group2",fill_lab="age group",y_lab="symptom duration (in days)",return_outliers=remove_outlier) 

	if(remove_outlier){
		df_remove <- match_df(df_reg_age, tmp,on=c("person_id","n_bout"))
		df_reg_age <- diff_df(df_reg_age, df_remove)
		tmp <- lme_regression(df_reg_age, formula = "S_duration2~symptom+age_group2",transformation = "boxcox", max_response = 20, test_dist = TRUE, CI_interval="prediction",suffix= paste0("full_info_SDmax=",S_duration_max,"_wo"),plot_outlier_TS=FALSE,df_data= df_data,df_profile=df_profile,x_var="symptom",x_lab="symptom severity",fill_var="age_group2",fill_lab="age group",y_lab="symptom duration (in days)",return_outliers=FALSE) 
	}

	#log transform
	if(tmp$coef_bc==0){
		df_dist_S_duration_bc <- ddply(tmp$prediction,c("symptom","age_group2"),function(df){
			x <- 0:max(df_reg_age$S_duration)
			px <- plnorm(x+0.1+1,meanlog=df$pred,sdlog=df$SE2)-plnorm(x+0.1,meanlog=df$pred,sdlog=df$SE2)
			return(data.frame(S_duration=x,freq=px))
		})

	}else{

		df_dist_S_duration_bc <- ddply(tmp$prediction,c("symptom","age_group2"),function(df){
			x <- 0:max(df_reg_age$S_duration)
			x_trans_up <- bcPower(x+0.1+1, tmp$coef_bc)
			x_trans <- bcPower(x+0.1, tmp$coef_bc)
			px <- pnorm((x_trans_up-df$pred)/df$SE2)-pnorm((x_trans-df$pred)/df$SE2)
			return(data.frame(S_duration=x,freq=px/sum(px)))
		})

	}

	df_dist_S_duration_bc$type <- "bc_regression"
	df_group <- arrange(na.omit(unique(df_reg_age[c("symptom","age_group","age_group2")])),symptom,age_group)	
	tmp <- join(df_group, df_dist_S_duration_bc)
	df_dist_S_duration_bc <- tmp
	#df_dist_S_duration_bc$age_group2 <- NULL


	df_S_duration <- na.omit(df_S_duration[c(S_duration_predictors,"S_duration")])
	#smoothing

	df_dist_S_duration_smooth <- ddply(df_S_duration,S_duration_predictors,function(df){
		x <- df$S_duration
		dx <- density(x,from=0,to=max(x),n=max(x)+1,...)
		dx$y <- dx$y/sum(dx$y)
		return(data.frame(S_duration=dx$x,freq=dx$y*nrow(df)))
	},.progress="text")
	df_dist_S_duration_smooth$type <- "density"

	#empirical distribution:
	df_dist_S_duration_emp <- count(df_S_duration, vars = c(S_duration_predictors, "S_duration"))
	df_dist_S_duration_emp$type <- "empirical"

	#interpolated distribution
	df_dist_S_duration_interp <- ddply(df_dist_S_duration_emp, S_duration_predictors,function(df){
		x <- df$S_duration
		y <- df$freq
		tmp <- approx(x,y,xout=0:max(x))
		df2 <- data.frame(S_duration=tmp$x,freq=tmp$y)
		return(df2)
	},.progress="text")
	df_dist_S_duration_interp$type <- "interpolate"

	df_dist_S_duration <- do.call("rbind",list(df_dist_S_duration_emp, df_dist_S_duration_smooth, df_dist_S_duration_interp))


	############
	# compute baseline health_score: focus on non ill reports unless symptom_end < report_date
	df_baseline <- compute_baseline_health_score(df_data,fun_summary_baseline)
	if(0){

		df_reg_baseline <- join(df_baseline,df_profile,by="person_id")
		#regression
		df_reg_baseline <- mutate(df_baseline,person_id_num=as.numeric(as.factor(person_id)),smoke_bool=smoke_as_logical(smoke))

		df_outlier <- lm_regression(df_reg_baseline, formula = "baseline_health_score~age_group+smoke_bool+is_risk+vaccine+gender",transformation = "logit", max_response = 100, test_dist = FALSE, CI_interval="confidence",suffix= "full_info",df_data= df_data,df_profile=df_profile,h_var="age_group",x_var="age_group",fill_var="gender",use_facet_grid=TRUE,facet_formula="is_risk~smoke_bool") 
		df_outlier2 <- lm_regression(df_reg_baseline, formula = "baseline_health_score~age_group+smoke_bool+is_risk+gender",transformation = "logit", max_response = 100, test_dist = TRUE, CI_interval="confidence",suffix= "full_info",df_data= df_data,df_profile=df_profile,h_var="age_group",x_var="is_risk",fill_var="smoke_bool",use_facet_grid=TRUE,facet_formula="age_group~gender") 
		df_outlier3 <- lm_regression(df_reg_baseline, formula = "baseline_health_score~age_group+smoke_bool+is_risk",transformation = "logit", max_response = 100, test_dist = TRUE, CI_interval="confidence",suffix= "full_info",df_data= df_data,df_profile=df_profile,h_var="age_group",x_var="is_risk",fill_var="smoke_bool",use_facet_wrap=TRUE,facet_formula="~age_group") 
		df_reg_baseline_age <- mutate(df_reg_baseline,age_group2=revalue(age_group,c("0-17"="0-64","18-44"="0-64","45-64"="0-64")))
		df_outlier4 <- lm_regression(df_reg_baseline_age, formula = "baseline_health_score~age_group2+smoke_bool+is_risk",transformation = "logit", max_response = 100, test_dist = TRUE, CI_interval="prediction",suffix= "full_info",df_data= df_data,df_profile=df_profile,h_var="age_group2",x_var="is_risk",fill_var="smoke_bool",use_facet_wrap=TRUE,facet_formula="~age_group2") 
	}

	df_reg_baseline <- join(df_baseline,df_profile,by="person_id")
	df_reg_baseline <- mutate(df_reg_baseline,person_id_num=as.numeric(as.factor(person_id)),age_group2=revalue(age_group,c("0-17"="0-64","18-44"="0-64","45-64"="0-64")),smoke_bool=smoke_as_logical(smoke))

	#remove baseline==100 and reinflate later
	# df_inflate <- na.omit(ddply(df_reg_baseline,c("smoke_bool","is_risk","age_group2"),function(df){

		# 	p_100 <- sum(df$baseline_health_score==100)/nrow(df)

		# 	return(data.frame(p_100=p_100))

		# }))

	df_reg_baseline <- subset(df_reg_baseline, baseline_health_score<100) 

	tmp <- lm_regression(df_reg_baseline, formula = "baseline_health_score~age_group2+smoke_bool+is_risk",transformation = "logit", max_response = 100, test_dist = TRUE, CI_interval="prediction",suffix= "full_info",df_data= df_data,df_profile=df_profile,h_var="age_group2",x_var="is_risk",fill_var="smoke_bool",use_facet_wrap=TRUE,facet_formula="~age_group2",return_outliers= remove_outlier) 
	if(remove_outlier){
		df_remove <- match_df(df_reg_baseline, tmp,on=c("person_id"))
		df_reg_baseline <- diff_df(df_reg_baseline, df_remove)
		tmp <- lm_regression(df_reg_baseline, formula = "baseline_health_score~age_group2+smoke_bool+is_risk",transformation = "logit", max_response = 100, test_dist = TRUE, CI_interval="prediction",suffix= "full_info_wo",df_data= df_data,df_profile=df_profile,h_var="age_group2",x_var="is_risk",fill_var="smoke_bool",use_facet_wrap=TRUE,facet_formula="~age_group2",return_outliers=FALSE) 
	}

#log transform
	df_dist_baseline_reg <- ddply(tmp$prediction,c("smoke_bool","is_risk","age_group2"),function(df){
	#df_100 <- match_df(df_inflate,df,on=c("smoke_bool","is_risk","age_group2"))
		x <-seq(floor(min(df_reg_baseline$baseline_health_score)),99.5,0.5)
		x <- x/100
		x_trans_up <- log(x)-log(1-x)
		x_trans <- x_trans_up[-length(x_trans_up)]
		x_trans_up <- x_trans_up[-1]
		px <- pnorm((x_trans_up-df$pred)/df$SE2)-pnorm((x_trans-df$pred)/df$SE2)
		x <- c(x[-1]*100)-0.25

	#x <- c(x[-1]*100,100)
	#px <- c(px/(1-df_100$p_100),df_100$p_100)
		return(data.frame(baseline_health_score= x,freq=px/sum(px)))
	})


	df_dist_baseline_reg$type <- "logit_regression"
	df_group <- arrange(na.omit(unique(df_reg_baseline[c("smoke_bool","is_risk","age_group","age_group2")])), smoke_bool, is_risk,age_group)	
	tmp <- join(df_group, df_dist_baseline_reg)
	df_dist_baseline_reg <- tmp

#####		

	if (plot_check) {

		if (!file.exists(dir_plot)) {
			dir.create(dir_plot)
		}

		df_dist_time_to_report$variable <- factor(df_dist_time_to_report$variable, levels = unique(df_dist_time_to_report$variable))
		df_hist <- subset(df_dist_time_to_report,type=="empirical")
		df_line <- subset(df_dist_time_to_report,type!="empirical")
		p <- ggplot(df_dist_time_to_report) + facet_grid(wday_report~variable,scales="free")
		p <- p+geom_bar(data= df_hist,aes(x=value,y=freq),stat="identity",alpha=0.5)
		p <- p+geom_area(data= df_line, aes(x=value,y=freq,fill=type),colour="black",alpha=0.5,position="identity")
		cairo_pdf(file.path(dir_plot, "time_to_report.pdf"), width = 6, height = 10)
		print(p)
		dev.off()

		p <- ggplot(df_time_to_report, aes(x = wday_report)) + facet_wrap(~variable)
		p <- p + geom_histogram()
		cairo_pdf(file.path(dir_plot, "wday_report.pdf"), width = 7, height = 3)
		print(p)
		dev.off()

		formu <- as.formula(paste(paste(c(S_duration_predictors,"S_duration"),collapse="+"),"type",sep="~"))
		if(length(S_duration_predictors)>1){
			facet_grid_formula <- paste(paste(S_duration_predictors[-1],collapse="+"),S_duration_predictors[1],sep="~")			
		}else{
			facet_grid_formula <- paste0(S_duration_predictors,"~.")
		}

		gdf <- dcast(df_dist_S_duration,formula=formu,value.var="freq")
		gdf <- melt(gdf,measure.vars=c("density","interpolate"))
		p <- ggplot(gdf) + facet_grid(facets=facet_grid_formula,scales="free")
		p <- p+geom_histogram(aes(x=S_duration,y=empirical),stat="identity",alpha=0.5)
		p <- p+geom_area(aes(x= S_duration,y=value,fill=variable),colour="black",position="identity",alpha=0.5)	
		cairo_pdf(file.path(dir_plot, "S_duration.pdf"), width = 8, height = 4)
		print(p)
		dev.off()

		gdf <-subset(df_S_duration,!is.na(age_group))
		gdf2 <- subset(df_dist_S_duration_bc,!age_group%in%c("0-17","18-44"))
		p <- ggplot(gdf)+facet_grid(symptom~age_group2)
		p <- p+geom_histogram(data= gdf,aes(x=S_duration,y=..density..),binwidth=1,origin=0)
		p <- p+geom_area(data= gdf2,aes(x=S_duration, y=freq),alpha=0.5,col="red")
		cairo_pdf(file.path(dir_plot, "S_duration_bc_regression.pdf"), width = 6, height = 4)
		print(p)
		dev.off()

		gdf <-subset(df_reg_baseline,!is.na(age_group2) & !is.na(is_risk) & !is.na(smoke_bool))
		gdf2 <- subset(df_dist_baseline_reg,!age_group%in%c("0-17","18-44"))
		p <- ggplot(gdf)+facet_grid(smoke_bool~age_group2+is_risk,scales="free_y")
		p <- p+geom_histogram(data= gdf,aes(x= baseline_health_score,y=..density..),binwidth=0.5,origin=0.25)
		p <- p+geom_area(data= gdf2,aes(x= baseline_health_score, y=freq),alpha=0.5,col="red")
		cairo_pdf(file.path(dir_plot, "baseline_health_score_regression.pdf"), width = 6, height = 4)
		print(p)
		dev.off()


		if (0) {

			p <- ggplot(df_time_to_report, aes(x = wday_report, y = value, fill = freq)) + facet_wrap(~variable)
			p <- p + geom_tile()
			p <- p + scale_fill_gradientn("proportion", colours = rev(brewer.pal(11, "Spectral")), guide = "colourbar")
			p <- p + scale_y_continuous("time to report (in days)", breaks = 0:10) + scale_x_discrete("week-day of report")
			cairo_pdf(file.path(dir_plot, "hist2D_day_vs_time_to_report.pdf"), width = 7, height = 3)
			print(p)
			dev.off()
		}





	}

	if (0) {
	#linear mixed effect regression to see what variables should be adjusted for when sampling missing data
	#rhs <- paste(c("symptom",setdiff(names(df_profile),c("person_id","register_date"))),collapse="+")
		rhs <- "symptom+age_group"
		df_reg <- mutate(df_S_start_end_ok, S_duration = S_duration + 0.1, person_id_num = as.numeric(as.factor(person_id)), smoke_bool = as.logical(revalue(smoke, c(no = "FALSE", occasionally = "TRUE", `<10_per_day` = "TRUE", `>10_per_day` = "TRUE", unknown = "NA"))))
		df_outliers <- lme_regression(df_reg, formula = paste("S_duration", rhs, sep = "~"), transformation = "boxcox", max_response = 25, test_dist = TRUE, CI_interval = "confidence", suffix = "complete", plot_outlier_TS = FALSE, df_data = df_data, df_profile = df_profile, x_var = "symptom", fill_var = "age_group", use_facet_wrap = FALSE, use_facet_grid = FALSE, facet_formula = NULL)
	}


	return(list(S_duration = df_dist_S_duration_bc, time_to_report = df_dist_time_to_report, baseline_health_score = df_dist_baseline_reg))


}
