#' Group reports by same bout
#' @param df_weekly \code{data.frame} containing weekly survey data for a single participant.
#' @param give_position logical, if TRUE the position within each bout is returned in the column "position_bout"
#' @param give_length logical, if TRUE the length of each bout is returned in the column "length_bout"
#' @note this function account for multiple report on the same date by grouping them in the same bout.
#' @inheritParams clean_weekly_survey
count_same_bout <- function(df_weekly, CR_as_TRUE = FALSE, give_position = FALSE, give_length=FALSE,subset=NULL){

	if(length(unique(df_weekly$person_id))>1){
		stop(sQuote("df_weekly")," argument contains more than one person id.",call.=FALSE)
	}

	# count only episodes for participants who verify subset condition
	x <- try(subset(df_weekly, eval(parse(text=subset))),silent=TRUE)
	if(inherits(x,"try-error")){
		stop("Invalid ",sQuote("subset")," argument. ",x)
	}

	df_weekly <- arrange(df_weekly,comp_time)
	df_weekly <- char2bool(df_weekly, "same_bout", CR_as_TRUE=CR_as_TRUE, NA_as_FALSE=TRUE)		

	# find first reports (account for multiple first report)
	ind <- with(df_weekly, which(!same_bout_bool & !duplicated(report_date)))

	#create variables
	df_weekly$n_bout <- NA
	if(give_length){
		df_weekly$length_bout <- NA		
	}
	if(give_position){
		df_weekly$position_bout <- NA		
	}

	#for each bout, start from the first and go down until you find !same_bout
	i_bout <- 0

	for(i in seq_along(ind)){

		# find bout boundary
		bout_start <- ind[i]
		bout_end <- bout_start
		while( ( bout_end < nrow(df_weekly) ) && ( df_weekly$same_bout_bool[bout_end+1] || ( df_weekly$report_date[bout_end+1] == df_weekly$report_date[bout_end] ) ) ){
			bout_end <- bout_end + 1
		}

		# if no subset or subset if present
		ind_bout <- bout_start:bout_end
		if(is.null(subset) || (!is.null(subset) &&  any(eval(parse(text=subset),df_weekly[ind_bout,]),na.rm=TRUE))){

			# increment i_bout
			i_bout <- i_bout + 1
			df_weekly$n_bout[ind_bout] <- i_bout

			if(give_length){
				#account for multiple report_date
				report_date <- df_weekly$report_date[ind_bout]
				df_weekly$length_bout[ind_bout] <- length(unique(report_date))		
			}	

			if(give_position){
				#account for multiple report_date
				report_date <- df_weekly$report_date[ind_bout]
				df_weekly$position_bout[ind_bout] <- factor(report_date,labels=seq_along(unique(report_date)))		
			}	
		}
	}
	return(df_weekly)
}

#' cluster bouts
#' 
#' Cluster different bouts based on closeness of symptom start dates.
#' @param lag_symptom_start numeric
#' @inheritParams count_same_bout
#' @note Temporarily overlapping clusters are grouped. For instance if the sequence temporarily ordered bouts is c(1,2,3,4) with bouts 1 and 4 belonging to the same cluster, then bouts 2 and 3 belong also to this cluster. 
#' @importFrom igraph graph.edgelist clusters
#' @import plyr
find_bout_cluster <- function(df_weekly, lag_symptom_start = 2) {

	stopifnot(length(unique(df_weekly$person_id))==1,c("n_bout","symptom_start")%in%names(df_weekly))

	#extract all unique s_start per bout
	s_start <- unlist(dlply(df_weekly, c("n_bout"), function(df) {
		tmp<-unique(na.omit(df$symptom_start))
		#account for multiple symptom start per bout
		if(length(tmp)>1){names(tmp)<-seq_along(tmp)}
		return(tmp)
	}))
	names(s_start)<-extract_string(names(s_start),".",1,"first")

	#compute matrix of time lags
	diff_time <- as.matrix(sapply(s_start, function(x) {
		return(abs(x - s_start))
	}))
	#keep only those n_bout with lag<=lag_max
	bout_connected <- match_df(as.data.frame(which(upper.tri(diff_time), arr.ind = T)), as.data.frame(which(diff_time <= lag_symptom_start, arr.ind = T)), on = c("row", "col"))
	#convert into graph and extract clusters
	tmp <- clusters(graph.edgelist(as.matrix(bout_connected), F))$membership
	#index of all bouts
	i_bout <- sort(unique(unlist(bout_connected)))
	#and their corresponding cluster id
	i_cluster <- tmp[i_bout]
	i_cluster <- as.numeric(factor(i_cluster, levels = unique(i_cluster), labels = seq_along(unique(i_cluster))))
	#if clusters overlap temporarily then group them
	while(any(overlap<-(diff(i_cluster)<0))){
		i<-which(overlap)[1]	
		i_cluster[i_cluster==i_cluster[i]]<-i_cluster[i+1]
		i_cluster <- as.numeric(factor(i_cluster, levels = unique(i_cluster), labels = seq_along(unique(i_cluster))))			
	}
	#bind them
	bout_cluster <- unique(data.frame(n_bout = names(s_start)[i_bout], bout_cluster = i_cluster, stringsAsFactors = F))

	#join to orginal df_weekly
	return(join(df_weekly, bout_cluster, by = "n_bout"))

}

#'Resolve NA in still_ill
#'
#'Resolve missing answer to the question "Are you still ill?"
#' @inheritParams count_same_bout
#' @inheritParams define_same_bout
#' @inheritParams clean_weekly_survey
resolve_missing_still_ill <- function(df_weekly, my_warning="W_same_S_start_diff_bout", delay_in_reporting = 10, debug=FALSE) {

	#still_ill==NA followed by a bout within same cluster
	i_NA <- with(df_weekly, which(is.na(still_ill) & c(diff(n_bout), 0)))
	if (length(i_NA) != 0) {
		df_weekly$still_ill[i_NA] <- T
		df_weekly$same_bout[i_NA + 1] <- "yes"
		df_weekly[c(i_NA, i_NA + 1), my_warning] <- T
	}

	#maybe there are n_bout == NA in the middle (account for multiple reports)
	i_NA <- with(df_weekly, which(is.na(n_bout) & !duplicated(report_date)))
	i_NA <- i_NA[!i_NA %in% c(1, nrow(df_weekly))]

	#check if they are all non-consecutive
	if (length(i_NA) == 1 || (length(i_NA) && all(diff(i_NA) > 1))) {
		#if so check lag between previous and next n_bout report
		for (i_na in i_NA) {
			i_prev <- i_na - 1
			i_next <- i_na + 1
			while (is.na(df_weekly$n_bout[i_next])) {
				i_next <- i_next + 1
			}
			if (with(df_weekly, diff(report_date[c(i_prev, i_na)]) <= delay_in_reporting && diff(report_date[c(i_na, i_next)]) <= delay_in_reporting)) {
				df_weekly$still_ill[i_prev:ifelse(i_next == nrow(df_weekly) || is.na(df_weekly$n_bout[i_next + 1]) || df_weekly$n_bout[i_next] != df_weekly$n_bout[i_next + 1], i_next - 1, i_next)] <- TRUE 
				df_weekly$same_bout[i_prev:i_next] <- "yes"
				df_weekly[c(i_prev:i_next), my_warning] <- TRUE
			}
		}
	}

	if (debug && !any(df_weekly[, my_warning])) {

		cat("The following were not edited, check if all good please\n\n")
		df_print_define_same_bout(df_weekly)
	}

	return(df_weekly)
}

#Define same bouts
#'
#'This function identifies reports belonging to the same episode of illness.
#' @param df_weekly \code{\link{data.frame}} containing weekly survey data
#' @param my_warning character, warning name
#' @param debug_id character, person_id of participant to debug
#' @inheritParams clean_weekly_survey
#' @note This function adds a warning when different bouts are considered to belong to the same episode of illness
#' @import plyr
define_same_bout <- function(df_weekly, subset = NULL, lag_symptom_start = 2, delay_in_reporting = 10, CR_as_TRUE = FALSE, my_warning="W_same_S_start_diff_bout",debug=FALSE,debug_id=NULL) {

	message("Start defining bout, you can have a coffee as it usually takes some time...")

	if(!is.null(subset)){
		# count only episodes for participants who verify subset condition
		x <- try(subset(df_weekly, eval(parse(text=subset))),silent=TRUE)
		if(inherits(x,"try-error")){
			stop("Invalid ",sQuote("subset")," argument. ",x)
		}
		tmp <- unique(x$person_id)
		message(length(tmp)," participants verify ",sQuote("subset")," argument.")
		df_2count <- subset(df_weekly, person_id %in% tmp)
		df_keep_for_the_end <- subset(df_weekly, !person_id %in% tmp)
	} else {
		df_2count <- df_weekly
		df_keep_for_the_end <- data.frame(NULL)
	}

	#create n_bout
	message("Start counting bout")

	df_weekly <- ddply(df_2count, "person_id", function(df) {

		df <- count_same_bout(df, CR_as_TRUE, give_position = TRUE, give_length = TRUE, subset = subset)
		return(df)

	}, .progress = "text")

	df_weekly <- arrange(df_weekly, person_id, comp_time)

	## everything below is cleaning

	#only those with more than one bout with non NA symptom_start 
	tmp <- subset(df_weekly, !is.na(n_bout) & !is.na(symptom_start))
	tmp2 <- unique(tmp[, c("person_id", "n_bout")])
	tmp3 <- subset(count(tmp2, vars = "person_id"), freq > 1)
	tmp <- match_df(df_weekly, tmp3, on = c("person_id"))
	tmp <- subset(tmp, !is.na(n_bout))

	if(!is.null(debug_id)){
		print(subset(tmp, person_id%in%debug_id))
	}

	############################################################################
	#Define bout clusters
	############################################################################

	cat("Define bout clusters\n")
	flush.console()


	tmp2 <- ddply(tmp, "person_id", find_bout_cluster, lag_symptom_start = lag_symptom_start, .progress = "text")

	tmp <- subset(tmp2, !is.na(bout_cluster))

	if(!is.null(debug_id)){
		print(subset(tmp, person_id%in%debug_id))
	}

	#count number of different n_bout for a given person_id and bout_cluster
	tmp2 <- unique(tmp[, c("person_id", "bout_cluster", "n_bout")])
	tmp3 <- subset(count(tmp2, vars = c("person_id", "bout_cluster")), freq > 1)
	tmp <- match_df(tmp, tmp3, on = c("person_id", "bout_cluster"))

	############################################################################
	#Select subset to clean
	############################################################################

	cat("Select subset to clean\n")
	flush.console()

	df_2clean <- ddply(tmp, c("person_id", "bout_cluster"), function(df) {

		my_person_id<-unique(df$person_id)
		min_report_date<-min(df$report_date)
		max_report_date<-max(df$report_date)

		return(subset(df_weekly, person_id == my_person_id & report_date >= min_report_date  & report_date <= max_report_date))
	}, .progress = "text")


	df_keep <- diff_df(df_weekly, df_2clean)

	df_2clean <- ddply(df_2clean, c("person_id", "bout_cluster"), function(df) {
		df$next_n_bout <- c(df$n_bout[-1], NA)
		return(df)
	})

	if(!is.null(debug_id)){
		df_print_define_same_bout(df_2clean, debug_id)		
	}

	############################################################################
	#Solve interrupted bout due non response to the question: Are you still ill?
	############################################################################

	tmp <- df_2clean[is.na(df_2clean$still_ill), c("person_id", "bout_cluster")]

	cat("Check",nrow(unique(tmp)),"interrupted bout due non response to the question \"Are you still ill?\"\n")
	flush.console()

	df_2clean1 <- match_df(df_2clean, tmp,on=c("person_id", "bout_cluster"))
	df_keep1 <- diff_df(df_2clean, df_2clean1)

	df_clean1 <- ddply(df_2clean1, c("person_id", "bout_cluster"), resolve_missing_still_ill, my_warning=my_warning,delay_in_reporting=delay_in_reporting , debug=debug,.progress = "text")

	df_2clean <- rbind(df_keep1, df_clean1)

	if(!is.null(debug_id)){
		df_print_define_same_bout(df_2clean, debug_id)		
	}

	############################################################################
	#Solve cases who changed their mind and extended their bout
	############################################################################

	tmp <- subset(df_2clean, !still_ill & !is.na(n_bout) & !is.na(next_n_bout) & (n_bout != next_n_bout), select = c("person_id", "bout_cluster"))

	cat("Check",nrow(unique(tmp)),"cases who changed their mind and extended their bout\n")
	flush.console()

	df_2clean2 <- match_df(df_2clean, tmp,on=c("person_id", "bout_cluster"))
	df_keep2 <- diff_df(df_2clean, df_2clean2)

	df_clean2 <- ddply(df_2clean2, c("person_id", "bout_cluster"), function(df) {

		i_test <- with(df, which(!still_ill & !is.na(n_bout) & !is.na(next_n_bout) & (n_bout != next_n_bout) & c(diff(report_date), 0) <= delay_in_reporting))
		if (length(i_test) != 0) {
			df$still_ill[i_test] <- TRUE
			df$same_bout[i_test + 1] <- "yes"
			df[c(i_test, i_test + 1), my_warning] <- TRUE
		} else if (debug) {
			cat("The following were not edited, check if all good please\n\n")
			df_print_define_same_bout(df)

		}
		return(df)
	}, .progress = "text")

	df_2clean <- rbind(df_keep2, df_clean2)

	if(!is.null(debug_id)){
		df_print_define_same_bout(df_2clean, debug_id)		
	}

	############################################################################
	#Solve interrupted bout due non or wrong response to the question: Is it the same bout as in previous report?
	############################################################################

	tmp <- subset(df_2clean, still_ill & !is.na(n_bout) & !is.na(next_n_bout) & (n_bout != next_n_bout), select = c("person_id", "bout_cluster"))

	cat("Check",nrow(unique(tmp)),"interrupted bout due non or wrong response to the question \"Is it the same bout as in previous report?\"\n")
	flush.console()		

	df_2clean3 <- match_df(df_2clean, tmp, on=c("person_id", "bout_cluster"))
	df_keep3 <- diff_df(df_2clean, df_2clean3)

	df_clean3 <- ddply(df_2clean3, c("person_id", "bout_cluster"), function(df) {

		i_test <- with(df, which(still_ill & !is.na(n_bout) & !is.na(next_n_bout) & (n_bout != next_n_bout) & c(diff(report_date), 0) <= delay_in_reporting))
		if (length(i_test) != 0) {

			for (i in i_test) {

				s_start_next <- first_na_rm(subset(df, n_bout == n_bout[i + 1])$symptom_start)
				if (!is.na(s_start_next) && df$report_date[i] >= s_start_next) {
					df$same_bout[i + 1] <- "yes"
					df[c(i, i + 1), my_warning] <- T
				} else if (debug) {
					cat("The following was not edited (case 1), check if all good please\n")
					df_print_define_same_bout(df)
				}

			}
		} else if (debug) {
			cat("The following was not edited (case 2), check if all good please\n")
			df_print_define_same_bout(df)
		}

		return(df)
	}, .progress = "text")

	df_2clean <- rbind(df_keep3, df_clean3)

	if(!is.null(debug_id)){
		df_print_define_same_bout(df_2clean, debug_id)		
	}

	if (debug) {
		cat("The following were not edited at all, check if all good please\n\n")
	#check what's remain, all good!
		tmp <- subset(df_2clean, eval(as.symbol(my_warning), df_2clean), select = c("person_id", "bout_cluster"))
		tmp <- match_df(df_2clean, tmp)
		tmp <- diff_df(df_2clean, tmp)
		df_print_define_same_bout(tmp)
	}

	df_weekly <- rbind(df_keep, df_2clean[names(df_keep)])

############################################################################
#Recount cleaned bout
############################################################################

	tmp <- subset(df_2clean, eval(as.symbol(my_warning), df_2clean), select = c("person_id"))
	df_2count <- match_df(df_weekly, tmp, on="person_id")
	df_keep <- diff_df(df_weekly, df_2count)

	cat("Recount cleaned bout\n")
	flush.console()

	df_counted <- ddply(df_2count, "person_id", function(df) {
		df <- count_same_bout(df, CR_as_TRUE, give_position = T, give_length = T, with_symptom = with_symptom)
		return(df)
	}, .progress = "text")

	df_weekly <- rbind(df_keep, df_counted)

#add person_id without symptom
	df_weekly <- rbind.fill(df_weekly, df_keep_for_the_end)

#
	df_weekly <- arrange(df_weekly, person_id, comp_time)

	return(df_weekly)

}


#'Find suitable symptom start date
#'
#'This function try to find a suitable symptom start date when several are available 
#' @param to_match \code{data.frame} with two columns: person_id and n_bout (bout number) whose symptom start dates need to be resolved.
#' @inheritParams define_same_bout
find_suitable_symptom_start <- function(df_weekly, to_match, delay_in_reporting, my_warning, debug = FALSE) {


	df_2clean <- match_df(df_weekly, to_match, on = c("person_id", "n_bout"))
	df_keep <- diff_df(df_weekly, df_2clean)

	df_clean <- ddply(df_2clean, c("person_id", "n_bout"), function(df) {

		date_min <- (df$report_date[1] - delay_in_reporting)

		#include report date just before if it exists
		if (x <- nrow(df2 <- subset(df_weekly, person_id == df$person_id[1] & report_date < df$report_date[1]))) {
			date_min <- max(c(df2$report_date[x], date_min))
		}

		#check if there is a symptom_start that is not delayed and that is before the first report 			
		if (length(ind <- with(df, which((symptom_start > date_min) & (symptom_start <= report_date[1]))))) {
			if (debug) {
				cat("if\n")
				print(df)
			}
			#take the first suitable s_start
			df$symptom_start <- df$symptom_start[ind[1]]
			if (debug) {
				print(df)
			}
		} else {
			df[, my_warning] <- T
			if(length(ind <- with(df, which((symptom_start > (report_date[1] - delay_in_reporting)) & (symptom_start <= report_date[1]))))){
				df_print_check(df)
			}
		}

		return(df)

	}, .progress = ifelse(debug,"none","text"))

	df_weekly <- rbind(df_keep, df_clean)

	return(df_weekly)

}


#'Find suitable symptom end date
#'
#'This function try to find a suitable symptom end date when several are available 
#' @param to_match \code{data.frame} with two columns: person_id and n_bout (bout number) whose symptom end dates need to be resolved.
#' @inheritParams define_same_bout
find_suitable_symptom_end <- function(df_weekly, to_match, delay_in_reporting, my_warning, debug = FALSE) {


	df_2clean <- match_df(df_weekly, to_match, on = c("person_id", "n_bout"))
	df_keep <- diff_df(df_weekly, df_2clean)

	df_clean <- ddply(df_2clean, c("person_id", "n_bout"), function(df) {

		x<-unique(df$report_date)
		date_min <- max(x) - delay_in_reporting

		#include report date just before if exist
		if (length(x)>1) {
			date_min <- max(rev(x)[2], date_min)
		}

		#check if there is a symptom_end that is not delayed and that is before the last report 			
		if (length(ind <- with(df, which((symptom_end >= date_min) & (symptom_end <= max(x)))))) {

			if (debug) {
				df_print_check(df)
			}
			#take the last suitable s_start (but it should not be more than one)
			df$symptom_end <- df$symptom_end[rev(ind)[1]]
			df$still_ill <-FALSE
			if(nrow(df)>1){
				df$symptom_end[df$report_date<max(x)]<-NA
				df$still_ill[df$report_date<max(x)]<-TRUE
			}
			if (debug) {
				df_print_check(df)
			}
		} else {
			df[, my_warning] <- T
		}

		return(df)

	}, .progress = ifelse(debug,"none","text"))

	df_weekly <- rbind(df_keep, df_clean)

	return(df_weekly)

}

#'Clean weekly survey
#'
#'This function peforms several checks, resolves them when possible and otherwise flags informative warnings on the corresponding reports.
#' @param flunet a \code{\link{flunet}} object
#' @param subset character, logical expression indicating reports to keep: missing values are taken as false. If present, only episode of illness with at least one report that verify \code{subset} will be processed. This is mainly to save time. E.g. to clean only episodes with at least one ARI report, one can uses \code{subset="ARI_ecdc"}.
#' @param lag_symptom_start numeric, maximum number of days between two symptom start dates of different reports. Below this threshold, reports are considered to belong to the same episode of illness and are concatenated. 
#' @param delay_in_reporting maximum number of days to report a date of \code{symptom_start} and \code{symptom_end}. Above this delay, a warning is put on the report.
#' @param CR_as_TRUE logical, if \code{TRUE}, CR (can't remember) is replaced by \code{TRUE} and \code{FALSE} otherwise. This choice is required for the question of whether current illness is the same bout as the one reported the previous time.
#' @param plot_check logical, if \code{TRUE}, plot checks.
#' @param debug logical, if \code{TRUE}, print checks.
#' @export
#' @import ggplot2 
#' @importFrom lubridate year year<- 
#' @import stringr
#' @import plyr
#' @note The option \code{lag_symptom_start} allows us to group reports belonging to the same bout but having different symptom start dates. This happens when participant change their mind from one report to the next.
#' @return a \code{flunet} object
clean_weekly_survey <- function(flunet, subset=NULL, lag_symptom_start = 2, delay_in_reporting = 10, CR_as_TRUE = FALSE, plot_check = FALSE, debug = FALSE) {

 # subset=NULL
 # lag_symptom_start = 2
 # delay_in_reporting = 10
 # CR_as_TRUE = FALSE
 # plot_check = FALSE
 # debug = FALSE

	stopifnot(is.logical(CR_as_TRUE),is.logical(plot_check),is.logical(debug))

	if(is_survey_present(flunet,survey="weekly",warning_message="no symptoms to summarize")){
		df_weekly <- flunet$surveys$weekly
	} else {
		return(flunet)
	}	

	#WARNING LIST
	W_SSCY <- c("W_S_start_change_year","date of symptom start is not correctly entered due to change of year, e.g. symptom started in december 2012, were reported in january 2013 and entered as december 2013")
	W_SSTF <- c("W_S_start_too_far","time to report the symptom start date is greater than max delay_in_reporting")
	W_SETF <- c("W_S_end_too_far","time to report the symptom end date is greater than max delay_in_reporting")
	W_SSBPR <- c("W_S_start_before_previous_report","symptom start date is before the previous report")
	W_SEBPR <- c("W_S_end_before_previous_report","symptom end date is before the previous report")
	W_SSASE <- c("W_S_start_after_S_end","symptom start date is after the symptom end date")
	W_SSW <- c("W_S_start_wrong","symptom start date is after the report date")
	W_SEW <- c("W_S_end_wrong","symptom end date is after the report date")
	W_SBBDSS <- c("W_same_bout_diff_S_start","several symptom start dates have been reported for the same episode and no suitable date was found")
	W_SBBDSE <- c("W_same_bout_diff_S_end","several symptom end dates have been reported for the same episode and no suitable date was found")
	W_SSSBDB <- c("W_same_S_start_diff_bout","several episodes have a symptom start date within lag_symptom_start")
	W_MISC <- c("W_misc","miscellaneous warnings such as unrealistic values, i.e. health-score < 0 or > 100")

	df_warnings <- as.data.frame(rbind(W_SSCY = W_SSCY, W_SSTF = W_SSTF, W_SETF = W_SETF, W_SSBPR= W_SSBPR, W_SEBPR= W_SEBPR, W_SSASE= W_SSASE, W_SSW = W_SSW, W_SEW = W_SEW, W_SBBDSS = W_SBBDSS, W_SBBDSE = W_SBBDSE, W_SSSBDB = W_SSSBDB, W_MISC = W_MISC),stringsAsFactors=F)
	names(df_warnings) <- c("name","description")
	df_weekly[df_warnings$name] <- FALSE

	######################################################################################################################################################
	#											check: symptom_start is wrong due to new year \n
	######################################################################################################################################################
	new_year <- as.numeric(unlist(str_split(flunet$season,"/"))[1])+1
	min_report <- as.Date(paste0(new_year,"-01-01"))
	max_report <- as.Date(paste0(new_year,"-01-31"))
	min_symptom <- as.Date(paste0(new_year,"-12-01"))
	max_symptom <- as.Date(paste0(new_year,"-12-31"))

	my_warning <- df_warnings["W_SSCY","name"]

	ind <- with(df_weekly, which(report_date >= min_report & report_date <= max_report & symptom_start >= min_symptom & symptom_start <= max_symptom & !eval(as.symbol(my_warning), df_weekly)))

	if(length(ind)){
		year(df_weekly$symptom_start[ind]) <- new_year - 1
		df_weekly[ind, my_warning] <- TRUE		
	}

	######################################################################################################################################################
	#											check: define same_bout \n
	######################################################################################################################################################


	my_warning <- df_warnings["W_SSSBDB","name"]
	
	df_weekly <- define_same_bout(df_weekly, subset = subset, lag_symptom_start = lag_symptom_start, delay_in_reporting = delay_in_reporting, CR_as_TRUE = CR_as_TRUE, my_warning = my_warning, debug = debug, debug_id = NULL)

	df_weekly <- arrange(df_weekly, person_id, comp_time)

	#define past episodes
	df_weekly <- transform(df_weekly, W_past_episode_full = (length_bout %in% c(1) & !is.na(symptom_start) & !is.na(symptom_end) & symptom_start <= symptom_end & symptom_start < report_date & symptom_end < report_date & still_ill %in% c(F)))


	######################################################################################################################################################
	#											check: same_bout but several symptom_start\n
	######################################################################################################################################################

	my_warning <- df_warnings["W_SBBDSS","name"]

	#same_bout but several symptom_start
	tmp <- unique(subset(df_weekly, !is.na(length_bout) & length_bout > 1 & !is.na(symptom_start) & !eval(as.symbol(my_warning), df_weekly), select = c("person_id", "n_bout", "symptom_start")))
	tmp_1 <- subset(count(tmp[, c("person_id", "n_bout")]), freq > 1)[, c("person_id", "n_bout")]
	#same_bout but first symptom_start is missing
	tmp <- unique(subset(df_weekly, !is.na(length_bout) & length_bout > 1 & is.na(symptom_start) & position_bout == 1 & !eval(as.symbol(my_warning), df_weekly), select = c("person_id", "n_bout")))
	tmp2 <- unique(subset(df_weekly, !is.na(length_bout) & length_bout > 1 & !is.na(symptom_start) & position_bout != 1, select = c("person_id", "n_bout")))
	tmp_2 <- match_df(tmp2, tmp)
	#bind
	tmp <- unique(rbind(tmp_1, tmp_2))

	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning if I can't find a suitable symptom_start date.")
		flush.console()
		df_weekly <- find_suitable_symptom_start(df_weekly, tmp, delay_in_reporting, my_warning, debug)
	}


	######################################################################################################################################################
	#											check: same_bout but several symptom_end\n
	######################################################################################################################################################

	my_warning <- df_warnings["W_SBBDSE","name"]

	#same_bout but several symptom_end
	tmp <- unique(subset(df_weekly, !is.na(length_bout) & length_bout > 1 & !is.na(symptom_end) & !eval(as.symbol(my_warning), df_weekly), select = c("person_id", "n_bout", "symptom_end")))
	tmp_1 <- subset(count(tmp[, c("person_id", "n_bout")]), freq > 1)[, c("person_id", "n_bout")]
	#same_bout but symptom_end is not at the last report
	tmp <- unique(subset(df_weekly, !is.na(length_bout) & length_bout > 1 & is.na(symptom_end) & position_bout == length_bout & !eval(as.symbol(my_warning), df_weekly), select = c("person_id", "n_bout")))
	tmp2 <- unique(subset(df_weekly, !is.na(length_bout) & length_bout > 1 & !is.na(symptom_end) & position_bout != length_bout, select = c("person_id", "n_bout")))
	tmp_2 <- match_df(tmp2, tmp)
	#bind
	tmp <- unique(rbind(tmp_1, tmp_2))

	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning if I can't find a suitable symptom_end date.\n")
		flush.console()
		df_weekly <- find_suitable_symptom_end(df_weekly, tmp_2, delay_in_reporting, my_warning, debug = F)
	}

	######################################################################################################################################################	
	#														check: symptom_start is too far from first report_date\n
	######################################################################################################################################################

	my_warning <- df_warnings["W_SSTF","name"]
	if (plot_check) {
		tmp <- subset(df_weekly, symptom_start < report_date & position_bout == 1 & !eval(as.symbol(my_warning), df_weekly))
		p <- ggplot(tmp, aes(x = as.numeric(report_date - symptom_start))) + geom_histogram(binwidth = 1) + xlim(c(0, delay_in_reporting * 4))
		p <- p + geom_vline(xintercept = delay_in_reporting, colour = "red")
		print(p)
	}

	tmp <- subset(df_weekly, symptom_start < (report_date - delay_in_reporting) & position_bout == 1 & !eval(as.symbol(my_warning), df_weekly))[, c("person_id", "n_bout")]
	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning.\n")
		flush.console()
		df_2clean <- match_df(df_weekly, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_weekly, df_2clean)
		df_2clean[, my_warning] <- T
		df_weekly <- rbind(df_keep, df_2clean)
	}

	######################################################################################################################################################	
	#														check: symptom_end is too far from last report_date\n
	######################################################################################################################################################
	my_warning <- df_warnings["W_SETF","name"]
	if (plot_check) {
		tmp <- subset(df_weekly, symptom_end < report_date & position_bout == length_bout & !eval(as.symbol(my_warning), df_weekly))
		p <- ggplot(tmp, aes(x = as.numeric(report_date - symptom_end))) + geom_histogram(binwidth = 1) + xlim(c(0, delay_in_reporting * 4))
		p <- p + geom_vline(xintercept = delay_in_reporting, colour = "red")
		print(p)
	}

	tmp <- subset(df_weekly, symptom_end < (report_date - delay_in_reporting) & position_bout == length_bout & !eval(as.symbol(my_warning), df_weekly))[, c("person_id", "n_bout")]
	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning.\n")
		flush.console()
		df_2clean <- match_df(df_weekly, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_weekly, df_2clean)
		df_2clean[, my_warning] <- T
		df_weekly <- rbind(df_keep, df_2clean)
	}


	######################################################################################################################################################
	#														check: symptom_start < previous report_date\n
	######################################################################################################################################################
	my_warning <- df_warnings["W_SSBPR","name"]

	df_weekly<-arrange(df_weekly,person_id,comp_time)
	df_weekly<-transform(df_weekly,previous_report_date=c(as.Date(NA),report_date[-nrow(df_weekly)]),previous_person_id=c(NA,person_id[-nrow(df_weekly)]),previous_n_bout=c(NA,n_bout[-nrow(df_weekly)]))
	#find all person_id
	tmp<-subset(df_weekly,!is.na(position_bout) & position_bout==1 & !is.na(previous_person_id) & person_id==previous_person_id & (is.na(previous_n_bout) | n_bout!=previous_n_bout) & !is.na(symptom_start) & symptom_start<previous_report_date & !eval(as.symbol(my_warning), df_weekly))
	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning.\n")
		flush.console()
		df_2clean <- match_df(df_weekly, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_weekly, df_2clean)
		df_2clean[, my_warning] <- T
		df_weekly <- rbind(df_keep, df_2clean)
	}

	######################################################################################################################################################
	#														check: symptom_end < previous report_date\n
	######################################################################################################################################################
	my_warning <- df_warnings["W_SEBPR","name"]
	df_weekly<-arrange(df_weekly,person_id,comp_time)
	df_weekly<-transform(df_weekly,previous_position_bout=c(NA,position_bout[-nrow(df_weekly)]))
	#find all person_id
	tmp<-subset(df_weekly,!is.na(position_bout) & position_bout==length_bout & (is.na(previous_position_bout) | position_bout!=previous_position_bout) & !is.na(previous_person_id) & person_id==previous_person_id & !is.na(previous_n_bout) & n_bout==previous_n_bout & !is.na(symptom_end) & symptom_end<previous_report_date & !eval(as.symbol(my_warning), df_weekly))
	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning.\n")
		flush.console()
		df_2clean <- match_df(df_weekly, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_weekly, df_2clean)
		df_2clean[, my_warning] <- T
		df_weekly <- rbind(df_keep, df_2clean)
	}

	df_weekly <- df_weekly[setdiff(names(df_weekly),paste("previous",c("report_date","n_bout","person_id","position_bout"),sep="_"))]

	######################################################################################################################################################
	#														check: symptom_start > report_date\n
	######################################################################################################################################################
	my_warning <- df_warnings["W_SSW","name"]
	if (plot_check) {
		tmp <- subset(df_weekly, symptom_start > report_date & position_bout == 1 & !eval(as.symbol(my_warning), df_weekly))
		p <- ggplot(tmp, aes(x = as.numeric(symptom_start - report_date))) + geom_histogram(binwidth = 1)
		print(p)
	}
	tmp <- subset(df_weekly, symptom_start > report_date & position_bout == 1 & !eval(as.symbol(my_warning), df_weekly))

	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning.\n")
		flush.console()
		df_2clean <- match_df(df_weekly, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_weekly, df_2clean)
		df_2clean[, my_warning] <- T
		df_weekly <- rbind(df_keep, df_2clean)
	}


	######################################################################################################################################################	
	#														check: symptom_end > report_date\n
	######################################################################################################################################################	
	my_warning <- df_warnings["W_SEW","name"]

	if (plot_check) {
		tmp <- subset(df_weekly, !is.na(n_bout) & symptom_end > report_date & !eval(as.symbol(my_warning), df_weekly))
		p <- ggplot(tmp, aes(x = as.numeric(symptom_end - report_date))) + geom_histogram(binwidth = 1)
		print(p)
	}
	tmp <- subset(df_weekly, !is.na(n_bout) & symptom_end > report_date & !eval(as.symbol(my_warning), df_weekly))
	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning\n")
		flush.console()
		df_clean <- match_df(df_weekly, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_weekly, df_clean)
		df_clean[, my_warning] <- T
		df_weekly <- rbind(df_keep, df_clean)
	}

	######################################################################################################################################################	
	#														check: symptom_start > symptom_end\n
	######################################################################################################################################################	
	my_warning <- df_warnings["W_SSASE","name"]

	if (plot_check) {
		tmp <- subset(df_weekly, !is.na(n_bout) & symptom_start > symptom_end & !eval(as.symbol(my_warning), df_weekly))
		p <- ggplot(tmp, aes(x = as.numeric(symptom_start - symptom_end))) + geom_histogram(binwidth = 1)
		print(p)
	}
	tmp <- subset(df_weekly, !is.na(n_bout) & symptom_start > symptom_end & !eval(as.symbol(my_warning), df_weekly))
	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning\n")
		flush.console()
		df_clean <- match_df(df_weekly, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_weekly, df_clean)
		df_clean[, my_warning] <- T
		df_weekly <- rbind(df_keep, df_clean)
	}

	######################################################################################################################################################
	#											check: misc\n
	######################################################################################################################################################	

	my_warning <- df_warnings["W_MISC","name"]

	#problem with some health_score
	ind <- which(df_weekly$health_score > 100)
	cat(length(ind), "health_score > 100\n ==> replaced by NA\n")
	df_weekly$health_score[ind] <- NA
	df_weekly[ind, my_warning]<-T
	
	#log
	tmp <- rename(count(subset(melt(df_weekly,measure.vars=df_warnings$name),value),vars="variable"),c("variable"="name"))
	tmp <- mutate(tmp,name=as.character(name))
	df_warnings <- join(df_warnings,tmp,by='name')

	flunet$log$clean_weekly_survey <- list("lag_symptom_start"=lag_symptom_start,"delay_in_reporting"=delay_in_reporting,"CR_as_TRUE"=CR_as_TRUE,"warnings"=df_warnings)

	#return
	flunet$surveys$weekly <- df_weekly

	return(flunet)
}


#' Merge multiple reports on the same date
#' 
#' This function merges multiple reports occuring on the same day, due to participant error. This function account for the fact that multiple reports might contain complementary information.
#' @inheritParams clean_weekly_survey
#' @export
#' @import plyr
resolve_multiple_report_date <- function(flunet) {

	#checks
	if(!inherits(flunet,"flunet")){
		stop("flunet is not a flunet object")
	}

	df_weekly <- flunet$surveys$weekly
	if(is.null(df_weekly)){
		warnings("flunet doesn't contain weekly survey, no symptoms to summarize")
		return(flunet)
	}


	#count how many entries have the same report_date
	tmp <- count(df_weekly[, c("person_id", "report_date")])
	per <- sum(tmp$freq > 1)/nrow(tmp) * 100

	print(paste(round(per, 2), "% of entries correspond to more than one report per day"))
	flush.console()
	#select person_id - report_date
	df_2clean<-match_df(df_weekly, subset(tmp,freq>1))	
	df_keep <-match_df(df_weekly, subset(tmp,freq==1))

	#for same_bout, if the duplicated report correspond to the start of a new episode
	#the first same_bout should be NA or No but the second will be "yes" by default although it should be NA or No
	#if this is not the start of a new episode then the first report should be "yes" unless the person changes it
	#thus always take first report
	var_always_first<-c("same_bout")

	#bool
	tmp<-vapply(df_weekly,function(x) all(is.logical(x)), logical(1))
	var_bool<-names(tmp[tmp])
	var_bool<-setdiff(var_bool,var_always_first)

	#dates
	tmp<-vapply(df_weekly,function(x) all(inherits(x,"Date")), logical(1))
	var_date<-names(tmp[tmp])
	var_date <-setdiff(var_date,var_always_first)

	#remainder
	var_char<-setdiff(names(df_weekly),c(var_always_first,var_bool,var_date))


	#clean multiple person_id,report_date entries
	df_clean <- ddply(df_2clean, c("person_id", "report_date"), function(df) {


		#need to arrange by compilation time for always_first
		df <- arrange(df, comp_time)

		#solve always first
		for(var in  var_always_first){
			df[,  var]<- df[1, var]
		}		

		#solve logical duplicate
		for(var in var_bool){
			df[, var]<- any_na_rm(df[, var])
		}		

		#solve date duplicate
		for(var in var_date){
			#TODO: if several S_start and S_end dates, choose the best pair.
			df[, var]<- last_na_rm(df[, var])
		}		

		#solve non-logical
		for(var in var_char){
			df[, var]<-last_na_rm(df[, var])
		}


		df <- unique(df)

		if (nrow(df) > 1) {
			print(df)
		}

		return(df)
	}, .progress = "text")

	#
	df_weekly <- rbind(df_keep, df_clean)
	df_weekly<-arrange(df_weekly,person_id,report_date)

	#TODO: remove this when previous TODO is done
	#check for people with symptom_start > symptom_end
	ind <- with(df_weekly,which(symptom_start > symptom_end & !is.na(n_bout) & !W_S_start_after_S_end))
	if(length(ind)){
		print(df_weekly[ind,])
		df_weekly$S_start_after_S_end[ind]<-T		
	}

	flunet$surveys$weekly <- df_weekly

	return(flunet)
}



