#'char2bool
#'
#'char2bool
#' @param x data frame
#' @param var_names variable to be converted
#' @param NA_as_FALSE convert \code{NA} into \code{FALSE}
#' @inheritParams clean_weekly_survey
#' @import plyr
char2bool <- function(x,var_names,CR_as_TRUE=FALSE,NA_as_FALSE=FALSE) {

	replace <- c("yes"=TRUE,"no"=FALSE,"CR"=CR_as_TRUE)

	for(var_name in var_names){
		tmp <- as.logical(revalue(x[[var_name]],replace=replace))
	}

	if(NA_as_FALSE){
		tmp[is.na(tmp)] <- FALSE
	}

	x[[paste(var_name,"bool",sep="_")]] <- tmp

	return(x)

}

#'print helper
#'
#'print helper
#' @param df data frame
#' @param id character
#' @import plyr
#' @name print helper
#' @aliases df_print_define_same_bout
df_print_define_same_bout <-function(df,id=NULL){

	if(!is.null(id)){
		df<-subset(df,person_id%in%id)
		if(!nrow(df)){
			cat("id not found\n")
		}
	}

	d_ply(df,c("person_id","bout_cluster"),function(x){
		print(x[c("person_id","report_date","health_score","same_bout","still_ill","n_bout","next_n_bout","bout_cluster","symptom_start","symptom_end","ARI_ecdc","ILI_ecdc","ILI_fever")])
		cat("########################\n")
	})

}

#' @name print helper
#' @aliases df_print_check
df_print_check <-function(df,id=NULL){

	if(!is.null(id)){
		df<-subset(df,person_id%in%id)
		if(!nrow(df)){
			cat("id not found\n")
		}
	}

	d_ply(df,c("person_id"),function(x){
		print(x[c("person_id","report_date","health_score","same_bout","length_bout","still_ill","n_bout","symptom_start","symptom_end","ARI_ecdc","ILI_ecdc","ILI_fever")])
		cat("########################\n")
	})

}

#' @name print helper
#' @aliases df_print_check_raw
df_print_check_raw <-function(df,id=NULL){

	if(!is.null(id)){
		df<-subset(df,person_id%in%id)
		if(!nrow(df)){
			cat("id not found\n")
		}
	}

	d_ply(df,c("person_id"),function(x){
		print(x[c("person_id","report_date","health_score","same_bout","length_bout","still_ill","symptom_start","symptom_end","ARI_ecdc","ILI_ecdc","ILI_fever")])
		cat("########################\n")
	})

}

first_na_rm <- function(x) {
	
	ind <- which(!is.na(x))

	if (length(ind)) {
		ans<-x[ind[1]]
	}else{
		ans<-x[1]	
	}

	return(ans)		
}

# perform logical operation "any" after NA's removing
# @param x_true a vector containing the values of x that should be considered as TRUE. The other values are considered as FALSE. The function returns the first value of x_true found in x.
any_na_rm <- function(x, x_true=NULL){
	
# TODO: this function should be split in two as the part with x_true doesn't return TRUE/FALSE but the first value found in x_true.

	if(!is.null(x_true)){
		
		# is there any of these "TRUE" values? if so return the first one	
		for(x_val in x_true){
			if(x_val %in% x){
				return(x_val)
			}
		}
		
		# if not, return the most common one among the "FALSE" one
		return(most_common_na_rm(x))
	}
	
	# x must be logical
	return(any(x, na.rm = TRUE))
	
}


most_common_na_rm<-function(x){

	if(all(is.na(x))){
		return(NA)
	}
	
	if(any(grepl("day",levels(x)))){
		#this is a time factor, take max after excluding "can't remember"
		xx <- x[x!="CR"]
		if(all(is.na(xx))){
			ans <- "CR"
		}else{
			tmp <- max(as.numeric(xx),na.rm=T)
			ans <- levels(x)[tmp]			
		}
	}else{
		#take the most common
		tmp <- table(x[!is.na(x)])
		ans <- names(which.max(tmp))
	}
	return(ans)
}



is_survey_present <- function(flunet, survey, warning_message="") {

	if(!inherits(flunet,"flunet")){
		stop(sQuote("flunet")," argument is not a flunet object")
	}

	missing_survey <- setdiff(survey,names(flunet$surveys))

	if(length(missing_survey)){
		warnings(sQuote("flunet")," object doesn't contain ",sQuote(missing_survey)," survey(s) ",warning_message,call.=FALSE)
		return(FALSE)
	}

	return(TRUE)

}






