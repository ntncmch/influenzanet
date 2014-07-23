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
			tmp <- max(as.numeric(xx),na.rm=TRUE)
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
		stop(sQuote("flunet")," argument is not a flunet object",call.=FALSE)
	}

	missing_survey <- setdiff(survey,names(flunet$surveys))

	if(length(missing_survey)){
		warnings(sQuote("flunet")," object doesn't contain ",sQuote(missing_survey)," survey(s) ",warning_message,call.=FALSE)
		return(FALSE)
	}

	return(TRUE)

}






