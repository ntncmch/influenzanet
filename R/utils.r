extract_string <- function(v.string,my.split,position,from=c("first","last")){
	
	from <- match.arg(from)

	res<-sapply(v.string,function(x){
		tmp<-strsplit(as.character(x),split=my.split,fixed=T)[[1]]
		if(from=="last"){
			position=length(tmp)-position+1
		}
		return(tmp[position])
	})

	return(res)
}



#'char2bool
#'
#'char2bool
#' @param x <what param does>
#' @param var_names <what param does>
#' @param CR_as_TRUE <what param does>
#' @param NA_as_FALSE <what param does>
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
		print(x[c("person_id","report_date","health_score","same_bout","still_ill","n_bout","bout_cluster","symptom_start","symptom_end","ARI_ecdc","ILI_ecdc","ILI_fever")])
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

diff_df <- function(df1, df2) {
	
	if(!nrow(df1)){return(df2)}
	if(!nrow(df2)){return(df1)}
		
	x<-intersect(names(df1),names(df2))
	
	df1$in_df1 <- T
	df2$in_df2 <- T

	res <- merge(df1, df2, all = T)

	res_diff <- subset(res, is.na(in_df1) | is.na(in_df2),select=x)
	
	return(res_diff)
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
# @param x.true a vector containing the values of x that should be considered as TRUE. The other values are considered as FALSE. The function returns the first value of x.true found in x.
any_na_rm <- function(x,x.true=NULL){
	
	if(!is.null(x.true)){
		
		for(x.val in x.true){
			if(x.val%in%x){
				return(x.val)
			}
		}
		
		#return one of the remaining values
		return(most_common_na_rm(x))
	}
	
	#x must be logical
	return(any(x, na.rm = T))
	
}


last_na_rm <- function(x) {
	
	ind <- which(!is.na(x))

	if (length(ind)) {
		ans<-x[max(ind)]
	}else{
		ans<-x[1]	
	}
		
	return(ans)		
}



most_common_na_rm<-function(x){

	if(all(is.na(x))){
		return(NA)
	}
	
	if(any(grepl("day",levels(x)))){
		#this is a time factor, take max after excluding "can't remember"
		xx<-x[x!="CR"]
		if(all(is.na(xx))){
			ans<-"CR"
		}else{
			tmp<-max(as.numeric(xx),na.rm=T)
			ans<-levels(x)[tmp]			
		}
	}else{
		#take the most common
		tmp<-table(x[!is.na(x)])
		ans<-names(which.max(tmp))
	}
	return(ans)
}



