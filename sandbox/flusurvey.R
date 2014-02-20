#analysis of FluSurvey data for cost-effectiveness
library(reshape2)
library(ggplot2)
library(plyr)
library(scales)
library(compare)
library(RColorBrewer)
library(pracma)
library(lubridate)
library(car)
library(stringr)
library(nlme)
library(grid)

DIR_DATA <- "/Users/Tonton/work/data/FluSurvey/2012_13"
DIR_PROJECT <- "/Users/Tonton/work/projects/Flusurvey"
RUTILS <- "/Users/Tonton/work/projects/Rutils/Rutils.R"

setwd(DIR_PROJECT)
source(RUTILS)

{
	PDF <- paste(DIR_PROJECT, "pdf", sep = "/")
	RSAVE <- paste(DIR_PROJECT, "Rsave", sep = "/")
	RES <- paste(DIR_PROJECT, "res", sep = "/")
}

#' Rename Flusurvey variables as well as values in R format 
#' @param df_data a \link{data.frame} containing FluSurvey data.
#' @return a \link{data.frame}.
#' @export
#' @importFrom plyr arrange
#'
rename_data<-function(df_data){
	
	#rename
	df_data <- rename(df_data,c(Person="person_id",health="health_score",muscle.joint="muscle_joint"))

	#char to factor
	levels_time<-c(7,0:6) # toto
	labels_time<-c("CR","0 day","1 day", "2 days", "3 days", "4 days", "5-7 days", ">7 days")
	exclude_time<-c(NA,100)
	ordered<-TRUE

	var_time<-c("How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...GP.or.GP.s.practice.nurse..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...Hospital.admission..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...Hospital.accident...emergency.department.out.of.hours.service..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...Other.medical.services..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...GP...spoke.to.receptionist.only..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...GP...spoke.to.doctor.or.nurse..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...NHS.Direct...NHS.24...NHS.Choices..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...Other..Medical.Service.","How.long.after.the.beginning.of.your.symptoms.did.you.start.taking.antiviral.medication.")

	new_var_time<-c("time_visit_GP","time_visit_hosp","time_visit_AE","time_visit_other","time_phone_GP_recept","time_phone_GP_doctor","time_phone_NHS_direct","time_phone_other","time_AV")

	df_data <- transform_factor(df_data,var_time, new_var_time,levels= levels_time,labels=labels_time,exclude=exclude_time,ordered=ordered)

	#num to factor	
	levels_cause<-rev(0:6)
	labels_cause<-rev(c("ILI", "cold", "allergy", "asthma", "gastro", "other", "unknown"))
	ordered<-TRUE
	df_data<-transform_factor(df_data,"cause", levels= levels_cause,labels=labels_cause,ordered=ordered)

	levels_top_fever<-c(6,0:5)
	labels_top_fever<-c("CR","<37", "37-37.4", "37.5-37.9", "38-38.9", "39-39.9", ">40")
	ordered<-TRUE
	df_data<-transform_factor(df_data,"top_fever", levels= levels_top_fever,labels= labels_top_fever,ordered=ordered)

	levels_routine<-0:2
	labels_routine<-c("no","yes","yes + off")
	ordered<-TRUE
	df_data<-transform_factor(df_data,"change_routine",levels= levels_routine,labels= labels_routine,ordered=ordered)

	levels_same_bout<-rev(0:2)
	labels_same_bout<-rev(c("yes","no","CR"))
	exclude_same_bout<-c(NA,3)
	ordered<-TRUE
	df_data<-transform_factor(df_data,"samebout","same_bout",levels= levels_same_bout,labels= labels_same_bout,exclude=exclude_same_bout,ordered=ordered)

	levels_time_off<-0:7
	labels_time_off<-c("1 day", "2 days", "3 days", "4 days", "5 days", "6-10 days", "11-15 days", ">15 days")
	ordered<-TRUE
	var_time_off<-c("How.long.have.you.been.off.work.school.")
	new_var_time_off<-c("time_off")
	df_data<-transform_factor(df_data,var_time_off, new_var_time_off,levels= levels_time_off,labels=labels_time_off,ordered=ordered)

	levels_ynCR<-rev(0:3)
	labels_ynCR<-rev(c("yes","no","CR","CR"))
	exclude_ynCR<-c(NA)
	ordered<-TRUE
	var_ynCR<-c("sym_sudden","fever_sudden")
	new_var_ynCR<-c("sympt_sudden","fever_sudden")
	df_data<-transform_factor(df_data, var_ynCR,new_var_ynCR,levels= levels_ynCR,labels= labels_ynCR, exclude=exclude_ynCR,logical=F,ordered=ordered)

	levels_ynOTH<-rev(0:2)
	labels_ynOTH<-rev(c("yes","no","other"))
	exclude_ynOTH<-c(NA)
	ordered<-TRUE
	var_ynOTH<-c("Are.you.still.off.work.school.")
	new_var_ynOTH<-c("still_off")
	df_data<-transform_factor(df_data, var_ynOTH,new_var_ynOTH,levels= levels_ynOTH,labels= labels_ynOTH, exclude=exclude_ynOTH,logical=F,ordered=ordered)

#num to logical
	levels_nny<-0:2
	labels_nny<-c(FALSE,FALSE,TRUE)
	var_nny<-c("stillill")
	new_var_nny<-c("still_ill")
	df_data<-transform_factor(df_data, var_nny, new_var_nny,levels= levels_nny,labels= labels_nny, logical=T)


#char to logical
	levels_logical<-c("False","True")
	labels_logical<-c(FALSE,TRUE)
	var_logical <- c(paste0("med_visit",0:5),paste0("med_phoneweb",c(0:3,5)),paste0("medication",0:6))
	new_var_logical<-c(paste("visit",c("no","GP","AE","hosp","other","schedul"),sep="_"), paste("phone",c("no","GP_recept","GP_doctor","NHS_direct","other"),sep="_"), paste("med",c("no","pain","cough","AV","AB","other","CR"),sep="_"))
	df_data<-transform_factor(df_data, var_logical,new_var_logical,levels= levels_logical,labels= labels_logical,logical=T)
#symptoms
	var_sym_logical<-c("none","fever","chills","nose","sneeze","throat","cough","breath","head","muscle_joint","chest","tired","appetite","phlegm","eyes","nausea","vomit","diarrhoea","stomach","other")
	df_data<-transform_factor(df_data, var_sym_logical,levels= levels_logical,labels= labels_logical,logical=T)


#as.Date
	df_data <- transform(df_data, report_date = as.Date(reportdate, format = "%Y-%m-%d"), comp_time = as.POSIXlt(Compilation.Date), symptom_start = as.Date(When.did.the.first.symptoms.appear...0.Open.Answer, format = "%Y-%m-%d"), symptom_end = as.Date(When.did.your.symptoms.end...0.Open.Answer, format = "%Y-%m-%d"))

#clean
	df_data <- arrange(df_data, comp_time)


	df_data <- subset(df_data, select = c("person_id", "comp_time","report_date", "health_score", "same_bout", "symptom_start", "symptom_end","change_routine","cause","top_fever", new_var_time_off ,new_var_ynOTH,new_var_ynCR, new_var_nny ,new_var_logical, var_sym_logical,new_var_time))

	return(df_data)
}

#' Summarize symptoms as ARI or ILI
#' @param df_data a \link{data.frame} containing FluSurvey data.
#' @param symptom a \link{vector} of characters containing the names of the symptom episode. Currently ARI_ecdc, ILI_ecdc and ILI_fever are implemented. 
#' @param CR_as_TRUE logical, when participants can't remember whether their symptom/fever suddenly developped over a few hours.
#' @param remove_original logical, if \code{TRUE} remove all original variables used to summarize the symptoms.
#' @return \code{df_data} with additional variable(s) \code{symptom}.
#' @export
#' @importFrom plyr arrange
#'
summarize_symptom<-function(df_data,symptom=c("ARI_ecdc","ILI_ecdc","ILI_fever"),CR_as_TRUE=FALSE,remove_original=FALSE){

	#factor to logical
	levels<-c("yes","no","CR")
	labels<-c(TRUE,FALSE,CR_as_TRUE)
	old_var<-c("sympt_sudden","fever_sudden")
	new_var<-paste(old_var,"bool",sep="_")
	df_data<-transform_factor(df_data, old_var, new_var,levels= levels,labels= labels, logical=T)

	#need to fix fever_sudden_bool==FALSE when fever==F & is.na(fever_sudden_bool)
	df_data$fever_sudden_bool[is.na(df_data$fever_sudden_bool) & !df_data$fever]<-FALSE

	#add ARI and ILI summary (ILI_ECDC & ILI_fever)
	df_data <- transform(df_data, ARI_ecdc = ((sympt_sudden_bool | fever_sudden_bool ) & (cough | throat | breath | nose )), ILI_ecdc = ((sympt_sudden_bool | fever_sudden_bool ) & (fever | chills | top_fever %in% c("38-38.9","39-39.9",">40") | tired | head | muscle_joint ) & (cough | throat | breath)), ILI_fever = ((sympt_sudden_bool | fever_sudden_bool) & (fever | chills | top_fever %in% c("38-38.9","39-39.9",">40")) & (tired | head | muscle_joint) & (cough | throat | breath)))

	df_data <- arrange(df_data, comp_time)

	all_symptoms<-c("ARI_ecdc","ILI_ecdc","ILI_fever")

	if(any(!all_symptoms%in%symptom)){	
		var_remove<-all_symptoms[!all_symptoms%in%symptom]
		df_data <-subset(df_data,select=setdiff(names(df_data),var_remove))
	}

	if(remove_original){
		var_remove<-c(new_var,old_var,"top_fever","none","fever","chills","nose","sneeze","throat","cough","breath","head","muscle_joint","chest","tired","appetite","phlegm","eyes","nausea","vomit","diarrhoea","stomach","other")
		df_data <-subset(df_data,select=setdiff(names(df_data),var_remove))
	}

	return(df_data)

}


#' Resolve multiple profile entries for Flusurvey participants. Rename variables and cast values in R format.
#' @param df_profile a \link{data.frame} containing FluSurvey participants profiles.
#' @param age_group_cut a numeric vector containing the start and end points of the age groups.
#' @param age_group_name a character vector containing the labels for the age groups.
#' @return a \link{data.frame}.
#' @export
#' @importFrom plyr arrange ddply rename
#'
clean_profile<-function(df_profile,age_group_cut=c(0,18,45,65,Inf),age_group_name=c("0-17", "18-44", "45-64", "65+")){

	if(!is.null(age_group_name)){
		stopifnot(length(age_group_name)==(length(age_group_cut)-1))		
	}

	#num to factor
	levels_gender<-0:1
	labels_gender<-c("male", "female")
	df_profile <-transform_factor(df_profile,"gender", levels= levels_gender,labels= labels_gender)

	levels_employment<-0:8
	labels_employment<-c("paid_full_time", "paid_part_time","self","student","home","none","long_term_leave","retired","other")
	df_profile <-transform_factor(df_profile,variable="employed",new_variable="employment", levels= levels_employment,labels= labels_employment)

	levels_activity<-0:5
	labels_activity<-c("professional", "office","service","skilled_manual","other_manual","other")
	df_profile <-transform_factor(df_profile,variable="activity",new_variable="occupation", levels= levels_activity,labels= labels_activity)

	levels_smoke<-0:4
	labels_smoke<-c("no", "occasionally","<10_per_day",">10_per_day","unknown")
	df_profile <-transform_factor(df_profile,variable="smoke", levels= levels_smoke,labels= labels_smoke)


	#char to logical
	levels_logical<-c("False","True")
	labels_logical<-c(FALSE,TRUE)
	var_logical<-c("asthma","diabetes","respiratoryother","heart","kidney","immuno")
	df_profile<-transform_factor(df_profile, var_logical,levels= levels_logical,labels= labels_logical,logical=T)

	#num to logical
	levels_vacc<-0:1
	labels_vacc<-c(TRUE,FALSE)
	exclude_vacc<-c(NA,2,3)
	df_profile<-transform_factor(df_profile,"vaccinelast","vaccine_last",levels= levels_vacc,labels= labels_vacc,exclude=exclude_vacc,logical=T)
	df_profile<-transform_factor(df_profile,"vaccine",levels= levels_vacc,labels= labels_vacc,exclude=exclude_vacc,logical=T)
	df_profile<-transform_factor(df_profile,"pregnant",levels= levels_vacc,labels= labels_vacc,exclude=exclude_vacc,logical=T)


	df_profile <- transform(df_profile, age_group = cut(age,breaks = age_group_cut, labels=as.factor(age_group_name),include=T,right=F), is_risk = (asthma | diabetes | respiratoryother | heart | kidney | immuno ), region=factor(region),register_date=as.Date(registerdate, format = "%Y-%m-%d"),comp_time = as.POSIXlt(Compilation.Date))

	df_profile <- rename(df_profile,c(Person = "person_id"))


	#order by date of compilation
	df_profile<-arrange(df_profile,comp_time)

	#select interesting variable and reduce
	df_profile <- unique(subset(df_profile, select = c("person_id","register_date","gender","age_group", "is_risk","pregnant","smoke", "vaccine","region","employment","occupation")))

	#how many duplicates?
	x <- table(df_profile[, c("person_id")])
	tmp <- sum(x > 1)/sum(x > 0) * 100
	print(paste(round(tmp, 2), "% of person_id have more than one profile"))
	flush.console()

	id_mult<-names(x)[x>1]
	df_mult<-subset(df_profile,person_id%in%id_mult)
	df_uni<-subset(df_profile,!person_id%in%id_mult)

	tmp<-sapply(df_mult,function(x) all(is.logical(x)))
	var_bool<-names(tmp[tmp])
	var_not_bool<-names(tmp[!tmp])


	#force 1 profile per person_id: multiple profiles arise when profile is updated by user
	df_resolve_mult <- ddply(df_mult, "person_id", function(df) {

		#solve logical duplicate
		for(var in var_bool){
			df[, var]<- any_na_rm(df[, var])
		}		

		#solve non-logical
		for(var in var_not_bool){
			df[, var]<-last_na_rm(df[, var])
		}

		df <- unique(df)

		if (nrow(df) > 1) {
			print(df)
		}

		return(df)

	}, .progress = "text")

	df_profile_unique<-rbind(df_uni, df_resolve_mult)

	return(df_profile_unique)
}

remove_first_report<-function(df_data,verbose=F){

	df_data <- arrange(df_data, comp_time)


	#remove all individuals with only one report
	tmp <- unique(df_data[, c("person_id", "report_date")])

	n_report <- rle(sort(tmp$person_id))
	id_to_remove <- n_report$values[n_report$lengths == 1]
	df_data <- subset(df_data, !person_id %in% id_to_remove)

	#remove the first report of each person_id with ARI_ecdc. First report is biased toward illness.
	df_data_without_first <- ddply(df_data, "person_id", function(df) {
		df <- arrange(df, report_date)
		min_report_date <- min(df$report_date)
		df2<-subset(df,report_date == min_report_date)#account for multiple first report

		if(any(df2$ARI_ecdc%in%c(T))){
			df <- subset(df, report_date != min_report_date) #remove first report with symptoms
		}else{
			return(df)
		}
		#remove next report if: it is same_bout or it is ARI_ecdc & symptom_start < previous report date (in this case we consider that either it is the same disease or the symptom start is wrong and report should be removed) 
		while(df$same_bout[1]%in%c("yes","CR") | (!is.na(df$ARI_ecdc[1]) & df$ARI_ecdc[1] & !is.na(df$symptom_start[1]) & df$symptom_start[1]<min_report_date)) {
			#if next report is same_bout (or can't remember) remove too
			if(nrow(df)==1){break}
			min_report_date<-df$report_date[1]
			df<-df[-1,]
		}
		return(df)
	}, .progress = "text")

	df_data<-df_data_without_first
	#again, remove all individuals with only one report
	tmp <- unique(df_data[, c("person_id", "report_date")])
	n_report <- rle(sort(tmp$person_id))
	id_to_remove <- n_report$values[n_report$lengths == 1]
	df_data <- subset(df_data, !person_id %in% id_to_remove)

	if(verbose){
		#some people still indicates illness at their "first" report 
		tmp<-df_data[!duplicated(df_data$person_id),]
		tmp2<-subset(tmp, ARI_ecdc & (report_date-symptom_start)>7)
		cat("Some person_id still indicate illness at their first report. This is actually their second report of illness but we removed the first one.")
		print(tmp2)
	}
	return(df_data)

}	

#' Group reports by same bout
#' @param df_data a \link{data.frame} containing FluSurvey longitudinal data for one person_id only.
#' @param CR_as_TRUE logical, shall we replace CR (can't remember) by TRUE when participants are asked whether current illness is the same bout as the one reported the previous time.
#' @param give_position logical, if TRUE the position within each bout is returned in the column "position_bout"
#' @param give_length logical, if TRUE the length of each bout is returned in the column "length_bout"
#' @param with_symptom character, if present then only bouts with at least one report with this symptom are counted. \code{with_symptom} must be the name of a logical column of \code{df_data}. 
#' @note this function account for multiple report on the same date by grouping them in the same bout.
#'
count_same_bout<-function(df_data,CR_as_TRUE = FALSE, give_position = FALSE, give_length=FALSE,with_symptom=NULL){

	stopifnot(length(unique(df_data$person_id))==1,is.null(with_symptom) | (!is.null(with_symptom) & with_symptom%in%names(df_data) & is.logical(df_data[,with_symptom])))

	df_data<-arrange(df_data,comp_time)
	df_data<-same_bout_as_bool(df_data, CR_as_TRUE)

	#find first reports but account for multiple report
	ind<-with(df_data,which(!same_bout_bool & !duplicated(report_date)))

	#create variables
	df_data$n_bout<-NA
	if(give_length){
		df_data$length_bout<-NA		
	}
	if(give_position){
		df_data$position_bout<-NA		
	}


	#for each bout, start from the first and go down until you find !same_bout
	i_bout<-0
	for(i in seq_along(ind)){
		#find bout boundary
		x<-ind[i]
		while((x<nrow(df_data)) & (df_data$same_bout_bool[x+1] || df_data$report_date[x+1]==df_data$report_date[x])){
			x<-x+1
		}

		if(is.null(with_symptom) | (!is.null(with_symptom) & any(df_data[ind[i]:x,with_symptom],na.rm=T))){
			i_bout<-i_bout+1
			df_data$n_bout[ind[i]:x]<-i_bout

			if(give_length){
				#account for multiple report_date
				report_date<-df_data$report_date[ind[i]:x]
				df_data$length_bout[ind[i]:x]<-length(unique(report_date))		
			}	

			if(give_position){
				#account for multiple report_date
				report_date<-df_data$report_date[ind[i]:x]
				df_data$position_bout[ind[i]:x]<-factor(report_date,labels=seq_along(unique(report_date)))		
			}	
		}


	}
	return(df_data)
}

find_suitable_symptom_start <- function(df_data, to_match, delay_in_reporting, my_warning, debug = F) {


	df_2clean <- match_df(df_data, to_match, on = c("person_id", "n_bout"))
	df_keep <- diff_df(df_data, df_2clean)

	df_clean <- ddply(df_2clean, c("person_id", "n_bout"), function(df) {

		date_min <- (df$report_date[1] - delay_in_reporting)

		#include report date just before if exist
		if (x <- nrow(df2 <- subset(df_data, person_id == df$person_id[1] & report_date < df$report_date[1]))) {
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

	df_data <- rbind(df_keep, df_clean)

	return(df_data)

}


find_suitable_symptom_end <- function(df_data, to_match, delay_in_reporting, my_warning, debug = F) {


	df_2clean <- match_df(df_data, to_match, on = c("person_id", "n_bout"))
	df_keep <- diff_df(df_data, df_2clean)

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

	df_data <- rbind(df_keep, df_clean)

	return(df_data)

}

#' Find cluster of different bout based on closeness of symptom start
#' @param df_data a \link{data.frame} containing FluSurvey longitudinal data for one person_id only.
#' @param lag_symptom_start maximum lag in days between symptom_start for clustering
#' @note Temporarily overlapping clusters are grouped. For instance if the sequence temporarily ordered bouts is c(1,2,3,4) with bouts 1 and 4 belonging to the same cluster, then bouts 2 and 3 belong also to this cluster. 
#' @importFrom igraph graph.edgelist clusters
#' @importFrom plyr dlply join
#'
find_bout_cluster <- function(df_data, lag_symptom_start = 2) {
	require(igraph)

	stopifnot(length(unique(df_data$person_id))==1,c("n_bout","symptom_start")%in%names(df_data))

	#extract all unique s_start per bout
	s_start <- unlist(dlply(df_data, c("n_bout"), function(df) {
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
	bout_connected <- match_df(as.data.frame(which(upper.tri(diff_time), arr.ind = T)), as.data.frame(which(diff_time <= 
		lag_symptom_start, arr.ind = T)), on = c("row", "col"))
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

	#join to orginal df_data
	return(join(df_data, bout_cluster, by = "n_bout"))

}


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

print_stat_pop_old <- function(df_profile, df_episode) {
	df_pop <- match_df(df_profile, df_episode)
	cat("full data set contains", nrow(df_pop), "individuals for", nrow(df_episode), "episodes\n")
	print(table(df_pop[, c("age_group", "is_risk")]))
	print(table(df_episode[, c("age_group", "symptom")]))
}

print_info_2 <- function(df_episode,W_check=c("W_approximate_baseline_health_score","W_approximate_S_start","W_approximate_S_end"),not_useful="is.na(QALD_loss) | QALD_loss<=0 | is.na(symptom) | is.na(age_group) | is.na(smoke_bool) | is.na(is_risk) | is.na(vaccine) | is.na(gender)",dir_tex=".") {

	library(xtable)
	# df_episode <- df_reg
	df_episode <- mutate(df_episode,not_useful=eval(parse(text=not_useful)))
	W_check <- c(W_check,"not_useful")
	tmp1 <- expand.grid(lapply(df_episode[W_check],unique))

	tmp <- count(df_episode,vars=W_check)
	tmp <- join(tmp1,tmp,by=W_check)
	tmp[is.na(tmp)] <- 0
	
	all_episodes <- sum(tmp$freq)
	useful_episodes <- sum(subset(tmp,!not_useful)$freq)
	print(c(useful=useful_episodes,all=all_episodes,per=round(useful_episodes/all_episodes*100,0)))

	tmp <- ddply(tmp,setdiff(W_check,"not_useful"),function(df) {
		df <- mutate(df,n_episode_total=as.integer(sum(df$freq)),n_episode_QALD=as.integer(freq),prop_episode_QALD=as.integer(round(freq/sum(df$freq)*100,digits=0)))
		df <- subset(df,!not_useful)			
		return(df)
	})

	tmp[W_check] <- !tmp[W_check]
	tmp$not_useful <- NULL
	tmp$freq <- NULL
	names(tmp) <- c("Baseline health-score","Symptom onset date","Symptom end date","Number of episodes","Number of episodes with QALD","Proportion (%)")
	tmp[tmp=="TRUE"] <- "yes"
	tmp[tmp=="FALSE"] <- "no"
	print(tmp)
#	xtmp <- xtable(tmp,align=c("lcccr"))

	xtmp <- xtable(tmp,align=c("l","p{2cm}","p{2cm}","p{2cm}","p{3.5cm}","p{3.5cm}","p{1cm}"))

	print(xtmp,file=file.path(dir_tex,"xtable.tex"),include.rownames = FALSE)

}
# print sample size by type of available information
print_info <- function(df_episode,W_check=c("W_approximate_baseline_health_score","W_approximate_S_start","W_approximate_S_end"),not_useful="is.na(QALD_loss) | QALD_loss<=0 | is.na(symptom) | is.na(age_group) | is.na(smoke_bool) | is.na(is_risk) | is.na(vaccine) | is.na(gender)",dir_tex=".") {

	library(xtable)
	# df_episode <- df_reg
	df_episode <- mutate(df_episode,not_useful=eval(parse(text=not_useful)))
	W_check <- c(W_check,"not_useful")
	tmp1 <- expand.grid(lapply(df_episode[W_check],unique))

	tmp <- count(df_episode,vars=W_check)
	tmp <- join(tmp1,tmp,by=W_check)
	tmp[is.na(tmp)] <- 0
	
	all_episodes <- sum(tmp$freq)
	useful_episodes <- sum(subset(tmp,!not_useful)$freq)
	print(c(useful=useful_episodes,all=all_episodes,per=round(useful_episodes/all_episodes*100,0)))

	tmp <- ddply(tmp,setdiff(W_check,"not_useful"),function(df) {
		df <- mutate(df,n_episode=paste0(freq,"/",sum(df$freq)," (",round(freq/sum(df$freq)*100,digits=0),"%)"))
		df <- subset(df,!not_useful)			
		return(df)
	})

	tmp[W_check] <- !tmp[W_check]
	tmp$not_useful <- NULL
	tmp$freq <- NULL
	names(tmp) <- c("Baseline health-score","Symptom onset date","Symptom end date","Number of episodes")
	tmp[tmp=="TRUE"] <- "yes"
	tmp[tmp=="FALSE"] <- "no"
	print(tmp)
#	xtmp <- xtable(tmp,align=c("lcccr"))

	xtmp <- xtable(tmp,align=c("l","p{2cm}","p{2cm}","p{2cm}","p{3.5cm}"))

	print(xtmp,file=file.path(dir_tex,"xtable.tex"),include.rownames = FALSE)

}



define_same_bout <- function(df_data, with_symptom = "ARI_ecdc", lag_symptom_start = 2, delay_in_reporting = 10, CR_as_TRUE = FALSE, my_warning,debug=F,debug_id=NULL,force=FALSE) {

	cat("Defining bout, you can have a coffee as it usually takes some time...\n")
	flush.console()
	#count same bout
	tmp <- subset(df_data, eval(as.symbol(with_symptom), df_data))
	df_2count <- subset(df_data, person_id %in% tmp$person_id)
	df_keep_for_the_end <- subset(df_data, !person_id %in% tmp$person_id)

	#create n_bout
	cat("Start counting bout\n")
	flush.console()

	RDSfile<-"./Rsave/df_data_counted.rds"
	if(!file.exists(RDSfile) || force){
		df_data <- ddply(df_2count, "person_id", function(df) {
			df <- count_same_bout(df, CR_as_TRUE, give_position = T, give_length = T, with_symptom = with_symptom)
			return(df)
		}, .progress = "text")

		df_data <- arrange(df_data, person_id, comp_time)
		saveRDS(df_data,file= RDSfile)

	}else{
		df_data<-readRDS(file= RDSfile)

	}

	#everything below is cleaning

	#only those with more than one bout with non NA symptom_start 
	tmp <- subset(df_data, !is.na(n_bout) & !is.na(symptom_start))
	tmp2 <- unique(tmp[, c("person_id", "n_bout")])
	tmp3 <- subset(count(tmp2, vars = "person_id"), freq > 1)
	tmp <- match_df(df_data, tmp3, on = c("person_id"))
	tmp <- subset(tmp, !is.na(n_bout))

	if(!is.null(debug_id)){
		print(subset(tmp, person_id%in%debug_id))
	}

	############################################################################
	#Define bout clusters
	############################################################################

	cat("Define bout clusters\n")
	flush.console()

	RDSfile<-"./Rsave/df_data_counted_cluster.rds"
	if(!file.exists(RDSfile) || force){

		tmp2 <- ddply(tmp, "person_id", find_bout_cluster, lag_symptom_start = lag_symptom_start, .progress = "text")

		tmp <- subset(tmp2, !is.na(bout_cluster))
		saveRDS(tmp,file= RDSfile)

	}else{
		tmp <-readRDS(file= RDSfile)

	}

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

	RDSfile<-"./Rsave/df_2clean.rds"
	if(!file.exists(RDSfile) || force){

		df_2clean <- ddply(tmp, c("person_id", "bout_cluster"), function(df) {

			my_person_id<-unique(df$person_id)
			min_report_date<-min(df$report_date)
			max_report_date<-max(df$report_date)

			return(subset(df_data, person_id == my_person_id & report_date >= min_report_date  & report_date <= max_report_date))
		}, .progress = "text")

		saveRDS(df_2clean,file= RDSfile)

	}else{
		df_2clean <-readRDS(file= RDSfile)

	}


	df_keep <- diff_df(df_data, df_2clean)

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



	df_clean1 <- ddply(df_2clean1, c("person_id", "bout_cluster"), function(df) {

		#still_ill==NA followed by a bout within same cluster
		i_NA <- with(df, which(is.na(still_ill) & c(diff(n_bout), 0)))
		if (length(i_NA) != 0) {
			df$still_ill[i_NA] <- T
			df$same_bout[i_NA + 1] <- "yes"
			df[c(i_NA, i_NA + 1), my_warning] <- T
		}

		#maybe there are n_bout == NA in the middle (account for multiple reports)
		i_NA <- with(df, which(is.na(n_bout) & !duplicated(report_date)))
		i_NA <- i_NA[!i_NA %in% c(1, nrow(df))]

		#check if they are all non-consecutive
		if (length(i_NA) == 1 || (length(i_NA) && all(diff(i_NA) > 1))) {
			#if so check lag between previous and next n_bout report
			for (i_na in i_NA) {
				i_prev <- i_na - 1
				i_next <- i_na + 1
				while (is.na(df$n_bout[i_next])) {
					i_next <- i_next + 1
				}
				if (with(df, diff(report_date[c(i_prev, i_na)]) <= delay_in_reporting & diff(report_date[c(i_na, i_next)]) <= delay_in_reporting)) {
					df$still_ill[i_prev:ifelse(i_next == nrow(df) || is.na(df$n_bout[i_next + 1]) || df$n_bout[i_next] != df$n_bout[i_next + 1], i_next - 1, i_next)] <- T
					df$same_bout[i_prev:i_next] <- "yes"
					df[c(i_prev:i_next), my_warning] <- T
				}
			}
		}

		if (debug && !any(df[, my_warning])) {

			cat("The following were not edited, check if all good please\n\n")
			df_print_define_same_bout(df)
		}

		return(df)

	},.progress = "text")

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
		df$still_ill[i_test] <- T
		df$same_bout[i_test + 1] <- "yes"
		df[c(i_test, i_test + 1), my_warning] <- T
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

	i_test <- with(df, which(still_ill & !is.na(n_bout) & !is.na(next_n_bout) & (n_bout != next_n_bout) & 
		c(diff(report_date), 0) <= delay_in_reporting))
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

df_data <- rbind(df_keep, df_2clean[names(df_keep)])

############################################################################
#Recount cleaned bout
############################################################################

tmp <- subset(df_2clean, eval(as.symbol(my_warning), df_2clean), select = c("person_id"))
df_2count <- match_df(df_data, tmp, on="person_id")
df_keep <- diff_df(df_data, df_2count)

cat("Recount cleaned bout\n")
flush.console()

df_counted <- ddply(df_2count, "person_id", function(df) {
	df <- count_same_bout(df, CR_as_TRUE, give_position = T, give_length = T, with_symptom = with_symptom)
	return(df)
}, .progress = "text")

df_data <- rbind(df_keep, df_counted)

#add person_id without symptom
df_data <- rbind.fill(df_data, df_keep_for_the_end)

#
df_data <- arrange(df_data, person_id, comp_time)

return(df_data)

}

#' Clean Flusurvey data by performing several basic checks.
#' @param df_data a \link{data.frame} containing FluSurvey data.
#' @param lag_symptom_start Maximum number of days between two symptom start dates, below which two bouts are eventually merged. This option allows us to group reports belonging to the same bout but having different symptom start dates, when participant change their mind from one report to the next.
#' @param delay_in_reporting the maximum number of days to report a date of symptom_start and symptom_end.
#' @param CR_as_TRUE logical, shall we replace CR (can't remember) by TRUE when participants are asked whether current illness is the same bout as the one reported the previous time.
#' @param plot_check logical, plot mode.
#' @param debug logical, debug mode.
#' @return a data frame.
#' @export
#' @import ggplot2 
#' @importFrom lubridate year
#' @importFrom plyr arrange ddply match_df
#'
clean_data_automatically <- function(df_data, lag_symptom_start = 2, delay_in_reporting = 10, CR_as_TRUE = FALSE, plot_check = FALSE, debug = FALSE, force=FALSE) {

	#WARNING LIST
	W_SSCY <- "W_S_start_change_year"
	W_SSTF <- "W_S_start_too_far"
	W_SETF <- "W_S_end_too_far"
	W_SSBPR <- "W_S_start_before_previous_report"
	W_SEBPR <- "W_S_end_before_previous_report"
	W_SSASE <- "W_S_start_after_S_end"	
	W_SSW <- "W_S_start_wrong"
	W_SEW <- "W_S_end_wrong"
	W_SBBDSS <- "W_same_bout_diff_S_start"
	W_SBBDSE <- "W_same_bout_diff_S_end"
	W_SSSBDB <- "W_same_S_start_diff_bout"
	W_MISC <- "W_misc"
	MY_WARNINGS <- c(W_SSCY = W_SSCY, W_SSTF = W_SSTF, W_SETF = W_SETF, W_SSBPR= W_SSBPR, W_SEBPR= W_SEBPR, W_SSASE= W_SSASE, W_SSW = W_SSW, W_SEW = W_SEW, W_SBBDSS = W_SBBDSS, 
		W_SBBDSE = W_SBBDSE, W_SSSBDB = W_SSSBDB, W_MISC = W_MISC)
	df_data[, MY_WARNINGS] <- F



	######################################################################################################################################################
	#											check: symptom_start is wrong due to new year \n
	######################################################################################################################################################

	my_warning <- W_SSCY
	ind <- with(df_data, which(report_date >= as.Date("2013-01-01") & report_date <= as.Date("2013-01-31") & symptom_start >= 
		as.Date("2013-12-01") & symptom_start <= as.Date("2013-12-31") & !eval(as.symbol(my_warning), df_data)))

	year(df_data$symptom_start[ind]) <- year(df_data$symptom_start[ind]) - 1
	df_data[ind, my_warning] <- T

	######################################################################################################################################################
	#											check: define same_bout \n
	######################################################################################################################################################

	RDS_file<-"./Rsave/df_data_same_bout.rds"
	if(!file.exists(RDS_file) || force){
		df_data <- same_bout_as_bool(df_data, CR_as_TRUE)

		#select only individuals with at least one ARI_ecdc==T	
		tmp <- subset(df_data, ARI_ecdc)
		df_2count <- subset(df_data, person_id %in% tmp$person_id)
		df_keep <- subset(df_data, !person_id %in% tmp$person_id)

		my_warning <- W_SSSBDB
		df_counted <- define_same_bout(df_2count, with_symptom = "ARI_ecdc", lag_symptom_start = lag_symptom_start, 
			delay_in_reporting = delay_in_reporting, CR_as_TRUE = CR_as_TRUE, my_warning = my_warning, debug = debug, 
			debug_id = NULL,force=force)


		df_data <- rbind.fill(df_counted, df_keep)
		df_data <- arrange(df_data, person_id, comp_time)

		saveRDS(df_data, file = RDS_file)
	}else{
		df_data<-readRDS(file= RDS_file)
	}

	#define past episodes
	df_data <- transform(df_data, W_past_episode_full = (length_bout %in% c(1) & !is.na(symptom_start) & !is.na(symptom_end) & 
		symptom_start <= symptom_end & symptom_start < report_date & symptom_end < report_date & still_ill %in% 
		c(F)))


	######################################################################################################################################################
	#											check: same_bout but several symptom_start\n
	######################################################################################################################################################

	my_warning <- W_SBBDSS

	#same_bout but several symptom_start
	tmp <- unique(subset(df_data, !is.na(length_bout) & length_bout > 1 & !is.na(symptom_start) & !eval(as.symbol(my_warning), 
		df_data), select = c("person_id", "n_bout", "symptom_start")))
	tmp_1 <- subset(count(tmp[, c("person_id", "n_bout")]), freq > 1)[, c("person_id", "n_bout")]
	#same_bout but first symptom_start is missing
	tmp <- unique(subset(df_data, !is.na(length_bout) & length_bout > 1 & is.na(symptom_start) & position_bout == 
		1 & !eval(as.symbol(my_warning), df_data), select = c("person_id", "n_bout")))
	tmp2 <- unique(subset(df_data, !is.na(length_bout) & length_bout > 1 & !is.na(symptom_start) & position_bout != 
		1, select = c("person_id", "n_bout")))
	tmp_2 <- match_df(tmp2, tmp)
	#bind
	tmp <- unique(rbind(tmp_1, tmp_2))

	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning if I can't find a suitable symptom_start date.")
		flush.console()
		df_data <- find_suitable_symptom_start(df_data, tmp, delay_in_reporting, my_warning, debug)
	}


	######################################################################################################################################################
	#											check: same_bout but several symptom_end\n
	######################################################################################################################################################

	my_warning <- W_SBBDSE

	#same_bout but several symptom_end
	tmp <- unique(subset(df_data, !is.na(length_bout) & length_bout > 1 & !is.na(symptom_end) & !eval(as.symbol(my_warning), 
		df_data), select = c("person_id", "n_bout", "symptom_end")))
	tmp_1 <- subset(count(tmp[, c("person_id", "n_bout")]), freq > 1)[, c("person_id", "n_bout")]
	#same_bout but symptom_end is not at the last report
	tmp <- unique(subset(df_data, !is.na(length_bout) & length_bout > 1 & is.na(symptom_end) & position_bout == 
		length_bout & !eval(as.symbol(my_warning), df_data), select = c("person_id", "n_bout")))
	tmp2 <- unique(subset(df_data, !is.na(length_bout) & length_bout > 1 & !is.na(symptom_end) & position_bout != 
		length_bout, select = c("person_id", "n_bout")))
	tmp_2 <- match_df(tmp2, tmp)
	#bind
	tmp <- unique(rbind(tmp_1, tmp_2))

	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning if I can't find a suitable symptom_end date.\n")
		flush.console()
		df_data <- find_suitable_symptom_end(df_data, tmp_2, delay_in_reporting, my_warning, debug = F)
	}

	######################################################################################################################################################	
	#														check: symptom_start is too far from first report_date\n
	######################################################################################################################################################

	my_warning <- W_SSTF
	if (plot_check) {
		tmp <- subset(df_data, symptom_start < report_date & position_bout == 1 & !eval(as.symbol(my_warning), 
			df_data))
		p <- ggplot(tmp, aes(x = as.numeric(report_date - symptom_start))) + geom_histogram(binwidth = 1) + xlim(c(0, 
			delay_in_reporting * 4))
		p <- p + geom_vline(xintercept = delay_in_reporting, colour = "red")
		print(p)
	}

	tmp <- subset(df_data, symptom_start < (report_date - delay_in_reporting) & position_bout == 1 & !eval(as.symbol(my_warning), 
		df_data))[, c("person_id", "n_bout")]
	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning.\n")
		flush.console()
		df_2clean <- match_df(df_data, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_data, df_2clean)
		df_2clean[, my_warning] <- T
		df_data <- rbind(df_keep, df_2clean)
	}

	######################################################################################################################################################	
	#														check: symptom_end is too far from last report_date\n
	######################################################################################################################################################
	my_warning <- W_SETF
	if (plot_check) {
		tmp <- subset(df_data, symptom_end < report_date & position_bout == length_bout & !eval(as.symbol(my_warning), 
			df_data))
		p <- ggplot(tmp, aes(x = as.numeric(report_date - symptom_end))) + geom_histogram(binwidth = 1) + xlim(c(0, 
			delay_in_reporting * 4))
		p <- p + geom_vline(xintercept = delay_in_reporting, colour = "red")
		print(p)
	}

	tmp <- subset(df_data, symptom_end < (report_date - delay_in_reporting) & position_bout == length_bout & !eval(as.symbol(my_warning), 
		df_data))[, c("person_id", "n_bout")]
	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning.\n")
		flush.console()
		df_2clean <- match_df(df_data, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_data, df_2clean)
		df_2clean[, my_warning] <- T
		df_data <- rbind(df_keep, df_2clean)
	}


	######################################################################################################################################################
	#														check: symptom_start < previous report_date\n
	######################################################################################################################################################
	my_warning <- W_SSBPR

	df_data<-arrange(df_data,person_id,comp_time)
	df_data<-transform(df_data,previous_report_date=c(as.Date(NA),report_date[-nrow(df_data)]),previous_person_id=c(NA,person_id[-nrow(df_data)]),previous_n_bout=c(NA,n_bout[-nrow(df_data)]))
	#find all person_id
	tmp<-subset(df_data,!is.na(position_bout) & position_bout==1 & !is.na(previous_person_id) & person_id==previous_person_id & (is.na(previous_n_bout) | n_bout!=previous_n_bout) & !is.na(symptom_start) & symptom_start<previous_report_date & !eval(as.symbol(my_warning), df_data))
	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning.\n")
		flush.console()
		df_2clean <- match_df(df_data, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_data, df_2clean)
		df_2clean[, my_warning] <- T
		df_data <- rbind(df_keep, df_2clean)
	}

	######################################################################################################################################################
	#														check: symptom_end < previous report_date\n
	######################################################################################################################################################
	my_warning <- W_SEBPR
	df_data<-arrange(df_data,person_id,comp_time)
	df_data<-transform(df_data,previous_position_bout=c(NA,position_bout[-nrow(df_data)]))
	#find all person_id
	tmp<-subset(df_data,!is.na(position_bout) & position_bout==length_bout & (is.na(previous_position_bout) | position_bout!=previous_position_bout) & !is.na(previous_person_id) & person_id==previous_person_id & !is.na(previous_n_bout) & n_bout==previous_n_bout & !is.na(symptom_end) & symptom_end<previous_report_date & !eval(as.symbol(my_warning), df_data))
	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning.\n")
		flush.console()
		df_2clean <- match_df(df_data, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_data, df_2clean)
		df_2clean[, my_warning] <- T
		df_data <- rbind(df_keep, df_2clean)
	}

	df_data<-df_data[setdiff(names(df_data),paste("previous",c("report_date","n_bout","person_id","position_bout"),sep="_"))]

	######################################################################################################################################################
	#														check: symptom_start > report_date\n
	######################################################################################################################################################
	#need to refine
	my_warning <- W_SSW
	if (plot_check) {
		tmp <- subset(df_data, symptom_start > report_date & position_bout == 1 & !eval(as.symbol(my_warning), 
			df_data))
		p <- ggplot(tmp, aes(x = as.numeric(symptom_start - report_date))) + geom_histogram(binwidth = 1)
		print(p)
	}
	tmp <- subset(df_data, symptom_start > report_date & position_bout == 1 & !eval(as.symbol(my_warning), 
		df_data))

	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning.\n")
		flush.console()
		df_2clean <- match_df(df_data, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_data, df_2clean)
		df_2clean[, my_warning] <- T
		df_data <- rbind(df_keep, df_2clean)
	}


	######################################################################################################################################################	
	#														check: symptom_end > report_date\n
	######################################################################################################################################################	
	#need to refine
	my_warning <- W_SEW

	if (plot_check) {
		tmp <- subset(df_data, !is.na(n_bout) & symptom_end > report_date & !eval(as.symbol(my_warning), df_data))
		p <- ggplot(tmp, aes(x = as.numeric(symptom_end - report_date))) + geom_histogram(binwidth = 1)
		print(p)
	}
	tmp <- subset(df_data, !is.na(n_bout) & symptom_end > report_date & !eval(as.symbol(my_warning), df_data))
	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning\n")
		flush.console()
		df_clean <- match_df(df_data, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_data, df_clean)
		df_clean[, my_warning] <- T
		df_data <- rbind(df_keep, df_clean)
	}

	######################################################################################################################################################	
	#														check: symptom_start > symptom_end\n
	######################################################################################################################################################	
	my_warning <- W_SSASE

	if (plot_check) {
		tmp <- subset(df_data, !is.na(n_bout) & symptom_start > symptom_end & !eval(as.symbol(my_warning), df_data))
		p <- ggplot(tmp, aes(x = as.numeric(symptom_start - symptom_end))) + geom_histogram(binwidth = 1)
		print(p)
	}
	tmp <- subset(df_data, !is.na(n_bout) & symptom_start > symptom_end & !eval(as.symbol(my_warning), df_data))
	if (nrow(tmp)) {
		cat(nrow(tmp), my_warning, "\n ==> I put a warning\n")
		flush.console()
		df_clean <- match_df(df_data, tmp, on = c("person_id", "n_bout"))
		df_keep <- diff_df(df_data, df_clean)
		df_clean[, my_warning] <- T
		df_data <- rbind(df_keep, df_clean)
	}

	saveRDS(df_data,file="./Rsave/df_data_here.rds")
	#df_data <- readRDS(file = "./Rsave/df_data_here.rds")

	######################################################################################################################################################	
	#											check: symptom_start==symptom_end and next report is same bout\n
	######################################################################################################################################################
	if(0){
		tmp <- subset(df_data, symptom_start == symptom_end)
		tmp <- subset(df_data, person_id %in% tmp$person_id)
		#define n_bout
		tmp <- ddply(tmp, "person_id", function(df) {
			df <- count_same_bout(df, CR_as_TRUE, TRUE)
			return(df)
		})
		df_2clean <- subset(tmp, symptom_start == symptom_end & !is.na(length_bout) & length_bout > 1)
		df_2clean <- subset(tmp, person_id %in% df_2clean$person_id)
		df_keep <- subset(df_data, !person_id %in% df_2clean$person_id)

		cat(length(unique(df_2clean$person_id)), "person_id with more than one report for a symptom_start==symptom_end\n ==> just NA symptom_end (for simplicity as we only had 1 case when it was coded.)\n")

		#put symptom_end <- NA
		ind <- with(df_2clean, which(symptom_start == symptom_end & !is.na(n_bout) & length_bout > 1))
		df_2clean$symptom_end[ind] <- NA
		df_clean <- df_2clean

		#bind
		df_data <- rbind(df_keep, subset(df_clean, select = c(names(df_clean) %in% names(df_keep))))

		######################################################################################################################################################
		#											check: misc\n
		######################################################################################################################################################	

		#problems with some dates (with ARI_ecdc=FALSE)
		ind <- which(df_data$symptom_start < as.Date("2012-01-01"))
		cat(length(ind), "symptom_start < 2012-01-01\n ==> replaced by NA\n")
		df_data$symptom_start[ind] <- NA

		ind <- which(df_data$symptom_end < as.Date("2012-01-01"))
		cat(length(ind), "symptom_end < 2012-01-01\n ==> replaced by NA\n")
		df_data$symptom_end[ind] <- NA
	}
	my_warning <- W_MISC

	#problem with some health_score
	ind <- which(df_data$health_score > 100)
	cat(length(ind), "health_score > 100\n ==> replaced by NA\n")
	df_data$health_score[ind] <- NA
	df_data[ind, my_warning]<-T
	#saveRDS(df_data,file="./Rsave/df_data_abstract.rds")
	#df_data <- readRDS(file = "./Rsave/df_data_abstract.rds")


	return(df_data)
}


#' Merge duplicated report dates.
#' @param df_data a \link{data.frame} containing FluSurvey data, with potentially duplicated report dates.
#' @note multiple reports on the same day arise due to participant error and usually contain complementary information.
#' @export
#' @importFrom plyr arrange ddply match_df
#'
resolve_multiple_report_date <- function(df_data) {


	#count how many entries have the same report_date
	tmp <- count(df_data[, c("person_id", "report_date")])
	x <- sum(tmp$freq > 1)/nrow(tmp) * 100

	print(paste(round(x, 2), "% of entries correspond to more than one report per day"))
	flush.console()
	#select person_id - report_date
	df_2clean<-match_df(df_data, subset(tmp,freq>1))	
	df_keep <-match_df(df_data, subset(tmp,freq==1))

	#for same_bout, if the duplicated report correspond to the start of a new episode
	#the first same_bout should be NA or No but the second will be "yes" by default although it should be NA or No
	#if this is not the start of a new episode then the first report should be "yes" unless the person changes it
	#thus always take first report
	var_always_first<-c("same_bout")

	#bool
	tmp<-vapply(df_data,function(x) all(is.logical(x)), logical(1))
	var_bool<-names(tmp[tmp])
	var_bool<-setdiff(var_bool,var_always_first)

	#dates
	tmp<-vapply(df_data,function(x) all(inherits(x,"Date")), logical(1))
	var_date<-names(tmp[tmp])
	var_date <-setdiff(var_date,var_always_first)

	#remainder
	var_char<-setdiff(names(df_data),c(var_always_first,var_bool,var_date))


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
	df_data <- rbind(df_keep, df_clean)
	df_data<-arrange(df_data,person_id,report_date)

	#TODO: remove this when previous TODO is done
	#check for people with symptom_start > symptom_end
	ind <- with(df_data,which(symptom_start > symptom_end & !is.na(n_bout) & !W_S_start_after_S_end))
	if(length(ind)){
		print(df_data[ind,])
		df_data$S_start_after_S_end[ind]<-T		
	}

	return(df_data)
}

same_bout_as_bool<-function(df_data, CR_as_TRUE=FALSE){

	levels <- c("no", "yes","CR",NA)
	labels <- c(FALSE, TRUE, CR_as_TRUE,FALSE)
	old_var <- c("same_bout")
	new_var <- c("same_bout_bool")
	df_data <- transform_factor(df_data, old_var,new_var, levels = levels, labels = labels, exclude=NULL,logical = T)

	return(df_data)
}

count_episode <- function(df_data, symptom = c("ARI_ecdc", "ILI_ecdc", "ILI_fever"), debug = FALSE) {

	#
	df_data<-arrange(df_data,person_id,report_date)

	## sometimes an episode (e.g. ILI_ecdc) starts with different symptoms (e.g. ILI_ecdc == FALSE)
	## the idea is to flag all report belonging to the same bout to TRUE when their is at least one symptom reported.
	episode <- paste(symptom, "episode", sep = "_")
	symptom_bool <- paste(symptom, "bool", sep = "_")

	df_data[, symptom_bool] <- df_data[, symptom]
	df_data[, symptom_bool][is.na(df_data[, symptom_bool])] <- FALSE
	df_data[, episode] <- NA

	#count only 
	df_2count<-subset(df_data,person_id%in%person_id[ind_2count])
	df_keep<-subset(df_data,!person_id%in%person_id[ind_2count])

	df_count <- ddply(df_2count, c("person_id"), function(df) {


		return(df)
	}, .progress = "text")

	df_data<-rbind(df_keep, df_count )
	df_data<-df_data[setdiff(names(df_data),c(symptom_bool, "same_bout_bool"))]
	return(df_data)

}

#' Trim longitudinale episode when start by one or more report without ARI symptom
#' @param df_episode data frame containing only one episode to trim
#' @inheritParams clean_data_automatically
#' @note we keep the last report with ARI=F unless time between this report and the next one is greater than delay_in_reporting
#' @export
#'
trim_first_report <- function(df_episode, delay_in_reporting = 10) {

	tmp <- rle(df_episode$ARI_ecdc_bool)
	F_index <- which(!tmp$values)

	if (!c(1) %in% F_index) {
		return(df_episode)
	}

	#check dist between report dates
	i_lastF <- tmp$lengths[1]
	i_firstT <- i_lastF + 1
	df_episode$same_bout[1:i_lastF] <- NA
	i_start <- i_lastF
	if (with(df_episode, as.numeric(report_date[i_firstT] - report_date[i_lastF]) > delay_in_reporting)) {
		#remove last FFF
		df_episode$same_bout[i_firstT] <- NA
		i_start <- i_firstT
	}

	#check symptom_start
	if (with(df_episode, !is.na(symptom_start[i_start]) & (as.numeric(report_date[i_start] - symptom_start[i_start]) > 
		delay_in_reporting))) {
		df_episode$symptom_start <- NA
	}

	return(df_episode)

}


#' Trim longitudinale episode when end by one or more report without ARI symptom
#' @param df_episode data frame containing only one episode to trim
#' @inheritParams clean_data_automatically
#' @note we keep the first ARI==F report at the end. If it was reported too long after the previous ARI==T report we consider that the participant is not ill anymore (i.e. symptom_end date should be before the last report).
#' @export
#'
trim_last_report <- function(df_episode, delay_in_reporting = 10) {

	tmp <- rle(df_episode$ARI_ecdc_bool)
	F_index <- which(!tmp$values)
	n <- length(tmp$values)

	if (!c(n) %in% F_index) {
		return(df_episode)
	}

	#check dist between report dates
	i_end <- sum(tmp$lengths[1:(n - 1)]) + 1
	i_lastT <- i_end - 1
	if(tmp$lengths[n]>1){
		df_episode$same_bout[(i_end + 1):nrow(df_episode)] <- NA		
	}
	if (with(df_episode, as.numeric(report_date[i_end] - report_date[i_lastT]) > delay_in_reporting)) {
		#remove still_ill in last FFF
		df_episode$still_ill[i_end] <- FALSE
	}

	#check symptom_end
	my_symptom_end <- na.omit(unique(df_episode$symptom_end))
	if (length(my_symptom_end) & with(df_episode, any(as.numeric(report_date[i_end] - my_symptom_end) <= delay_in_reporting))) {
		ind <- with(df_episode,which(as.numeric(report_date[i_end] - my_symptom_end) <= delay_in_reporting))
		df_episode$symptom_end[i_end] <- max(my_symptom_end[ind])
	}

	return(df_episode)

}



#'  longitudinale episode when start by one or more report without ARI symptom
#' @param df_episode data frame containing only one episode to trim
#' @inheritParams clean_data_automatically
#' @note when one FFF in the middle => keep it unless dist with next !FFF is >delay_rep
#' when several FFF in the middle => put first one in first episode and last one as the first report of second episode (GOTO 1)
#' @export
#'
trim_middle_report <- function(df_episode, delay_in_reporting = 10) {

	tmp <- rle(df_episode$ARI_ecdc_bool)
	F_index <- which(!tmp$values)
	n<-length(tmp$values)
	F_index<-F_index[!F_index%in%c(1,n)]

	#return if nothing
	if (!length(F_index)) {
		return(df_episode)
	}

	#check middle F
	for(F_i in F_index){

		if(tmp$lengths[F_i]==1){
			i_middleF<-sum(tmp$lengths[1:F_i])
			i_nextT<-i_middleF+1
			if(with(df_episode,as.numeric(report_date[i_nextT]-report_date[i_middleF])> delay_in_reporting)){
				#cut
				df_episode$same_bout[i_nextT]<-NA			
				df_episode1<-trim_last_report(df_episode[1:i_middleF,], delay_in_reporting)
				df_episode2<-df_episode[i_nextT:nrow(df_episode),]
				df_episode<-rbind(df_episode1, df_episode2)
			}

		}else{
			i_firstF<-sum(tmp$lengths[1:(F_i-1)])+1			
			df_episode1<-trim_last_report(df_episode[1:i_firstF,], delay_in_reporting)
			df_episode2<-trim_first_report(df_episode[(i_firstF+1):nrow(df_episode),], delay_in_reporting)
			df_episode<-rbind(df_episode1, df_episode2)

		}


	}

	return(df_episode)

}


#' Define and enumerate all distinct episodes
#' @param df_data a \link{data.frame} containing FluSurvey data.
#' @param symptom a \link{vector} of characters containing the names of the symptom episode. Should match the names of \code{df_data}.
#' @param CR_as_TRUE logical, shall we replace CR (can't remember) by TRUE when participants are asked whether current illness is the same bout as the one reported the previous time.
#' @inheritParams clean_data_automatically
#' @return \code{df_data} with additional variables \code{symptom_episode} which enumerate each episodes. For instance \code{1 1 NA NA 2 NA 3 3} indicates that the first episode lasted the two first reports, the 2nd episode occurred on the 5th report, etc.
#' @note an episode should always start with \code{same_bout} equal \code{NA} or \code{"no"} and (potentially) be followed by a series of \code{same_bout} equal \code{"yes"}.
#' Reports with \code{same_bout} equal \code{"CR"} (i.e. can't remember) is treated as specified by \code{CR_as_TRUE}.
#' A \code{symptom} episode is defined by all the reports belonging to the same bout and with at least one occurence of \code{symptom} reported.
#' @export
#' @import plyr
#'
define_episode <- function(df_data, symptom = c("ARI_ecdc", "ILI_ecdc", "ILI_fever"), CR_as_TRUE = FALSE, delay_in_reporting = 10) {
	print(paste("Counting episodes for",symptom))
	flush.console()
	#
	#count
	df_data <- count_episode(df_data, symptom, CR_as_TRUE)

	#clean
	df_data[, symptom_bool] <- df_data[, symptom]
	df_data[, symptom_bool][is.na(df_data[, symptom_bool])] <- FALSE

	tmp<-unique(subset(df_data,!ARI_ecdc_bool & !is.na(ARI_ecdc_episode) & still_ill,select=c(person_id, ARI_ecdc_episode)))

	df_2clean<-match_df(df_data,tmp)
	df_keep<-diff_df(df_data,df_2clean)


	df_clean<-ddply(df_2clean,c("person_id","ARI_ecdc_episode"),function(df){

		df1<-df
		df<-trim_first_report(df, delay_in_reporting)
		df<-trim_last_report(df, delay_in_reporting)
		df<-trim_middle_report(df, delay_in_reporting)
		df2<-df
		if(!identical(df1,df2)){
			print(df1[,c(1:2,4:7,12,47)])
			cat("###############################\n")
			print(df2[,c(1:2,4:7,12,47)])
			cat("###############################\n")
			cat("###############################\n")
			cat("###############################\n")	
		}

		return(df)
	})
	#recount


	#bind


	if(0){


		tmp3<-ddply(tmp2,c("person_id","ARI_ecdc_episode"),function(df){print(df[,c(1,3,5:9,11:12,2,44)])})


		#for 4) and 5), last FFF is still_ill=T, in this case, symptom_end should be between this report and the next one unless dist between report >delay
#check if there is a symptom_end date in the removed line. If so: erase it unless it is before the new last report | <new_last_report +delay
# for 1) and 2) check if dist(new_first_report - symptom_start)<report_delay, if not : erase symptom_start
	}

	return(df_data)
}


compute_baseline_health_score <- function(df_data,fun_summary=c("mean","median")) {

	fun_summary <- match.arg(fun_summary)
	fun_summary <- switch (fun_summary, mean = mean, median = median)

	df <- subset(df_data, !is.na(health_score) & (!(!is.na(n_bout) | still_ill %in% c(TRUE) | still_off %in% c(TRUE) | !is.na(cause)) | 
		(!is.na(symptom_end) & symptom_end < report_date) | (!still_ill & (is.na(n_bout) | position_bout == length_bout))))

	df_baseline <- aggregate(df$health_score, list(person_id = df$person_id), fun_summary)

	df_baseline <- rename(df_baseline, c(x = "baseline_health_score"))

	return(df_baseline)
}

# time to report vs wday for S_start, S_end. dateuration of symptom (by age and type of symptom). baseline heatl score.
compute_empirical_distribution_for_missing_data <- function(df_data, df_profile, symptom_ordered = c("ARI_ecdc", "ILI_ecdc", "ILI_fever"), W_S_start = c("W_S_start_too_far", "W_S_start_before_previous_report", "W_S_start_after_S_end", "W_S_start_wrong"), W_S_end = c("W_S_end_too_far", "W_S_end_before_previous_report", "W_S_start_after_S_end", "W_S_end_wrong"),S_duration_predictors=c("symptom","age_group"), S_duration_max=30,plot_check = FALSE, dir_plot = "./pdf/data_processing", remove_outlier=F,fun_summary_baseline=fun_summary_baseline,...) {


	symptom_ordered <- ordered(symptom_ordered)

	########
	#time between first report and date of symptom onset, by week-day of report
	tmp <- subset(df_data, !is.na(symptom_start) & position_bout == 1)
	ind <- which(apply(tmp[, W_S_start], 1, any))
	df_data_S_start_ok <- mutate(tmp[-ind, ], wday_report = wday(report_date, label = T,abbr=FALSE), variable = "time_to_report_S_start", value = as.numeric(report_date - symptom_start))
	df_time_to_report <- df_data_S_start_ok[c("person_id", "wday_report", "variable", "value")]

	#time between last report and date of symptom end, by wday of report
	tmp <- subset(df_data, !is.na(symptom_end) & position_bout == length_bout)
	ind <- which(apply(tmp[, W_S_end], 1, any))
	df_data_S_end_ok <- mutate(tmp[-ind, ], wday_report = wday(report_date, label = T,abbr=FALSE), variable = "time_to_report_S_end", value = as.numeric(report_date - symptom_end))
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
		df_outlier <- lme_regression(df_reg, formula = "S_duration2~symptom+age_group+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 20, test_dist = F, CI_interval="confidence",suffix= paste0("full_info_SDmax=",S_duration_max),plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group",use_facet_grid=T,facet_formula="gender~smoke_bool") 
		df_remove <- match_df(df_reg, df_outlier,on=c("person_id","n_bout"))
		df_reg_wo <- diff_df(df_reg, df_remove)
		df_outlier_wo <- lme_regression(df_reg_wo, formula = "S_duration2~symptom+age_group+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 20, test_dist = F, CI_interval="confidence",suffix= "full_info_wo",plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group",use_facet_grid=T,facet_formula="gender~smoke_bool") 
		df_outlier_wo_2 <- lme_regression(df_reg_wo, formula = "S_duration2~symptom+age_group+smoke_bool+is_risk+gender",transformation = "boxcox", max_response = 20, test_dist = F, CI_interval="confidence",suffix= "full_info_wo",plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group",use_facet_grid=T,facet_formula="gender~smoke_bool") 
		df_outlier_wo_3 <- lme_regression(df_reg_wo, formula = "S_duration2~symptom+age_group+smoke_bool+gender",transformation = "boxcox", max_response = 20, test_dist = F, CI_interval="confidence",suffix= "full_info_wo",plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group",use_facet_grid=T,facet_formula="gender~smoke_bool") 
		df_outlier_wo_4 <- lme_regression(df_reg_wo, formula = "S_duration2~symptom+age_group+smoke_bool",transformation = "boxcox", max_response = 20, test_dist = F, CI_interval="confidence",suffix= "full_info_wo",plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group",use_facet_wrap=T,facet_formula="~smoke_bool") 
		df_outlier_wo_5 <- lme_regression(df_reg_wo, formula = "S_duration2~symptom+age_group",transformation = "boxcox", max_response = 20, test_dist = F, CI_interval="prediction",suffix= "full_info_wo",plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group") 
	}
	df_reg_age <- mutate(df_reg,age_group2=revalue(age_group,c("0-17"="0-64","18-44"="0-64","45-64"="0-64")))
	#df_reg_wo_age <- mutate(df_reg_wo,age_group2=revalue(age_group,c("0-17"="0-64","18-44"="0-64","45-64"="0-64")))
	#df_outlier_wo_6 <- lme_regression(df_reg_wo_age, formula = "S_duration2~symptom+age_group2",transformation = "boxcox", max_response = 20, test_dist = T, CI_interval="prediction",suffix= "full_info_wo",plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group2") 
	#tmp <- lme_regression(df_reg_wo_age, formula = "S_duration2~symptom+age_group2",transformation = "boxcox", max_response = 20, test_dist = T, CI_interval="prediction",suffix= "full_info_wo",plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="age_group2",return_outliers=F) 
	tmp <- lme_regression(df_reg_age, formula = "S_duration2~symptom+age_group2",transformation = "boxcox", max_response = 20, test_dist = F, CI_interval="prediction",suffix= paste0("full_info_SDmax=",S_duration_max),plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",x_lab="symptom severity",fill_var="age_group2",fill_lab="age group",y_lab="symptom duration (in days)",return_outliers=remove_outlier) 
	
	if(remove_outlier){
		df_remove <- match_df(df_reg_age, tmp,on=c("person_id","n_bout"))
		df_reg_age <- diff_df(df_reg_age, df_remove)
		tmp <- lme_regression(df_reg_age, formula = "S_duration2~symptom+age_group2",transformation = "boxcox", max_response = 20, test_dist = T, CI_interval="prediction",suffix= paste0("full_info_SDmax=",S_duration_max,"_wo"),plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",x_lab="symptom severity",fill_var="age_group2",fill_lab="age group",y_lab="symptom duration (in days)",return_outliers=F) 
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

		df_outlier <- lm_regression(df_reg_baseline, formula = "baseline_health_score~age_group+smoke_bool+is_risk+vaccine+gender",transformation = "logit", max_response = 100, test_dist = F, CI_interval="confidence",suffix= "full_info",df_data= df_data,df_profile=df_profile,h_var="age_group",x_var="age_group",fill_var="gender",use_facet_grid=T,facet_formula="is_risk~smoke_bool") 
		df_outlier2 <- lm_regression(df_reg_baseline, formula = "baseline_health_score~age_group+smoke_bool+is_risk+gender",transformation = "logit", max_response = 100, test_dist = T, CI_interval="confidence",suffix= "full_info",df_data= df_data,df_profile=df_profile,h_var="age_group",x_var="is_risk",fill_var="smoke_bool",use_facet_grid=T,facet_formula="age_group~gender") 
		df_outlier3 <- lm_regression(df_reg_baseline, formula = "baseline_health_score~age_group+smoke_bool+is_risk",transformation = "logit", max_response = 100, test_dist = T, CI_interval="confidence",suffix= "full_info",df_data= df_data,df_profile=df_profile,h_var="age_group",x_var="is_risk",fill_var="smoke_bool",use_facet_wrap=T,facet_formula="~age_group") 
		df_reg_baseline_age <- mutate(df_reg_baseline,age_group2=revalue(age_group,c("0-17"="0-64","18-44"="0-64","45-64"="0-64")))
		df_outlier4 <- lm_regression(df_reg_baseline_age, formula = "baseline_health_score~age_group2+smoke_bool+is_risk",transformation = "logit", max_response = 100, test_dist = T, CI_interval="prediction",suffix= "full_info",df_data= df_data,df_profile=df_profile,h_var="age_group2",x_var="is_risk",fill_var="smoke_bool",use_facet_wrap=T,facet_formula="~age_group2") 
	}

	df_reg_baseline <- join(df_baseline,df_profile,by="person_id")
	df_reg_baseline <- mutate(df_reg_baseline,person_id_num=as.numeric(as.factor(person_id)),age_group2=revalue(age_group,c("0-17"="0-64","18-44"="0-64","45-64"="0-64")),smoke_bool=smoke_as_logical(smoke))

	#remove baseline==100 and reinflate later
	# df_inflate <- na.omit(ddply(df_reg_baseline,c("smoke_bool","is_risk","age_group2"),function(df){

		# 	p_100 <- sum(df$baseline_health_score==100)/nrow(df)

		# 	return(data.frame(p_100=p_100))

		# }))

	df_reg_baseline <- subset(df_reg_baseline, baseline_health_score<100) 

	tmp <- lm_regression(df_reg_baseline, formula = "baseline_health_score~age_group2+smoke_bool+is_risk",transformation = "logit", max_response = 100, test_dist = T, CI_interval="prediction",suffix= "full_info",df_data= df_data,df_profile=df_profile,h_var="age_group2",x_var="is_risk",fill_var="smoke_bool",use_facet_wrap=T,facet_formula="~age_group2",return_outliers= remove_outlier) 
	if(remove_outlier){
		df_remove <- match_df(df_reg_baseline, tmp,on=c("person_id"))
		df_reg_baseline <- diff_df(df_reg_baseline, df_remove)
		tmp <- lm_regression(df_reg_baseline, formula = "baseline_health_score~age_group2+smoke_bool+is_risk",transformation = "logit", max_response = 100, test_dist = T, CI_interval="prediction",suffix= "full_info_wo",df_data= df_data,df_profile=df_profile,h_var="age_group2",x_var="is_risk",fill_var="smoke_bool",use_facet_wrap=T,facet_formula="~age_group2",return_outliers=F) 
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
		df_outliers <- lme_regression(df_reg, formula = paste("S_duration", rhs, sep = "~"), transformation = "boxcox", max_response = 25, test_dist = T, CI_interval = "confidence", suffix = "complete", plot_outlier_TS = F, df_data = df_data, df_profile = df_profile, x_var = "symptom", fill_var = "age_group", use_facet_wrap = F, use_facet_grid = F, facet_formula = NULL)
	}


	return(list(S_duration = df_dist_S_duration_bc, time_to_report = df_dist_time_to_report, baseline_health_score = df_dist_baseline_reg))


}

sample_S_start_using_S_duration <- function(ans, ans_profile, df_data_id, df_dist_S_duration, df_dist_time_to_report_S_start,my_warning="W_approximate_S_start") {



	S_duration_min <- as.numeric(ans$symptom_end - ans$first_report_date)

	S_duration_max <- Inf
	prev_report_date <- -Inf

	ind <- which(df_data_id$report_date < ans$first_report_date)
	if (length(ind)) {
		#Symptom can't start before previous report
		prev_report_date <- max(df_data_id$report_date[ind])
		S_duration_max <- as.numeric(ans$symptom_end - prev_report_date)
	}


	#sample S_duration
	df_sample <- subset(df_dist_S_duration, age_group== ans_profile$age_group & symptom == ans$symptom & S_duration >= S_duration_min & S_duration <= S_duration_max)

	if (nrow(df_sample)) {
		S_duration_sampled <- ifelse(nrow(df_sample)==1,df_sample$S_duration,sample(df_sample$S_duration, 1, prob = df_sample$freq))
		ans$symptom_start <- ans$symptom_end - S_duration_sampled
		ans[, my_warning] <- T

	} else if (!is.null(df_dist_time_to_report_S_start)) {
		#S_duration is not in empirical distribution	
		#if no sample then use df_dist_time_to_report_S_start
		ans <- sample_S_start_using_time_to_report(ans, df_data_id, df_dist_time_to_report_S_start)
	}

	return(ans)
}

sample_S_start_using_time_to_report <- function(ans, df_data_id, df_dist_time_to_report_S_start,my_warning="W_approximate_S_start") {

	prev_report_date <- -Inf

	ind <- which(df_data_id$report_date < ans$first_report_date)
	if (length(ind)) {
		#Symptom can't start before previous report
		prev_report_date <- max(df_data_id$report_date[ind])
	}

	time_to_report_max <- as.numeric(ans$first_report_date - prev_report_date)
	df_sample <- subset(df_dist_time_to_report_S_start, wday_report == wday(ans$first_report_date, label = T) & time_to_report_S_start <= 
		time_to_report_max)
	if (nrow(df_sample)) {
		time_to_report_sampled <- ifelse(nrow(df_sample)==1,df_sample$time_to_report_S_start,sample(df_sample$time_to_report_S_start, 1, prob = df_sample$freq))
		ans$symptom_start <- ans$first_report_date - time_to_report_sampled
		ans[, my_warning] <- T
	}
	return(ans)
}


sample_S_end_using_time_to_report <- function(ans, df_data_id, df_dist_time_to_report_S_end,my_warning="W_approximate_S_end") {

	if(!is.na(ans$still_ill) && ans$still_ill){
		stop("sampling method not relevant for episode ending by still ill:",ans)
	}
	prev_report_date <- -Inf

	ind <- which(df_data_id$report_date < ans$last_report_date)
	if (length(ind)) {
		#Symptom can't end before previous report
		prev_report_date <- max(df_data_id$report_date[ind])
	}

	if(prev_report_date<ans$first_report_date){
		stop("episode should have at least 2 report dates:",ans)
	}

	time_to_report_max <- as.numeric(ans$last_report_date - prev_report_date)
	df_sample <- subset(df_dist_time_to_report_S_end, wday_report == wday(ans$last_report_date, label = T) & time_to_report_S_end <= 
		time_to_report_max)
	if (nrow(df_sample)) {
		time_to_report_sampled <- ifelse(nrow(df_sample)==1,df_sample$time_to_report_S_end,sample(df_sample$time_to_report_S_end, 1, prob = df_sample$freq))
		ans$symptom_end <- ans$last_report_date - time_to_report_sampled
		ans[, my_warning] <- T
	}
	return(ans)
}


sample_S_end_using_S_duration <- function(ans, ans_profile ,df_data_id, df_dist_S_duration, my_warning= W_approximate_S_end) {


	if (!is.na(ans$still_ill) && ans$still_ill) {
		S_duration_min <- as.numeric(ans$last_report_date - ans$symptom_start)
		#find next report
		ind <- which(df_data_id$report_date > ans$last_report_date)
		if (length(ind)) {
			#Symptom can't end after next report
			next_report_date <- min(df_data_id$report_date[ind])
			S_duration_max <- as.numeric(next_report_date - ans$symptom_start)
		} else {
			S_duration_max <- Inf
		}

	} else {
		S_duration_max <- as.numeric(ans$last_report_date - ans$symptom_start)
		#find previous report	
		ind <- which(df_data_id$report_date < ans$last_report_date)
		if (length(ind)) {
			#Symptom can't end before previous report
			prev_report_date <- max(df_data_id$report_date[ind])
			S_duration_min <- max(c(0,as.numeric(prev_report_date - ans$symptom_start)))
		} else {
			#this should normally never happen ;-)
			print(ans)
			print(df_data_id)
			flush.console()
			stop("probleme 1 in summarize_episode")
		}
	}

#sample S_duration
	df_sample <- subset(df_dist_S_duration, age_group== ans_profile$age_group & symptom == ans$symptom & S_duration >= S_duration_min & S_duration <= S_duration_max)

	if (nrow(df_sample)) {
		S_duration_sampled <- ifelse(nrow(df_sample)==1,df_sample$S_duration, sample(df_sample$S_duration, 1, prob = df_sample$freq))
		ans$symptom_end <- ans$symptom_start + S_duration_sampled
		ans[, my_warning] <- T

	} else {
	#S_duration is not in empirical distribution	
	#take last report date + warning
		cat("\n no empirical sample for symptom=", as.character(ans$symptom),"age_group=",as.character(ans_profile$age_group), "min=", S_duration_min, "max=", S_duration_max,"\n")
		flush.console()
	}
	return(ans)
}

#' Summarize episode information from longitudinal data.
#' @export
#' @import ggplot2 RColorBrewer grid
#' @importFrom plyr count ldply match_df ddply mutate
#' @importFrom lubridate ceiling_date wday
#'
summarize_episode <- function(df_data, df_profile, symptom_ordered = c("ARI_ecdc", "ILI_ecdc", "ILI_fever"), fun_summary_baseline = fun_summary_baseline, approx_W_S_start = c("W_S_start_too_far", "W_S_start_before_previous_report", "W_S_start_after_S_end", "W_S_start_wrong"), approx_W_S_end = c("W_S_end_too_far", "W_S_end_before_previous_report", "W_S_start_after_S_end", "W_S_end_wrong"), missing_S_start_date=c("sample_using_S_duration","sample_using_time_to_report","sample_mix","use_report_date"), missing_S_end_date_not_ill=c("sample_using_S_duration","sample_using_time_to_report","use_report_date"), missing_S_end_date_still_ill=c("sample_using_S_duration","use_report_date"), df_dist_time_to_report_S_start=NULL, df_dist_time_to_report_S_end=NULL, df_dist_S_duration=NULL, df_dist_baseline_health_score=NULL) {

	missing_S_start_date <- match.arg(missing_S_start_date)
	missing_S_end_date_not_ill <- match.arg(missing_S_end_date_not_ill)
	missing_S_end_date_still_ill <- match.arg(missing_S_end_date_still_ill)

	symptom_ordered <- ordered(symptom_ordered)

	W_ASS <- "W_approximate_S_start"
	W_ASE <- "W_approximate_S_end"
	W_ABHS <- "W_approximate_baseline_health_score"

	var_names <- names(df_data)

	#take always last
	var_always_last <- c("still_ill", "still_off")

	#ordered factor (always take the highest value)
	var_ordered <- var_names[vapply(df_data, function(x) all(is.ordered(x)),logical(1))]
	var_ordered_min <- unlist(sapply(c("time_visit","time_phone","AV"),grep,x=var_ordered,value=T),use.names=F)
	var_ordered_max <- setdiff(var_ordered, c(var_always_last,var_ordered_min))


	#boolean values 
	x <- vapply(df_data, function(x) all(is.logical(x)),logical(1))
	var_bool <- setdiff(var_names[x], var_always_last)

	#remaining
	#var_remaining<-setdiff(var_names,c(var_always_last, var_ordered, var_bool))

	#compute baseline health_score: focus on non ill reports unless symptom_end < report_date
	df_baseline <- compute_baseline_health_score(df_data,fun_summary_baseline)	
	df_baseline[, W_ABHS] <- F

	#empirical distribution

	#summarize bouts	
	df_2summarize <- subset(df_data, !is.na(n_bout))
	df_data_match <- match_df(df_data, df_2summarize,on="person_id")

	if(!is.null(df_dist_baseline_health_score)){
		#sample baseline for those who don't have one
		id_2sample<-setdiff(df_2summarize$person_id, df_baseline$person_id)

		df_baseline_2sample_data<-subset(df_data,person_id%in%id_2sample)

		df_baseline_2sample_profile <- subset(df_profile,person_id%in%id_2sample)
		df_baseline_2sample_profile <- mutate(df_baseline_2sample_profile,smoke_bool=smoke_as_logical(smoke))
		df_baseline_NA <- subset(df_baseline_2sample_profile, is.na(smoke_bool) | is.na(is_risk) | is.na(age_group))	
		cat("baseline health-score can't be sampled for",nrow(df_baseline_NA),"participants due to missing value for predictors\n")
		df_baseline_2sample_profile <- diff_df(df_baseline_2sample_profile, df_baseline_NA)

		df_baseline_sample <- ddply(df_baseline_2sample_profile,"person_id",function(df){

			df_data_id <- match_df(df_baseline_2sample_data,df,on="person_id")
			health <- df_data_id$health_score
			health[is.na(health)]<-0

			df_sample <- subset(df_dist_baseline_health_score, smoke_bool== df$smoke_bool & is_risk== df$is_risk & age_group== df$age_group & baseline_health_score > max(health))
			if (nrow(df_sample)) {
				df$baseline_health_score <- ifelse(nrow(df_sample) > 1, sample(df_sample$baseline_health_score, 
					1, prob = df_sample$freq), df_sample$baseline_health_score)
			} else {
				#dist max =99.5
				print(df)
				cat("baseline at 100 for max(health)=",max(health),"\n")
				flush.console()
				df$baseline_health_score <- 100
			}

			return(df)
		},.progress="text")
		df_baseline_sample[, W_ABHS]<-T	
		df_baseline_sample <- df_baseline_sample[names(df_baseline)]

		df_baseline_NA$baseline_health_score <- NA
		df_baseline_NA[, W_ABHS]<-F
		df_baseline_NA <- df_baseline_NA[names(df_baseline)]


		#bind
		df_baseline<-do.call("rbind",list(df_baseline, df_baseline_sample, df_baseline_NA))
	}

	cat("Summarize",nrow(unique(df_2summarize[,c("person_id","n_bout")])),"episodes for",length(unique(df_2summarize$person_id)),"participants\n")

	flush.console()

	df_episode <- ddply(df_2summarize, c("person_id", "n_bout"), function(df) {

		df <- arrange(df,report_date)
		ans <- unique(df[, c("person_id", "n_bout")])
		ans_profile <- subset(df_profile, person_id == ans$person_id)
		#report_date
		tmp <- range(df$report_date)
		ans$first_report_date <- tmp[1]
		ans$last_report_date <- tmp[2]

		#last
		ans[, var_always_last] <- df[nrow(df), var_always_last]
		#logical		
		ans[, var_bool] <- vapply(df[, var_bool], function(x) any_na_rm(x),FUN.VALUE=c(TRUE))
		#ordered max	
		ans[, var_ordered_max] <- llply(df[, var_ordered_max], max_ordered_na_rm)
		#ordred min	
		ans[, var_ordered_min] <- llply(df[, var_ordered_min], min_ordered_na_rm)

		#n_bout
		ans$n_bout <- df$n_bout[1]
		ans$length_bout <- df$length_bout[1]

		#type of symptom
		ans$symptom <- max(symptom_ordered[as.logical(ans[as.character(symptom_ordered)])])

		#symptom start and end
		ans$symptom_start <- df$symptom_start[1]
		ans$symptom_end <- rev(df$symptom_end)[1]

		#approx symptom_start?
		ans[, W_ASS] <- F
		#condition: 
		# no symptom start OR symptom start has a warning
		# AND
		# (
		# length of the bout is > 1 report [=> remove all past episodes with only one report]
		# OR 
		# no symptom end AND still ill [=> except if still ill when reporting]
		# )
#
		valid_S_start <- (!is.na(ans$symptom_start) && all(!as.logical(ans[approx_W_S_start])))
		approx_S_start <- ((!valid_S_start) && (ans$length_bout > 1 || (is.na(ans$symptom_end) && !is.na(ans$still_ill) && ans$still_ill)))

		if (is.na(approx_S_start)) {
			print(ans$symptom_start)
			print(ans[approx_W_S_start])
			print(ans$length_bout)
			print(ans$symptom_end)
			print(ans$still_ill)
		}

#approx symptom_end?
		ans[, W_ASE] <- F
#condition: 
# no symptom end OR symptom end has a warning
# AND
# length of the bout is > 1 report OR still ill [=>remove all past episodes with only one report, except if still ill when reporting]
#
		valid_S_end <- (!is.na(ans$symptom_end) && all(!as.logical(ans[approx_W_S_end])))
		approx_S_end <- ((!valid_S_end) && (ans$length_bout > 1 || (!is.na(ans$still_ill) && ans$still_ill)))

		if (is.na(approx_S_end)) {
			print(ans$symptom_end)
			print(ans[approx_W_S_end])
			print(ans$length_bout)
			print(ans$still_ill)
		}

#use report date	
		if(approx_S_start && missing_S_start_date=="use_report_date"){
			ans$symptom_start <- ans$first_report_date
			ans[, W_ASS] <- T
		}

		if(approx_S_end && !is.na(ans$still_ill) && ans$still_ill && missing_S_end_date_still_ill=="use_report_date"){
			ans$symptom_end <- ans$last_report_date
			ans[, W_ASE] <- T				
		}

		if(approx_S_end && (is.na(ans$still_ill) || !ans$still_ill) && missing_S_end_date_not_ill=="use_report_date"){
			ans$symptom_end <- ans$last_report_date
			ans[, W_ASE] <- T				
		}

		if((approx_S_start || approx_S_end) && (missing_S_start_date%in%c("sample_using_S_duration","sample_using_time_to_report","sample_mix") || missing_S_end_date_not_ill%in%c("sample_using_S_duration","sample_using_time_to_report") || missing_S_end_date_still_ill%in%c("sample_using_S_duration"))){
			df_data_id <- match_df(df_data_match, df, on = "person_id")
		}

#use time to report
		if(approx_S_start && missing_S_start_date=="sample_using_time_to_report"){						
			ans <- sample_S_start_using_time_to_report(ans, df_data_id, df_dist_time_to_report_S_start, my_warning= W_ASS)			
		}

		if(approx_S_end && (is.na(ans$still_ill) || !ans$still_ill) && missing_S_end_date_not_ill=="sample_using_time_to_report"){
			ans <- sample_S_end_using_time_to_report(ans, df_data_id, df_dist_time_to_report_S_end, my_warning= W_ASE)			
		}


#use S_duration		
#1) approx S_start and valid S_end => sample S_start using S_duration
		if(approx_S_start && valid_S_end && !is.null(df_dist_S_duration)){		
			if(missing_S_start_date=="sample_using_S_duration"){
				ans <- sample_S_start_using_S_duration(ans, ans_profile,df_data_id, df_dist_S_duration, df_dist_time_to_report_S_start=NULL, my_warning= W_ASS)				
			}
			if(missing_S_start_date=="sample_mix"){
				ans <- sample_S_start_using_S_duration(ans, ans_profile ,df_data_id, df_dist_S_duration, df_dist_time_to_report_S_start, my_warning= W_ASS)				
			}

		}

#2) approx S_end and valid S_start => sample S_end using S_duration
		if(approx_S_end && valid_S_start && !is.null(df_dist_S_duration)){

			if(!is.na(ans$still_ill) && ans$still_ill && missing_S_end_date_still_ill=="sample_using_S_duration"){
				ans <- sample_S_end_using_S_duration(ans, ans_profile, df_data_id, df_dist_S_duration, my_warning= W_ASE)
			}
			if((is.na(ans$still_ill) || !ans$still_ill) && missing_S_end_date_not_ill=="sample_using_S_duration"){
				ans <- sample_S_end_using_S_duration(ans, ans_profile, df_data_id, df_dist_S_duration, my_warning= W_ASE)
			}

		}	

#3)	approx S_start and S_end => sample S_start using time to report and then go to 2)
		if(approx_S_start && approx_S_end && !is.null(df_dist_time_to_report_S_start) && !is.null(df_dist_S_duration)){
			if(missing_S_start_date=="sample_mix"){						
				ans <- sample_S_start_using_time_to_report(ans, df_data_id, df_dist_time_to_report_S_start, my_warning= W_ASS)			
			}
			if(missing_S_start_date%in%c("sample_mix","sample_using_time_to_report")){
				if(!is.na(ans$still_ill) && ans$still_ill && missing_S_end_date_still_ill=="sample_using_S_duration"){
					ans <- sample_S_end_using_S_duration(ans, ans_profile ,df_data_id, df_dist_S_duration, my_warning= W_ASE)
				}
				if((is.na(ans$still_ill) || !ans$still_ill) && missing_S_end_date_not_ill=="sample_using_S_duration"){
					ans <- sample_S_end_using_S_duration(ans, ans_profile ,df_data_id, df_dist_S_duration, my_warning= W_ASE)
				}
			}
		}	

#QALD loss
		ans[, c("baseline_health_score", "min_health_score", "QALD_loss")] <- rep(NA, 3)

#baseline
#option 1: take the average baseline over non-ill reports
		ans[c("baseline_health_score",W_ABHS)] <- subset(df_baseline, person_id == ans$person_id)[,c("baseline_health_score", W_ABHS)]

#TODO: option 2: take the health score immediately following (or preceding) the ill reports.
#this more like in AJ's paper

#min health score			
		ill_HS <- df$health_score
		remove_last_report <- (!is.na(ans$symptom_end) && ans$symptom_end < ans$last_report_date)
		if (remove_last_report) {
	#remove last report
			ill_HS <- ill_HS[-length(ill_HS)]
		}

		if (length(ill_HS) && any(!is.na(ill_HS))) {
			ans$min_health_score <- min(ill_HS, na.rm = T)
		}

		if_cond <- try(with(ans, is.na(baseline_health_score) || is.na(min_health_score) || 
			is.na(symptom_start) || is.na(symptom_end) || symptom_start > symptom_end || 
			symptom_start > first_report_date))

		if(inherits(if_cond,"try-error") || is.na(if_cond)){
			print(ans)
			print(df)
			flush.console()
			stop("error if if_cond")
		}	

#QALD loss
		if (if_cond) {
			ans$QALD_loss <- NA
		} else {
			ans$QALD_loss <- compute_QALD_loss(df, ans$baseline_health_score, ans$symptom_start, ans$symptom_end, symptom_end_at_baseline = TRUE)
		}

		return(ans)

	}, .progress = "text")

#	saveRDS(df_episode, file = paste(RSAVE, "df_episode_test.rds", sep = "/"))
#	df_episode <- readRDS(file = paste(RSAVE, "df_episode_test.rds", sep = "/"))

df_episode<- mutate(df_episode, symptom_duration=as.numeric(symptom_end - symptom_start))


#check
#all non NA time_off must correspond to change_routine "yes + off"
df_episode$change_routine[!is.na(df_episode$time_off)] <- "yes + off"

#all non NA time_visit/phone_X must correspond to visit/phone_X = TRUE
var_time<-as.vector(sapply(c("time_visit","time_phone"),grep,x=names(df_episode),value=T))
#var_bool<-as.vector(sapply(c("time_","time_phone_"),extract_string,v.string=var_time,position=2))

var_bool<-extract_string(var_time,"time_",2)
names(var_bool) <- NULL
df_episode[var_bool]<-(df_episode[var_bool] | !is.na(df_episode[var_time]))

#logical variable *_no
my_warnings <- grep("W_", names(var_bool), value = T)
var <- setdiff(var_bool, my_warnings)
var_no <- grep("_no", var,value=T)
var_grep <- unique(extract_string(var_no, "_", 1))
var_yes <- sapply(var_grep,grep,x=setdiff(var,var_no),value=T)
df_episode[, var_no] <- sapply(var_grep, function(v) {
	return(!as.logical(rowSums(df_episode[, var_yes[[v]]])))
})


return(df_episode)
}

#' Calculate the Quality Adjusted Life Day (QALY) lost using the health score.
#' @param df_data a \link{data.frame} containing FluSurvey data for a single participant and a single episode.
#' @param baseline value of health score without symptom.
#' @param symptom_start date of symptom onset (class 'Date').
#' @param symptom_end date of end of symptoms (class 'Date').
#' @param symptom_end_at_baseline if \code{TRUE}, then \code{baseline} value is used for the health score at symptom end date. Default is \code{FALSE} and the health score of the last report is used.
#' @return numeric value.
#' @export
#' @import pracma
#'
compute_QALD_loss<-function(df_data,baseline,symptom_start, symptom_end, symptom_end_at_baseline=FALSE, timeline_only = FALSE){
	
	
	stopifnot(length(unique(df_data$person_id))==1,length(unique(df_data$n_bout))==1,!is.na(baseline) & baseline>=0,!is.na(symptom_start),!is.na(symptom_end), symptom_start<=symptom_end, symptom_start<=df_data$report_date[1])


	#use report dates with available health score		
	y <- df_data$health_score
	x <- df_data$report_date[!is.na(y)]
	y <- y[!is.na(y)]

	#add symptom start date with baseline health_score
	if(timeline_only){
		#add baseline just the day before as some people report on symptom onset
		x <- c(symptom_start - 1, x)
	}else{
		x <- c(symptom_start, x)
	}

	y <- c(baseline, y)

	last_report_date <- max(df_data$report_date)
	if(symptom_end > last_report_date){
		#case where n_bout finish with still_ill==TRUE
		#add symptom end date with baseline health_score
		x <- c(x,symptom_end)
		y <- c(y,baseline)
	}else{
		#replace last report date by symptom end date 
		x[length(x)] <- symptom_end
		#with baseline health_score at recovery date? (only if symptom_end<last_report)
		if(symptom_end_at_baseline && (symptom_end < last_report_date)){
			y[length(y)] <- baseline		
		}

	}


	if(timeline_only){
		x <- as.numeric(x-symptom_start)
		y[y>baseline] <- baseline
		return(data.frame(time_SSS=x,health_score=y))
	}

	x<-as.numeric(x)		
	y <- baseline - y
	y[y < 0] <- 0
	y<-y/100
	ans <- trapz(x, y)

	return(ans)	
}

PN_integrand <- function(y, mu, sigma, lambda, K, r) {

	return(1/(K * sigma) * (lambda * y + 1)^(r/lambda) * dnorm((y - mu)/sigma))

}

PN_quantile <- function(p,mu,sigma,lambda, K){


	if(lambda > 0){
		V_p<-1-(1-p)*K
		return((lambda*(sigma*qnorm(V_p)+mu)+1)^(1/lambda))

	}else if(lambda<0){
		return((lambda*(sigma*qnorm(p)+mu)+1)^(1/lambda))

	}else{
		return(exp(mu+sigma*qnorm(p)))
	}

}

bc_inverse <- function(coef_bc){
	if(coef_bc!=0){
		return(function(x){(x * coef_bc + 1)^(1/coef_bc)})
	}else{
		return(exp)
	}

}


lme_regression <- function(df_reg, formula = "QALD_loss~symptom+age_group", transformation = c("boxcox","log"), max_response = 10, test_dist = F, CI_interval=c("confidence","prediction"), suffix="most_recent", plot_outlier_TS=T, df_data=NULL, df_profile=NULL, x_var="symptom", x_lab=x_var, fill_var="age_group", fill_lab=fill_var, y_lab=NULL, use_facet_wrap=F, use_facet_grid=F, facet_formula=NULL, return_outliers=TRUE, predict_all=FALSE, ...) {

	transformation <- match.arg(transformation)
	CI_interval <- match.arg(CI_interval)

	dir_res <- file.path(PDF,"lme", formula, transformation,suffix)
	dir.create(path = dir_res, recu = T)

	require(stringr)
	bc_formula <- as.formula(formula)

	response <- as.character(bc_formula[[2]])
	explanatory <- setdiff(all.vars(bc_formula), response)

	var_plotted <- c(x_var,fill_var)
	if(use_facet_wrap || use_facet_grid){
		tmp <- explanatory[str_detect(string=facet_formula,pattern=explanatory)]
		var_plotted <- unique(c(var_plotted,tmp))
	}

	var_different_plot <- setdiff(explanatory, var_plotted)
	df_different_plot <- na.omit(expand.grid(lapply(df_reg[var_different_plot],unique)))

	#transform ordered factor
	df_reg[explanatory] <- lapply(df_reg[explanatory], function(x) {
		if (is.ordered(x)) {
			x <- factor(x, ordered = F)
		}
		x
	})

	if ("pregnant" %in% explanatory) {

		df_reg$pregnant[is.na(df_reg$pregnant)] <- FALSE
		df_reg <- subset(df_reg,age_group%in%c("18-44"))
	}

	if(0){
		if ("smoke_bool" %in% explanatory) {

			df_reg <- subset(df_reg,age_group%in%c("18-44","45-64"))
		}
	}

	#remove explanatory with NA
	tmp <- df_reg[c("person_id_num",explanatory)]
	tmp <- na.omit(tmp)
	df_reg <- match_df(df_reg,tmp)

	if(all(explanatory%in%c("age_group","symptom"))){
		n_row <-length(unique(df_reg$age_group))
		n_col <- length(unique(df_reg$symptom))

	}else{
		n_row <- nrow(unique(df_reg[explanatory]))
		n_col <- round(sqrt(n_row))
		n_row <- ceiling(n_row/n_col)	
	}

	if(length(explanatory)==2 && !"age_group"%in%explanatory){
		x_plot <- setdiff(explanatory,"symptom")
	}else{
		x_plot <- "age_group"
	}
		#plot data
		#boxplot
	if("symptom"%in% explanatory){
		p <- ggplot(df_reg, aes_string(x = x_plot, y = response)) + facet_wrap(~symptom, scales = "free_y", ncol = n_col)
		p <- p + geom_boxplot() + ylim(c(0, max_response))
			#p<-p+geom_violin()+ylim(c(0,20))
		cairo_pdf(file.path(dir_res, "data_boxplot.pdf"), width = 8, height = 4)
		print(p)
		dev.off()

			#distribution
		p <- ggplot(df_reg, aes_string(x = response)) + facet_grid(paste(x_plot,"~symptom"), scales = "free")
		p <- p + geom_histogram(aes(y = ..density..), position = "identity", binwidth = 0.25, alpha = 0.5)
		p <- p + geom_density(alpha = 0.25) + xlim(c(0, max_response))
		cairo_pdf(file.path(dir_res, "data_histo_original.pdf"), width = 8, height = 4)
		print(p)
		dev.off()
	}

	response_trans<-paste(response,transformation,sep="_")

	if(transformation=="boxcox"){
		lm_bc <- powerTransform(bc_formula, data = df_reg)
		coef_bc<-lm_bc$roundlam
		print(paste("coef_bc =", coef_bc))
		df_reg[,response_trans] <- bcPower(df_reg[,response], coef_bc)
			#test_bc <- sf.test(df_reg[,response_trans)$p
		back_to_ori <- bc_inverse(coef_bc)

	}

	if(transformation=="log"){
		df_reg[,response_trans] <- log(df_reg[,response])
			#test_bc <- sf.test(df_reg[,response_trans)$p
		back_to_ori <- exp
	}

	if("symptom"%in% explanatory){	
		p <- ggplot(df_reg, aes_string(x = response_trans)) + facet_grid(paste(x_plot,"~symptom"), scales = "free")
		p <- p + geom_histogram(aes(y = ..density..), position = "identity", binwidth = 0.25, alpha = 0.5)
		p <- p + geom_density(alpha = 0.25)
		cairo_pdf(file.path(dir_res, paste0("data_histo_", transformation, ".pdf")), width = 8, height = 4)
		print(p)
		dev.off()
	}

	if(test_dist){
			#test of distribution
		require(nortest)
		dist_test <- dlply(df_reg, explanatory, function(df) {

			tmp <- df[, response_trans]
			p_val = try(sf.test(tmp)$p)

			best_fit <- data.frame(mean = mean(tmp), sd = sd(tmp),p_val=ifelse(inherits(p_val,"try-error"),NA,p_val) )
			best_fit$normal<-(best_fit$p_val>0.05)

			best_fit$sample_size <- nrow(df)
			if("symptom"%in% explanatory){
				p <- ggplot(df, aes_string(x = response_trans)) + facet_grid(paste(x_plot,"~symptom"), scales = "free")
				p <- p + geom_histogram(aes(y = ..density..), position = "identity", binwidth = 0.25, alpha = 0.5)
				p <- p + geom_density(alpha = 0.25)

				p <- p + stat_function(fun = dnorm, args = list(mean = best_fit$mean, sd = best_fit$sd), colour = "red")

				p <- p + scale_x_continuous("") + scale_y_continuous("")
				return(list(best_fit = best_fit, plot = p))
			}else{
				return(list(best_fit = best_fit))

			}

		}, .progress = "none")

		if("symptom"%in% explanatory){
			library(grid)
			cairo_pdf(file.path(dir_res, paste0("data_fit_",transformation, ".pdf")), width = 8, height = 8)
			grid.newpage()
			pushViewport(viewport(layout = grid.layout(n_row, n_col)))
			for (i in 1:n_row) {
				for (j in 1:n_col) {
					if(i*j<=length(dist_test)){
						print(dist_test[[i * j]]$plot, vp = vplayout(i, j))			
					}
				}
			}

			dev.off()
		}

		all_best_fit <- ldply(dist_test, function(x) x$best_fit)

		write.csv(all_best_fit, file = file.path(dir_res, "all_best_fit.csv"), quote = F, row.names = F)
	}

		#original lme
		#lme_formula <- bc_formula
		#lme_formula[[2]] <<- as.symbol(response_trans)

	rhs_formula <- str_trim(extract_string(formula, "~", 2))
	lme_formula <<- as.formula(paste(response_trans, rhs_formula,sep="~"))

	lm1 <- gls(model=lme_formula, data = df_reg)
	lme1 <- lme(fixed=lme_formula,random=~1|person_id_num, data = df_reg) #,control=list(opt="optim")
#lme2 <- lme(fixed=lme_formula,random=~0+symptom|person_id_num, data = df_reg,control=list(opt="optim"))
#lme2 <- lme(fixed=lme_formula,random=~0+symptom|person_id_num, data = df_reg,control=list(opt="optim"))

#test heteroscedastic model by age_group
#lme2 <- lme(fixed=lme_formula,random=~1|person_id_num, weights=varIdent(form=~1|age_group),data = df_reg)

	lme2 <- update(lme1,weights=varIdent(form=as.formula(paste0("~1|",x_plot))))

	cat("#################################################\n")
	cat("summary of lm1\n")	
	print(summary(lm1))	
	cat("#################################################\n")
	cat("anova of lm1\n")		
	print(anova(lm1))	
	cat("#################################################\n")
	cat("#################################################\n")
	cat("#################################################\n")
	cat("summary of lme1\n")	
	print(summary(lme1))	
	cat("#################################################\n")
	cat("anova of lme1\n")		
	print(anova(lme1,type="marginal"))	
	cat("#################################################\n")
	cat("#################################################\n")
	cat("#################################################\n")
	cat("summary of lme2 (heteroscedasticity)\n")	
	print(summary(lme2))	
	cat("#################################################\n")
	cat("anova of lme2 (heteroscedasticity)\n")		
	print(anova(lme2,type="marginal"))	
	cat("#################################################\n")
	cat("#################################################\n")
	cat("#################################################\n")
	cat("#################################################\n")
	cat("anova of lme2 (heteroscedasticity) sequential\n")		
	print(anova(lme2,type="sequential"))	
	cat("#################################################\n")
	cat("#################################################\n")
	cat("#################################################\n")
	cat("comparison of lme1 with lm1\n")
	try(print(anova(lme1,lm1)))
	cat("#################################################\n")
	cat("comparison of lme1 with lme2\n")
	try(print(anova(lme1,lme2)))
#Sys.sleep(5)

#write summary and anova lme2
	tmp <- summary(lme2)
	write.csv(tmp$tTable,file=file.path(dir_res,"lme2_tTable.csv"),row.names=T,quote=F)
	write.csv(as.data.frame(anova(lme2,type="marginal")),file=file.path(dir_res,"lme2_anova.csv"),row.names=T,quote=F)

	p <- plot(lme1,as.formula(paste0("resid(.,type=\"p\")~fitted(.)|",x_plot)),id=0.05,adj=-0.1)
	cairo_pdf(file.path(dir_res, "check_homoscedasticity.pdf"), width = 8, height = 8)
	print(p)
	dev.off()

	p <- plot(lme2,as.formula(paste0("resid(.,type=\"p\")~fitted(.)|",x_plot)),id=0.05,adj=-0.1)	
	pdf(file.path(dir_res, "check_heteroscedasticity.pdf"), width = 8, height = 8)
	print(p)
	dev.off()

	df_reg$pearson_resid <- resid(lme2,type="p")
	df_outlier <- subset(df_reg,abs(pearson_resid)>2)
	if(plot_outlier_TS){
		df_outlier <- arrange(df_outlier,pearson_resid)
		id <- unique(df_outlier$person_id)
		tmp <- length(id)
		cat(tmp,"outliers\n")
	#plot only the episode outlier
		tmp <- ceiling(4*sqrt(tmp))
		cairo_pdf(paste(dir_res,"check_outliers_TS.pdf",sep='/'),width=tmp,height=tmp)
		plot_TS_ind(id, df_profile, df_data, df_reg, df_outlier,strip_label="simple")
		dev.off()
	}

	response_var <- strsplit(formula,split="~")[[1]][1]

	p <- ggplot(df_outlier,aes_string(x=response_var))+geom_histogram(binwidth=0.1)
	pdf(file.path(dir_res, paste0("check_outliers_",response_var,".pdf")), width = 5, height = 5)
	print(p)	
	dev.off()

	if("symptom"%in% explanatory){	
		p <- plot(lme2,resid(.,type="p")~fitted(.)|age_group*symptom,id=0.05,adj=-0.1)
		pdf(file.path(dir_res, "check_residuals.pdf"), width = 8, height = 8)
		print(p)	
		dev.off()

	#check outliers
		p <- qqnorm(lme2,~resid(.)|age_group*symptom,id=0.05,adj=-0.1)	
		pdf(file.path(dir_res, "check_residuals_qqnorm.pdf"), width = 8, height = 8)	
		print(p)	
		dev.off()
	}

#prediction
	newdat <- expand.grid(lapply(df_reg[explanatory],unique))
	newdat$pred <- predict(lme2,newdat,level=0)

	Designmat <- model.matrix(eval(eval(lme2$call$fixed)[-2]), newdat) 
	Designmat <- Designmat[,colnames(lme2$varFix)]
	predvar <- diag(Designmat %*% lme2$varFix %*% t(Designmat)) 

	newdat$SE <- sqrt(predvar) 
	newdat$SE2 <- sqrt(predvar + lme2$sigma^2)
	Q95 <- qnorm(0.975)
	newdat <- mutate(newdat,original_pred=back_to_ori(pred),lower_conf= back_to_ori(pred-Q95*SE),upper_conf= back_to_ori(pred+Q95*SE),lower_pred= back_to_ori(pred-Q95*SE2),upper_pred= back_to_ori(pred+Q95*SE2))	

	df_pop_size <- unique(df_reg[explanatory])
	df_pop_size$freq <- 1
	newdat <- join(newdat, df_pop_size)
	if(!predict_all){
		newdat[is.na(newdat$freq),setdiff(names(newdat),explanatory)] <- 0		
	}
	newdat["freq"] <- NULL

	pd <- position_dodge(width=0.75)
	ps <- position_stack(width=0,height=0)
	if(CI_interval=="prediction"){
		y_min <- "lower_pred"
		y_max <- "upper_pred"
	}
	if(CI_interval=="confidence"){
		y_min <- "lower_conf"
		y_max <- "upper_conf"
	}	

	if(nrow(df_different_plot)) {

		d_ply(df_different_plot,var_different_plot,function(df){

			gdf <- match_df(newdat,df,on= var_different_plot)
			p <- ggplot(gdf,aes_string(x=x_var,fill=fill_var))
			if(use_facet_wrap){
				p <- p+facet_wrap(eval(parse(text=facet_formula)),...)		
			}else if(use_facet_grid){
				p <- p+facet_grid(eval(parse(text=facet_formula)),...)				
			}
			p <- p+geom_bar(aes(y= original_pred),position=pd,stat="identity",width=0.75,alpha=0.85)
			p <- p+geom_errorbar(aes_string(ymin=y_min,ymax=y_max),position=pd,width=0.5)
			p <- p+scale_fill_brewer(fill_var,palette="Dark2")
			p <- p+xlab(x_lab)+ylab(ifelse(is.null(y_lab), response_var, y_lab))
			pdf_file <- paste0("lme_fixed_",CI_interval,"_",paste(paste(names(df),df,sep="="),collapse="_"),".pdf")
			cairo_pdf(file.path(dir_res,pdf_file),width=7,height=4)
			print(p)
			dev.off()

		},.progress="text")

	} else {
		gdf <- newdat
		p <- ggplot(gdf,aes_string(x=x_var,fill=fill_var))
		if(use_facet_wrap){
			p <- p+facet_wrap(eval(parse(text=facet_formula)),...)		
		}else if(use_facet_grid){
			p <- p+facet_grid(eval(parse(text=facet_formula)),...)				
		}

		p <- p+geom_bar(aes(y= original_pred),position=pd,stat="identity",width=0.75,alpha=0.85)
		p <- p+geom_errorbar(aes_string(ymin=y_min,ymax=y_max),position=pd,width=0.5)

		p <- p+scale_fill_brewer(fill_lab,palette="Dark2")
		p <- p+xlab(x_lab)+ylab(ifelse(is.null(y_lab), response_var, y_lab))
		pdf_file <- paste0("lme_fixed_",CI_interval,".pdf")
		cairo_pdf(file.path(dir_res,pdf_file),width=7,height=4)
		print(p)
		dev.off()

	}


#compute other statistics in the original space: mean, mode etc
	if(transformation=="log"){
		sigma<-summary(lme2)$sigma
		newdat$mean<-exp(newdat$pred+sigma^2/2)
		newdat$mode<-exp(newdat$pred-sigma^2)		
	}

	if(transformation=="boxcox"){
		sigma<-summary(lme2)$sigma
		bound <- 1/(sigma*coef_bc)+newdat$pred/sigma
		newdat$bound <- bound
		newdat$norm <- pnorm(bound)
		newdat <-ddply(newdat, explanatory,function(df){
		#compute sample size
			tmp <- match_df(df_reg,df,on= explanatory)
			df$sample_size <- nrow(tmp)
		#compute mean
			tmp <- integrate(PN_integrand,lower=-1/coef_bc,upper=Inf,mu=df$pred,sigma=sigma,lambda=coef_bc,K=df$norm,r=1)
			df$mean <- tmp$value
			df$abs_error_mean <- tmp$abs.error
			df$PN_lower_95 <- PN_quantile(p=1-0.95^(1/df$sample_size),mu=df$pred,sigma=sigma,lambda=coef_bc,K=df$norm)				
			df$PN_upper_95 <- PN_quantile(p=0.95^(1/df$sample_size),mu=df$pred,sigma=sigma,lambda=coef_bc,K=df$norm)			
			return(df)
		})
	}


	write.csv(newdat, file = file.path(dir_res, "prediction.csv"), quote = F, row.names = F)

	if(return_outliers){
		return(df_outlier)			
	}else{
	#return bc transform + save it
		ans <- list(lme=lme2,coef_bc=ifelse(transformation=="boxcox",coef_bc,0),prediction=newdat,sigma=sigma,df_reg=df_reg,anova=anova(lme2,type="marginal"))
		saveRDS(ans,file.path(dir_res,"lme_pred.rds"))
		return(ans)
	}
}



lm_regression <- function(df_reg, formula = "QALD_loss~symptom+age_group", transformation = c("boxcox","log","logit","test"), max_response = 10, test_dist = F, CI_interval=c("confidence","prediction"), suffix="most_recent", df_data=NULL, df_profile=NULL, h_var="age_group",x_var="symptom",fill_var="age_group",use_facet_wrap=F,use_facet_grid=F, facet_formula=NULL, return_outliers=TRUE,...) {

	transformation <- match.arg(transformation)
	CI_interval <- match.arg(CI_interval)

	dir_res <- file.path(PDF,"lm", formula, transformation,suffix)
	dir.create(path = dir_res, recu = T)

	require(stringr)
	bc_formula <- as.formula(formula)

	response <- as.character(bc_formula[[2]])
	explanatory <- setdiff(all.vars(bc_formula), response)

	var_plotted <- c(x_var,fill_var)
	if(use_facet_wrap || use_facet_grid){
		tmp <- explanatory[str_detect(string=facet_formula,pattern=explanatory)]
		var_plotted <- unique(c(var_plotted,tmp))
	}

	var_different_plot <- setdiff(explanatory, var_plotted)
	df_different_plot <- na.omit(expand.grid(lapply(df_reg[var_different_plot],unique)))

	#transform ordered factor
	df_reg[explanatory] <- lapply(df_reg[explanatory], function(x) {
		if (is.ordered(x)) {
			x <- factor(x, ordered = F)
		}
		x
	})

	if ("pregnant" %in% explanatory) {

		df_reg$pregnant[is.na(df_reg$pregnant)] <- FALSE
		df_reg <- subset(df_reg,age_group%in%c("18-44"))
	}


	#remove explanatory with NA
	tmp <- df_reg[c("person_id_num",explanatory)]
	tmp <- na.omit(tmp)
	df_reg <- match_df(df_reg,tmp)

	if(all(explanatory%in%c("age_group","symptom"))){
		n_row <-length(unique(df_reg$age_group))
		n_col <- length(unique(df_reg$symptom))

	}else{
		n_row <- nrow(unique(df_reg[explanatory]))
		n_col <- round(sqrt(n_row))
		n_row <- ceiling(n_row/n_col)	
	}

	#plot data
	#boxplot
	p <- ggplot(df_reg, aes_string(x = x_var, y = response, fill=fill_var)) 
	if(use_facet_wrap){
		p <- p+facet_wrap(eval(parse(text=facet_formula)),...)		
	}else if(use_facet_grid){
		p <- p+facet_grid(eval(parse(text=facet_formula)),...)				
	}	
	p <- p + geom_boxplot() + ylim(c(0, max_response))
	#p<-p+geom_violin()+ylim(c(0,20))
	cairo_pdf(file.path(dir_res, "data_boxplot.pdf"), width = 8, height = 4)
	print(p)
	dev.off()

	#distribution
	p <- ggplot(df_reg, aes_string(x = response, fill=x_var)) 
	if(use_facet_wrap){
		p <- p+facet_wrap(eval(parse(text=facet_formula)),...)		
	}else if(use_facet_grid){
		p <- p+facet_grid(eval(parse(text=facet_formula)),...)				
	}	
	p <- p + geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5)
	p <- p + geom_density(alpha = 0.25) + xlim(c(0, max_response))
	cairo_pdf(file.path(dir_res, "data_histo_original.pdf"), width = 8, height = 4)
	print(p)
	dev.off()


	response_trans<-paste(response,transformation,sep="_")

	if(transformation=="boxcox"){
		lm_bc <- powerTransform(bc_formula, data = df_reg)
		coef_bc<-lm_bc$roundlam
		print(paste("coef_bc =", coef_bc))
		df_reg[,response_trans] <- bcPower(df_reg[,response], coef_bc)
		#test_bc <- sf.test(df_reg[,response_trans)$p
		back_to_ori <- bc_inverse(coef_bc)

	}

	if(transformation=="logit"){
		shift_value <- 0
		if(0){
			if(any(df_reg[,response]==100)){
				shift_value <- min(c(1,min(df_reg[,response])))
				df_reg[,response] <- df_reg[,response]-shift_value
			}}
			df_reg <- subset(df_reg, baseline_health_score<100)
			x <- df_reg[,response]/100
			df_reg[,response_trans] <- log(x) - log(1-x)
			#test_bc <- sf.test(df_reg[,response_trans)$p
			back_to_ori <- function(x){100*exp(x)/(exp(x)+1)+shift_value}
		}

		if(transformation=="test"){

			x <- df_reg[,response]
			x[x==100] <- max(x[x<100])
			x <- x/(100-x)
			df_reg[,response_trans] <- x
			#test_bc <- sf.test(df_reg[,response_trans)$p
			back_to_ori <- function(x){100*x/(1+x)}
		}


		if(transformation=="log"){
			df_reg[,response_trans] <- log(df_reg[,response])
			#test_bc <- sf.test(df_reg[,response_trans)$p
			back_to_ori <- exp
		}

		p <- ggplot(df_reg, aes_string(x = response_trans, fill=x_var)) 
		if(use_facet_wrap){
			p <- p+facet_wrap(eval(parse(text=facet_formula)),...)		
		}else if(use_facet_grid){
			p <- p+facet_grid(eval(parse(text=facet_formula)),...)				
		}	
		p <- p + geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5)
		p <- p + geom_density(alpha = 0.25) 
		cairo_pdf(file.path(dir_res, paste0("data_histo_", transformation, ".pdf")), width = 8, height = 4)
		print(p)
		dev.off()


		if(test_dist){
			#test of distribution
			require(nortest)
			dist_test <- dlply(df_reg, explanatory, function(df) {

				tmp <- df[, response_trans]
				p_val = try(sf.test(tmp)$p)

				best_fit <- data.frame(mean = mean(tmp), sd = sd(tmp),p_val=ifelse(inherits(p_val,"try-error"),NA,p_val) )
				best_fit$normal<-(best_fit$p_val>0.05)

				best_fit$sample_size <- nrow(df)




				p <- ggplot(df, aes_string(x = response_trans, fill=x_var)) 
				if(use_facet_wrap){
					p <- p+facet_wrap(eval(parse(text=facet_formula)),...)		
				}else if(use_facet_grid){
					p <- p+facet_grid(eval(parse(text=facet_formula)),...)				
				}	

				p <- p + geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5)
				p <- p + geom_density(alpha = 0.25)


				p <- p + stat_function(fun = dnorm, args = list(mean = best_fit$mean, sd = best_fit$sd), colour = "red")

				p <- p + scale_x_continuous("") + scale_y_continuous("")
				return(list(best_fit = best_fit, plot = p))


			}, .progress = "none")

			if(0){
				library(grid)
				cairo_pdf(file.path(dir_res, paste0("data_fit_",transformation, ".pdf")), width = 8, height = 8)
				grid.newpage()
				pushViewport(viewport(layout = grid.layout(n_row, n_col)))
				for (i in 1:n_row) {
					for (j in 1:n_col) {
						if(i*j<=length(dist_test)){
							print(dist_test[[i * j]]$plot, vp = vplayout(i, j))			
						}
					}
				}

				dev.off()
			}

			all_best_fit <- ldply(dist_test, function(x) x$best_fit)

			write.csv(all_best_fit, file = file.path(dir_res, "all_best_fit.csv"), quote = F, row.names = F)
		}

		#original lme
		#lme_formula <- bc_formula
		#lme_formula[[2]] <<- as.symbol(response_trans)

		#	rhs_formula <- paste0("-1+",str_trim(extract_string(formula, "~", 2)))	
		rhs_formula <- str_trim(extract_string(formula, "~", 2))
		lm_formula <<- as.formula(paste(response_trans, rhs_formula,sep="~"))

		lm1 <- gls(model= lm_formula, data = df_reg)

		lm1_hetero <- update(lm1,weights=varIdent(form=as.formula(paste0("~1|",h_var))))

		cat("#################################################\n")
		cat("summary of lm1\n")	
		print(summary(lm1))	
		cat("#################################################\n")
		cat("anova of lm1\n")		
		print(anova(lm1))	
		cat("#################################################\n")
		cat("#################################################\n")
		cat("#################################################\n")
		cat("summary of lme1\n")	
		print(summary(lm1_hetero))	
		cat("#################################################\n")
		cat("anova of lme1\n")		
		print(anova(lm1_hetero))	
		cat("#################################################\n")
		cat("#################################################\n")
		cat("#################################################\n")
		cat("comparison of lm1_hetero with lm1\n")
		try(print(anova(lm1_hetero,lm1)))

		#write summary and anova lme2
		tmp <- summary(lm1_hetero)
		write.csv(tmp$tTable,file=file.path(dir_res,"lm1_hetero_tTable.csv"),row.names=T,quote=F)
		write.csv(as.data.frame(anova(lm1_hetero)),file=file.path(dir_res,"lm1_hetero_anova.csv"),row.names=T,quote=F)

		if(0){
			p <- plot(lm1_hetero,id=0.05,adj=-0.1)
			cairo_pdf(file.path(dir_res, "check_plot.pdf"), width = 8, height = 8)
			print(p)
			dev.off()

			#form <- paste0("",h_var)
			p <- plot(lm1_hetero, form=resid(.,type="p")~fitted(.)|age_group2,id=0.05,adj=-0.1)
			cairo_pdf(file.path(dir_res, "check_homoscedasticity.pdf"), width = 8, height = 8)
			print(p)
			dev.off()

			form <- formula(paste0("resid(.,type=\"p\")~fitted(.)|",h_var))
			p <- plot(x=lm1_hetero,form,id=0.05,adj=-0.1)	
			pdf(file.path(dir_res, "check_heteroscedasticity.pdf"), width = 8, height = 8)
			print(p)
			dev.off()
		}
		df_reg$pearson_resid <- resid(lm1_hetero,type="p")
		df_outlier <- subset(df_reg,abs(pearson_resid)>2)
		cat(nrow(df_outlier),"outliers\n")	
		response_var <- strsplit(formula,split="~")[[1]][1]

		p <- ggplot(df_outlier,aes_string(x=response_var))+geom_histogram(binwidth=0.1)
		pdf(file.path(dir_res, paste0("check_outliers_",response_var,".pdf")), width = 5, height = 5)
		print(p)	
		dev.off()

		if(0){
			form <- formula(paste0("resid(.,type=\"p\")~fitted(.)|",h_var))
			p <- plot(lm1_hetero,form,id=0.05,adj=-0.1)
			pdf(file.path(dir_res, "check_residuals.pdf"), width = 8, height = 8)
			print(p)	
			dev.off()
		}

		#prediction
		newdat <- expand.grid(lapply(df_reg[explanatory],unique))
		newdat$pred <- predict(lm1_hetero,newdat)

		Designmat <- model.matrix(eval(eval(lm1_hetero$call$model)[-2]), newdat) 
		Designmat <- Designmat[,colnames(lm1_hetero$varBeta)]
		predvar <- diag(Designmat %*% lm1_hetero$varBeta %*% t(Designmat)) 

		newdat$SE <- sqrt(predvar) 
		newdat$SE2 <- sqrt(predvar+ lm1_hetero$sigma^2)

		newdat <- mutate(newdat,original_pred=back_to_ori(pred),lower_conf= back_to_ori(pred-2*SE),upper_conf= back_to_ori(pred+2*SE),lower_pred= back_to_ori(pred-2*SE2),upper_pred= back_to_ori(pred+2*SE2))	

		df_pop_size <- unique(df_reg[explanatory])
		df_pop_size$freq <- 1
		newdat <- join(newdat, df_pop_size)
		newdat[is.na(newdat$freq),setdiff(names(newdat),explanatory)] <- 101
		newdat["freq"] <- NULL

		pd <- position_dodge(width=0.75)
		ps <- position_stack(width=0,height=0)
		if(CI_interval=="prediction"){
			y_min <- "lower_pred"
			y_max <- "upper_pred"
		}
		if(CI_interval=="confidence"){
			y_min <- "lower_conf"
			y_max <- "upper_conf"
		}

		if(nrow(df_different_plot)){

			d_ply(df_different_plot,var_different_plot,function(df){

				gdf <- match_df(newdat,df,on= var_different_plot)
				p <- ggplot(gdf,aes_string(x=x_var,colour=fill_var))
				if(use_facet_wrap){
					p <- p+facet_wrap(eval(parse(text=facet_formula)),...)		
				}else if(use_facet_grid){
					p <- p+facet_grid(eval(parse(text=facet_formula)),...)				
				}

				p <- p+geom_point(aes(y= original_pred),position=pd,stat="identity",width=0.75,alpha=0.85)
				p <- p+geom_linerange(aes_string(ymin=y_min,ymax=y_max),position=pd,width=0.5)

				p <- p+scale_colour_brewer(fill_var,palette="Dark2")
				p <- p+xlab(x_var)+ylab(response_var)
				pdf_file <- paste0("lm_",CI_interval,"_",paste(paste(names(df),df,sep="="),collapse="_"),".pdf")
				cairo_pdf(file.path(dir_res,pdf_file),width=7,height=4)
				print(p)
				dev.off()

			},.progress="text")

		}else{
			gdf <- newdat
			p <- ggplot(gdf,aes_string(x=x_var,colour=fill_var))
			if(use_facet_wrap){
				p <- p+facet_wrap(eval(parse(text=facet_formula)),...)		
			}else if(use_facet_grid){
				p <- p+facet_grid(eval(parse(text=facet_formula)),...)				
			}

			p <- p+geom_point(aes(y= original_pred),position=pd,stat="identity",width=0.75,alpha=0.85)
			p <- p+geom_linerange(aes_string(ymin=y_min,ymax=y_max),position=pd,width=0.5)

	#p <- p+geom_text(aes(label=paste("n =",freq),y=max(upper_conf)*1.1+as.numeric(smoke_bool)/5),size=3)
			p <- p+ scale_colour_brewer(fill_var,palette="Dark2")
	#p <- p+scale_colour_brewer("age group",palette="Dark2")#+theme(legend.position="none")
			p <- p+xlab(x_var)+ylab(response_var)
			pdf_file <- paste0("lm_",CI_interval,".pdf")
			cairo_pdf(file.path(dir_res,pdf_file),width=7,height=4)
			print(p)
			dev.off()

		}


#compute other statistics in the original space: mean, mode etc
		if(transformation=="log"){
			sigma<-summary(lme2)$sigma
			newdat$mean<-exp(newdat$pred+sigma^2/2)
			newdat$mode<-exp(newdat$pred-sigma^2)		
		}


		if(transformation=="boxcox"){
			sigma<-summary(lme2)$sigma
			bound <- 1/(sigma*coef_bc)+newdat$pred/sigma
			newdat$bound <- bound
			newdat$norm <- pnorm(bound)
			newdat <-ddply(newdat, explanatory,function(df){
		#compute sample size
				tmp <- match_df(df_reg,df,on= explanatory)
				df$sample_size <- nrow(tmp)
		#compute mean
				tmp <- integrate(PN_integrand,lower=-1/coef_bc,upper=Inf,mu=df$pred,sigma=sigma,lambda=coef_bc,K=df$norm,r=1)
				df$mean <- tmp$value
				df$abs_error_mean <- tmp$abs.error
				df$PN_lower_95 <- PN_quantile(p=1-0.95^(1/df$sample_size),mu=df$pred,sigma=sigma,lambda=coef_bc,K=df$norm)				
				df$PN_upper_95 <- PN_quantile(p=0.95^(1/df$sample_size),mu=df$pred,sigma=sigma,lambda=coef_bc,K=df$norm)			
				return(df)
			})
		}


		write.csv(newdat, file = file.path(dir_res, "prediction.csv"), quote = F, row.names = F)

		if(return_outliers){
			return(df_outlier)			
		}else{
	#return bc transform + 
			return(list(coef_bc=ifelse(transformation=="boxcox",coef_bc,0),prediction=newdat))
		}
	}


	glm_regression <- function(df_reg, formula = "QALD_loss~symptom+age_group+vaccine+gender+gender:age_group:pregnant+age_group:smoke", family = "Gamma", link = "log", max_response = 10, model_selection = F, test_dist = F, ...) {


		dir_res <- file.path(PDF, formula, family, link)
		dir.create(path = dir_res, recu = T)

		n_row <- length(levels(df_reg$age_group))
		n_col <- length(levels(df_reg$symptom))

		require(stringr)
		glm_formula <- formula(formula)

		response <- str_trim(extract_string(formula, "~", 1))
		explanatory <- setdiff(all.vars(glm_formula), response)

	#transform ordered factor
		df_reg[explanatory] <- lapply(df_reg[explanatory], function(x) {
			if (is.ordered(x)) {
				x <- factor(x, ordered = F)
			}
			x
		})

		if ("pregnant" %in% explanatory) {

			df_reg$pregnant[is.na(df_reg$pregnant)] <- FALSE

		}

	#plot data
	#boxplot
		p <- ggplot(df_reg, aes_string(x = "age_group", y = response)) + facet_wrap(~symptom, scales = "free_y", ncol = n_col)
		p <- p + geom_boxplot() + ylim(c(0, max_response))
	#p<-p+geom_violin()+ylim(c(0,20))
		cairo_pdf(file.path(dir_res, "data_boxplot.pdf"), width = 8, height = 4)
		print(p)
		dev.off()

	#distribution
		p <- ggplot(df_reg, aes_string(x = response)) + facet_grid(age_group ~ symptom, scales = "free")
		p <- p + geom_histogram(aes(y = ..density..), position = "identity", binwidth = 0.25, alpha = 0.5)
		p <- p + geom_density(alpha = 0.25) + xlim(c(0, max_response))
		cairo_pdf(file.path(dir_res, "data_histo_original.pdf"), width = 8, height = 4)
		print(p)
		dev.off()

		p <- ggplot(df_reg, aes_string(x = paste0("log(", response, ")"))) + facet_grid(age_group ~ symptom, scales = "free")
		p <- p + geom_histogram(aes(y = ..density..), position = "identity", binwidth = 0.25, alpha = 0.5)
		p <- p + geom_density(alpha = 0.25)
		cairo_pdf(file.path(dir_res, paste0("data_histo_", link, ".pdf")), width = 8, height = 4)
		print(p)
		dev.off()

		if(test_dist){
		#test of distribution
			require(robustloggamma)
			require(nortest)
			dist_test <- dlply(df_reg, c("symptom", "age_group"), function(df) {

				x <- df[, response]
				link_response <- paste(link, response, sep = "_")

				if (link == "log") {
					df[, link_response] <- log(x)

				}

				if (family == "Gamma" & link == "log") {
					tmp <- loggammarob(df[, link_response, drop = T], control = loggammarob.control(lower = 0, upper = 2, n = 30))
					best_fit <- data.frame(mu = tmp$mu, sigma = tmp$sigma, lambda = tmp$lambda )
				}

				if (family == "gaussian") {
					tmp <- df[, link_response]
					best_fit <- data.frame(mean = mean(tmp), sd = sd(tmp), p_val = sf.test(tmp)$p)
					best_fit$normal<-(best_fit$p_val>0.05)
				}

				best_fit$sample_size <- nrow(df)

				p <- ggplot(df, aes_string(x = link_response)) + facet_grid(age_group ~ symptom, scales = "free")
				p <- p + geom_histogram(aes(y = ..density..), position = "identity", binwidth = 0.25, alpha = 0.5)
				p <- p + geom_density(alpha = 0.25)
				if (family == "Gamma" & link == "log") {
					p <- p + stat_function(fun = dloggamma, args = list(mu = best_fit$mu, sigma = best_fit$sigma, lambda = best_fit$lambda), 
						colour = "red")
				}
				if (family == "gaussian") {
					p <- p + stat_function(fun = dnorm, args = list(mean = best_fit$mean, sd = best_fit$sd), colour = "red")
				}
				p <- p + scale_x_continuous("") + scale_y_continuous("")

				return(list(best_fit = best_fit, plot = p))
			}, .progress = "text")

library(grid)
cairo_pdf(file.path(dir_res, paste0("data_fit_", family, "_on_", link, ".pdf")), width = 8, height = 8)
grid.newpage()
pushViewport(viewport(layout = grid.layout(n_row, n_col)))
for (i in 1:n_row) {
	for (j in 1:n_col) {
		print(dist_test[[i * j]]$plot, vp = vplayout(i, j))
	}
}

dev.off()

all_best_fit <- ldply(dist_test, function(x) x$best_fit)

write.csv(all_best_fit, file = file.path(dir_res, "all_best_fit.csv"), quote = F, row.names = F, col.names = F)
}
#original glm
m1 <- glm(glm_formula, family = eval(parse(text = paste0(family, "(link=", link, ")"))), data = df_reg)

print(summary(m1))

cairo_pdf(file.path(dir_res, "check_default.pdf"), width = 8, height = 8)
par(mfrow = c(2, 2))
plot(m1)
par(mfrow = c(1, 1))
dev.off()


if (model_selection) {
	#model selection
	m2 <- step(m1, test = "F")
	print(summary(m2))

	cairo_pdf(file.path(dir_res, "check_default.pdf"), width = 8, height = 8)
	par(mfrow = c(2, 2))
	plot(m2)
	par(mfrow = c(1, 1))
	dev.off()
}

#prediction
pred <- paste(response, "pred", sep = "_")
se_pred <- paste(response, "se_pred", sep = "_")
df_reg[c(pred, se_pred)] <- predict(m1, type = "response", se.fit = T)[c("fit", "se.fit")]
#browser()
#Pearson residuals
df_reg$pearson_residuals <- residuals(m1,type="pearson")

p <- ggplot(df_reg, aes(x = age_group, y = pearson_residuals)) + facet_wrap(~symptom, ncol = n_col)
#p<-p+geom_violin(alpha=0.75)
p <- p + geom_boxplot(alpha = 0.75) + coord_cartesian(ylim = quantile(df_reg$pearson_residuals, c(0.025, 0.975)))
cairo_pdf(file.path(dir_res, "check_pearson_residuals.pdf"), width = 8, height = 4)
print(p)
dev.off()

#plot prediction
gdf <- arrange(unique(subset(df_reg, select = c(pred, se_pred, explanatory))), symptom, age_group)
gdf <- ddply(gdf, explanatory, function(df) {
	df[1, ]
})

write.csv(gdf, file = file.path(dir_res, "prediction.csv"), quote = F, row.names = F, col.names = F)

p <- ggplot(gdf, aes_string(x = "age_group", y = pred, ymax = paste(pred, se_pred, sep = "+"), ymin = paste(pred, 
	se_pred, sep = "-"))) + facet_wrap(~symptom, scales = "free_y", ncol = n_col)
p <- p + geom_bar(stat = "identity", alpha = 0.75)
p <- p + geom_errorbar()
cairo_pdf(file.path(dir_res, "prediction.pdf"), width = 8, height = 4)
print(p)
dev.off()


}

compute_risk_ratio_binom_old <- function(df_episode, success = "visit_GP", risk = "is_risk", by = c("symptom"), simplify_multiple = c("none", "sample", "any_success", "all_success"), conf = 0.95, method = "exact") {

	require(binom)
	require(PropCIs)

	simplify_multiple <- match.arg(simplify_multiple)
	stopifnot(sapply(df_episode[, c(risk, success)], is.logical))

	#remove NA
	tmp <- na.omit(df_episode[c(success, risk, by)])
	df_episode <- match_df(df_episode, tmp,on=c(success, risk, by))

	df_risk_mult <- ddply(df_episode, by, function(df) {

		if (simplify_multiple != "none") {
			#for those with more than one episode, keep only one, with success
			tmp <- count(df, vars = "person_id")
			df_mult <- match_df(df, subset(tmp, freq > 1), on = "person_id")
			df_uniq <- match_df(df, subset(tmp, freq == 1), on = "person_id")


			if (simplify_multiple == "sample") {
				df_mult_resampled <- df_mult[sample(nrow(df_mult), nrow(df_mult)), ]
				df_uniq_2 <- df_mult_resampled[!duplicated(df_mult_resampled$person_id), ]
			} else if (simplify_multiple == "any_success") {
				df_uniq_2 <- ddply(df_mult, "person_id", function(df_id) {
					ind <- which(df_id[, success])
					if (!length(ind)) {
						ind <- 1:nrow(df_id)
					}
					return(df_id[sample(ind, 1), ])
				})
			} else if (simplify_multiple == "all_success") {
				df_uniq_2 <- ddply(df_mult, "person_id", function(df_id) {
					if (all(df_id[, success])) {
						ind <- 1:nrow(df_id)
					} else {
						ind <- which(!df_id[, success])
					}
					return(df_id[sample(ind, 1), ])
				})
			}

			df <- rbind(df_uniq, df_uniq_2)
		}

		at_risk <- df[, risk]

		#not at risk		
		x_low <- binom.confint(x = sum(df[!at_risk, success]), n = nrow(df[!at_risk, ]), conf.level = conf, methods = method)
		x_low <- data.frame(variable = "p_no_risk", x_low)

		#at risk
		x_high <- binom.confint(x = sum(df[at_risk, success]), n = nrow(df[at_risk, ]), conf.level = conf, methods = method)
		x_high <- data.frame(variable = "p_risk", x_high)

		#risk ratio
		x_rr <- riskscoreci(x2 = sum(df[!at_risk, success]), n2 = nrow(df[!at_risk, ]), x1 = sum(df[at_risk, success]), 
			n1 = nrow(df[at_risk, ]), conf.level = conf)
		x_rr <- as.vector(x_rr[[1]])
		x_rr <- data.frame(variable = "risk_ratio", method = "p_risk/p_no_risk", x = NA, n = NA, mean = x_high$mean/x_low$mean, 
			lower = x_rr[1], upper = x_rr[2])

		tab_mf <- do.call("rbind", list(x_low, x_high, x_rr))

	}, .progress = "none")

return(df_risk_mult)
}


compute_success_by_risk <- function(df, risk, success) {
	at_risk <- df[, risk]
	data.frame(
		n_success_no_risk = sum(df[!at_risk, success]),
		n_sample_no_risk = nrow(df[!at_risk, ]),
		n_success_risk = sum(df[at_risk, success]),
		n_sample_risk = nrow(df[at_risk, ]))
}

compute_risk_ratio_binom <- function(df_episode, success = "visit_GP", risk = "is_risk", by = c("symptom"),  conf = 0.95, method = "exact", bootstrap = 10, plot_bootstrap=T) {

	require(binom)
	require(PropCIs)

	stopifnot(sapply(df_episode[, c(risk, success)], is.logical))

	#remove NA
	tmp <- na.omit(df_episode[c(success, risk, by)])
	df_episode <- match_df(df_episode, tmp,on=c(success, risk, by))

	df_risk_mult <- ddply(df_episode, by, function(df) {

		#bootstrap for those with more than one episode
		tmp <- count(df, vars = "person_id")
		df_uniq <- match_df(df, subset(tmp, freq == 1), on = "person_id")			
		df_x_n <- compute_success_by_risk(df_uniq,risk,success)

		df_mult <- match_df(df, subset(tmp, freq > 1), on = "person_id")
		if(nrow(df_mult)){
			cat("Start bootstrap analysis with", bootstrap,"replicates\n")
			#bootstrap analysis
			n_row <- nrow(df_mult)
			df_bootstrap <- ldply(1:bootstrap,function(i){
				df_mult_resampled <- df_mult[sample(n_row,n_row), ]
				df_uniq_bootstrap <- df_mult_resampled[!duplicated(df_mult_resampled$person_id), ]
				compute_success_by_risk(df_uniq_bootstrap,risk,success)
			},.progress="text")
			#take the mean
			df_x_n_bootstrap <- data.frame(t(round(vapply(df_bootstrap,mean,numeric(1)))))

			#plot
			if(plot_bootstrap){
				gdf <- melt(df_bootstrap,measure.vars=c("n_success_no_risk","n_success_risk"))
				gdf_use <- melt(df_x_n_bootstrap,measure.vars=c("n_success_no_risk","n_success_risk"))
				p <- ggplot(gdf)+facet_wrap(~variable)
				p <- p+geom_histogram(data=gdf,aes(x=as.factor(value)),alpha=0.5)
				p <- p+geom_point(data=gdf_use,aes(x=as.factor(value),y=0),size=4)
				quartz()
				print(p)	
			}

			df_x_n[] <- sapply(rbind(df_x_n,df_x_n_bootstrap),sum,simplify=F)
		}	

		#not at risk		
		x_no_risk <- with(df_x_n,binom.confint(x = n_success_no_risk, n = n_sample_no_risk, conf.level = conf, methods = method))
		x_no_risk <- data.frame(variable = "p_no_risk", x_no_risk)

		#at risk
		x_risk <- with(df_x_n,binom.confint(x = n_success_risk, n = n_sample_risk, conf.level = conf, methods = method))
		x_risk <- data.frame(variable = "p_risk", x_risk)

		#risk ratio
		x_rr <- with(df_x_n,riskscoreci(x2 = n_success_no_risk, n2 = n_sample_no_risk, x1 = n_success_risk, n1 = n_sample_risk, conf.level = conf))
		x_rr <- as.vector(x_rr[[1]])
		x_rr <- data.frame(variable = "risk_ratio", method = "p_risk/p_no_risk", x = NA, n = NA, mean = x_risk$mean/x_no_risk$mean, 
			lower = x_rr[1], upper = x_rr[2])

		tab_mf <- do.call("rbind", list(x_no_risk, x_risk, x_rr))

	}, .progress = "none")

return(df_risk_mult)
}



risk_ratio_analysis <- function(df_episode) {
	
	#group ILI_ecdc and ILI_fever
	df_risk_ratio <- mutate(df_episode, symptom2 = revalue(symptom, c(ILI_fever = "ILI_ecdc")))
	#risk ratio by age group
	risk_ratio_symptom_age <- compute_risk_ratio_binom(df_risk_ratio, by = c("symptom2", "age_group"), bootstrap = 1000)
	#plot
	gdf_text <- subset(risk_ratio_symptom_age, variable != "risk_ratio")
	gdf_text <- ddply(gdf_text, "variable", function(df) {
		df$y <- max(df$upper)
		df
	})
	p <- ggplot(risk_ratio_symptom_age) + facet_grid(variable ~ symptom2, scales = "free_y")
	p <- p + geom_bar(data = risk_ratio_symptom_age, aes(x = age_group, y = mean), alpha = 0.5, stat = "identity")
	p <- p + geom_errorbar(data = risk_ratio_symptom_age, aes(x = age_group, ymax = upper, ymin = lower))
	p <- p + geom_text(data = gdf_text, aes(label = paste("n =", n), x = age_group, y = y * 1.2), vjust = 1)
	cairo_pdf("res/risk_ratio_visit_GP_by_age_and_symptom.pdf", width = 7, height = 6)
	print(p)
	dev.off()
	#
	write.csv(risk_ratio_symptom_age, "res/risk_ratio_visit_GP_by_age_and_symptom.csv", row.names = F)

	#risk ratio by symptom only
	risk_ratio_symptom <- compute_risk_ratio_binom(df_risk_ratio, by = c("symptom2"), bootstrap = 1000)
	#plot
	gdf_text <- subset(risk_ratio_symptom, variable != "risk_ratio")
	gdf_text <- ddply(gdf_text, "variable", function(df) {
		df$y <- max(df$upper)
		df
	})
	p <- ggplot(risk_ratio_symptom) + facet_wrap(~variable, scales = "free_y", ncol = 1)
	p <- p + geom_bar(data = risk_ratio_symptom, aes(x = symptom2, y = mean), alpha = 0.5, stat = "identity")
	p <- p + geom_errorbar(data = risk_ratio_symptom, aes(x = symptom2, ymax = upper, ymin = lower))
	p <- p + geom_text(data = gdf_text, aes(label = paste("n =", n), x = symptom2, y = y * 1.2), vjust = 1)
	cairo_pdf("res/risk_ratio_visit_GP_by_symptom.pdf", width = 4, height = 7)
	print(p)
	dev.off()
	#
	write.csv(risk_ratio_symptom, "res/risk_ratio_visit_GP_by_symptom.csv", row.names = F)
}


compute_stat_episode <- function(df_episode, id_vars=c("symptom","age_group"),measure_vars = c("symptom_duration", "baseline_health_score", "min_health_score"), n_digits=3) {

	df_epi <- melt(df_episode,id.vars= id_vars, measure.vars = measure_vars)
	df_stat <- ddply(df_epi, c(id_vars,"variable"), function(df) {

		x <- na.omit(df$value)
		ans <- data.frame(t(quantile(x, prob = c(0.025, 0.25, 0.5, 0.75, 0.975))))
		names(ans) <- c("low_95", "low_IQR", "median", "up_IQR", "up_95")
		ans$mean <- mean(x)
		ans$sd <- sd(x)
		ans$n <- length(x)
		ans <- round(ans,n_digits)
		return(ans)
	})
	return(df_stat)
}

compute_population_size_UK <- function() {

	age_group_smoke <- c("0-10","11","12","13","14","15","16-19","20-24","25-34","35-49","50-59","60+")
	cut_smoke <- c(0,11,12,13,14,15,16,20,25,35,50,60,Inf)
	risk_group <- c("LR","HR")
	age_group_risk<-c("0 - 1 y","1 - 4 y","5 - 14 y","15 - 24 y","25 - 44 y","45 - 64 y","65+ y")
	cut_risk <- c(0,1,5,15,25,45,65,Inf)
	age_group_vaccine_LR <- c("0-4","5-14","15-24","25-44","45-64","65+")
	cut_vaccine_LR <- c(0,5,15,25,45,65,Inf)	
	age_group_vaccine_HR <- c("0-1","2-15","16-64","65+")
	cut_vaccine_HR <- c(0,2,16,65,Inf)	
	age_group <- c("0-17","18-44","45-64","65+")
	cut_age_group <- c(0,18,45,65,Inf)

	# age + gender
	dir_AG <- "/Users/Tonton/work/data/Demography/pop_size_UK"
	df_AG <- read.table(file.path(dir_AG,"pop_size_by_gender_2012.txt"),h=TRUE,dec=",")
	df_AG <- df_AG*1000
	df_AG$age <- df_AG$age/1000
	df_AG$persons <- NULL
	df_pop <- melt(df_AG,id.var="age",value.name="size",variable.name="gender")

	# age + gender + smoke
	#add smoking habits from office national statistics
	# http://www.ons.gov.uk
	# 2011 for <16  and 2012 for >16
	dir_AGS <- "/Users/Tonton/work/data/Demography/smoker_UK"
	df_AGS <- read.table(file.path(dir_AGS,"smoking_by_age_and_gender_2012.txt"),h=TRUE,dec=",")
	df_AGS <- rename(df_AGS,c(Age_group="age_group",Females="females",Males="males"))
	df_AGS <- melt(df_AGS,id.vars="age_group",variable.name="gender",value.name="smoke")
	df_pop <- mutate(df_pop,age_group=cut(age,breaks=cut_smoke,labels=age_group_smoke,include.lowest=T,right=F))
	df_pop <- join(df_pop,df_AGS)
	df_pop <- mutate(df_pop,yes=size*smoke/100,no=size-yes)
	df_pop <- df_pop[setdiff(names(df_pop),c("size","age_group","smoke"))]
	df_pop <- melt(df_pop,measure.var=c("yes","no"),value.name="size",variable.name="smoke")

	# age + gender +smoke + risk
	dir_AR <- "/Users/Tonton/work/projects/rethinking_vaccine_uk/code/MCMC_project/data"
	df_AR <- read.table(file.path(dir_AR,"age_risk_group_sizes.txt"),h=T)
	df_AR <- mutate(df_AR,HR=high+preg,LR=low,prop_HR=HR/(LR+HR))[c(risk_group,"prop_HR")]
	df_AR$age_group <- age_group_risk
	df_AR <- df_AR[c("age_group","prop_HR")]
	df_pop <- mutate(df_pop,age_group=cut(age,breaks=cut_risk,labels=age_group_risk,include.lowest=T,right=F))
	df_pop <- join(df_pop,df_AR)
	df_pop <- mutate(df_pop,yes=size*prop_HR,no=size-yes)
	df_pop <- df_pop[setdiff(names(df_pop),c("size","age_group","prop_HR"))]
	df_pop <- melt(df_pop,measure.var=c("yes","no"),value.name="size",variable.name="risk")



	# age + gender +smoke + risk + vaccine	
	if(0){
		dir_data_marc <- "/Users/Tonton/work/projects/rethinking_vaccine_uk/code/MCMC_project/data"
		df_coverage_marc <- read.csv(file.path(dir_data_marc,"coverage_figures.csv"))
		df_coverage_marc <- subset(df_coverage_marc,year==200708)
		df_coverage_marc <- df_coverage_marc[,-1]
		coverage_200708 <- df_coverage_marc[1,,drop=T]

		df_ARV <- data.frame(age_group=c(age_group_vaccine_LR,age_group_vaccine_HR),risk=rep(c("no","yes"),c(length(age_group_vaccine_LR),length(age_group_vaccine_HR))))
		df_ARV$coverage <- c(as.numeric(coverage)[1+1:(length(age_group_vaccine_LR)-1)],73.4,24.3,38.7,52.8,73.4) 		
	}
	dir_ARV <- "/Users/Tonton/work/data/Demography/vaccine_coverage_UK"
	df_ARV	<- read.table(file.path(dir_ARV,"flu_201213.txt"),h=TRUE)
	#coverage for not at risk and at risk must be joined separately as age_group are different
	df_ARV_risk <- rename(subset(df_ARV,risk=="yes"),c(age_group="age_group_HR",coverage="coverage_HR"))
	df_ARV_no_risk <- rename(subset(df_ARV,risk=="no"),c(age_group="age_group_LR",coverage="coverage_LR"))

	df_pop <- mutate(df_pop,age_group_LR=cut(age,breaks=cut_vaccine_LR,labels=age_group_vaccine_LR,include.lowest=T,right=F),age_group_HR=cut(age,breaks=cut_vaccine_HR,labels=age_group_vaccine_HR,include.lowest=T,right=F))
	df_pop <- join(df_pop,df_ARV_risk)
	df_pop <- join(df_pop,df_ARV_no_risk)
	df_pop$coverage <- df_pop$coverage_LR
	ind <- which(is.na(df_pop$coverage))
	df_pop$coverage[ind] <- df_pop$coverage_HR[ind]
	df_pop <- mutate(df_pop,yes=size*coverage/100,no=size-yes)
	df_pop <- df_pop[setdiff(names(df_pop),c("size","age_group_LR","age_group_HR","coverage_LR","coverage_HR","coverage"))]
	df_pop <- melt(df_pop,measure.var=c("yes","no"),value.name="size",variable.name="vaccine")


	# group and sum by age_group
	df_pop <- mutate(df_pop,age_group=cut(age,breaks=cut_age_group,labels=age_group,include.lowest=T,right=F))
	df_pop$age <- NULL
	df_pop <- ddply(df_pop,setdiff(names(df_pop),"size"),function(df){
		return(data.frame(size=round(sum(df$size))))
	})

	# arrange
	df_pop <- arrange(df_pop[c("age_group","gender","risk","vaccine","smoke","size")],age_group,gender,risk,vaccine,smoke)

	return(df_pop)
}

aggregate_population_UK <- function(df_pred_UK,pop_var=c("symptom","age_group","smoke_bool"),pop_name=c("FS","UK")) {
	
	df_pop_UK_aggregated <- ddply(df_pred_UK,pop_var,function(df){

		df_pop <- as.data.frame(t(colSums(df[pop_name])))
		return(df_pop)

	})

	df_pop_UK_aggregated <- mutate(df_pop_UK_aggregated,FS_per=FS/sum(FS)*100,UK_per=UK/sum(UK)*100)

	return(melt(df_pop_UK_aggregated,measure.vars=pop_var))
}


aggregate_prediction_UK <- function(df_pred_UK,pop_var=c("symptom","age_group","smoke_bool"),CI=0.95,pred_var=c("pred_mean","pred_var"),back_to_ori=NULL) {
	
	stopifnot(CI>0 & CI<1,!is.null(back_to_ori))

	df_pred_UK_aggregated <- ddply(df_pred_UK,pop_var,function(df){
		# normalize weight
		df <- mutate(df,weight=UK_weight/sum(UK_weight))
		# weighted sum of normal distributions
		df[pred_var] <- df[pred_var]*df$weight 

		pred <- as.data.frame(t(colSums(df[pred_var])))
		pred$SE <- sqrt(pred$pred_var) 
		Q_CI <- qnorm(1-(1-CI)/2)
		ans  <- with(pred,data.frame(original_pred=back_to_ori(pred_mean),lower_conf=back_to_ori(pred_mean-Q_CI*SE),upper_conf=back_to_ori(pred_mean+Q_CI*SE)))
		return(ans)

	})

	return(df_pred_UK_aggregated)
}

compute_effect_UK_symptom <- function(df_pred=df_pred_UK_symptom_age,var_effect="age_group",var_effect_name="age group",lme,n_digits=2) {

	lme_anova <- anova(lme,type="marginal")
	lme_anova <- as.data.frame(lme_anova)
	p_val_terms <- lme_anova[var_effect,"p-value"]


	df_coef <- as.data.frame(summary(lme)$tTable[,c("Value","p-value")])

	# position
	coef_names <- grep(var_effect,rownames(df_coef),value=T)
	coef_names_suffix <- unlist(str_split(coef_names,var_effect))
	coef_names_suffix <- coef_names_suffix[coef_names_suffix!=""]

	df_coef <- df_coef[coef_names,]
	df_coef$level <- as.character(coef_names_suffix)
	names(df_coef) <- c("beta","p_val_coef",var_effect)
	# print(df_coef)	
	df_coef <- bool_to_char(df_coef)

	tmp <- join(df_pred,df_coef,by=var_effect)

	# some coef are 0	
	ind <- which(is.na(tmp$beta))
	tmp$beta[ind] <- 0

	# print(df_pred)
	# print(df_coef)
	# print(tmp)
	# x <- as.vector(df_pred[,var_effect])
	# y <- as.vector(df_coef[,var_effect])
	# print(x==y)

	# print(tmp)
	tmp <- mutate(tmp,pred=paste0(round(original_pred,n_digits)," [",round(lower_conf,n_digits),"-",round(upper_conf,n_digits),"]"),p_val_coef=round(p_val_coef,3),beta=round(beta,3))	
	tmp <- dcast(tmp,as.formula(paste0(eval(var_effect),"+beta+p_val_coef~symptom")),value.var="pred")
	tmp$effect <- ""
	tmp$effect[1] <- var_effect_name
	tmp$p_val_terms <- ""
	tmp$p_val_terms[1] <- round(p_val_terms,3)
	v_rename <- c("level")
	names(v_rename) <- var_effect
	tmp <- rename(tmp,v_rename)
	tmp <- tmp[,c("effect","p_val_terms",setdiff(names(tmp),c("effect","p_val_terms")))]
	tmp$level <- as.character(tmp$level)
	return(tmp)

}

bool_to_char <- function(df,value=c("TRUE"="yes","FALSE"="no")) {
	df[df=="TRUE"] <- value["TRUE"]
	df[df=="FALSE"] <- value["FALSE"]
	return(df)
}

print_info_participants <- function(dir_prediction,dir_tex=dir_prediction,dir_figs=dir_prediction){

	lme_pred <- readRDS(file.path(dir_prediction,"lme_pred.rds"))
	df_episode <- lme_pred$df_reg

	print(min(df_episode$first_report_date))
	print(max(df_episode$last_report_date))

	df <- count(df_episode,vars="person_id")
	print(summary(df$freq))
	print(sum(df$freq>1))
	p <- ggplot(df,aes(x=as.factor(freq)))+geom_histogram(aes(y=..count../sum(..count..)*100),binwidth=1)
	p <- p+scale_x_discrete("Number of episodes")+scale_y_continuous("Proportion of participants (in %)")
	file_name <- file.path(dir_figs,"histo_n_epi_per_participant")
	cairo_pdf(paste(file_name,"pdf",sep="."),width=4,height=4)
	print(p+theme_bw())
	dev.off()
}

prediction_UK <- function(dir_prediction,dir_tex=dir_prediction,dir_figs=dir_prediction){

	lme_pred <- readRDS(file.path(dir_prediction,"lme_pred.rds"))
	lme <- lme_pred$lme
	df_pred <- lme_pred$prediction
	df_episode <- lme_pred$df_reg
	lme_anova <- lme_pred$anova
	back_to_ori <- bc_inverse(lme_pred$coef_bc)

	df_pop <- readRDS(file.path(RSAVE,"df_pop.rds"))

	# rename variable names and values
	df_pop <- rename(df_pop,c(risk="is_risk",smoke="smoke_bool",size="UK_weight"))
	var_logical <- names(df_pred)[which(sapply(df_pred,is.logical))]
	for(x in var_logical){
		df_pop[,x] <- as.logical(revalue(df_pop[,x],c(yes="TRUE",no="FALSE")))
	}
	df_pop$gender <- revalue(df_pop$gender,c(males="male",females="female"))

	# use variance
	df_pred <- mutate(df_pred,pred_var=SE^2,pred_mean=pred)
	pop_var <- c("age_group","smoke_bool","is_risk","vaccine","gender")
	df_pred_all <- subset(mutate(df_pred,pred_CI=paste(round(original_pred,2)," [",round(lower_conf,2),"-",round(upper_conf,2),"]",sep="")),select=c("symptom",pop_var,"pred_CI"))
	df_pred_all <- dcast(df_pred_all,formula(paste(paste(pop_var,collapse="+"),"symptom",sep="~")),value.var = "pred_CI")
	# compare Flusurvey cohort with UK pop
	df_UK <- df_pop
	df_profile <- readRDS(file = paste(RSAVE, "profile_clean.rds", sep = "/"))	
	df_profile <- mutate(df_profile,smoke_bool=smoke_as_logical(smoke))
	df_FS <- subset(match_df(df_profile,df_episode,on="person_id"),select=pop_var)
	df_FS <- count(df_FS,vars=pop_var)

	df_UK_vs_FS <- join(df_UK,df_FS,by=pop_var)
	df_UK_vs_FS[is.na(df_UK_vs_FS)] <- 0
	df_UK_vs_FS <- mutate(df_UK_vs_FS,FS_per=freq/sum(freq),UK_per=UK_weight/sum(UK_weight))
	# df_UK_vs_FS <- df_UK_vs_FS[setdiff(names(df_UK_vs_FS),c("UK_weight","freq"))]
	gdf_UK_vs_FS <- melt(df_UK_vs_FS,measure.vars=c("UK_per","FS_per"))
	gdf_UK_vs_FS <- mutate(gdf_UK_vs_FS,smoke_bool=revalue(as.character(smoke_bool),c("TRUE"="smoker","FALSE"="non smoker")),vaccine=revalue(as.character(vaccine),c("TRUE"="vaccinated","FALSE"="not vaccinated")),is_risk=revalue(as.character(is_risk),c("TRUE"="UHC","FALSE"="no UHC")))
	
	pd <- position_dodge(width=0.75)
	ps <- position_stack(width=0,height=0)

	p <- ggplot(gdf_UK_vs_FS,aes(x=gender,y=value*100,fill=variable))+facet_grid(smoke_bool+is_risk+vaccine~age_group,scales="free_y")
	p <- p+geom_bar(position=pd,stat="identity",width=0.75,alpha=0.85)
	p <- p+scale_fill_brewer("population",palette="Dark2",breaks=c("UK_per","FS_per"),labels=c("UK","Flusurvey"))
	p <- p+xlab("gender")+ylab("percentage of the population")
	p <- p+theme_bw()
	p <- p+theme(legend.background=element_rect(fill=NA),legend.position="top")

	file_name <- file.path(dir_figs,"pop_UK_vs_FS")
	cairo_pdf(paste(file_name,"pdf",sep="."),width=8,height=10)
	print(p)
	dev.off()

	# print table in latex for SI
	require(xtable)
	#add QALD prediction
	#TODO
	df_UK_vs_FS <- join(df_UK_vs_FS,df_pred_all,by=pop_var)
	tmp <- df_UK_vs_FS
	tmp <- mutate(tmp,UK_per2=round(UK_per*100,2),FS_per2=round(FS_per*100,2))
	tmp$UK_weight <- as.integer(tmp$UK_weight)
	tmp$freq <- as.integer(tmp$freq)
	tmp$UK_per <- NULL
	tmp$FS_per <- NULL
	tmp[tmp=="TRUE"] <- "yes"
	tmp[tmp=="FALSE"] <- "no"
	tmp <- tmp[,c("age_group","is_risk","smoke_bool","gender","vaccine","freq","FS_per2","UK_weight","UK_per2","ARI_ecdc","ILI_no_fever","ILI_fever")]
	tmp <- arrange(tmp,age_group,is_risk,smoke_bool,gender,vaccine)
	tmp <- rename(tmp,c("age_group"="age group","is_risk"="UHC","vaccine"="vaccinated","smoke_bool"="smoker","UK_weight"="population\nsize UK","UK_per2"="% of UK","freq"="population\nsize Flusurvey","FS_per2"="% of Flusurvey"))
	tmp <- mutate(tmp,gender=revalue(gender,c("male"="M","female"="F")))
	#print(tmp)
#	xtmp <- xtable(tmp,align=c("lcccr"))

	xtmp <- xtable(tmp)

	print(xtmp,file=file.path(dir_tex,"xtable_UK_vs_FS.tex"),include.rownames = FALSE,tabular.environment="longtable",scalebox=0.8)
	#,tabular.environment='longtable')


	# write.csv(rename(df_pred_UK_symptom_age,c(original_pred="median")),file=paste(file_name,"csv",sep="."))

	
	# look at prediction
	# pred_var <- c("original_pred","lower_conf","upper_conf","mean")
	pred_var <- c("pred_mean","pred_var")


	df_pred <- df_pred[c("symptom", pop_var, pred_var)]	


	df_pred_UK <- join(df_pred, df_pop,by= pop_var)
	df_pred_UK <- mutate(df_pred_UK, symptom=factor(symptom,levels=unique(symptom)))

	# df <- subset(df_pred_UK,symptom=="ILI_fever" & age_group=="45-64")
	df_pop_UK <- rename(df_UK_vs_FS[,c(pop_var,"freq","UK_weight")],c("freq"="FS","UK_weight"="UK"))
	df_pop_age_group <- aggregate_population_UK(df_pop_UK,pop_var=c("age_group"))
	df_pop_smoke <- aggregate_population_UK(df_pop_UK,pop_var=c("smoke_bool"))
	df_pop_UHC <- aggregate_population_UK(df_pop_UK,pop_var=c("is_risk"))
	df_pop_gender <- aggregate_population_UK(df_pop_UK,pop_var=c("gender"))
	df_pop_vaccine <- aggregate_population_UK(df_pop_UK,pop_var=c("vaccine"))

	df_pop <- do.call(rbind,list(df_pop_age_group,df_pop_smoke,df_pop_UHC,df_pop_gender,df_pop_vaccine))
	df_pop <- mutate(subset(df_pop,!value%in%c("FALSE","male"),select=c("variable","value","FS","FS_per","UK_per")),variable=as.character(revalue(variable,c("age_group"="age group",smoke_bool="smoker","is_risk"="UHC","vaccine"="vaccinated"))),FS=as.integer(FS),FS_per=round(FS_per,1),UK_per=round(UK_per,1))
	df_pop[df_pop=="TRUE"] <- "yes"
	df_pop$variable[2:4] <- ""
	xtmp <- xtable(df_pop)

	print(xtmp,file=file.path(dir_tex,"xtable_pop_UK_vs_FS.tex"),include.rownames = FALSE)
	#,tabular.environment='longtable')

	# Q95 <- qnorm(0.975)


	df_pred_UK_symptom_age_smoke <- aggregate_prediction_UK(df_pred_UK,c("symptom","age_group","smoke_bool"),back_to_ori=back_to_ori)

	df_pred_UK_symptom <- aggregate_prediction_UK(df_pred_UK,c("symptom"),back_to_ori=back_to_ori)

	df_pred_UK_symptom_age <- aggregate_prediction_UK(df_pred_UK,c("symptom","age_group"),back_to_ori=back_to_ori)

	df_pred_UK_symptom_smoke <- aggregate_prediction_UK(df_pred_UK,c("symptom","smoke_bool"),back_to_ori=back_to_ori)

	df_pred_UK_symptom_risk <- aggregate_prediction_UK(df_pred_UK,c("symptom","is_risk"),back_to_ori=back_to_ori)

	df_pred_UK_symptom_gender <- aggregate_prediction_UK(df_pred_UK,c("symptom","gender"),back_to_ori=back_to_ori)
	
	df_pred_UK_symptom_vaccine <- aggregate_prediction_UK(df_pred_UK,c("symptom","vaccine"),back_to_ori=back_to_ori)
	
# figs combo
	age_strip <- "age-group"
	smoke_strip <- "smoker"
	risk_strip <- "any UHC"
	severity_strip <- "symptom severity"
	df_pred_UK_symptom_age$strip <- age_strip
	df_pred_UK_symptom_smoke$strip <- smoke_strip
	df_pred_UK_symptom_risk$strip <- risk_strip
	df_pred_UK_symptom$strip <- severity_strip
	df_pred_UK_symptom$level <- NA
	df1 <- rename(df_pred_UK_symptom_age,c("age_group"="level"))
	df2 <- rename(df_pred_UK_symptom_smoke,c("smoke_bool"="level"))
	df3 <- rename(df_pred_UK_symptom_risk,c("is_risk"="level"))
	df1 <- mutate(df1,level=as.character(level))
	df2 <- mutate(df2,level=as.character(level))
	df3 <- mutate(df3,level=as.character(level))

	df <- do.call("rbind",list(df1,df2,df3,df_pred_UK_symptom))
	df <- mutate(df,strip=factor(strip,levels=c(severity_strip,age_strip,risk_strip,smoke_strip)))

	p <- ggplot(df,aes(x=symptom,y= original_pred,ymin= lower_conf, ymax=upper_conf, fill=level, colour=level))+facet_wrap(~strip)
	# p <- p+geom_bar(data=df_pred_UK_symptom,stat="identity",width=0.75,alpha=1)
	p <- p+geom_bar(position=pd,stat="identity",width=0.65,alpha=0.5)
	p <- p+geom_errorbar(position=pd,width=0.5)
	p <- p+scale_fill_brewer("levels",palette="Dark2",na.value=NA,labels=c("TRUE"="yes","FALSE"="no"))
	p <- p+scale_colour_brewer("levels",palette="Dark2",na.value="black",labels=c("TRUE"="yes","FALSE"="no"),guide="none")
	p <- p+scale_x_discrete("symptom severity",labels=c("ARI_ecdc"="ARI","ILI_no_fever"=expression(ILI[paste("no fever")]),"ILI_fever"=expression(ILI[fever])))+ylab("median QALD loss")
	p <- p+theme_bw()
	# p <- p+guides(fill=guide_legend(override.aes=list(alpha=0.85)))
	file_name <- file.path(dir_figs,"prediction_fixed_effects")
	cairo_pdf(paste(file_name,"pdf",sep="."),width=7,height=5)
	print(p)
	dev.off()

# table


	symptom <- list(var_effect="symptom",var_effect_name="severity",df_pred=df_pred_UK_symptom)
	age <- list(var_effect="age_group",var_effect_name="age",df_pred=df_pred_UK_symptom_age)
	smoke <- list(var_effect="smoke_bool",var_effect_name="smoker",df_pred=bool_to_char(df_pred_UK_symptom_smoke))
	risk <- list(var_effect="is_risk",var_effect_name="UHC",df_pred=bool_to_char(df_pred_UK_symptom_risk))
	gender <- list(var_effect="gender",var_effect_name="gender",df_pred=df_pred_UK_symptom_gender)
	vaccine <- list(var_effect="vaccine",var_effect_name="vaccine",df_pred=bool_to_char(df_pred_UK_symptom_vaccine))

	tmp <- list(symptom,age,risk,smoke,gender,vaccine)

	df_effect_UK_symptom <- ldply(tmp,function(x){return(compute_effect_UK_symptom(df_pred=x$df_pred,var_effect=x$var_effect,var_effect_name=x$var_effect_name,lme,n_digits=2))})

	print(df_effect_UK_symptom)

	xtmp <- xtable(df_effect_UK_symptom,digits=3,align=c("l","p{2cm}","p{1cm}","p{2cm}","p{1cm}","p{1cm}","p{3cm}","p{3cm}","p{3cm}"))

	print(xtmp,file=file.path(dir_tex,"xtable_regression.tex"),include.rownames = FALSE,scalebox=1,NA.string="-")

# figs
	if(0){
		p <- ggplot(df_pred_UK_symptom_age_smoke,aes(x=symptom,y= original_pred,ymin= lower_conf, ymax=upper_conf,fill=smoke_bool))+facet_wrap(~age_group)
		p <- p+geom_bar(position=pd,stat="identity",width=0.75,alpha=0.85)
		p <- p+geom_errorbar(position=pd,width=0.5)
		p <- p+scale_fill_brewer("smoke",palette="Dark2")
		p <- p+xlab("symptom")+ylab("QALD loss")
	# print(p)


		p <- ggplot(df_pred_UK_symptom_age,aes(x=symptom,y= original_pred,ymin= lower_conf, ymax=upper_conf,fill=age_group))#+facet_wrap(~age_group)
		p <- p+geom_bar(position=pd,stat="identity",width=0.75,alpha=0.85)
		p <- p+geom_errorbar(position=pd,width=0.5)
		p <- p+scale_fill_brewer("age group",palette="Dark2")
		p <- p+scale_x_discrete("symptom severity",labels=c("ARI_ecdc"="ARI","ILI_no_fever"=expression(ILI[paste("no fever")]),"ILI_fever"=expression(ILI[fever])))+ylab("median QALD loss")
		p <- p+theme_bw()
		p <- p+theme(legend.position=c(0.1,0.725),legend.background=element_rect(fill=NA))
	#p <- p+theme(legend.position="top")
		file_name <- file.path(dir_figs,"prediction_symptom_age")

		cairo_pdf(paste(file_name,"pdf",sep="."),width=5,height=3)
		print(p)
		dev.off()
		write.csv(rename(df_pred_UK_symptom_age,c(original_pred="median")),file=file.path(dir_prediction,paste(basename(file_name),"csv",sep=".")))

		p <- ggplot(df_pred_UK_symptom_smoke,aes(x=symptom,y= original_pred,ymin= lower_conf, ymax=upper_conf,fill=smoke_bool))#+facet_wrap(~age_group)
		p <- p+geom_bar(position=pd,stat="identity",width=0.75,alpha=0.85)
		p <- p+geom_errorbar(position=pd,width=0.5)
		p <- p+scale_fill_brewer("",palette="Dark2",labels=c("TRUE"="Smoker","FALSE"="Non smoker"))
		p <- p+scale_x_discrete("symptom severity",labels=c("ARI_ecdc"="ARI","ILI_no_fever"=expression(ILI[paste("no fever")]),"ILI_fever"=expression(ILI[fever])))+ylab("median QALD loss")
		p <- p+theme_bw()	
		p <- p+theme(legend.position=c(0.15,0.86),legend.background=element_rect(fill=NA),legend.direction="vertical")

		file_name <- file.path(dir_figs,"prediction_symptom_smoke")
		cairo_pdf(paste(file_name,"pdf",sep="."),width=5,height=3)
		print(p)
		dev.off()
		write.csv(df_pred_UK_symptom_smoke,file=file.path(dir_prediction,paste(basename(file_name),"csv",sep=".")))

		p <- ggplot(df_pred_UK_symptom_risk,aes(x=symptom,y= original_pred,ymin= lower_conf, ymax=upper_conf,fill=is_risk))#+facet_wrap(~age_group)
		p <- p+geom_bar(position=pd,stat="identity",width=0.75,alpha=0.85)
		p <- p+geom_errorbar(position=pd,width=0.5)
		p <- p+scale_fill_brewer("Underlying health conditions",palette="Dark2",labels=c("TRUE"="yes","FALSE"="no"))
		p <- p+scale_x_discrete("symptom severity",labels=c("ARI_ecdc"="ARI","ILI_no_fever"=expression(ILI[paste("no fever")]),"ILI_fever"=expression(ILI[fever])))+ylab("median QALD loss")
		p <- p+theme_bw()	
		p <- p+theme(legend.position=c(0.25,0.8),legend.background=element_rect(fill=NA),legend.direction="vertical")

		file_name <- file.path(dir_figs,"prediction_symptom_risk")
		cairo_pdf(paste(file_name,"pdf",sep="."),width=5,height=3)
		print(p)
		dev.off()
		write.csv(df_pred_UK_symptom_risk,file=file.path(dir_prediction,paste(basename(file_name),"csv",sep=".")))

		p <- ggplot(df_pred_UK_symptom,aes(x=symptom,y= original_pred,ymin= lower_conf, ymax=upper_conf))#+facet_wrap(~age_group)
		p <- p+geom_bar(position=pd,stat="identity",width=0.75,alpha=0.85,fill=NA,colour="black")
		p <- p+geom_errorbar(position=pd,width=0.5)
	#p <- p+scale_fill_brewer("",palette="Dark2",guide="none")
		p <- p+scale_x_discrete("symptom severity",labels=c("ARI_ecdc"="ARI","ILI_no_fever"=expression(ILI[paste("no fever")]),"ILI_fever"=expression(ILI[fever])))+ylab("median QALD loss")
		p <- p+theme_bw()	

		file_name <- file.path(dir_figs,"prediction_symptom")
		cairo_pdf(paste(file_name,"pdf",sep="."),width=5,height=3)
		print(p)
		dev.off()
		write.csv(df_pred_UK_symptom,file=file.path(dir_prediction,paste(basename(file_name),"csv",sep=".")))
	}

}

plot_sample_characterictics <- function(dir_profile){

	if(!file.exists(dir_profile)){
		dir.create(dir_profile)
	}


	df_profile <- readRDS(file = paste(RSAVE, "profile_clean.rds", sep = "/"))

	df <- subset(df_profile,!is.na(region))
	p <- ggplot(df,aes(x=region,fill=age_group))
	p <- p+geom_histogram()
	p <- p+scale_fill_brewer("age group",palette="Dark2",guide=guide_legend(reverse=T))
	p <- p+theme_bw()
	p<-p+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1),legend.position=c(0.9,0.7),legend.background=element_rect(fill=NA))
	p <- p+xlab("UK region")+ylab("number of participants")
	cairo_pdf(file.path(dir_profile,"region_participants.pdf"),width=6,height=4)
	print(p)
	dev.off()




}

smoke_as_logical <- function(x) {
	return(as.logical(revalue(x,c("no"="FALSE","occasionally"="TRUE","<10_per_day"="TRUE",">10_per_day"="TRUE","unknown"="NA"))))
}

QALD_analysis<-function(SD_max=30){


	episode_suffix <- paste0("mix_SD_SD__SDmax=",SD_max,"_baseline_summary=median")

	df_data<-readRDS(file = paste(RSAVE, "data_renamed__symptom_summarized__auto_clean__no_duplicate.rds", sep = "/"))
	df_profile <- readRDS(file = paste(RSAVE, "profile_clean.rds", sep = "/"))	
	#	df_episode <-readRDS(file = paste(RSAVE, "df_episode.rds", sep = "/"))
	df_episode <-readRDS(file = file.path(RSAVE, paste0("df_episode_",episode_suffix,".rds")))

	df_episode <-join(df_episode,df_profile,by="person_id")
	df_episode$symptom <- revalue(df_episode$symptom,c(ILI_ecdc="ILI_no_fever"))

	#tmp <- readRDS(file="./for_alma/all_episodes_08102013.rds")
	#saveRDS(df_episode,file="./for_alma/all_episodes_08102013.rds")
	#warnings
	W_names<-grep("W_",names(df_episode),value=T)
	W_approx_names <- grep("approx",W_names,value=T)

	df_episode <- df_episode[,c(setdiff(names(df_episode),W_names),W_names)]

	print(dim(df_episode))

	approx_S_start = c("W_S_start_too_far", "W_S_start_before_previous_report","W_S_start_after_S_end", "W_S_start_wrong")
	approx_S_end = c("W_S_end_too_far", "W_S_end_before_previous_report", "W_S_start_after_S_end", "W_S_end_wrong")	

	#summary warnings
	#	summary(df_episode[W_names])
	#	summary(df_episode[W_approx_names])
	#	summary(df_episode[approx_S_start])
	#	summary(df_episode[approx_S_end])

	##start analysis here

	## some preliminary plots
	#pre_pdf <- file.path(PDF,"preliminary_plots")
	pre_pdf <- file.path(PDF,paste0("preliminary_plots_",episode_suffix))

	if(!file.exists(pre_pdf)){
		dir.create(pre_pdf)
	}

	#symptom duration with and without approximation
	#exclude those with a S_start or S_end warning but no approximation
	to_parse <- paste(c(approx_S_start,approx_S_end),collapse=" | ")
	df_episode_exclude <-subset(df_episode,eval(parse(text=to_parse)) & !(W_approximate_S_start | W_approximate_S_end))
	df_episode_clean <-diff_df(df_episode,df_episode_exclude)
	# print_stat_pop(df_episode_clean)

	if(0){
		tmp <-subset(df_episode, W_approximate_S_end)
		tmp <- mutate(tmp,wday_s_end=wday(symptom_end,label=T))

		p <- ggplot(tmp,aes(x= wday_s_end))
		p <- p+geom_bar()
		print(p)
	}	

	#rename symptoms
	df_episode <- mutate(df_episode,symptom=revalue(symptom,c("ARI_ecdc"="ARI","ILI_no_fever"="ILI no fever","ILI_fever"="ILI fever")))

	if(0){
	#
		plot_symptom_duration(df_episode_clean,pre_pdf)
	#drop in health-score
		plot_health_score(df_episode,pre_pdf)

	#time line of health score
		plot_timeline_health_score(df_data, df_episode_clean, pre_pdf,x_max=16,force=F,compare_whiskers=F)

	#phone and visit medical

		plot_assistance(df_episode,type="phone",time=F,to_by_line=T,dir_pdf=pre_pdf)
		plot_assistance(df_episode,type="phone",time=T,to_by_line=T,dir_pdf=pre_pdf)
		plot_assistance(df_episode,type="visit",time=F,to_by_line=T,dir_pdf=pre_pdf)
		plot_assistance(df_episode,type="visit",time=T,to_by_line=T,dir_pdf=pre_pdf)

	#time off

		plot_off_work(df_episode,group_by="employment",worker_student_only=T,dir_pdf=pre_pdf,print_table=TRUE)
		plot_off_work(df_episode,group_by="employment",worker_student_only=T,time_off=T,dir_pdf=pre_pdf,print_table=TRUE)
	}
	if(0){

		#for alma: average time off school for under 18
		df <- subset(df_episode,age_group=="0-17" & symptom!="ARI_ecdc" & !is.na(time_off))

		df$time_off_num <- as.numeric(revalue(df$time_off,c("1 day"="1","2 days"="2","3 days"="3","4 days"="4","5 days"="5","6-10 days"="8","11-15 days"="13",">15 days"="15")))
		mean(df$time_off_num)
		median(df$time_off_num)
		dim(df)

		#for Marc: risk ratio to visit_GP
		risk_ratio_analysis(df_episode)

	}

	#prepare for regression

	df_reg <- mutate(df_episode_clean,person_id_num=as.numeric(as.factor(person_id)),smoke_bool=smoke_as_logical(smoke))
		# print_info(df_reg,not_useful="W_S_start_too_far | W_S_end_too_far | is.na(QALD_loss) | QALD_loss<=0 | is.na(symptom) | is.na(age_group) | is.na(smoke_bool) | is.na(is_risk) | is.na(vaccine) | is.na(gender)",dir_tex="/Users/Tonton/work/arcticles/QALY_flusurvey/paper")

	df_reg <- subset(df_reg, !is.na(QALD_loss) & QALD_loss>0 )

	df_reg_fever <- mutate(df_reg, fever=as.logical(revalue(symptom,c("ARI_ecdc"="FALSE","ILI_no_fever"="FALSE","ILI_fever"="TRUE"))),symptom=revalue(symptom,c("ILI_no_fever"="ILI_ecdc","ILI_fever"="ILI_ecdc")))

	#	summary(df_reg[W_names])
	#	summary(df_reg[W_approx_names])
	#	summary(df_reg[approx_S_start])
	#	summary(df_reg[approx_S_end])

	if(0){
	########
	# full model
	#######
		df_outlier_1 <- lme_regression(df_reg, formula = "QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 10, test_dist = T, CI_interval="confidence",suffix= episode_suffix,plot_outlier_TS=T,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="smoke_bool",use_facet_grid=T,facet_formula="is_risk~age_group") 

	#df_outlier_smoke <- lme_regression(df_reg, formula = "QALD_loss~symptom+age_group+smoke+is_risk+vaccine+gender",transformation = "boxcox", max_response = 10, test_dist = T, CI_interval="confidence",suffix= episode_suffix,plot_outlier_TS=T,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="smoke",use_facet_grid=T,facet_formula="is_risk~gender") 

	#remove all outliers and re-run
		df_remove <- match_df(df_reg, df_outlier_1,on=c("person_id","n_bout"))
		df_reg_wo_1 <- diff_df(df_reg, df_remove)
		df_outlier_wo_1 <- lme_regression(df_reg_wo_1, formula = "QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 10, test_dist = T, CI_interval="confidence",suffix= paste0(episode_suffix,"_wo"),plot_outlier_TS=T,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="smoke_bool",use_facet_grid=T,facet_formula="is_risk~age_group") 

	#df_stat <- compute_stat_episode(df_reg_1_wo,id_vars=c("symptom"))	
	#arrange(df_stat,variable)

	#df_stat <- compute_stat_episode(df_reg_1_wo,id_vars=c("age_group","symptom"),measure=c("QALD_loss"))	
	#arrange(df_stat,variable,symptom)


	#####
	# sensitivity analysis
	}
	#full info
	to_parse <- paste(W_approx_names,collapse=" | ")
	df_reg_exclude <-subset(df_reg,eval(parse(text=to_parse)))
	df_reg_full_info <-diff_df(df_reg, df_reg_exclude)
	# print_stat_pop(df_reg_full_info)

	# df_outlier_full_info <- lme_regression(df_reg_full_info, formula = "QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 10, test_dist = T, CI_interval="confidence",suffix= "full_info",plot_outlier_TS=T,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="smoke_bool",use_facet_grid=T,facet_formula="is_risk~age_group") 
	# list_pred_full_info <- lme_regression(df_reg_full_info, formula = "QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender-1",transformation = "boxcox", max_response = 10, test_dist = F, CI_interval="confidence",suffix= "full_info",predict_all=T,plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="smoke_bool",use_facet_grid=T,facet_formula="is_risk~age_group",return_outliers=FALSE) 

	# regression on the symptom duration

	df_reg_sd <- mutate(df_reg_full_info,S_duration2=symptom_duration+0.1,age_group2=revalue(age_group,c("0-17"="0-64","18-44"="0-64","45-64"="0-64")))
	tmp <- lme_regression(df_reg_sd, formula = "S_duration2~symptom+age_group2+med_no-1",transformation = "boxcox", max_response = 20, test_dist = F, CI_interval="prediction",suffix= paste0("full_info_SDmax=",S_duration_max),plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",x_lab="symptom severity",fill_var="age_group2",fill_lab="age group",y_lab="symptom duration (in days)",return_outliers=F) 


	if(0){
	#remove baseline sampled
		to_parse <- "!W_approximate_baseline_health_score"
		df_reg_remove_baseline <- subset(df_reg,eval(parse(text=to_parse)))
	# print_stat_pop(df_reg_remove_baseline)
		df_outlier_remove_baseline <- lme_regression(df_reg_remove_baseline, formula = "QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 10, test_dist = T, CI_interval="confidence",suffix= paste0(episode_suffix,"_remove_baseline"),plot_outlier_TS=T,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="smoke_bool",use_facet_grid=T,facet_formula="is_risk~age_group") 
	}

	#remove S_start/S_end too far
	to_parse <- "!W_S_start_too_far & !W_S_end_too_far"
	df_reg_remove_too_far <- subset(df_reg,eval(parse(text=to_parse)))
	# print_stat_pop(df_reg_remove_too_far)
	# df_outlier_remove_too_far <- lme_regression(df_reg_remove_too_far, formula = "QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 10, test_dist = F, CI_interval="confidence",suffix= paste0(episode_suffix,"_remove_too_far"),plot_outlier_TS=T,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="smoke_bool",use_facet_grid=T,facet_formula="is_risk~age_group") 

	# predict_all
	# list_pred <- lme_regression(df_reg_remove_too_far, formula = "QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender-1",transformation = "boxcox", max_response = 10, test_dist = F, CI_interval="confidence",suffix= paste0(episode_suffix,"_remove_too_far_predict_all"),predict_all=T,plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="smoke_bool",use_facet_grid=T,facet_formula="is_risk~age_group",return_outliers=FALSE) 
	# list_pred <- lme_regression(df_reg_remove_too_far, formula = "QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender+med_no+visit_no-1",transformation = "boxcox", max_response = 10, test_dist = F, CI_interval="confidence",suffix= paste0(episode_suffix,"_remove_too_far_predict_all"),predict_all=T,plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="smoke_bool",use_facet_grid=T,facet_formula="is_risk~age_group",return_outliers=FALSE) 

	df_reg_sd <- mutate(df_reg_remove_too_far,S_duration2=symptom_duration+0.1,age_group2=revalue(age_group,c("0-17"="0-64","18-44"="0-64","45-64"="0-64")))
	tmp <- lme_regression(df_reg_sd, formula = "S_duration2~symptom+age_group2+med_no-1",transformation = "boxcox", max_response = 20, test_dist = F, CI_interval="prediction",suffix= paste0("remove_too_far_SDmax=",S_duration_max),plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",x_lab="symptom severity",fill_var="age_group2",fill_lab="age group",y_lab="symptom duration (in days)",return_outliers=F) 

# time line of health score
	# plot_timeline_health_score(df_data, df_reg_remove_too_far, pre_pdf,x_max=20,force=T,compare_whiskers=F,interpolate=F)
	# plot_symptom_duration(df_reg_remove_too_far,pre_pdf,30)
	#drop in health-score
	# plot_health_score(df_reg_remove_too_far,pre_pdf)

	if(0){
	#remove S_start/S_end too far/before previous
		to_parse <- "!W_S_start_too_far & !W_S_start_before_previous_report & !W_S_end_too_far & !W_S_end_before_previous_report"
		df_reg_remove_too_far_prev <-subset(df_reg,eval(parse(text=to_parse)))
	# print_stat_pop(df_reg_remove_too_far_prev)
		df_outlier_remove_too_far_prev <- lme_regression(df_reg_remove_too_far_prev, formula = "QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 10, test_dist = T, CI_interval="confidence",suffix= paste0(episode_suffix,"_remove_too_far_prev"),plot_outlier_TS=T,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="smoke_bool",use_facet_grid=T,facet_formula="is_risk~age_group") 

	# predict_all
		list_pred <- lme_regression(df_reg_remove_too_far_prev, formula = "QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 10, test_dist = F, CI_interval="confidence",suffix= paste0(episode_suffix,"_remove_too_far_prev_predict_all"),predict_all=T,plot_outlier_TS=F,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="smoke_bool",use_facet_grid=T,facet_formula="is_risk~age_group",return_outliers=FALSE) 



	########
	# regression on age and symptom, remove start_approx only
	#######
		to_parse <- "W_approximate_S_start & !W_approximate_S_end & !W_approximate_baseline_health_score"
		df_reg_exclude <-subset(df_reg,eval(parse(text=to_parse)))
		df_reg_remove_S_start_approx <-diff_df(df_reg, df_reg_exclude)
	# print_stat_pop(df_reg_remove_S_start_approx)
		df_outlier_remove_S_start_approx <- lme_regression(df_reg_remove_S_start_approx, formula = "QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 10, test_dist = T, CI_interval="confidence",suffix= paste0(episode_suffix,"_remove_S_start_approx"),plot_outlier_TS=T,df_data= df_data,df_profile=df_profile,x_var="symptom",fill_var="smoke_bool",use_facet_grid=T,facet_formula="is_risk~age_group") 

	#
		tmp <- ddply(df_reg,c("person_id"),function(df){
			df$select <- with(df,all(QALD_loss>0.2 & QALD_loss<3 & length(unique(symptom))>2)) # & all(!W_approximate_baseline_health_score)
			return(df)	
		},.progress="text")

		example_TS <- subset(tmp,select & n_bout==3)
		length(unique(example_TS$person_id))

		cairo_pdf(paste(pre_pdf,"TS_example.pdf",sep='/'),width=9,height=6)
	# cairo_pdf(paste(pre_pdf,"TS_example.pdf",sep='/'),width=30,height=30)
	#[-c(2,6,9)]#"42931d22-34d7-4893-a22a-9f7817a3fbc5"
		id <- example_TS$person_id[-6] #[-c(7,11)]
		plot_TS_ind(id,df_profile,df_data,df_episode,strip_label="simple",palette="Dark2")
		dev.off()
	}
# #poster

	# tmp <- ddply(df_reg,c("person_id"),function(df){
	# 	df$select <- with(df,all(QALD_loss>0.2 & QALD_loss<3) & length(unique(symptom))>2 & all(!W_approximate_baseline_health_score))
	# 	return(df)	
	# },.progress="text")

	# length(unique(tmp$person_id))
	# example_TS <- subset(tmp,select & n_bout>3)
	# length(unique(example_TS$person_id))
	# dim(example_TS)

	# cairo_pdf(paste(pre_pdf,"TS_example.pdf",sep='/'),width=6,height=3)
	# plot_TS_ind(c("f4ddd6b7-44cf-4724-bbf7-6cc9c82cad62"),df_profile,df_data,df_episode,palette="Dark2",strip_label="full")#[c(1:9)+20]
	# dev.off()



	if(0){

	########
	# regression on age and symptom (reg_fever)	
	#######

		df_outlier_0_1 <- lme_regression(df_reg_fever, formula = "QALD_loss~symptom+age_group",transformation = "boxcox", max_response = 10, test_dist = T, CI_interval="confidence",suffix=paste(episode_suffix,"ARI_ILI_ecdc",sep="_"),plot_outlier_TS=T,df_data= df_data,df_profile=df_profile) 

	#remove all outliers and re-run
		df_remove <- match_df(df_reg_fever, df_outlier_0_1,on=c("person_id","n_bout"))
		df_reg_0_wo <- diff_df(df_reg_fever, df_remove)
		df_outlier_0_2 <- lme_regression(df_reg_0_wo, formula = "QALD_loss~symptom+age_group",transformation = "boxcox", max_response = 10, test_dist = T, CI_interval="confidence",suffix=paste(episode_suffix,"ARI_ILI_ecdc","wo",sep="_"),df_data= df_data,df_profile=df_profile) 

		df_stat <- compute_stat_episode(df_reg_0_wo,id_vars=c("symptom"))	
		arrange(df_stat,variable)

		df_stat <- compute_stat_episode(df_reg_0_wo,id_vars=c("age_group","symptom"))	
		arrange(df_stat,variable,symptom)

		df_stat <- compute_stat_episode(df_reg_0_wo,id_vars=c("symptom","age_group"),measure_vars=c("symptom_duration","QALD_loss"),n_digits=2)	
		na.omit(arrange(df_stat,symptom, variable))
	########
	# regression on age, symptom, fever	and smoke_bool
	#######

		df_outlier_0_1_1 <- lme_regression(df_reg_fever, formula = "QALD_loss~symptom+age_group+fever+smoke_bool+is_risk+vaccine+gender",transformation = "boxcox", max_response = 10, test_dist = T, CI_interval="confidence",suffix=paste(episode_suffix,"ARI_ILI_ecdc",sep="_"),plot_outlier_TS=T,df_data= df_data,df_profile=df_profile) 

	#remove all outliers and re-run
		df_remove <- match_df(df_reg_fever, df_outlier_0_1_1,on=c("person_id","n_bout"))
		df_reg_0_1_wo <- diff_df(df_reg_fever, df_remove)
		df_outlier_0_1_2 <- lme_regression(df_reg_0_1_wo, formula = "QALD_loss~symptom+age_group+fever+smoke_bool+is_risk+vaccine",transformation = "boxcox", max_response = 10, test_dist = T, CI_interval="confidence",suffix=paste(episode_suffix,"ARI_ILI_ecdc_wo",sep="_"),df_data= df_data,df_profile=df_profile) 

		df_stat <- compute_stat_episode(df_reg_0_wo,id_vars=c("symptom"))	
		arrange(df_stat,variable)

		df_stat <- compute_stat_episode(df_reg_0_wo,id_vars=c("age_group","symptom"))	
		arrange(df_stat,variable,symptom)

		df_stat <- compute_stat_episode(df_reg_0_wo,id_vars=c("symptom"),measure_vars=c("QALD_loss"))	
		arrange(df_stat,variable,symptom)

	#some interesting guys to plot"
	#	
	#



	############################################################################################################################################################################################################################################################
	############################################################################################################################################################################################################################################################
	############################################################################################################################################################################################################################################################
	############################################################################################################################################################################################################################################################
	############################################################################################################################################################################################################################################################
	#old stuff
	########################################
	# random interaction term
	########################################
		df_reg <- subset(df_episode_clean, !is.na(age_group) & !is.na(QALD_loss) & QALD_loss>0 )
	#subset with more than one episode on each symptom
		tmp <- subset(count(unique(df_reg[,c("person_id","symptom")]),vars="person_id"),freq==3)
	#37 individuals
		df_mult_epi <- match_df(df_reg,tmp,on="person_id")
		# print_stat_pop(df_mult_epi,var_episode=c("person_id","symptom"))
	#quite unbalanced
	#plot some ind TS
		id=c("6c0e385e-54b9-4e5a-b597-4aa6061313a2","b0f57731-3b94-4a3b-9133-b94e0dd5442a")
		plot_TS_ind(id,df_profile,df_data,df_episode)
	####
	#
		with(df_mult_epi,interaction.plot(symptom,person_id,QALD_loss,legend=0))
	#bc transform
		lm_bc <- powerTransform(QALD_loss~symptom-1, data = df_mult_epi)
		coef_bc<-lm_bc$roundlam
		df_mult_epi <- mutate(df_mult_epi,QALD_loss_bc=bcPower(QALD_loss, coef_bc),symptom=factor(symptom,ordered=F))


	#follow book
	#
		lmlist1 <- lmList(QALD_loss_bc~symptom|person_id,data=df_mult_epi[c("QALD_loss_bc","symptom","person_id")])
		plot(intervals(lmlist1),labels=F)
		pairs(lmlist1,id=0.01,adj=-0.5)

	#use lme
		lme1 <- lme(QALD_loss_bc~symptom,random=~ 1 |person_id,data=df_mult_epi)
		summary(lme1)
		compFit <- compareFits(coef(lmlist1),coef(lme1))
		plot(compFit,mark=fixef(lme1))
	# line's are not parallel so there must me some interaction
	#basic lme
		lme1 <- lme(QALD_loss_bc~symptom,random=~1|person_id,data=df_mult_epi,control=list(opt="optim"),method="ML")
		summary(lme1)
	#VarCorr(lme1)
		anova(lme1)
		plot(lme1)
		intervals(lme1)

	# age is not significant
		lme2 <- update(lme1,fixed=~.+age_group)
		summary(lme2)
		anova(lme1,lme2)
		intervals(lme2)
	#lme2 <- lme1
	#test random interaction
		lme3 <- update(lme1,random=~1|person_id/symptom)
		summary(lme3)
		anova(lme1,lme3)
	#not significative
		plot(lme3)
		intervals(lme3)#doesn't work: cannot get confidence intervals on var-cov components: Non-positive definite approximate variance-covariance

	#more general random interaction (one var-covar matrix per individual)
		lme4 <- update(lme1,random=~symptom-1|person_id)
		summary(lme4)
		anova(lme4)
		anova(lme1,lme3,lme4)
	#prefer lme1

	######################### 
	#simple regression
	######################### 

	#full regression
		df_reg<-subset(df_episode_clean, !is.na(age_group) & !is.na(QALD_loss) & QALD_loss>0 & !is.na(gender) & !is.na(vaccine) & !is.na(smoke) & !is.na(is_risk))
		# print_stat_pop(df_profile, df_reg)

		lm_regression(df_reg,formula="QALD_loss~symptom+age_group+pregnant+smoke+gender+vaccine+is_risk",transformation="boxcox",test_dist=F,model_selection=T)

	#aggregate ILI_ecdc and ILI_fever
		df_episode<-mutate(df_episode,symptom_2=revalue(symptom,c(ILI_fever="ILI_ecdc")))
		df_episode$symptom_2[df_episode$symptom=="ILI_fever"]
		df_reg<-subset(df_episode, !is.na(age_group) & !is.na(QALD_loss) & QALD_loss>0)
		# print_stat_pop(df_profile, df_reg)

		lm_regression(df_reg,formula="QALD_loss~symptom_2+age_group",transformation="boxcox",test_dist=T)

	#lm regression
		df_reg<-subset(df_episode, !is.na(age_group) & !is.na(QALD_loss) & QALD_loss>0)
		# print_stat_pop(df_profile, df_reg)

		lm_regression(df_reg,formula="QALD_loss~symptom+age_group",transformation="boxcox",test_dist=T)
	#outlier
		rownames_outlier<-as.character(c(3744,60,4419))
		df_outlier<-df_reg[rownames_outlier,]
		plot_TS_ind(id= unique(df_outlier$person_id), df_profile, df_data, df_episode)
	#leverage
		rownames_leverage<-as.character(c(4297,4498,4653))
		df_leverage<-df_reg[rownames_leverage,]
		plot_TS_ind(id= unique(df_leverage$person_id), df_profile, df_data, df_episode)
	#remove outlier and leverage
		df_reg2<-diff_df(df_reg, rbind(df_outlier,df_leverage))

	#re-analyse
		lm_regression(df_reg2,formula="QALD_loss~symptom+age_group",transformation="boxcox",test_dist=T)
	#outlier
		rownames_outlier<-as.character(c(2338,1957,1794))
		df_outlier<-df_reg2[rownames_outlier,]
		plot_TS_ind(id= unique(df_outlier$person_id), df_profile, df_data, df_episode)
	#leverage
		rownames_leverage<-as.character(c(1557,1531,2023))
		df_leverage<-df_reg2[rownames_leverage,]
		plot_TS_ind(id= unique(df_leverage$person_id), df_profile, df_data, df_episode)

	#fdaMixed regression: works only for balanced data
		require(fdaMixed)
		df_mult_epi2 <- ddply(df_mult_epi,c("person_id","symptom"),function(df){df[1,]})
		est0 <- fdaLm(QALD_loss|person_id ~ age_group+symptom|1,data= arrange(df_mult_epi2,person_id),boxcox=0.07)
		summary(est0)
	}
}

#symptom duration:
#duration of symptoms by symptom and age group
plot_symptom_duration<-function(df_episode,dir_pdf,S_duration_max=25){

	gdf<-subset(df_episode, !is.na(age_group) & !is.na(symptom_duration) & symptom_duration>0)
	# print_stat_pop(gdf)

	df_count<-count(gdf,vars=c("age_group","symptom"))

	p<-ggplot(gdf,aes(y=as.numeric(symptom_duration),x=age_group,fill= symptom))#+facet_wrap(~is_risk)
	p<-p+geom_boxplot(position="dodge")
	p<-p+geom_text(data=df_count,aes(x=age_group,label=paste("n =",freq),y=0.5-as.numeric(symptom),colour=symptom),vjust=0,size=3)	
	p<-p+scale_fill_brewer("Symptom\nseverity",palette="Dark2",breaks = c("ARI_ecdc", "ILI_no_fever", "ILI_fever"), labels=c("ARI",expression(ILI["no fever"]),expression(ILI[fever])))	
	p<-p+scale_colour_brewer("symptom",palette="Dark2",guide="none")	
	p<-p+xlab("age group")+ylab("symptom duration")+ coord_cartesian(ylim = c(-3,S_duration_max))	
	p <- p+theme_bw() %+replace% theme(legend.key=element_rect(size=0),legend.text.align=0)
	cairo_pdf(paste(dir_pdf,"symptom_duration_boxplot.pdf",sep='/'),width=7,height=4)
	print(p)
	dev.off()

	#subset: full information	
	gdf<-mutate(gdf,info=factor(W_approximate_S_start+W_approximate_S_end*2))
	gdf$info<-revalue(gdf$info,c("0"="full","1"="S_start_approx","2"="S_end_approx","3"="S_start_end_approx"))
	#sample size
	df_count<-count(gdf,vars=c("age_group","symptom","info"))

	p<-ggplot(gdf)+facet_grid(symptom~age_group)
	p<-p+geom_boxplot(data=gdf,aes(y=as.numeric(symptom_duration),x=info,fill= info),position="dodge")
	p<-p+geom_text(data=df_count,aes(x=info,label=paste("n =",freq),y=-2),vjust=0,size=3)
	p<-p+scale_fill_brewer("symptom",palette="Accent")	
	p<-p+xlab("info")+ylab("symptom duration")+ coord_cartesian(ylim = c(-3,S_duration_max))	
	p<-p+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1),legend.position="top")
	cairo_pdf(paste(dir_pdf,"symptom_duration_boxplot_check_approx.pdf",sep='/'),width=9,height=6)
	print(p)
	dev.off()


	#full info only
	gdf_full <- subset(gdf,info=="full")
	df_count_full<-subset(df_count,info=="full")
	p<-ggplot(gdf_full,aes(y=as.numeric(symptom_duration),x=age_group,fill= symptom))#+facet_wrap(~is_risk)
	p<-p+geom_boxplot(position="dodge")
	p<-p+geom_text(data=df_count_full,aes(x=age_group,label=paste("n =",freq),y=0.5-as.numeric(symptom),colour=symptom),vjust=0,size=3)	
	p<-p+scale_fill_brewer("symptom",palette="Dark2")	
	p<-p+scale_colour_brewer("symptom",palette="Dark2")		
	p<-p+xlab("age group")+ylab("symptom duration")+ coord_cartesian(ylim = c(-3,S_duration_max))	
	cairo_pdf(paste(dir_pdf,"symptom_duration_boxplot_full_info.pdf",sep='/'),width=7,height=4)
	print(p)
	dev.off()
}


plot_health_score<-function(df_episode,dir_pdf){

	#exclude those without a minimum health score
	df<-subset(df_episode, !is.na(age_group) & !is.na(min_health_score))
	# print_stat_pop(df)

	df_count<-count(df,vars=c("age_group","symptom"))

	gdf <- melt(df,measure.vars=c("baseline_health_score","min_health_score"))
	gdf$variable<-revalue(gdf$variable,c("baseline_health_score"="baseline","min_health_score"="lowest"))
	gdf$symptom <- revalue(gdf$symptom,c("ARI_ecdc"="ARI","ILI_no_fever"=expression(ILI[paste("no fever")]),"ILI_fever"=expression(ILI[fever])))
	df_count$symptom <- revalue(df_count$symptom,c("ARI_ecdc"="ARI","ILI_no_fever"=expression(ILI[paste("no fever")]),"ILI_fever"=expression(ILI[fever])))

	p<-ggplot(gdf)+facet_grid(~symptom,scales="fixed",labeller=label_parsed)
	p<-p+geom_boxplot(aes(y=value,x=age_group,fill=variable),position="dodge")
	p<-p+geom_text(data=df_count,aes(x=age_group,label=paste("n =",freq),y=0),vjust=1.2,size=3)	
	p<-p+scale_fill_brewer("Health-score",palette="Dark2")	
	p<-p+xlab("age group")+ylab("health-score")
	p <- p+theme_bw() %+replace% theme(legend.key=element_rect(size=0),legend.text.align=0)
	cairo_pdf(paste(dir_pdf,"health_score_boxplot.pdf",sep='/'),width=9,height=4)
	print(p)
	dev.off()


	gdf<-mutate(gdf,info=as.character(W_approximate_baseline_health_score))
	gdf$info<-revalue(gdf$info,c("FALSE" ="computed","TRUE" ="sampled"))
	#sample size
	df_count<-count(gdf,vars=c("age_group","symptom","info"))
	df_count$freq <- df_count$freq/length(c("baseline_health_score","min_health_score"))

	p<-ggplot(gdf)+facet_grid(symptom~age_group)
	p<-p+geom_boxplot(data=gdf,aes(y=value,x=variable,fill= info),position="dodge")
	p<-p+geom_text(data=df_count,aes(x=1.3,label=paste("n =",freq),y=rev(as.numeric(as.factor(info))*12),colour=info),vjust=0,size=3)
	p<-p+scale_fill_brewer("baseline",palette="Accent")	
	p<-p+scale_colour_brewer("baseline",palette="Accent")	
	p<-p+xlab("type")+ylab("score")
	p<-p+theme(legend.position="top")
	cairo_pdf(paste(dir_pdf,"health_score_boxplot_check_approx.pdf",sep='/'),width=7,height=6)
	print(p)
	dev.off()


	#full information only
	gdf_full <- subset(gdf,info=="computed")
	df_count_full<-subset(df_count,info=="computed")

	p<-ggplot(gdf_full)+facet_wrap(~symptom,scales="free_y")
	p<-p+geom_boxplot(aes(y=value,x=age_group,fill=variable),position="dodge")
	p<-p+geom_text(data=df_count_full,aes(x=age_group,label=paste("n =",freq),y=0),vjust=1.2,size=3)	
	p<-p+scale_fill_brewer("health score",palette="Dark2")	
	p<-p+xlab("age group")+ylab("health score")
	cairo_pdf(paste(dir_pdf,"health_score_boxplot_full_info.pdf",sep='/'),width=9,height=4)
	print(p)
	dev.off()

}

compute_boxplot_no_outlier <- function(data, id_vars = c("symptom", "age_group", "time_SSS"), value_name = "health_score_rescaled") {

	cat("computing boxplot without outliers\n")
	flush.console()

	tmp <- ddply(data, id_vars, function(df) {

		x <- df[[value_name]]
		ans <- as.data.frame(t(boxplot.stats(x)$stats))
		#ans <- as.data.frame(t(quantile(x,prob=c(0.025,0.25,0.5,0.75,0.975))))
		names(ans) <- c("ymin", "lower", "middle", "upper", "ymax")
		return(ans)

	}, .progress = "text")

	tmp <- melt(tmp, measure.vars = c("ymin", "lower", "upper", "ymax"))
	tmp <- mutate(tmp, type = revalue(variable, replace = c(ymin = "whiskers", lower = "IQR", upper = "IQR", ymax = "whiskers")), variable = revalue(variable, replace = c(lower = "ymin", upper = "ymax")))
	my_formula <- formula(paste(paste(setdiff(names(tmp), c("value", "variable")), collapse = "+"), "variable", sep = "~"))
	gdf <- dcast(tmp, my_formula, value.var = "value")

	return(gdf)

}

plot_timeline_health_score<-function(df_data,df_episode,dir_pdf,x_max=20,force=FALSE,to_check="W_approximate_S_start | W_approximate_S_end | W_approximate_baseline_health_score",compare_whiskers=FALSE, interpolate=TRUE){

	df_episode_clean<-subset(df_episode,!is.na(age_group) & !is.na(symptom_duration) & symptom_duration>0 & !is.na(QALD_loss) & QALD_loss > 0)
	# print_stat_pop(df_episode_clean)
	flush.console()

	#for each episode take all reports between symptom start and symptom end date
	df_2timeline<-match_df(df_data, df_episode_clean,on=c("person_id","n_bout"))

	max_duration <- max(df_episode_clean$symptom_duration)

	RDSfile <- file.path(RSAVE, "timeline_health_score.rds")

	if(force || !file.exists(RDSfile)){
		cat("Compute all timelines, that can take some time...")
		flush.console()
		df_timeline <- ddply(df_2timeline,c("person_id","n_bout"),function(df){

			#print(c(df$person_id,df$n_bout))

			df_epi<-match_df(df_episode_clean,df,on=c("person_id","n_bout"))

			ans <- compute_QALD_loss(df, df_epi$baseline_health_score,df_epi$symptom_start, df_epi$symptom_end, symptom_end_at_baseline=TRUE , timeline_only = TRUE)

			if(interpolate){
				#linear interpolation until max duration
				ans2 <-data.frame(approx(x=ans$time_SSS,y=ans$health_score,xout=-1:max_duration,rule=2))
				names(ans2)<-names(ans)
			}else{
				ans2 <- ans
			}

			ans$W_interpolate <- F
			ans3 <- join(ans2,ans,by=names(ans2))		
			ans3$W_interpolate[is.na(ans3$W_interpolate)] <- T	

			#change time
			ans4 <- mutate(ans3,time_SSS=time_SSS+0.5)
			ans4$time_SSS[1] <- 0

			#rescale
			ans<-transform(ans4,health_score_rescaled=health_score-df_epi$baseline_health_score)

			#fix health score higher than baseline
			x<-which(ans$health_score_rescaled>0)
			ans$health_score_rescaled[x] <- 0
			ans$health_score[x] <- df_epi$baseline_health_score
			ans$W_HS_higher_than_baseline<-F
			ans$W_HS_higher_than_baseline[x]<-T		


			return(ans)
		},.progress="text")


df_timeline <- join(df_timeline,df_episode,by=c("person_id","n_bout"))
saveRDS(df_timeline, file = RDSfile)
}else{
	df_timeline <- readRDS(file = RDSfile)
}

#all data
df_subset<-subset(df_timeline,time_SSS<=x_max)
df_subset <- mutate(df_subset,age_group=revalue(age_group,c("65+"="paste(\"65+\")")),symptom=revalue(symptom,c("ARI_ecdc"="ARI","ILI_no_fever"=expression(ILI[paste("no fever")]),"ILI_fever"=expression(ILI[fever]))))

cat("compute boxplot.stats:\n")
flush.console()
gdf <- compute_boxplot_no_outlier(df_subset,id_vars = c("symptom", "age_group", "time_SSS"), value_name = "health_score_rescaled")
#gdf <- mutate(gdf,symptom=revalue(symptom,c("ARI_ecdc"="ARI","ILI_no_fever"="ILI no fever","ILI_fever"="ILI fever")))
#compute check
#all cases:
#	df_check <- mutate(df_subset,approx=revalue(factor(W_approximate_S_start+W_approximate_S_end*2+W_approximate_baseline_health_score*4),c("0"="none","1"="S_start","2"="S_end","3"="S_start & S_end","4"="baseline","5"="S_start & baseline","6"="S_end & baseline","7"="S_start & S_end & baseline")))
#only symptom duration:
#	df_check <- mutate(df_subset,approx=revalue(factor(W_approximate_S_start+W_approximate_S_end*2),c("0"="none","1"="S_start","2"="S_end","3"="S_start & S_end")))
#none vs any

#check approx
W_check <- vapply(unlist(strsplit(to_check,split="|",fixed=T)),str_trim,character(1),USE.NAMES=F)
stopifnot(all(W_check%in%names(df_episode_clean)))
tmp <- count(df_episode_clean,vars=W_check)
cat("Check:\n")
print(tmp)
flush.console()
df_check <- mutate(df_subset,approx=(eval(parse(text=to_check))))

#
cat("compute boxplot.stats with check of approximation:\n")
flush.console()
gdf_check <- compute_boxplot_no_outlier(df_check,id_vars = c("symptom", "age_group", "approx", "time_SSS"), value_name = "health_score_rescaled")  
#gdf_check <- mutate(gdf_check,symptom=revalue(symptom,c("ARI_ecdc"="ARI","ILI_no_fever"="ILI no fever","ILI_fever"="ILI fever")))

#gdf_point<-subset(df_check, health_score_rescaled <0 & !W_interpolate)
gdf_point<-subset(df_check, !W_interpolate)
#gdf_point <- mutate(gdf_point,symptom=revalue(symptom,c("ARI_ecdc"="ARI","ILI_no_fever"="ILI no fever","ILI_fever"="ILI fever")))

##without check
p<-ggplot(data= gdf,aes(x=time_SSS,y=middle,ymax=ymax,ymin=ymin))+facet_grid(symptom~age_group,scales="free",labeller=label_parsed)
p <- p+geom_ribbon(aes(alpha=type))
p <- p+geom_line()
p <- p+scale_alpha_manual("confidence",breaks=c("IQR","whiskers"),values=c(0.25,0.4))
p <- p+scale_x_continuous("time since symptom onset")+scale_y_continuous("drop in health score")
#p <- p+theme(legend.position="top")
cairo_pdf(paste(dir_pdf,"timeline_health_score.pdf",sep='/'),width=9,height=6)
print(p)
dev.off()

#add data
vars=c("age_group","symptom")
df_count<-count(unique(subset(df_check,select=c("n_bout","person_id",vars))),vars=vars)
#df_count<-count(unique(subset(df_check,select=c("person_id",vars))),vars=vars)

df_count <- ddply(df_count,c("symptom"),function(df){

	tmp <- match_df(gdf,df,on=c("symptom"))
	df$y <- min(tmp$ymin)			

	return(df)
})


p<-ggplot(data= gdf)+facet_grid(symptom~age_group,scales="fixed",labeller=label_parsed)
p<-p+geom_point(data= gdf_point,aes(x=time_SSS,y=health_score_rescaled,colour="data"),alpha=0.1)
p <- p+geom_ribbon(data= gdf,aes(x=time_SSS,ymax=ymax,ymin=ymin,alpha=type))
p <- p+geom_line(data= gdf,aes(x=time_SSS,y=middle))
p <- p+geom_text(data= df_count,aes(label=paste("n =",freq),x=15,y= -75),size=4)
p <- p+scale_alpha_manual("confidence:",breaks=c("IQR","whiskers"),labels=c("IQR","95% CI"),values=c(0.25,0.4))
p <- p+scale_colour_hue("data:",labels="",guide=guide_legend(override.aes=list(alpha=1),order=1))
p <- p+scale_x_continuous("days since symptom onset")+scale_y_continuous("drop in health score",breaks=seq(-100,0,25),labels=-seq(-100,0,25))
p <- p+theme_bw() %+replace% theme(legend.key=element_rect(size=0))
p <- p+theme(legend.position="top",legend.box="horizontal")

#p <- p+theme(legend.position="top")
cairo_pdf(paste(dir_pdf,"timeline_health_score_with_data.pdf",sep='/'),width=7,height=5,bg="transparent")
print(p)
dev.off()

##with check
#count sampel size and set y position
vars=c("age_group","symptom","approx")
df_count<-count(unique(subset(df_check,select=c("n_bout","person_id",vars))),vars=vars)
df_count <- ddply(df_count,c("symptom"),function(df){

	tmp <- match_df(gdf_check,df,on=c("symptom"))
	if(compare_whiskers){
		df$y <- min(tmp$ymin)		
	}else{
		df$y <- min(tmp$ymin[tmp$type=="IQR"])			
	}

	return(df)
})

#median +IQR (+whiskers?)
if(compare_whiskers){
	gdf_use <- gdf_check
}else{
	gdf_use <- subset(gdf_check,type=="IQR")	
}

p<-ggplot(data= gdf_use)+facet_grid(symptom~age_group,scales="free",labeller=label_parsed)
p <- p+geom_ribbon(data= gdf_use,aes(x=time_SSS,ymax=ymax,ymin=ymin,alpha=type,fill=approx))
p <- p+geom_line(data= gdf_use,aes(x=time_SSS,y=middle,colour=approx))
p <- p+geom_text(data= df_count,aes(label=paste("n =",freq),colour=approx,x=7+as.numeric(approx)*6,y= y*0.95),size=4)
p <- p+scale_alpha_manual("confidence",breaks=c("IQR","whiskers"),values=c(ifelse(compare_whiskers,0.15,0.4),0.3))
p <- p+scale_fill_hue("approximation")
p <- p+scale_colour_hue("approximation")
p <- p+scale_x_continuous("time since symptom onset")+scale_y_continuous("drop in health score")
#p <- p+theme(legend.position="top")
cairo_pdf(paste(dir_pdf,"timeline_health_score_check_approx.pdf",sep='/'),width=9,height=6)
print(p)
dev.off()

#no approx and add data
gdf_full <- subset(gdf_check,!approx)
gdf_point_full <- subset(gdf_point,!approx)
df_count_full <- subset(df_count,!approx)

p<-ggplot(data= gdf_full)+facet_grid(symptom~age_group,scales="free",labeller=label_parsed)
p<-p+geom_point(data= gdf_point_full,aes(x=time_SSS,y=health_score_rescaled,colour="data"),alpha=0.5)
p <- p+geom_ribbon(data= gdf_full,aes(x=time_SSS,ymax=ymax,ymin=ymin,alpha=type))
p <- p+geom_line(data= gdf_full,aes(x=time_SSS,y=middle))
if(!compare_whiskers){
	df_count_full$y <- df_count_full$y*1.5
}
p <- p+geom_text(data= df_count_full,aes(label=paste("n =",freq),x=10,y=y),hjust=0,size=3)		
p <- p+scale_alpha_manual("confidence",breaks=c("IQR","whiskers"),values=c(0.25,0.4))
p <- p+scale_colour_hue("",guide=guide_legend(override.aes=list(alpha=1)))
p <- p+scale_x_continuous("time since symptom onset")+scale_y_continuous("drop in health score")
#p <- p+theme(legend.position="top")
cairo_pdf(paste(dir_pdf,"timeline_health_score_full_info.pdf",sep='/'),width=9,height=6)
print(p)
dev.off()

}


plot_TS_ind <- function(id, df_profile, df_data, df_episode, df_episode_plot=df_episode,strip_label=c("full","simple"),palette="Accent",plot_baseline=TRUE, plot_dates=FALSE, plot_QALD=TRUE, plot_symptom=TRUE, plot_symptom_without_healthscore=plot_symptom, plot_point_report=FALSE, legend=TRUE, all_dates_xlim=FALSE,health_score_xlim=FALSE, keep_only_episode_with_QALD_loss=FALSE) {

	stopifnot(palette%in%rownames(brewer.pal.info),all(id%in%df_episode_plot$person_id))

	strip_label <- match.arg(strip_label)

	df_data_id <- subset(df_data, person_id %in% id)
	df_episode_id <- subset(df_episode, person_id %in% id)
	df_episode_plot_id <- subset(df_episode_plot, person_id %in% id)
	df_profile_id <- subset(df_profile, person_id %in% id)

	if(keep_only_episode_with_QALD_loss){
		df_episode_plot_id <- subset(df_episode_plot_id, !is.na(QALD_loss))
		df_data_id <- subset(df_data_id,n_bout%in%df_episode_plot_id$n_bout | is.na(n_bout))
		df_episode_id <- match_df(df_episode_id,df_episode_plot_id,on="n_bout")
		# df_episode_id$n_bout <- 1:nrow(df_episode_id)
		# df_episode_plot_id$n_bout <- 1:nrow(df_episode_plot_id)
	}
	

	#order df_profile_id according to id_num if available, id otherwise
	if("person_id_num"%in%names(df_profile)){
		row.names(df_profile_id) <- df_profile_id$person_id_num
		df_profile_id <- df_profile_id[id,]
	}else{
		row.names(df_profile_id) <- df_profile_id$person_id
		df_profile_id <- df_profile_id[id,]
	} 
	
	#define episode colour
	# if(remove_uncomplete_episode){
	# 		df_episode_id <- subset(df_episode_id,!is.na(QALD_loss))

	# }
	n_episode_max <- max(count(df_episode_id$person_id)$freq)
	colour_episode = brewer.pal(min(c(brewer.pal.info[palette,"maxcolors"],n_episode_max)), palette)
	if(length(colour_episode)>n_episode_max){
		colour_episode <- colour_episode[1:n_episode_max]
	}
	
	#define upper bound of rectangular area
	df_episode_plot_id <- transform(df_episode_plot_id, health_max = max(df_data_id$health_score, na.rm = T), health_min = min(df_data_id$health_score, na.rm = T))
	df_data_id <- transform(df_data_id, health_max = max(health_score, na.rm = T), health_min = min(health_score, na.rm = T))
	df_data_id$symptom <- apply(df_data_id[, c("ARI_ecdc", "ILI_ecdc", "ILI_fever")], 1, function(x) rev(names(x)[x])[1])
	if(strip_label=="simple"){
		df_profile_id <- transform(df_profile_id, strip_label = paste("Individual", seq_along(id)))		
	}else if(strip_label=="full"){
		df_profile_id <- transform(df_profile_id, strip_label = paste("Gender:",gender, "/ Age-group:", age_group,"years old / Vaccinated:",vaccine,"/ UHC:",is_risk,"/ Smoker:",smoke_bool))		
	}
	

	df_data_id <- merge(df_profile_id, df_data_id)
	df_episode_plot_id <- merge(df_profile_id, df_episode_plot_id)


	#compute integration area for QALD loss
	df_episode_id_QALD <- subset(df_episode_plot_id,!is.na(QALD_loss))
	

	if(plot_QALD){
		plot_QALD <- as.logical(nrow(df_episode_id_QALD))		
	}

	if(plot_QALD){
		df_plot_QALD <- ddply(df_episode_id_QALD,c("person_id","n_bout"),function(df){
			df_data_epi <- match_df(df_data,df,on=c("person_id","n_bout"))
			tmp <- compute_QALD_loss(df_data_epi,df$baseline_health_score,df$symptom_start, df$symptom_end,timeline_only = TRUE, symptom_end_at_baseline=TRUE)
			tmp$time_SSS <- tmp$time_SSS+df$symptom_start
			tmp$time_SSS[1] <- tmp$time_SSS[1]+1
			tmp$baseline <- df$baseline_health_score
			return(tmp)
		})
		df_plot_QALD <- merge(df_profile_id, df_plot_QALD)

		# if(remove_uncomplete_episode){
		# 	#df_plot_QALD$n_bout <- 1:nrow(df_plot_QALD)		
		# 	df_episode_plot_id <- df_episode_id_QALD
		# 	df_episode_plot_id$n_bout <- 1:nrow(df_episode_plot_id)		

		# }

	}

	df_missing_date <- subset(df_episode_plot_id,is.na(symptom_start) | is.na(symptom_end))
	plot_missing_date <- as.logical(nrow(df_missing_date))


	p <- ggplot(data = df_data_id) + facet_wrap(~strip_label, scales = "free_x")

	if(plot_dates){
		p <- p + geom_rect(data = df_episode_plot_id, aes(xmin = symptom_start, xmax = symptom_end, ymin = health_min, 	ymax = health_max, fill = n_bout), alpha = 0.5)
	}

	tmp <- subset(df_episode_plot_id, W_approximate_S_start | W_approximate_S_end, select = c("strip_label", "symptom_start", "symptom_end", "n_bout", "W_approximate_S_start", "W_approximate_S_end","baseline_health_score","QALD_loss"))
	if (nrow(tmp)) {
		tmp1 <- subset(tmp, W_approximate_S_start)
		tmp1$approx_date <- tmp1$symptom_start
		tmp2 <- subset(tmp, W_approximate_S_end)
		tmp2$approx_date <- tmp2$symptom_end
		gdf <- rbind(tmp1, tmp2)
		gdf$vjust <- 1.2
		gdf$vjust[gdf$QALD_loss>0] <- -0.2
		gdf$vjust[is.na(gdf$QALD_loss)] <- NA
		p <- p + geom_text(data = gdf, aes(x = approx_date,y=baseline_health_score, colour = n_bout,vjust=vjust),label="?",size=3)#,guide=guide_legend(override.aes=list(colour=NA))

	}

	if(plot_missing_date){
		df_missing_date <- mutate(df_missing_date, first_report_date=as.character(first_report_date),last_report_date=as.character(last_report_date))
		gdf_missing_date <- melt(df_missing_date,measure.vars=c("first_report_date","last_report_date"))
		gdf_missing_date <- subset(gdf_missing_date,(variable=="first_report_date" & is.na(symptom_start)) | (variable=="last_report_date" & is.na(symptom_end)))
		p <- p + geom_text(data = gdf_missing_date, aes(x = as.Date(value),y=baseline_health_score, colour = n_bout),label="!",vjust= -0.2,size=3)#vjust=sign(QALD_loss)
	}


	p <- p + geom_line(data = df_data_id, aes(x = report_date, y = health_score), alpha = 0.5)
	if(plot_point_report){
		p <- p + geom_point(data = df_data_id, aes(x = report_date, y = health_score), alpha = 1,size=3)	
	}
	#p <- p + geom_hline(data = df_episode_plot_id, aes(yintercept = baseline_health_score,linetype="baseline"), alpha = 0.5)
	if(plot_baseline){
		p <- p + geom_hline(data = df_episode_plot_id, aes(yintercept = baseline_health_score,linetype=W_approximate_baseline_health_score),show_guide=T, alpha = 1)
		# p <- p + scale_linetype_manual("Baseline\nhealth-score:",breaks=c("TRUE","FALSE"),labels=c("sampled","estimated"),values=c("dotted","dashed"),guide=guide_legend(order=1))
		# for paper example
		p <- p+scale_linetype_manual("Health-score:",breaks=c("TRUE","FALSE"),labels=c("estimated","baseline"),values=c("dotted","dashed"),guide=guide_legend(order=1))
	}


	#QALD trapeze
	if(plot_QALD){
		p <- p+geom_ribbon(data= df_plot_QALD, aes(x=time_SSS,ymin=health_score,ymax=baseline,group=n_bout,fill=n_bout,colour=n_bout),alpha=0.5)

	}

	if(plot_symptom){
		p <- p + geom_point(data = df_data_id, aes(x = report_date, y = health_score, shape = symptom, fill = n_bout, colour= n_bout), size = 0)	
		p <- p + geom_point(data = df_data_id, aes(x = report_date, y = health_score, shape = symptom, fill = n_bout), size = 3)	
		# p <- p + scale_shape_manual("Symptom\nseverity:", breaks = c("no_symptom","ARI_ecdc", "ILI_ecdc", "ILI_fever"), labels=c("no symptom","ARI",expression(ILI["no fever"]),expression(ILI[fever])), values = c(21, 22, 24, 8),guide=guide_legend(override.aes=list(linetype=0),order=3))
		# for paper example
		p <- p + scale_shape_manual("Symptom severity:", breaks = c("no_symptom","ARI_ecdc", "ILI_ecdc", "ILI_fever"), labels=c("no symptom","ARI",expression(ILI["no fever"]),expression(ILI[fever])), values = c(21, 22, 24, 8),guide=guide_legend(override.aes=list(linetype=0),order=3))
	}

	df_na_health_score <- subset(df_data_id, is.na(health_score) & !is.na(symptom))
	if (nrow(df_na_health_score) & plot_symptom & plot_symptom_without_healthscore) {
		p <- p + geom_point(data = df_na_health_score, aes(x = report_date, y = health_max, shape = symptom,colour=n_bout), size = 3)
	}

	# for paper example
	tmp <- sort(unique(df_plot_QALD$n_bout, df_na_health_score$n_bout))
	p <- p + scale_colour_gradientn("Episode:", breaks = tmp, labels = seq_len(n_episode_max), colours = colour_episode,guide="none")#[sort(unique(gdf$n_bout))]
	# p <- p + scale_colour_gradientn("episode", breaks = tmp, labels = tmp, colours = colour_episode[tmp],guide="none")#[sort(unique(gdf$n_bout))]
	# df_episode_plot_id$n_bout
	p <- p + scale_fill_gradientn("Episode:", breaks = tmp, labels = seq_len(n_episode_max), colours = colour_episode, guide = "legend")

	if(n_episode_max>9){
		p <- p+guides(fill=guide_colourbar(barheight= n_episode_max*2/3,nbin=n_episode_max,order=2,ticks=F),colour= guide_colourbar(barheight= n_episode_max*2/3,nbin= n_episode_max,order=1,ticks=F))
	}else{
		#ceiling(n_episode_max/2) #TODO fix that so that last colour appears
		p <- p+guides(fill=guide_legend(order=2,nrow=1,byrow=T,override.aes=list(shape=NA,alpha=1)))#,colour=guide_legend(order=2,nrow=2,byrow=T,override.aes=list(shape=NA,alpha=1)))		
}	


p <- p+theme_bw() %+replace% theme(legend.key=element_rect(size=0),legend.text.align=0)
p <- p + scale_x_date("date of report") + scale_y_continuous("health score")

# for example paper
p <- p+theme(legend.position="top",legend.direction="horizontal",legend.box="horizontal")

if(!legend){
	p <- p+theme(legend.position="none")
}

if(all_dates_xlim){
	p <- p + scale_x_date("date of report",limits=range(c(df_data_id$report_date,df_episode_id$symptom_start),na.rm=T))
}

if(health_score_xlim){
	tmp <- subset(df_data_id,!is.na(health_score))
	p <- p + scale_x_date("date of report",limits=range(tmp$report_date,na.rm=T))
}
#p<-p+scale_x_date(labels=date_format("%d%b"),minor_breaks="1 week")
print(p)

}

plot_timeline_example <- function() {

	episode_suffix <- "mix_SD_SD__SDmax=Inf_baseline_summary=median"

	df_data <- readRDS(file = paste(RSAVE, "data_renamed__symptom_summarized__auto_clean__no_duplicate.rds", sep = "/"))
	df_profile <- readRDS(file = paste(RSAVE, "profile_clean.rds", sep = "/"))
	df_profile <- mutate(df_profile,smoke_bool=smoke_as_logical(smoke), vaccine=revalue(as.character(vaccine),c("TRUE"="yes","FALSE"="no")),is_risk=revalue(as.character(is_risk),c("TRUE"="yes","FALSE"="no")))

	#	df_episode <-readRDS(file = paste(RSAVE, "df_episode.rds", sep = "/"))
	df_episode <- readRDS(file = file.path(RSAVE, paste0("df_episode_", episode_suffix, ".rds")))

	df_episode <- join(df_episode, df_profile, by = "person_id")
	df_episode$symptom <- revalue(df_episode$symptom, c(ILI_ecdc = "ILI_no_fever"))


	pre_pdf <- "/Users/Tonton/work/projects/Flusurvey/pdf/time_line"
	if (!file.exists(pre_pdf)) {
		dir.create(pre_pdf)
	}

	id <- c("f4ddd6b7-44cf-4724-bbf7-6cc9c82cad62")
	cairo_pdf(file.path(dir_paper_graph, "example_full.pdf"), width = 8, height = 4)
	plot_TS_ind(id, df_profile, df_data, df_episode, palette = "Dark2", plot_QALD = T, plot_symptom = T, plot_symptom_without_healthscore=F , plot_baseline = T, legend = T, all_dates_xlim = F,health_score_xlim=T,keep_only_episode_with_QALD_loss=TRUE)
	dev.off()

	# cairo_pdf(file.path(pre_pdf, "example_legend.pdf"), width = 6, height = 3)
	# plot_TS_ind(c("f4ddd6b7-44cf-4724-bbf7-6cc9c82cad62"), df_profile, df_data, df_episode, palette = "Dark2", plot_QALD = T, 
	# 	plot_symptom = T, plot_baseline = T, legend = T, all_dates_xlim=T)
	# dev.off()

	# cairo_pdf(file.path(pre_pdf, "example_5.pdf"), width = 6, height = 3)
	# plot_TS_ind(c("f4ddd6b7-44cf-4724-bbf7-6cc9c82cad62"), df_profile, df_data, df_episode, palette = "Dark2", plot_QALD = T, 
	# 	plot_symptom = T, plot_baseline = T, legend = F, all_dates_xlim =T)
	# dev.off()

	# cairo_pdf(file.path(pre_pdf, "example_4.pdf"), width = 6, height = 3)
	# plot_TS_ind(c("f4ddd6b7-44cf-4724-bbf7-6cc9c82cad62"), df_profile, df_data, df_episode, palette = "Dark2", plot_dates=T,plot_QALD = F, 
	# 	plot_symptom = T, plot_baseline = T, legend = F, all_dates_xlim =T)
	# dev.off()

	# cairo_pdf(file.path(pre_pdf, "example_3.pdf"), width = 6, height = 3)
	# plot_TS_ind(c("f4ddd6b7-44cf-4724-bbf7-6cc9c82cad62"), df_profile, df_data, df_episode, palette = "Dark2", plot_dates=T,plot_QALD = F, 
	# 	plot_symptom = T, plot_baseline = F, legend = F, all_dates_xlim =T)
	# dev.off()

	# cairo_pdf(file.path(pre_pdf, "example_2.pdf"), width = 6, height = 3)
	# plot_TS_ind(c("f4ddd6b7-44cf-4724-bbf7-6cc9c82cad62"), df_profile, df_data, df_episode, palette = "Dark2", plot_dates=F,plot_QALD = F, 
	# 	plot_symptom = T, plot_baseline = F, legend = F, all_dates_xlim =T)
	# dev.off()

	# cairo_pdf(file.path(pre_pdf, "example_1.pdf"), width = 6, height = 3)
	# plot_TS_ind(c("f4ddd6b7-44cf-4724-bbf7-6cc9c82cad62"), df_profile, df_data, df_episode, palette = "Dark2", plot_QALD = F, 
	# 	plot_symptom = F, plot_baseline = F, legend = F, all_dates_xlim =T)
	# dev.off()

	# cairo_pdf(file.path(pre_pdf, "example_0.pdf"), width = 6, height = 3)
	# plot_TS_ind(c("f4ddd6b7-44cf-4724-bbf7-6cc9c82cad62"), df_profile, df_data, df_episode, palette = "Dark2", plot_QALD = F, 
	# 	plot_symptom = F, plot_point_report=T, plot_baseline = F, legend = F, all_dates_xlim =T)
	# dev.off()

}


plot_TS_episode <- function(df_episode, episode_name) {
	n_episode_name <- paste("n", episode_name, sep = "_")
	n_episode <- as.symbol(n_episode_name)
	i_episode_name <- paste("i", episode_name, sep = "_")
	i_episode <- as.symbol(i_episode_name)

	n_episode_max <- length(na.omit(unique(df_episode[, i_episode_name])))

	#select individual with more than one episode
	df_1<-unique(subset(df_episode, !is.na(df_episode[, i_episode_name]) & !is.na(symptom_start), select = c("person_id", 
		"symptom_start", i_episode_name)))	

	p <- ggplot(data = df_1, aes_string(x = "symptom_start", fill = i_episode_name))
	p<-p+geom_histogram(alpha=0.75,binwidth=1,position='identity')
	#p <- p + geom_histogram(alpha = 0.75, position = "identity")
	p <- p + scale_fill_brewer(episode_name, palette = "Accent", breaks = seq_len(n_episode_max), labels = paste("episode", 
		seq_len(n_episode_max)))
	p<-p+scale_x_date(labels=date_format("%b %d"))
	print(p)

}

plot_off_work<-function(df_episode,time_off=FALSE, group_by =c("age_group","employment","occupation"),worker_student_only=FALSE,dir_pdf,print_table=FALSE){
	#TODO check if multiple episode with same symptom, if so take the one with longest time_off

	group_by <- match.arg(group_by)
	var_name <- ifelse(time_off,"time_off","change_routine")

	#group by work/student
	if(group_by=="employment" & worker_student_only){
		df_episode <- subset(df_episode,employment%in%c("paid_full_time","paid_part_time","student","self")) 
		df_episode <- mutate(df_episode,employment2=revalue(employment,c(paid_full_time="worker", paid_part_time="worker", self="worker"))) 
		group_by <- "employment2"
	}

	df_episode_clean<-subset(df_episode, eval(parse(text=paste("!is.na(",group_by,")"))) & eval(parse(text=paste("!is.na(",var_name,")"))),select=c(group_by,"symptom", var_name))

	gdf_pop_size<-count(df_episode_clean,vars=c(group_by,"symptom"))
	gdf_pop_size<-rename(gdf_pop_size,c(freq="size"))

	gdf_value <- melt(df_episode_clean,measure.vars=var_name)

	gdf_value$value<-factor(gdf_value$value,levels=levels(df_episode[[var_name]]))
	gdf_count<-count(gdf_value)
	tmp<-expand.grid(x=unique(gdf_value[[group_by]]),symptom=unique(gdf_value$symptom),variable=unique(gdf_value$variable),value=unique(gdf_value$value))
	names(tmp)[1] <- group_by
	gdf_count <-join(tmp,gdf_count)
	gdf_count$freq[is.na(gdf_count$freq)]<-0

	gdf_plot<-join(gdf_count,gdf_pop_size)


	p<-ggplot()

	if(time_off){
		p<-p+geom_bar(data= gdf_plot,aes(x=value,y=freq/size*100),position="dodge",stat="identity",alpha=0.75)			
		p<-p+geom_text(data= gdf_plot,aes(x=length(unique(value))/2,label=paste("n =",size),y=max(freq/size*100)),vjust=1)#,size=4			
		p<-p+facet_grid(eval(parse(text=paste0("symptom~",group_by))),scales="fixed")											
		x_lab <- "time off"

	}else{
		p<-p+geom_bar(data= gdf_plot,aes(x=symptom,y=freq/size*100,fill=value),position="stack",stat="identity")			
		p<-p+geom_text(data= gdf_plot,aes(x=symptom,label=paste("n =",size),y=110),vjust=1,size=4)				
		p<-p+facet_wrap(eval(parse(text=paste0("~",group_by))),scales="fixed")
		x_lab <- "symptom"								
	}


	p <- p+theme_bw()	
	if(time_off){
		p<-p+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
	}else{
		p<-p+scale_fill_brewer("did you change your routine?",palette="Dark2")
		p <- p+theme(legend.position="top")

	}
	p <- p+scale_y_continuous("percentage of the n cases",breaks=seq(0,100,10))
	p<-p+xlab(x_lab)

	file_name <- file.path(dir_pdf,paste0("change_routine_",group_by,ifelse(time_off,"_time_off","")))

	cairo_pdf(paste(file_name,"pdf",sep="."),width=6,height=4)
	print(p)
	dev.off()

	if(print_table){
		tmp <- rename(gdf_plot,c("size"="sample_size","freq"="group_size"))
		tmp <- arrange(tmp,employment2,symptom,value)
		tmp <- mutate(tmp,proportion=round(group_size/sample_size,3))
		write.csv(tmp,file=paste(file_name,"csv",sep="."))	
	}

}


plot_assistance<-function(df_episode,type=c("visit","phone"),to=c("GP","hosp","AE","NHS","other"),time=F,to_by_line=FALSE,no_other=TRUE,dir_pdf){

	type<-match.arg(type)

	no_age <- (time & to_by_line)

	if(no_age){
		df_episode$age_group <- "all"
	}

	var_col <- "symptom"
	var_line <- ifelse(no_age,"to","age_group")

	var_name <- grep(type,names(df_episode),value=T)
	var_name <- unlist(sapply(to,grep,x=var_name,value=T))

	var_time <- grep("time",var_name,value=T)

	if(time){
		var_name <- var_time
	}else{
		var_name<-setdiff(var_name,var_time)
	}

	if(!length(var_name)){
		stop("argument \"to\" is not valid")
	}

	gdf<-subset(df_episode, !is.na(age_group) & eval(parse(text=paste(paste0("!is.na(",var_name,")"),collapse="|"))),select=c("age_group","symptom", var_name))

	gdf_pop_size<-count(gdf,vars=c("age_group","symptom"))
	gdf_pop_size<-rename(gdf_pop_size,c(freq="size"))

	gdf3 <- melt(gdf,measure.vars=var_name)

	if(time){
		gdf3<-na.omit(gdf3)	
		gdf3$value<-factor(gdf3$value,levels=levels(gdf[,var_name[1]]))
		gdf3<-count(gdf3)
		tmp<-expand.grid(age_group=unique(gdf3$age_group),symptom=unique(gdf3$symptom),variable=unique(gdf3$variable),value=unique(gdf3$value))

	}else{
		gdf3 <-ddply(gdf3,c("age_group","symptom","variable"),function(df) data.frame(freq=sum(df$value)))
		tmp<-expand.grid(age_group=unique(gdf3$age_group),symptom=unique(gdf3$symptom),variable=unique(gdf3$variable))

	}

	gdf3<-join(tmp,gdf3)
	gdf3$freq[is.na(gdf3$freq)]<-0

	gdf4<-join(gdf3,gdf_pop_size)
	gdf4<-transform(gdf4,to=extract_string(variable,paste0(type,"_"),2))

	gdf5<-ddply(gdf4,ifelse(to_by_line,"to","symptom"),function(df){
		df$n_sample=paste("n =",df$size)
		df$y=max(df$freq/df$size*100)
		return(df)
	})

	if(no_other){
		gdf4 <- subset(gdf4,to!="other")
		gdf5 <- subset(gdf5,to!="other")
	}

	p<-ggplot()
	if(time){
		gdf4 <- subset(gdf4,value!="CR")
		gdf5 <- subset(gdf5,value!="CR")


		p<-p+geom_bar(data=gdf4,aes(x=value,y=freq/size*100,fill=to),position="dodge",stat="identity",colour="black")			
		if(no_age){
			p<-p+geom_text(data=subset(gdf5,to==levels(to)[1]),aes(x=length(unique(value))/2,label=n_sample,y=y*1.2),vjust=1)
			p<-p+facet_grid(to~symptom,scales="free_y")				
		}else{
			p<-p+geom_text(data=gdf5,aes(x=length(unique(value))/2,label=n_sample,y=y*1.2),vjust=1)				
			p<-p+facet_grid(symptom~age_group,scales="free_y")								
		}
		x_label <- paste("time to",type)	
	}else{

		if(to_by_line){
			p<-p+geom_bar(data=gdf4,aes(x=age_group,y=freq/size*100,fill=symptom),position="dodge",stat="identity",colour="black")	
			p<-p+geom_text(data=subset(gdf5,to==levels(to)[1]),aes(x=age_group,label=n_sample,y=y*1.2),vjust=1,size=3)
			p<-p+facet_grid(to~symptom,scales="free_y")	
		}else{
			p<-p+geom_bar(data=gdf4,aes(x=age_group,y=freq/size*100,fill=to),position="dodge",stat="identity",colour="black")	
			p<-p+geom_text(data=gdf5,aes(x=age_group,label=n_sample,y=y*1.2),vjust=1)
			p<-p+facet_wrap(~symptom,scales="free_y",ncol=1)	
		}

		x_label <- "age group"


	}

	p <- p+theme_bw()
	p<-p+scale_fill_brewer(type,palette="Accent")		
	if(time){
		p<-p+theme(axis.text.x=element_text(angle=45,vjust=1,hjust=1))
	}
	if(to_by_line){
		p <- p+theme(legend.position="none")
	}
	p<-p+xlab(x_label)+ylab("percentage of the n cases")#+ coord_cartesian(ylim = c(0,30))	

	cairo_pdf(paste0(dir_pdf,"/seek_assistance_",type,ifelse(time,"_time",""),".pdf"),width=7,height=5)
	print(p)
	dev.off()

}


compute_interval_episode<-function(df_episode, select_symptom=c("ILI_ecdc","ILI_fever")){

	#df_episode <-readRDS(file = paste(RSAVE, "df_episode.rds", sep = "/"))

	#remove all S_start warning
	df_episode<-subset(df_episode,symptom%in%select_symptom)

	#keep only individuals with more than one episode
	tmp <- subset(count(df_episode,vars=c("person_id")),freq>1)
	df_episode<-match_df(df_episode,tmp)

	#compute episode interval
	df_episode <- ddply(df_episode, c("person_id"), function(df) {
		df<-arrange(df,n_bout)
		df$interval <- as.numeric(c(NA,diff(df$symptom_start)))
		df$reinfection <- c(NA,1:(nrow(df)-1))
		return(df)
	},.progress="text")

	gdf<-subset(df_episode,!is.na(reinfection) & reinfection>=1 & !is.na(interval) & !W_approximate_S_start & interval>0)
	gdf<-subset(df_episode,!is.na(reinfection) & reinfection>=1 & !is.na(interval) & interval>0)

	p<-ggplot(gdf,aes(x=interval))+geom_histogram(position="identity",binwidth=1)
	#	p<-p+geom_density()
	print(p)

	df_episode_interval$episode<-episode_name

	saveRDS(df_episode_interval,file=paste(RSAVE, paste(episode_name,"interval.rds",sep="_"), sep = "/"))

	return(df_episode_interval)
}

plot_interval_episode <- function(df_episode,symptoms=c("ARI_ecdc","ILI_no_fever","ILI_fever"),dir_pdf) {

	if (!file.exists(dir_pdf)) {
		dir.create(dir_pdf)
	}


	df <- subset(df_episode,symptom%in%symptoms)

	#select participants with more than one episode
	tmp <- subset(count(df,vars="person_id"),freq>1)
	df_mult_epi <- match_df(df,tmp,on="person_id")

	df_mult_epi <- arrange(df_mult_epi,person_id,symptom_start)

	df_mult_epi <- mutate(df_mult_epi,previous_symptom_start=c(as.Date(NA),symptom_start[-length(symptom_start)]))
	df_mult_epi$previous_symptom_start[!duplicated(df_mult_epi$person_id)]=NA
	df_mult_epi <- mutate(df_mult_epi,interval_episode=as.numeric(symptom_start-previous_symptom_start))


	gdf <- df_mult_epi
	#gdf <- subset(df_mult_epi,!W_approximate_S_start)	
	p <- ggplot(data =gdf, aes(x = interval_episode))#+facet_wrap(~episode,scales="free_y",ncol=1)
	p <- p + geom_histogram(binwidth = 1, alpha = 0.5)#, position = "stack"
	p <- p+scale_x_continuous("reinfection interval (in days)",limits=c(0,60))
#p<-p+geom_smooth()
#	p<-p+geom_density(alpha=0.5,kernel="")
	cairo_pdf(file.path(dir_pdf,paste0("reinfection_interval_for_",paste(symptoms,collapse="_"),".pdf")),width=5,height=3)
	print(p)
	dev.off()


}

analysis_reinfection_episodes<-function(){

	#load episodes
	df_episode <- readRDS(file = paste(RSAVE, "episode.rds", sep = "/"))

	plot_TS_episode(df_episode,"ILI_ecdc")	

	df_interval<-ldply(c("ARI_ecdc","ILI_ecdc","ILI_fever"),function(episode){
		df<-compute_interval_episode(df_episode, episode)
		return(df)
	})	

	#inspect interval
	#negative interval
	id<-unique(with(df_interval,person_id[dt<0 & episode=="ILI_ecdc"]))
	df<-subset(df_episode,person_id%in%id)
	plot_TS_ind(df,"ILI_ecdc")

	#short interval
	id<-unique(with(df_interval,person_id[dt<7 & episode=="ILI_ecdc"]))
	df<-subset(df_episode,person_id%in%id)
	plot_TS_ind(df,"ILI_ecdc")

	#plot
	df_interval_plot<-subset(df_interval,type_of_date=="symptom" & as.numeric(dt)<60)
	plot_interval_episode(df_interval_plot)


	#compute 		
}

plot_predictive_models <- function() {

	SD_max <- 30
	emp_dist <- readRDS(file = file.path(RSAVE, paste0("empirical_distribution_for_missing_data_SDmax=",SD_max,".rds")))

	df_dist_S_duration <- subset(emp_dist$S_duration,type=="bc_regression")
	df_dist_baseline_health_score <- subset(emp_dist$baseline_health_score,type=="logit_regression")


#symptom duration
	gdf <- mutate(subset(df_dist_S_duration,!age_group%in%c("0-17","18-44")),symptom2=revalue(symptom,c("ARI_ecdc"="ARI","ILI_ecdc"=expression(ILI[paste("no fever")]),"ILI_fever"=expression(ILI[fever]))))
	p <- ggplot(gdf)+facet_grid(~symptom2,labeller=label_parsed)
	p <- p+geom_bar(aes(x=S_duration,y=freq,fill=age_group2),alpha=0.75,stat="identity",position="identity")
	p <- p+scale_fill_hue("Age group")
	p <- p+ylab("Probability")+xlab("Symptom duration (in days)")
	p <- p+theme_bw()
	p <- p+theme(legend.background=element_rect(fill=NA),legend.position="top")
	p1 <- p
	# cairo_pdf(file.path(dir_plot, "S_duration_bc_regression.pdf"), width = 6, height = 4)
	#print(p)
	# dev.off()

#baseline
	gdf <- mutate(subset(df_dist_baseline_health_score,!age_group%in%c("0-17","18-44")),smoke_bool=bool_to_char(smoke_bool,c("TRUE"="Smoker","FALSE"="Non-smoker")),UHC=bool_to_char(is_risk,c("TRUE"="Any UHC","FALSE"="No UHC")))
	p <- ggplot(gdf)+facet_grid(smoke_bool~age_group2,scales="fixed")
	p <- p+geom_bar(aes(x=baseline_health_score,y=freq,fill=UHC),alpha=0.75,stat="identity",position="identity")
	p <- p+ylab("Probability")+xlab("Baseline health-score ")
	p <- p+scale_fill_hue("Underlying health conditions (UHC)")
	p <- p+theme_bw()
	p <- p+theme(legend.background=element_rect(fill=NA),legend.position="top")
	p2 <- p
	# cairo_pdf(file.path(dir_plot, "baseline_health_score_regression.pdf"), width = 6, height = 4)
	#print(p)
	# dev.off()


#time to report s_onset
	gdf <- rename(subset(emp_dist$time_to_report,variable=="time_to_report_S_start" & type=="interpolate"),c("value"="time_to_report_S_start"))
	p <- ggplot(gdf,aes(x=time_to_report_S_start,y=freq))+facet_grid(wday_report~.,scales="free_y")
	p <- p+geom_histogram(stat="identity")
	p <- p+scale_x_continuous("Time to report symptom onset date (in days)",breaks=0:10)+ylab("count")
	p <- p+theme_bw()
	p3 <- p

	# pdf_file <- paste0("empirical_distribution_time_to_report_symptom_onset.pdf")
	# cairo_pdf(file.path(dir_pdf,pdf_file),width=5,height=10)
	#print(p)
	# dev.off()

#combine all plots
	
	file_name <- "predictive_models_and_empirical_distribution_time_to_report_symptom_onset.pdf"

	MAR<-c(0.2,0.2,0,0)
	cairo_pdf(file.path(dir_paper_graph,file_name),height=8,width=10)
	grid.newpage()
	pushViewport(viewport(layout = grid.layout(100, 100)))
	print(p1+ggtitle("A")+theme_letter(MAR)+theme(plot.title=element_text(vjust=-2)), vp = vplayout(1:40, 1:60))
	print(p2+ggtitle("B")+theme_letter(MAR)+theme(plot.title=element_text(vjust=-2)), vp = vplayout(41:100, 1:60))
	print(p3+ggtitle("C")+theme_letter(MAR)+theme(plot.title=element_text(vjust=1)), vp = vplayout(1:100, 61:100))
	dev.off()

}

plot_symptom_cloud <- function(df_data,symptoms,labels,dir_pdf){

	require(wordcloud)
	require(tm)

	df_data <- subset(df_data,!none)

	df <- df_data[symptoms]
	names(df) <- labels
	gdf <- melt(df,measure.vars=labels)
	gdf <- subset(gdf,value)
	gdf <- count(gdf,vars="variable")
	pal <- brewer.pal(n=8,name="Dark2")

	if (!file.exists(dir_pdf)) {
		dir.create(dir_pdf)
	}


	pdf(file.path(dir_pdf,"symptom.pdf"),10,10)
	wordcloud(gdf$variable,gdf$freq,scale=c(8,.05),1,Inf,F,rot.per=0,colors=pal) 
	#,vfont=c("sans serif","plain")
	dev.off()
}


data_processing <- function(SD_max=30,fun_summary_baseline=c("median","mean")){

	fun_summary_baseline <- match.arg(fun_summary_baseline)

	if(0){

		age_group_cut <- c(0, 18, 45, 65,Inf)
		age_group_name <- c("0-17", "18-44", "45-64", "65+")

		## clean profile
		df_profile <- read.csv(paste(DIR_DATA, "profiledata.csv", sep = "/"), stringsAsFactors = F)
		df_profile<-clean_profile(df_profile,age_group_cut,age_group_name)
		saveRDS(df_profile, file = paste(RSAVE, "profile_clean.rds", sep = "/"))

		## clean data
		#load
		df_data <- read.csv(paste(DIR_DATA, "symptomdata_22072013.csv", sep = "/"), stringsAsFactors = F)
		#rename
		df_data<-rename_data(df_data)
		saveRDS(df_data,file=paste(RSAVE,"data_renamed.rds",sep="/"))
		#df_data<-readRDS(file=paste(RSAVE,"data_renamed.rds",sep="/"))

		#summarize symptoms
		df_data<-summarize_symptom(df_data,remove_original=T)
		saveRDS(df_data,file=paste(RSAVE,"data_renamed__symptom_summarized.rds",sep="/"))
		#df_data<-readRDS(file=paste(RSAVE,"data_renamed__symptom_summarized.rds",sep="/"))

		#some automatic cleaning
		df_data <- clean_data_automatically(df_data,lag_symptom_start=2,delay_in_reporting=10,CR_as_TRUE=F,plot_check=FALSE,debug=FALSE,force=TRUE)
		saveRDS(df_data,file=paste(RSAVE,"data_renamed__symptom_summarized__auto_clean.rds",sep="/"))
		#df_data <- readRDS(file=paste(RSAVE,"data_renamed__symptom_summarized__auto_clean.rds",sep="/"))

		#resolve multiple report on the same day
		df_data <- resolve_multiple_report_date(df_data)
		saveRDS(df_data, file = paste(RSAVE, "data_renamed__symptom_summarized__auto_clean__no_duplicate.rds", sep = "/"))

	}

	df_data<-readRDS(file = paste(RSAVE, "data_renamed__symptom_summarized__auto_clean__no_duplicate.rds", sep = "/"))
	df_profile <- readRDS(file = paste(RSAVE, "profile_clean.rds", sep = "/"))	
	# compute empirical distribution
	emp_dist <- compute_empirical_distribution_for_missing_data(df_data, df_profile,plot_check=T,S_duration_predictors=c("symptom","age_group"),S_duration_max= SD_max,fun_summary_baseline=fun_summary_baseline,kernel="gau",bw=2,dir_plot=file.path("./pdf/data_processing", paste0("full_info_for_S_duration_SDmax=",SD_max)),remove_outlier=F)
	# saveRDS(emp_dist, file = file.path(RSAVE, paste0("empirical_distribution_for_missing_data_SDmax=",SD_max,"_baseline_summary=",fun_summary_baseline,".rds")))
	
	if(0){	

		df_data<-readRDS(file = paste(RSAVE, "data_renamed__symptom_summarized__auto_clean__no_duplicate.rds", sep = "/"))	
		emp_dist <- readRDS(file = file.path(RSAVE, paste0("empirical_distribution_for_missing_data_SDmax=",SD_max,".rds")))
		emp_dist_2 <- readRDS(file = file.path(RSAVE, paste0("empirical_distribution_for_missing_data_SDmax=30_baseline_summary=",fun_summary_baseline,".rds")))
		df_profile <- readRDS(file = paste(RSAVE, "profile_clean.rds", sep = "/"))	

		df_dist_time_to_report_S_start <- rename(subset(emp_dist$time_to_report,variable=="time_to_report_S_start" & type=="interpolate"),c("value"="time_to_report_S_start"))
		df_dist_time_to_report_S_end <- rename(subset(emp_dist$time_to_report,variable=="time_to_report_S_end" & type=="interpolate"),c("value"="time_to_report_S_end"))

		df_dist_S_duration <- subset(emp_dist$S_duration,type=="bc_regression")
		df_dist_baseline_health_score <- subset(emp_dist_2$baseline_health_score,type=="logit_regression")

		#df <- subset(df_data,person_id=="4a937a55-8d29-43ec-9ddc-fdc2a460f59b" & !is.na(n_bout))

		#df_episode <- summarize_episode(df_data, df_profile, missing_S_start_date="sample_using_time_to_report", missing_S_end_date_not_ill="sample_using_time_to_report",missing_S_end_date_still_ill="use_report_date",df_dist_time_to_report_S_start=df_dist_time_to_report_S_start, df_dist_time_to_report_S_end= df_dist_time_to_report_S_end,df_dist_S_duration=df_dist_S_duration, df_dist_baseline_health_score=df_dist_baseline_health_score)
		#saveRDS(df_episode, file = paste(RSAVE, "df_episode_TTR_TTR_RD.rds", sep = "/"))

		#df_episode <- summarize_episode(df_data, df_profile, missing_S_start_date="sample_using_time_to_report", missing_S_end_date_not_ill="sample_using_time_to_report",missing_S_end_date_still_ill="sample_using_S_duration",df_dist_time_to_report_S_start=df_dist_time_to_report_S_start, df_dist_time_to_report_S_end= df_dist_time_to_report_S_end,df_dist_S_duration=df_dist_S_duration, df_dist_baseline_health_score=df_dist_baseline_health_score)
		#saveRDS(df_episode, file = paste(RSAVE, "df_episode_TTR_TTR_SD.rds", sep = "/"))

		#df_episode <- summarize_episode(df_data, df_profile, missing_S_start_date="sample_mix", missing_S_end_date_not_ill="sample_using_S_duration",missing_S_end_date_still_ill="use_report_date",df_dist_time_to_report_S_start=df_dist_time_to_report_S_start, df_dist_time_to_report_S_end= df_dist_time_to_report_S_end,df_dist_S_duration=df_dist_S_duration, df_dist_baseline_health_score=df_dist_baseline_health_score)
		#saveRDS(df_episode, file = paste(RSAVE, "df_episode_mix_SD_RD.rds", sep = "/"))

		df_episode <- summarize_episode(df_data, df_profile, fun_summary_baseline=fun_summary_baseline, missing_S_start_date="sample_mix", missing_S_end_date_not_ill="sample_using_S_duration",missing_S_end_date_still_ill="sample_using_S_duration",df_dist_time_to_report_S_start=df_dist_time_to_report_S_start, df_dist_time_to_report_S_end= df_dist_time_to_report_S_end,df_dist_S_duration=df_dist_S_duration, df_dist_baseline_health_score=df_dist_baseline_health_score)
		saveRDS(df_episode, file = file.path(RSAVE, paste0("df_episode_mix_SD_SD__SDmax=",SD_max,"_baseline_summary=",fun_summary_baseline,".rds")))
	}	

}


main<-function(){	

	dir_paper_graph <<- "/Users/Tonton/work/arcticles/QALY_flusurvey/paper/graph"
	SD_max <- 30
	#options(error = NULL)
	data_processing(SD_max=SD_max,fun_summary_baseline="median")
	# QALD_analysis(SD_max=SD_max)
	# df_pop <- compute_population_size_UK()
	# saveRDS(df_pop,file.path(RSAVE,"df_pop.rds"))
	# compare with flusurvey: TODO

	# dir_prediction <- file.path(PDF,paste0("lme/QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender-1/boxcox/full_info"))
	# prediction_UK(dir_prediction)
	
	dir_prediction <- file.path(PDF,paste0("lme/QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender-1/boxcox/mix_SD_SD__SDmax=",SD_max,"_baseline_summary=median_remove_too_far_predict_all"))
	# dir_prediction <- file.path(PDF,paste0("lme/QALD_loss~symptom+age_group+smoke_bool+is_risk+vaccine+gender/boxcox/mix_SD_SD__SDmax=",SD_max,"_baseline_summary=median_remove_too_far_predict_all"))
	# dir_tex <- "/Users/Tonton/work/arcticles/QALY_flusurvey/paper"
	# prediction_UK(dir_prediction)
	# prediction_UK(dir_prediction,dir_tex,dir_figs=dir_paper_graph)
	# print_info_participants(dir_prediction,dir_figs=dir_paper_graph)
	# plot_timeline_example()

	#dir_profile <- "/Users/tonton/Documents/Data/FluSurvey/pdf/profile"
	#plot_sample_characterictics(dir_profile=dir_profile)

	# plot_predictive_models()

	if(0){
		df_data<-readRDS(file=paste(RSAVE,"data_renamed.rds",sep="/"))	
		symptoms <-c("fever","chills","nose","sneeze","throat","cough","breath","head","muscle_joint","chest","tired","appetite","phlegm","eyes","nausea","vomit","diarrhoea","stomach")
		labels <- c("Fever","Chills","Runny nose","Sneezing","Sore throat","Cough","Shortness of breath","Headache","Muscle pain","Chest pain","Feeling tired","Loss of appetite","Coloured sputum","Watery eyes","Nausea","Vomiting","Diarrhoea","Stomach ache")

		plot_symptom_cloud(df_data=df_data,symptoms=symptoms,labels=labels,dir_pdf="./pdf/wordcloud")
	}

	if(0){
		episode_suffix <- "mix_SD_SD__SDmax=Inf"
		df_episode <-readRDS(file = file.path(RSAVE, paste0("df_episode_",episode_suffix,".rds")))
		df_episode$symptom <- revalue(df_episode$symptom,c(ILI_ecdc="ILI_no_fever"))

		plot_interval_episode(df_episode,symptoms=c("ILI_no_fever","ILI_fever"),dir_pdf="./pdf/reinfection")

	}


}

main()

