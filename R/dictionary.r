#'Valid names for flunet variables
#'
#'This function return a vector of all the valid names for flunet variables.
#' @export
#' @return character vector
valid_variable_names <- function() {

	#data
	time_to_visit <- paste("time_visit",c("GP","hosp","AE","other"),sep="_")
	time_to_phone <- paste("time_phone",c("GP_recept","GP_doctor","NHS_direct","other"),sep="_")
	time_to_other  <- paste("time","AV",sep="_")
	misc <- c("person_id","health_score","cause","top_fever","change_routine","same_bout","time_off","still_off","still_ill")
	sudden_onset <- paste(c("sympt","fever"),"sudden",sep="_")
	visit <-  paste("visit",c("no","GP","AE","hosp","other","schedul"),sep="_")
	phone <- paste("phone",c("no","GP_recept","GP_doctor","NHS_direct","other"),sep="_")
	medication <- paste("med",c("no","pain","cough","AV","AB","other","CR"),sep="_")
	symptoms <- c("none","fever","chills","nose","sneeze","throat","cough","breath","head","muscle_joint","chest","tired","appetite","phlegm","eyes","nausea","vomit","diarrhoea","stomach","other")

	return(c(time_to_visit,time_to_phone,time_to_other,misc,sudden_onset,visit,phone,medication,symptoms))

}

#'Test validity of variable names
#'
#'This function test whether all variable names are valid and throw a warning with unmatched names if any.
#' @param var_name character vector
#' @export
#' @seealso \link{\code{valid_variable_names}}
#' @return \code{TRUE} or \code{FALSE}
#' @examples \dontrun{
#' variable_name_OK(c("not_valid","person_id"))
#'}
variable_name_OK <- function(var_name=NULL){

	stopifnot(is.character(var_name))

	valid_names <- valid_variable_names()

	ind <- which(!var_name%in%valid_names)

	if(length(ind)){
		warning("The following variable names are not valid:",var_name[ind])
	}

	return(!as.logical(length(ind)))

}

#'Dictionary of flunet variable
#'
#'Create a dictionary for a variable with all necessary information to convert it into a flunet object.
#' @param from_name,to_name character, old and new variable names. See \code{rename} for alternative argument passing.
#' @param from_values,to_values character vector, old and new variable values. See \code{revalue} for alternative argument passing.
#' @param rename named character vector of length 1, with new name as value, and old name as name.
#' @param revalue named character vector, with new values as values, and old values as names.
#' @param format character, indicate the R format of the variable.
#' @param order_date date format orders to look for ("ymd" by default). See \code{\link{lubridate::guess_formats}}.
#' @export
#' @return list
variable_dico <- function(from_name=NULL,to_name=NULL,from_values=NULL,to_values=NULL,rename=NULL,revalue=NULL,format=c("factor","ordered","numeric","date","character","logical"),order_date="ymd") {
	
	format <- match.arg(format)

	if(!((is.character(rename) & !is.null(names(rename))) | (is.character(from_name) & is.character(to_name)))){
		stop("Either rename or from_name and to_name must be provided")
	}

	if(length(from_values)!=length(to_values)){
		stop("from_values and to_values must have the same length")
	}

	if(format=="date" & !is.character(order_date)){
		stop("invalid order_date for date format")
	}

	if(!is.null(rename)){
		from_name <- names(rename)
		to_name <- as.vector(rename)
	}

	if(!is.null(revalue)){
		from_values <- names(revalue)
		to_values <- as.vector(revalue)
	}

	ans <- list(from_name=from_name[1],to_name=to_name[1],format=format)
	
	if(format=="date"){
		ans$order_date <- order_date
	}

	if(!is.null(from_values)){
		ans$from_values <- from_values
		ans$to_values <- to_values
	}

	return(ans)
}

#'Template dictionary
#'
#'These functions are wrapers of \code{\link{variable_dico}} that ensure the use of valid names for flunet variables.
#' The last digit indicates the number of levels for this variable.
#' @param from_name character, old name to be replaced
#' @param who character, who did participant visited, phoned?
#' @param CR_rm if \code{TRUE}, the response "Can't remember" are ignored
#' @export
#' @return list
#' @name template dico
#' @aliases time_to_visit_7
time_to_visit_7 <- function(from_name=NULL,who=c("GP","hosp","AE","other"),CR_rm=FALSE) {
	
	stopifnot(is.character(from_name),is.logical(CR_rm))
	
	who <- match.arg(who)
	
	rename <- paste("time_visit",who,sep="_")		
	
	names(rename) <- from_name
	
	revalue=c("7"="CR", "0"="0 day", "1"="1 day", "2"="2 days", "3"="3 days", "4"="4 days", "5"="5-7 days", "6"=">7 days")
	if(CR_rm){
		ind <- which(revalue=="CR")
		revalue <- revalue[-ind]
	}

	return(variable_dico(rename=rename,revalue=revalue,format="ordered")) 
}

#' @name template dico
#' @export
#' @aliases time_to_phone_7
time_to_phone_7 <- function(from_name=NULL,who=c("GP_recept","GP_doctor","NHS_direct","other"),CR_rm=FALSE) {
	
	stopifnot(is.character(from_name),is.logical(CR_rm))
	
	who <- match.arg(who)
	
	rename <- paste("time_phone",who,sep="_")		
	
	names(rename) <- from_name
	
	revalue=c("7"="CR", "0"="0 day", "1"="1 day", "2"="2 days", "3"="3 days", "4"="4 days", "5"="5-7 days", "6"=">7 days")
	if(CR_rm){
		ind <- which(revalue=="CR")
		revalue <- revalue[-ind]
	}

	return(variable_dico(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template dico
#' @export
#' @aliases time_to_AV_7
time_to_AV_7 <- function(from_name=NULL,CR_rm=FALSE) {
	
	stopifnot(is.character(from_name),is.logical(CR_rm))
	
	rename <- "time_AV"
	
	names(rename) <- from_name
	
	revalue=c("7"="CR", "0"="0 day", "1"="1 day", "2"="2 days", "3"="3 days", "4"="4 days", "5"="5-7 days", "6"=">7 days")
	if(CR_rm){
		ind <- which(revalue=="CR")
		revalue <- revalue[-ind]
	}

	return(variable_dico(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template dico
#' @export
#' @aliases cause_illness_7
cause_illness_7 <- function(from_name=NULL) {
	stopifnot(is.character(from_name))
	rename <- "cause"
	names(rename) <- from_name
	revalue <- c("6"="unknown","5"="other","4"="asthma","3"="allergy","2"="cold","1"="gastro","0"="ILI")

	return(variable_dico(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template dico
#' @export
#' @aliases top_fever_7
top_fever_7 <- function(from_name=NULL,CR_rm=FALSE) {
	stopifnot(is.character(from_name),is.logical(CR_rm))
	rename <- "top_fever"
	names(rename) <- from_name
	revalue <- c("6"="CR","0"="<37", "1"="37-37.4", "2"="37.5-37.9", "3"="38-38.9", "4"="39-39.9", "5"=">40")
	if(CR_rm){
		ind <- which(revalue=="CR")
		revalue <- revalue[-ind]
	}

	return(variable_dico(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template dico
#' @export
#' @aliases change_routine_3
change_routine_3 <- function(from_name=NULL) {
	stopifnot(is.character(from_name))
	rename <- "change_routine"
	names(rename) <- from_name
	revalue <- c("0"="no","1"="yes","2"="yes + off")

	return(variable_dico(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template dico
#' @export
#' @aliases same_bout_3
same_bout_3 <- function(from_name=NULL,CR_rm=FALSE) {
	stopifnot(is.character(from_name),is.logical(CR_rm))
	rename <- "same_bout"
	names(rename) <- from_name
	revalue <- c("2"="CR","1"="no","0"="yes")
	if(CR_rm){
		ind <- which(revalue=="CR")
		revalue <- revalue[-ind]
	}

	return(variable_dico(rename=rename,revalue=revalue,format=ifelse(CR_rm,"logical","ordered")))
}

#' @name template dico
#' @export
#' @aliases time_off_8
time_off_8 <- function(from_name=NULL) {
	stopifnot(is.character(from_name))
	rename <- "time_off"
	names(rename) <- from_name
	revalue <- c("0"="1 day", "1"="2 days", "2"="3 days", "3"="4 days", "4"="5 days", "5"="6-10 days", "6"="11-15 days", "7"=">15 days")

	return(variable_dico(rename=rename,revalue=revalue,format="ordered"))
}

#' @param what character, suddden onset of what?
#' @name template dico
#' @export
#' @aliases sudden_onset_of_4
sudden_onset_of_4 <- function(from_name=NULL,what=c("sympt","fever"),CR_rm=FALSE) {
	stopifnot(is.character(from_name),is.logical(CR_rm))
	what <- match.arg(what)

	rename <- paste(what,"sudden",sep="_")
	names(rename) <- from_name
	revalue <- c("3"="CR","2"="CR","1"="no","0"="yes")
	if(CR_rm){
		ind <- which(revalue=="CR")
		revalue <- revalue[-ind]
	}

	return(variable_dico(rename=rename,revalue=revalue,format=ifelse(CR_rm,"logical","ordered")))
}

#' @name template dico
#' @export
#' @aliases still_off_3
still_off_3 <- function(from_name=NULL) {
	stopifnot(is.character(from_name))
	rename <- "still_off"
	names(rename) <- from_name
	revalue <- c("2"="other","1"="no","0"="yes")

	return(variable_dico(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template dico
#' @export
#' @aliases still_ill_3
still_ill_3 <- function(from_name=NULL) {
	stopifnot(is.character(from_name))
	rename <- "still_ill"
	names(rename) <- from_name
	revalue <- c("0"="FALSE","1"="FALSE","2"="TRUE")

	return(variable_dico(rename=rename,revalue=revalue,format="logical"))
}

#' @name template dico
#' @export
#' @aliases visit_logical
visit_logical  <- function(from_name=NULL,who=c("no","GP","AE","hosp","other","schedul")) {
	stopifnot(is.character(from_name))
	who <- match.arg(who)
	
	rename <- paste("visit",who,sep="_")
	names(rename) <- from_name
	revalue <- c("False"="FALSE","True"="TRUE")

	return(variable_dico(rename=rename,revalue=revalue,format="logical"))
}

#' @name template dico
#' @export
#' @aliases phone_logical
phone_logical  <- function(from_name=NULL,who=c("no","GP_recept","GP_doctor","NHS_direct","other")) {
	stopifnot(is.character(from_name))
	who <- match.arg(who)
	
	rename <- paste("phone",who,sep="_")
	names(rename) <- from_name
	revalue <- c("False"="FALSE","True"="TRUE")

	return(variable_dico(rename=rename,revalue=revalue,format="logical"))
}

#' @param med character, name of medication
#' @name template dico
#' @export
#' @aliases medication_logical
medication_logical  <- function(from_name=NULL,med=c("no","pain","cough","AV","AB","other","CR"),CR_rm=FALSE) {
	stopifnot(is.character(from_name),is.logical(CR_rm))
	med <- match.arg(med)
	
	rename <- paste("med",med,sep="_")
	names(rename) <- from_name
	
	revalue <- c("False"="FALSE","True"="TRUE")
	
	if(CR_rm){
		ind <- which(revalue=="CR")
		revalue <- revalue[-ind]
	}

	return(variable_dico(rename=rename,revalue=revalue,format="logical"))
}

#' @param sympt character, name of symptom
#' @name template dico
#' @aliases symptom_logical
#' @export
symptom_logical  <- function(from_name=NULL,sympt=c("none","fever","chills","nose","sneeze","throat","cough","breath","head","muscle_joint","chest","tired","appetite","phlegm","eyes","nausea","vomit","diarrhoea","stomach","other")) {
	stopifnot(is.character(from_name))
	
	rename <- match.arg(sympt)
	names(rename) <- from_name
	revalue <- c("False"="FALSE","True"="TRUE")

	return(variable_dico(rename=rename,revalue=revalue,format="logical"))
}

#' @param date character, type of date reported
#' @name template dico
#' @export
#' @aliases date_ymd
date_ymd <- function(from_name=NULL, date=c("report_date","comp_time","symptom_start","symptom_end")) {

	stopifnot(is.character(from_name))

	rename <- match.arg(date)
	names(rename) <- from_name

	return(variable_dico(rename=rename,format="date",order_date="ymd"))
}


#'Dictionary of the 2012/2013 Flusurvey season
#'
#'This function create the dictionary of the 2012/2013 Flusurvey season
#' @export
#' @import jsonlite
#' @return list of two items: 
#' \enumerate{
#'	\item dico_R: list of dico variables in R format
#'	\item dico_JSON: list of dico variables in JSON format
#' }
create_dictionnary_flusurvey_201213 <- function() {

	# library(jsonlite)
	
	# rename only
	dico_rename <- Map(variable_dico,from_name=c("Person","health"),to_name=c("person_id","health_score"),format=c("character","numeric"))

	# time to
	var_time_visit <- c("How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...GP.or.GP.s.practice.nurse..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...Hospital.admission..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...Hospital.accident...emergency.department.out.of.hours.service..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...Other.medical.services..Medical.Service.")
	var_time_phone <- c("How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...GP...spoke.to.receptionist.only..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...GP...spoke.to.doctor.or.nurse..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...NHS.Direct...NHS.24...NHS.Choices..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...Other..Medical.Service.")
	var_time_AV <- c("How.long.after.the.beginning.of.your.symptoms.did.you.start.taking.antiviral.medication.")

	dico_time_visit <- Map(time_to_visit_7,from_name=var_time_visit,who=c("GP","hosp","AE","other"))
	dico_time_phone <- Map(time_to_phone_7,from_name=var_time_phone,who=c("GP_recept","GP_doctor","NHS_direct","other"))
	dico_time_AV <- time_to_AV_7(from_name=var_time_AV)

	# numeric to factor
	dico_cause <- cause_illness_7("cause")
	dico_top_fever <- top_fever_7("top_fever")
	dico_routine <- change_routine_3("change_routine")
	dico_bout <- same_bout_3("samebout")
	dico_time_off <- time_off_8("How.long.have.you.been.off.work.school.")
	dico_sudden  <- Map(sudden_onset_of_4,from_name=c("sym_sudden","fever_sudden"),what=c("sympt","fever"))
	dico_still_off <- still_off_3("Are.you.still.off.work.school.")

	# numeric to logical
	dico_still_ill <- still_ill_3("stillill")

	# character to logical
	var_visit <- paste0("med_visit",0:5)
	var_phone <- paste0("med_phoneweb",c(0:3,5))
	var_med <- paste0("medication",0:6)
	var_sympt <- c("none","fever","chills","nose","sneeze","throat","cough","breath","head","muscle.joint","chest","tired","appetite","phlegm","eyes","nausea","vomit","diarrhoea","stomach","other")

	dico_visit <- Map(visit_logical,from_name=var_visit,who=c("no","GP","AE","hosp","other","schedul"))
	dico_phone <- Map(phone_logical,from_name=var_phone,who=c("no","GP_recept","GP_doctor","NHS_direct","other"))
	dico_med <- Map(medication_logical,from_name=var_med,med=c("no","pain","cough","AV","AB","other","CR"))
	dico_sympt <- Map(symptom_logical,from_name=var_sympt,sympt=c("none","fever","chills","nose","sneeze","throat","cough","breath","head","muscle_joint","chest","tired","appetite","phlegm","eyes","nausea","vomit","diarrhoea","stomach","other"))

	# ISO date
	var_date <- c("reportdate","Compilation.Date","When.did.the.first.symptoms.appear...0.Open.Answer","When.did.your.symptoms.end...0.Open.Answer")
	dico_date <- Map(date_ymd,from_name=var_date,date=c("report_date","comp_time","symptom_start","symptom_end"))

	# TODO: add dico for profile and order of variable
	dico <- list(country="UK",season="2012/13",survey=c(dico_rename,dico_time_visit,dico_time_phone,dico_time_AV,dico_cause,dico_top_fever,dico_routine,dico_bout,dico_time_off,dico_sudden,dico_still_off,dico_still_ill,dico_visit,dico_phone,dico_med,dico_sympt,dico_date))
	names(dico$survey) <- NULL

	dico_json <- toJSON(dico,pretty=T)

	return(list(dico_R=dico,dico_JSON=dico_json))
}

# initialise a flunet_data with data, profile and dico, 
# following warnings: validate dico (all to_names must be ok), look at what variable are not in the data+profile/dico
rename_data<-function(df_data){

	#rename
	df_data <- rename(df_data,c(Person="person_id",health="health_score",muscle.joint="muscle_joint"))

	#type of data: time_to
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