#'Valid names for flunet variables
#'
#'This function return a list of valid names for flunet surveys.
#' @export
#' @return character vector
valid_survey_names <- function() {
	return(c("intake","weekly","contact"))
}

survey_names_validated <- function(survey=NULL){

	return(is.character(survey) & survey%in%valid_survey_names())

}

#'Valid names for flunet variables
#'
#'This function return a list of two vectors with all the valid names for flunet variables corresponding to the intake and weekly questionaires.
#' @export
#' @return character vector
valid_variable_names <- function() {

	# intake questions
	names_intake <- c(
		"person_id",
		"age",
		"comp_time",
		"register_date",
		"region",
		"vaccine_last",
		"vaccine",
		"pregnant",
		"asthma",
		"diabetes",
		"other_respiratory",
		"heart",
		"kidney",
		"immuno",
		"smoke",
		"occupation",
		"employment",
		"gender"
		)

	# weekly questionaire
	names_weekly <- c(
		"appetite",
		"breath",
		"cause",
		"change_routine",
		"chest",
		"chills",
		"comp_time",
		"cough",
		"diarrhoea",
		"eyes",
		"fever",
		"fever_sudden",
		"head",
		"health_score",
		"med_AB",
		"med_AV",
		"med_cough",
		"med_CR",
		"med_no",
		"med_other",
		"med_pain",
		"muscle_joint",
		"nausea",
		"none",
		"nose",
		"other",
		"person_id",
		"phlegm",
		"phone_GP_doctor",
		"phone_GP_recept",
		"phone_NHS_direct",
		"phone_no",
		"phone_other",
		"report_date",
		"same_bout",
		"sneeze",
		"still_ill",
		"still_off",
		"stomach",
		"sympt_sudden",
		"symptom_end",
		"symptom_start",
		"throat",
		"time_AV",
		"time_off",
		"time_phone_GP_doctor",
		"time_phone_GP_recept",
		"time_phone_NHS_direct",
		"time_phone_other",
		"time_visit_AE",
		"time_visit_GP",
		"time_visit_hosp",
		"time_visit_other",
		"tired",
		"top_fever",
		"visit_AE",
		"visit_GP",
		"visit_hosp",
		"visit_no",
		"visit_other",
		"visit_schedul",
		"vomit"
		)


	valid_names <- list(intake=names_intake,weekly=names_weekly,contact=c(NULL))

	return(valid_names)

}



#'Test validity of variable names
#'
#'This function test whether all variable names are valid and throw a warning with unmatched names if any.
#' @param var_name character vector
#' @param survey character, the survey where to look at the variable name
#' @export
#' @seealso \link{\code{valid_variable_names}}
#' @return \code{TRUE} or \code{FALSE}
#' @examples \dontrun{
#' is_variable_name_OK(c("not_valid","person_id"))
#'}
variable_name_validated <- function(var_name=NULL,survey=NULL){

	stopifnot(is.character(var_name),survey_names_validated(survey))

	valid_names <- valid_variable_names()
	valid_names <- unique(unlist(valid_names[survey]))

	ind <- which(!var_name%in%valid_names)

	if(length(ind)){
		warning("The following variable names are not valid for the ",paste(survey,collapse=" & ")," survey: ",var_name[ind])
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

#' @param UHC character, name of underlying health-condition
#' @name template dico
#' @aliases UHC_logical
#' @export
UHC_logical  <- function(from_name=NULL,UHC=c("asthma","diabetes","other_respiratory","heart","kidney","immuno")){
	stopifnot(is.character(from_name))
	
	rename <- match.arg(UHC)
	names(rename) <- from_name
	revalue <- c("False"="FALSE","True"="TRUE")

	return(variable_dico(rename=rename,revalue=revalue,format="logical"))
}

#' @param date character, name of date reported. If name contains "time", the date must be in the ymdhms order to be converted as POSIXlt. Otherwise, format order is ymd and date will be converted as Date.
#' @name template dico
#' @export
#' @aliases date_time
date_time <- function(from_name=NULL, date=c("report_date","comp_time","symptom_start","symptom_end","register_date")) {

	stopifnot(is.character(from_name))

	rename <- match.arg(date)
	names(rename) <- from_name

	order_date <- ifelse(grepl("time",rename),"ymdhms","ymd")

	return(variable_dico(rename=rename,format="date",order_date=order_date))
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
#' @aliases zero_one_to_true_false
zero_one_to_true_false <- function(from_name=NULL,to_name=NULL) {
	stopifnot(is.character(from_name),is.character(to_name))
	revalue <- c("0"="TRUE","1"="FALSE")

	return(variable_dico(from_name=from_name,to_name=to_name,revalue=revalue,format="logical"))
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

	###############
	# weekly survey

	# rename only
	dico_rename <- Map(variable_dico,from_name=c("Person","health"),to_name=c("person_id","health_score"),format=c("character","numeric"))

	# time to
	var_time_visit <- c("How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...GP.or.GP.s.practice.nurse..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...Hospital.admission..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...Hospital.accident...emergency.department.out.of.hours.service..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...Other.medical.services..Medical.Service.")
	var_time_phone <- c("How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...GP...spoke.to.receptionist.only..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...GP...spoke.to.doctor.or.nurse..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...NHS.Direct...NHS.24...NHS.Choices..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...Other..Medical.Service.")
	var_time_AV <- c("How.long.after.the.beginning.of.your.symptoms.did.you.start.taking.antiviral.medication.")

	dico_time_visit <- Map(time_to_visit_7,from_name=var_time_visit,who=c("GP","hosp","AE","other"))
	dico_time_phone <- Map(time_to_phone_7,from_name=var_time_phone,who=c("GP_recept","GP_doctor","NHS_direct","other"))
	dico_time_AV <- list(time_to_AV_7(from_name=var_time_AV))

	# numeric to factor
	dico_cause <- list(cause_illness_7("cause"))
	dico_top_fever <- list(top_fever_7("top_fever"))
	dico_routine <- list(change_routine_3("change_routine"))
	dico_bout <- list(same_bout_3("samebout"))
	dico_time_off <- list(time_off_8("How.long.have.you.been.off.work.school."))
	dico_sudden  <- Map(sudden_onset_of_4,from_name=c("sym_sudden","fever_sudden"),what=c("sympt","fever"))
	dico_still_off <- list(still_off_3("Are.you.still.off.work.school."))

	# numeric to logical
	dico_still_ill <- list(still_ill_3("stillill"))

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
	dico_date <- Map(date_time,from_name=var_date,date=c("report_date","comp_time","symptom_start","symptom_end"))
	dico_weekly <- c(dico_rename,dico_time_visit,dico_time_phone,dico_time_AV,dico_cause,dico_top_fever,dico_routine,dico_bout,dico_time_off,dico_sudden,dico_still_off,dico_still_ill,dico_visit,dico_phone,dico_med,dico_sympt,dico_date)

	###############
	# intake survey
	dico_gender <- list(variable_dico(rename=c("gender"="gender"),revalue=c("0"="male","1"="female"),format="factor"))
	dico_employ <- list(variable_dico(from_name="employed",to_name="employment",from_values=as.character(0:8),to_values=c("paid_full_time", "paid_part_time","self","student","home","none","long_term_leave","retired","other"),format="factor"))
	dico_occup <- list(variable_dico(from_name="activity",to_name="occupation",from_values=as.character(0:5),to_values=c("professional", "office","service","skilled_manual","other_manual","other"),format="factor"))
	dico_smoke <- list(variable_dico(from_name="smoke",to_name="smoke",from_values=as.character(c(4,0:3)),to_values=c("unknown","no", "occasionally","<10_per_day",">10_per_day"),format="ordered"))

	var_UHC <- c("asthma","diabetes","respiratoryother","heart","kidney","immuno")
	dico_UHC <- Map(UHC_logical,from_name=var_UHC,UHC=c("asthma","diabetes","other_respiratory","heart","kidney","immuno"))

	dico_01 <- Map(zero_one_to_true_false,from_name=c("vaccinelast","vaccine","pregnant"),to_name=c("vaccine_last","vaccine","pregnant"))
	dico_region <- list(variable_dico(rename=c("region"="region"),format="factor"))
	dico_date_profile <- Map(date_time,from_name=c("Compilation.Date","registerdate"),date=c("comp_time","register_date"))
	dico_rename_profile <- Map(variable_dico,from_name=c("Person","age"),to_name=c("person_id","age"),format=c("character","numeric"))
	dico_intake <- c(dico_rename_profile,dico_date_profile,dico_region,dico_01,dico_UHC,dico_smoke,dico_occup,dico_employ,dico_gender)


	################
	# contact survey
	# TODO


	##############
	# create dico
	dico <- list(country="UK",season="2012/13",surveys=list(intake=dico_intake,weekly=dico_weekly))
	names(dico$surveys$intake) <- NULL
	names(dico$surveys$weekly) <- NULL
	names(dico$surveys$contact) <- NULL

	dico_json <- toJSON(dico,pretty=T)

	return(list(dico_R=dico,dico_JSON=dico_json))
}

#' @export
#' @aliases is_valid_dico_survey
#' @name dictionary_validated
is_valid_from_names <- function(x=NULL) {

	stopifnot(!is.null(x))

	if("from_name"%in%names(x)){
		return(TRUE)	
	}else{
		warning("from_name is missing")
		return(FALSE)
	}

}

#' @export
#' @aliases is_valid_dico_survey
#' @name dictionary_validated
is_valid_to_names <- function(x=NULL,survey=NULL) {

	stopifnot(!is.null(x),survey_names_validated(survey))

	if("to_name"%in%names(x)){
		return(variable_name_validated(x$to_name,survey))	
	}else{
		warning("to_name is missing")
		return(FALSE)
	}

}

#' @export
#' @aliases is_valid_dico_survey
#' @name dictionary_validated
is_valid_format <- function(x=NULL) {

	stopifnot(!is.null(x))

	if("format"%in%names(x)){
		return(x$format%in%eval(formals(variable_dico)$format))	
	}else{
		warning("format is missing")
		return(FALSE)
	}

}

#' @param survey character, name of the survey
#' @export
#' @aliases is_valid_dico_survey
#' @name dictionary_validated
is_valid_dico_survey <- function(dico=NULL,survey=NULL) {

	stopifnot(!is.null(dico),survey_names_validated(survey))

	if(survey%in%names(dico$surveys)){
		
		dico_survey <- dico$surveys[[survey]]

		valid_from_names <- lapply(dico_survey,is_valid_from_names)
		valid_to_names <- lapply(dico_survey,is_valid_to_names,survey=survey)
		valid_format <- lapply(dico_survey,is_valid_format)

		return(all(unlist(c(valid_from_names,valid_to_names,valid_format))))

	}else{
		return(FALSE)
	}

}

#'Validate dictionary
#'
#'These functions check and validate the fields of the dictionary, the names of the surveys and the names of the survey variables.
#' @param dico flunet dictionary
#' @export
#' @seealso \code{\link{create_dictionnary_flusurvey_201213}} for an example of how to create a flunet dictionary, 
#' \code{\link{variable_dico}} for how to define a variable in the dictionary.
#' @return \code{TRUE} if valid, \code{FALSE} otherwise.
dictionary_validated <- function(dico) {

	valid_dico <- TRUE

	# check names
	mandatory_fields_names <- c("country","season","surveys")
	missing_mandatory_fields_names <- setdiff(mandatory_fields_names,names(dico))
	if(length(missing_mandatory_fields_names)){
		valid_dico <- FALSE
		warning("The dictionary miss the following mandatory fields: ",paste(missing_mandatory_fields_names,collapse=", "))
	}

	# check whether the dico contains unrecognised field names
	extra_names <- setdiff(names(dico),mandatory_fields_names)
	if(length(extra_names)){
		warning("The dictionary contains the following unrecognized fields: ",paste(extra_names,collapse=", "))
	}

	# check which surveys are available
	if("surveys"%in%names(dico)){

		if(!length(dico$surveys)){
			valid_dico <- FALSE
			warning("There is no survey in the dictionary")

		}else{
			valid_survey_names <- valid_survey_names()
			dico_survey_names <- names(dico$surveys)

			missing_survey_names <- setdiff(valid_survey_names,dico_survey_names)
			if(length(missing_survey_names)){
				warning("The dictionary miss the following surveys: ",paste(missing_survey_names,collapse=", "))
			}

			extra_survey_names <- setdiff(dico_survey_names,valid_survey_names)
			if(length(extra_survey_names)){
				warning("The dictionary contains the following unrecognized surveys: ",paste(extra_survey_names,collapse=", "))
			}

			# check the variable names in each of the available survey dico
			available_survey_names <- intersect(valid_survey_names,dico_survey_names)
			check_survey <- lapply(available_survey_names,is_valid_dico_survey,dico=dico)
			if(!all(unlist(check_survey))){
				valid_dico <- FALSE			
			}

		}
		
	}else{
		valid_dico <- FALSE
	}

	return(valid_dico)
}

import_dictionary <- function(file) {
# TODO read in json format and check

	# dico <- 

}

get_info_dico <- function(dico,info) {
	as.vector(unlist(lapply(dico,function(x){return(x[[info]])})))
}

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