#'Valid names for flunet variables
#'
#'This function return a list of valid names for flunet surveys.
#' @export
#' @return character vector
valid_survey_names <- function() {
	return(c("intake","weekly","contact"))
}


is_survey_name_valid <- function(survey=NULL){

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



#'Test validity of a variable name
#'
#'This function test whether a variable name is valid and throw a warning otherwise. If more than one \code{var_name} and/or \code{survey} is passed, validity of all names is checked by looking at all the surveys.
#' @param var_name character vector
#' @param survey character, the survey where to look at the variable name
#' @export
#' @seealso \code{\link{valid_variable_names}}
#' @return \code{TRUE} or \code{FALSE}
#' @examples \dontrun{
#' is_variable_name_valid(c("not_valid","person_id"),survey="intake")
#'}
is_variable_name_valid <- function(var_name=NULL,survey=NULL){

	stopifnot(is.character(var_name),all(is_survey_name_valid(survey)))

	valid_names <- valid_variable_names()
	valid_names <- unique(unlist(valid_names[survey]))

	ind <- which(!var_name%in%valid_names)
	ans <- rep(TRUE,length(var_name))

	if(length(ind)){
		warning("The following variable name(s):",sQuote(var_name[ind])," is (are) not valid for the following survey(s):",sQuote(survey))
		ans[ind] <- FALSE
	}

	return(ans)
}


#'context of flunet variable
#'
#'Create a context for a variable with all necessary information to convert it into a flunet object.
#' @param from_name,to_name character, old and new variable names. See \code{rename} for alternative argument passing.
#' @param from_values,to_values character vector, old and new variable values. See \code{revalue} for alternative argument passing.
#' @param rename named character vector of length 1, with new name as value, and old name as name.
#' @param revalue named character vector, with new values as values, and old values as names.
#' @param format character, indicate the \R format of the variable.
#' @param order_date date character vector, format orders to look for ("ymd" by default). See \code{\link[lubridate]{guess_formats}}.
#' @export
#' @return list
variable_context <- function(from_name=NULL,to_name=NULL,from_values=NULL,to_values=NULL,rename=NULL,revalue=NULL,format=c("factor","ordered","numeric","date","character","logical"),order_date="ymd") {
	
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

#'Template context
#'
#'These functions are wrapers of \code{\link{variable_context}} that ensure the use of valid names for flunet variables.
#' The last digit indicates the number of levels for this variable.
#' @param from_name character, old name to be replaced
#' @param who character, who did participant visited, phoned?
#' @param CR_rm if \code{TRUE}, the response "Can't remember" are ignored
#' @export
#' @return list
#' @name template context
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

	return(variable_context(rename=rename,revalue=revalue,format="ordered")) 
}

#' @name template context
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

	return(variable_context(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template context
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

	return(variable_context(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template context
#' @export
#' @aliases cause_illness_7
cause_illness_7 <- function(from_name=NULL) {
	stopifnot(is.character(from_name))
	rename <- "cause"
	names(rename) <- from_name
	revalue <- c("6"="unknown","5"="other","4"="asthma","3"="allergy","2"="cold","1"="gastro","0"="ILI")

	return(variable_context(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template context
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

	return(variable_context(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template context
#' @export
#' @aliases change_routine_3
change_routine_3 <- function(from_name=NULL) {
	stopifnot(is.character(from_name))
	rename <- "change_routine"
	names(rename) <- from_name
	revalue <- c("0"="no","1"="yes","2"="yes + off")

	return(variable_context(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template context
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

	return(variable_context(rename=rename,revalue=revalue,format=ifelse(CR_rm,"logical","ordered")))
}

#' @name template context
#' @export
#' @aliases time_off_8
time_off_8 <- function(from_name=NULL) {
	stopifnot(is.character(from_name))
	rename <- "time_off"
	names(rename) <- from_name
	revalue <- c("0"="1 day", "1"="2 days", "2"="3 days", "3"="4 days", "4"="5 days", "5"="6-10 days", "6"="11-15 days", "7"=">15 days")

	return(variable_context(rename=rename,revalue=revalue,format="ordered"))
}

#' @param what character, suddden onset of what?
#' @name template context
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

	return(variable_context(rename=rename,revalue=revalue,format=ifelse(CR_rm,"logical","ordered")))
}

#' @name template context
#' @export
#' @aliases still_off_3
still_off_3 <- function(from_name=NULL) {
	stopifnot(is.character(from_name))
	rename <- "still_off"
	names(rename) <- from_name
	revalue <- c("2"="other","1"="no","0"="yes")

	return(variable_context(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template context
#' @export
#' @aliases still_ill_3
still_ill_3 <- function(from_name=NULL) {
	stopifnot(is.character(from_name))
	rename <- "still_ill"
	names(rename) <- from_name
	revalue <- c("0"="FALSE","1"="FALSE","2"="TRUE")

	return(variable_context(rename=rename,revalue=revalue,format="logical"))
}

#' @name template context
#' @export
#' @aliases visit_logical
visit_logical  <- function(from_name=NULL,who=c("no","GP","AE","hosp","other","schedul")) {
	stopifnot(is.character(from_name))
	who <- match.arg(who)
	
	rename <- paste("visit",who,sep="_")
	names(rename) <- from_name
	revalue <- c("False"="FALSE","True"="TRUE")

	return(variable_context(rename=rename,revalue=revalue,format="logical"))
}

#' @name template context
#' @export
#' @aliases phone_logical
phone_logical  <- function(from_name=NULL,who=c("no","GP_recept","GP_doctor","NHS_direct","other")) {
	stopifnot(is.character(from_name))
	who <- match.arg(who)
	
	rename <- paste("phone",who,sep="_")
	names(rename) <- from_name
	revalue <- c("False"="FALSE","True"="TRUE")

	return(variable_context(rename=rename,revalue=revalue,format="logical"))
}

#' @param med character, name of medication
#' @name template context
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

	return(variable_context(rename=rename,revalue=revalue,format="logical"))
}

#' @param sympt character, name of symptom
#' @name template context
#' @aliases symptom_logical
#' @export
symptom_logical  <- function(from_name=NULL,sympt=c("none","fever","chills","nose","sneeze","throat","cough","breath","head","muscle_joint","chest","tired","appetite","phlegm","eyes","nausea","vomit","diarrhoea","stomach","other")) {
	stopifnot(is.character(from_name))
	
	rename <- match.arg(sympt)
	names(rename) <- from_name
	revalue <- c("False"="FALSE","True"="TRUE")

	return(variable_context(rename=rename,revalue=revalue,format="logical"))
}

#' @param UHC character, name of underlying health-condition
#' @name template context
#' @aliases UHC_logical
#' @export
UHC_logical  <- function(from_name=NULL,UHC=c("asthma","diabetes","other_respiratory","heart","kidney","immuno")){
	stopifnot(is.character(from_name))
	
	rename <- match.arg(UHC)
	names(rename) <- from_name
	revalue <- c("False"="FALSE","True"="TRUE")

	return(variable_context(rename=rename,revalue=revalue,format="logical"))
}

#' @param date character, name of date reported. If name contains "time", the date must be in the ymdhms order to be converted as POSIXlt. Otherwise, format order is ymd and date will be converted as Date.
#' @name template context
#' @export
#' @aliases date_time
date_time <- function(from_name=NULL, date=c("report_date","comp_time","symptom_start","symptom_end","register_date")) {

	stopifnot(is.character(from_name))

	rename <- match.arg(date)
	names(rename) <- from_name

	order_date <- ifelse(grepl("time",rename),"ymdhms","ymd")

	return(variable_context(rename=rename,format="date",order_date=order_date))
}


#' @name template context
#' @export
#' @aliases still_off_3
still_off_3 <- function(from_name=NULL) {
	stopifnot(is.character(from_name))
	rename <- "still_off"
	names(rename) <- from_name
	revalue <- c("2"="other","1"="no","0"="yes")

	return(variable_context(rename=rename,revalue=revalue,format="ordered"))
}

#' @name template context
#' @param to_name character, new variable name
#' @export
#' @aliases zero_one_to_true_false
zero_one_to_true_false <- function(from_name=NULL,to_name=NULL) {
	stopifnot(is.character(from_name),is.character(to_name))
	revalue <- c("0"="TRUE","1"="FALSE")

	return(variable_context(from_name=from_name,to_name=to_name,revalue=revalue,format="logical"))
}



#'context of the 2012/2013 Flusurvey season
#'
#'This function create the context of the 2012/2013 Flusurvey season
#' @export
#' @import jsonlite
#' @return list of two items: 
#' \enumerate{
#'	\item context_R: list of context variables in R format
#'	\item context_JSON: list of context variables in JSON format
#' }
create_context_flusurvey_201213 <- function() {

	###############
	# weekly survey

	# rename only
	context_rename <- Map(variable_context,from_name=c("Person","health"),to_name=c("person_id","health_score"),format=c("character","numeric"))

	# time to
	var_time_visit <- c("How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...GP.or.GP.s.practice.nurse..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...Hospital.admission..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...Hospital.accident...emergency.department.out.of.hours.service..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.VISIT.a.medical.service...Other.medical.services..Medical.Service.")
	var_time_phone <- c("How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...GP...spoke.to.receptionist.only..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...GP...spoke.to.doctor.or.nurse..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...NHS.Direct...NHS.24...NHS.Choices..Medical.Service.","How.soon.after.your.symptoms.appeared.did.you.first.contact.a.medical.service.via.TELEPHONE.or.INTERNET...Other..Medical.Service.")
	var_time_AV <- c("How.long.after.the.beginning.of.your.symptoms.did.you.start.taking.antiviral.medication.")

	context_time_visit <- Map(time_to_visit_7,from_name=var_time_visit,who=c("GP","hosp","AE","other"))
	context_time_phone <- Map(time_to_phone_7,from_name=var_time_phone,who=c("GP_recept","GP_doctor","NHS_direct","other"))
	context_time_AV <- list(time_to_AV_7(from_name=var_time_AV))

	# numeric to factor
	context_cause <- list(cause_illness_7("cause"))
	context_top_fever <- list(top_fever_7("top_fever"))
	context_routine <- list(change_routine_3("change_routine"))
	context_bout <- list(same_bout_3("samebout"))
	context_time_off <- list(time_off_8("How.long.have.you.been.off.work.school."))
	context_sudden  <- Map(sudden_onset_of_4,from_name=c("sym_sudden","fever_sudden"),what=c("sympt","fever"))
	context_still_off <- list(still_off_3("Are.you.still.off.work.school."))

	# numeric to logical
	context_still_ill <- list(still_ill_3("stillill"))

	# character to logical
	var_visit <- paste0("med_visit",0:5)
	var_phone <- paste0("med_phoneweb",c(0:3,5))
	var_med <- paste0("medication",0:6)
	var_sympt <- c("none","fever","chills","nose","sneeze","throat","cough","breath","head","muscle.joint","chest","tired","appetite","phlegm","eyes","nausea","vomit","diarrhoea","stomach","other")

	context_visit <- Map(visit_logical,from_name=var_visit,who=c("no","GP","AE","hosp","other","schedul"))
	context_phone <- Map(phone_logical,from_name=var_phone,who=c("no","GP_recept","GP_doctor","NHS_direct","other"))
	context_med <- Map(medication_logical,from_name=var_med,med=c("no","pain","cough","AV","AB","other","CR"))
	context_sympt <- Map(symptom_logical,from_name=var_sympt,sympt=c("none","fever","chills","nose","sneeze","throat","cough","breath","head","muscle_joint","chest","tired","appetite","phlegm","eyes","nausea","vomit","diarrhoea","stomach","other"))

	# ISO date
	var_date <- c("reportdate","Compilation.Date","When.did.the.first.symptoms.appear...0.Open.Answer","When.did.your.symptoms.end...0.Open.Answer")
	context_date <- Map(date_time,from_name=var_date,date=c("report_date","comp_time","symptom_start","symptom_end"))
	context_weekly <- c(context_rename,context_time_visit,context_time_phone,context_time_AV,context_cause,context_top_fever,context_routine,context_bout,context_time_off,context_sudden,context_still_off,context_still_ill,context_visit,context_phone,context_med,context_sympt,context_date)

	###############
	# intake survey
	context_gender <- list(variable_context(rename=c("gender"="gender"),revalue=c("0"="male","1"="female"),format="factor"))
	context_employ <- list(variable_context(from_name="employed",to_name="employment",from_values=as.character(0:8),to_values=c("paid_full_time", "paid_part_time","self","student","home","none","long_term_leave","retired","other"),format="factor"))
	context_occup <- list(variable_context(from_name="activity",to_name="occupation",from_values=as.character(0:5),to_values=c("professional", "office","service","skilled_manual","other_manual","other"),format="factor"))
	context_smoke <- list(variable_context(from_name="smoke",to_name="smoke",from_values=as.character(c(4,0:3)),to_values=c("unknown","no", "occasionally","<10_per_day",">10_per_day"),format="ordered"))

	var_UHC <- c("asthma","diabetes","respiratoryother","heart","kidney","immuno")
	context_UHC <- Map(UHC_logical,from_name=var_UHC,UHC=c("asthma","diabetes","other_respiratory","heart","kidney","immuno"))

	context_01 <- Map(zero_one_to_true_false,from_name=c("vaccinelast","vaccine","pregnant"),to_name=c("vaccine_last","vaccine","pregnant"))
	context_region <- list(variable_context(rename=c("region"="region"),format="factor"))
	context_date_profile <- Map(date_time,from_name=c("Compilation.Date","registerdate"),date=c("comp_time","register_date"))
	context_rename_profile <- Map(variable_context,from_name=c("Person","age"),to_name=c("person_id","age"),format=c("character","numeric"))
	context_intake <- c(context_rename_profile,context_date_profile,context_region,context_01,context_UHC,context_smoke,context_occup,context_employ,context_gender)


	################
	# contact survey
	# TODO


	##############
	# create context
	context <- list(country="UK",season="2012/13",surveys=list(intake=context_intake,weekly=context_weekly))
	names(context$surveys$intake) <- NULL
	names(context$surveys$weekly) <- NULL
	names(context$surveys$contact) <- NULL

	context_json <- toJSON(context,pretty=T)

	return(list(context_R=context,context_JSON=context_json))
}

#' @export
#' @aliases is_context_survey_valid
#' @name is_context_valid
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
#' @aliases is_context_survey_valid
#' @name is_context_valid
is_valid_to_names <- function(x=NULL,survey=NULL) {

	stopifnot(!is.null(x),all(is_survey_name_valid(survey)))

	if("to_name"%in%names(x)){
		return(is_variable_name_valid(x$to_name,survey))	
	}else{
		warning("to_name is missing")
		return(FALSE)
	}

}

#' @export
#' @param x a context variable
#' @aliases is_context_survey_valid
#' @name is_context_valid
is_valid_format <- function(x=NULL) {

	stopifnot(!is.null(x))

	if("format"%in%names(x)){
		return(x$format%in%eval(formals(variable_context)$format))	
	}else{
		warning("format is missing")
		return(FALSE)
	}

}

#' @param survey character, name of the survey
#' @export
#' @aliases is_context_survey_valid
#' @name is_context_valid
is_context_survey_valid <- function(context=NULL,survey=NULL) {

	stopifnot(!is.null(context),all(is_survey_name_valid(survey)))

	ans <- sapply(survey, function(x) {

		if(x%in%names(context$surveys)){

			context_survey <- context$surveys[[x]]

			valid_from_names <- lapply(context_survey,is_valid_from_names)
			valid_to_names <- lapply(context_survey,is_valid_to_names,survey=x)
			valid_format <- lapply(context_survey,is_valid_format)

			return(all(unlist(c(valid_from_names,valid_to_names,valid_format))))

		}else{
			return(FALSE)
		}

	})
	
	return(ans)
}

#'Validate context
#'
#'These functions check and validate the fields of the context, the names of the surveys and the names of the survey variables.
#' @param context flunet context
#' @export
#' @seealso \code{\link{create_context_flusurvey_201213}} for an example of how to create a flunet context, 
#' \code{\link{variable_context}} for how to define a variable in the context.
#' @return \code{TRUE} if valid, \code{FALSE} otherwise.
is_context_valid <- function(context) {

	valid_context <- TRUE

	# check names
	mandatory_fields_names <- c("country","season","surveys")
	missing_mandatory_fields_names <- setdiff(mandatory_fields_names,names(context))
	if(length(missing_mandatory_fields_names)){
		valid_context <- FALSE
		warning("The context misses the following mandatory fields: ",paste(missing_mandatory_fields_names,collapse=", "))
	}

	# check whether the context contains unrecognised field names
	extra_names <- setdiff(names(context),mandatory_fields_names)
	if(length(extra_names)){
		warning("The context contains the following unrecognized fields: ",paste(extra_names,collapse=", "))
	}

	# check which surveys are available
	if("surveys"%in%names(context)){

		if(!length(context$surveys)){
			valid_context <- FALSE
			warning("There is no survey in the context")

		}else{
			valid_survey_names <- valid_survey_names()
			context_survey_names <- names(context$surveys)

			missing_survey_names <- setdiff(valid_survey_names,context_survey_names)
			if(length(missing_survey_names)){
				warning("The context miss the following surveys: ",sQuote(missing_survey_names),call.=FALSE)
			}

			extra_survey_names <- setdiff(context_survey_names,valid_survey_names)
			if(length(extra_survey_names)){
				warning("The context contains the following unrecognized surveys: ",sQuote(extra_survey_names),call.=FALSE)
			}

			# check the variable names in each of the available survey context
			available_survey_names <- intersect(valid_survey_names,context_survey_names)
			check_survey <- is_context_survey_valid(context=context,survey=available_survey_names)
			if(!all(unlist(check_survey))){
				valid_context <- FALSE			
			}

		}
		
	}else{
		valid_context <- FALSE
	}

	return(valid_context)
}

import_context <- function(file) {
# TODO read in json format and check

	# context <- 

}

get_info_context <- function(context,info) {
	as.vector(unlist(lapply(context,function(x){return(x[[info]])})))
}
