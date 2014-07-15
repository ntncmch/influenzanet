#'Constructor for flunet object
#'
#'Constructor for flunet object
#' @param intake_survey data frame
#' @param  weekly_survey data frame
#' @param  contact_survey data frame
#' @param  context list
#' @export
#' @import plyr lubridate
flunet <- function(intake_survey=NULL, weekly_survey=NULL, contact_survey=NULL, context=NULL){

	# check surveys
	surveys <- list()

	if(!is.null(intake_survey) && !is.data.frame(intake_survey)){
		stop("intake_survey is not a data.frame")
	}else{
		surveys$intake <- intake_survey
	}

	if(!is.null(weekly_survey) && !is.data.frame(weekly_survey)){
		stop("weekly_survey is not a data.frame")
	}else{
		surveys$weekly <- weekly_survey
	}

	if(!is.null(contact_survey) && !is.data.frame(contact_survey)){
		stop("contact_survey is not a data.frame")
	}else{
		surveys$contact <- contact_survey
	}

	if(!length(surveys)){
		stop("At least one survey must be provided")
	}

	# check context
	if(is.null(context)){
		stop("A context must be provided")
	} 

	if(!is_context_valid(context)){
		stop("The context is not valid, see documentation")
	}

	# select surveys that are both provided and in context
	missing_survey_context <- setdiff(names(surveys),names(context$surveys))
	if(length(missing_survey_context)){
		warning("The context does not contain the following surveys, which are thus ignored:\n",paste(missing_survey_context,collapse="\n"))
	}

	tmp <- intersect(names(surveys),names(context$surveys))
	surveys <- surveys[tmp]

	if(!length(surveys)){
		stop("Surveys are not in the context provided")
	}

	# for all available surveys
	for(name_survey in names(surveys)){
		survey <- surveys[[name_survey]]
		context_survey <- context$surveys[[name_survey]]
		
		# questions
		name_questions_survey  <- names(survey)
		name_questions_survey_context  <- get_info_context(context_survey,"from_name")

		# check what question are in the data.frame
		missing_question_context <- setdiff(name_questions_survey,name_questions_survey_context)
		if(length(missing_question_context)){
			warning("The context for the ",name_survey," survey does not contain the following questions, which are thus ignored:\n",paste(missing_question_context,collapse="\n"))
		}

		# keep intersect
		name_questions_survey <- intersect(name_questions_survey,name_questions_survey_context)
		survey <- survey[name_questions_survey]

		#rename survey
		rename_questions <- get_info_context(context_survey,"to_name")
		names(context_survey) <- rename_questions
		names(rename_questions) <- name_questions_survey_context
		rename_questions <- rename_questions[name_questions_survey]
		survey <- rename(survey,replace=rename_questions)

		#for each question
		for(name_question in names(survey)){

			context_question <- context_survey[[name_question]]
			#revalue
			revalue <- all(c("to_values","from_values")%in%names(context_question))

			if(revalue){
				#revalue vector
				from <- context_question$from_values
				to <- context_question$to_values
				tmp <- survey[ ,name_question]
				tmp <- mapvalues(tmp,from=from,to=to)
				# NA all values not in context
				tmp[!tmp%in%to] <- NA
				survey[[name_question]] <- tmp
			}

			#format
			if(context_question$format%in%c("factor","ordered")){

				tmp <- survey[[name_question]]
				if(revalue){
					levels=unique(to)
				}else{
					levels=sort(unique(tmp))
				}
				tmp <- factor(tmp,levels=levels,ordered=(context_question$format=="ordered"))
				survey[[name_question]] <- tmp
			}

			if(context_question$format=="character"){
				survey[[name_question]] <- as.character(survey[[name_question]])
			}

			if(context_question$format=="numeric"){
				survey[[name_question]] <- as.numeric(survey[[name_question]])
			}

			if(context_question$format=="logical"){
				survey[[name_question]] <- as.logical(survey[[name_question]])
			}

			if(context_question$format=="date"){
				#guess format
				tmp <- 	survey[[name_question]]
				formats <- guess_formats(tmp,orders=context_question$order_date)
				ind <- which(is.null(formats))
				if(length(ind)){
					stop(length(ind)," dates in the question ",name_survey,":",context_question$from_name," are not in the following order: ",context_question$order_date,"\nFor instance:",tmp[ind[1]])
				}
				ind <- which(tmp!="")
				if(length(formats)!=length(ind)){
					stop("several date formats are not matched for question ",name_survey,":",context_question$from_name," provide additional order_date in context")
				}
				
				# if question name contains "time" then use POSIXct, otherwise use Date.
				if(grepl("time",name_question)){
					# HMS precision
					survey[[name_question]] <- as.POSIXct(NA)
					survey[[name_question]][ind] <- as.POSIXct(x=tmp[ind],format=formats)
				}else{
					# daily precision
					survey[[name_question]] <- as.Date(NA)
					survey[ind,name_question] <- as.Date(x=tmp[ind],format=formats)					
				}
			}	
		}
		surveys[[name_survey]] <- survey
	}

	# create object 
	structure(list(country=context$country,season=context$season,surveys=surveys,log=list()),class="flunet")
}


