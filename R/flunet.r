# constructor for flunet object


flunet <- function(intake_survey=NULL, weekly_survey=NULL, contact_survey=NULL, dictionary=NULL){

	library(plyr)
	library(lubridate)

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

	# check dictionary
	if(is.null(dictionary)){
		stop("A dictionary must be provided")
	} 

	if(!dictionary_validated(dictionary)){
		stop("The dictionary is not valid, see documentation")
	}

	# select surveys that are both provided and in dico
	missing_survey_dico <- setdiff(names(surveys),names(dictionary$surveys))
	if(length(missing_survey_dico)){
		warning("The dictionary does not contain the following surveys, which are thus ignored:\n",paste(missing_survey_dico,collapse="\n"))
	}

	tmp <- intersect(names(surveys),names(dictionary$surveys))
	surveys <- surveys[tmp]

	if(!length(surveys)){
		stop("Surveys are not in the dictionary provided")
	}

	# for all available surveys
	for(name_survey in names(surveys)){
		survey <- surveys[[name_survey]]
		dico_survey <- dictionary$surveys[[name_survey]]
		
		# questions
		name_questions_survey  <- names(survey)
		name_questions_survey_dico  <- get_info_dico(dico_survey,"from_name")

		# check what question are in the data.frame
		missing_question_dico <- setdiff(name_questions_survey,name_questions_survey_dico)
		if(length(missing_question_dico)){
			warning("The dictionary for the ",name_survey," survey does not contain the following questions, which are thus ignored:\n",paste(missing_question_dico,collapse="\n"))
		}

		# keep intersect
		name_questions_survey <- intersect(name_questions_survey,name_questions_survey_dico)
		survey <- survey[name_questions_survey]

		#rename survey
		rename_questions <- get_info_dico(dico_survey,"to_name")
		names(dico_survey) <- rename_questions
		names(rename_questions) <- name_questions_survey_dico
		rename_questions <- rename_questions[name_questions_survey]
		survey <- rename(survey,replace=rename_questions)

		#for each question
		for(name_question in names(survey)){

			dico_question <- dico_survey[[name_question]]
			#revalue
			revalue <- all(c("to_values","from_values")%in%names(dico_question))

			if(revalue){
				#revalue vector
				from <- dico_question$from_values
				to <- dico_question$to_values
				tmp <- survey[ ,name_question]
				tmp <- mapvalues(tmp,from=from,to=to)
				# NA all values not in dico
				tmp[!tmp%in%to] <- NA
				survey[[name_question]] <- tmp
			}

			#format
			if(dico_question$format%in%c("factor","ordered")){

				tmp <- survey[[name_question]]
				if(revalue){
					levels=unique(to)
				}else{
					levels=sort(unique(tmp))
				}
				tmp <- factor(tmp,levels=levels,ordered=(dico_question$format=="ordered"))
				survey[[name_question]] <- tmp
			}

			if(dico_question$format=="character"){
				survey[[name_question]] <- as.character(survey[[name_question]])
			}

			if(dico_question$format=="numeric"){
				survey[[name_question]] <- as.numeric(survey[[name_question]])
			}

			if(dico_question$format=="logical"){
				survey[[name_question]] <- as.logical(survey[[name_question]])
			}

			if(dico_question$format=="date"){
				#guess format
				tmp <- 	survey[[name_question]]
				formats <- guess_formats(tmp,orders=dico_question$order_date)
				ind <- which(is.null(formats))
				if(length(ind)){
					stop(length(ind)," dates in the question ",name_survey,":",dico_question$from_name," are not in the following order: ",dico_question$order_date,"\nFor instance:",tmp[ind[1]])
				}
				ind <- which(tmp!="")
				if(length(formats)!=length(ind)){
					stop("several date formats are not matched for question ",name_survey,":",dico_question$from_name," provide additional order_date in dictionary")
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
	structure(list(country=dictionary$country,season=dictionary$season,surveys=surveys,log=list()),class="flunet")
}


