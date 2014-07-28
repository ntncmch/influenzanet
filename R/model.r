#'Linear mixed effect model
#'
#'This function runs a linear mixed effect model on a boxcox normalised response value. It returns the fitted values (with confidence intervals) as well as several plots and data frames to check normality of the transformed variable as well as of the residuals.
#' @param df_reg \code{data.frame}.
#' @param form character, a formula.
#' @param heteroscedasticity character, name of the variable used to adjust heteroscedasticity.
#' @param range_response numeric vector, range of the response variable (before transformation) to include in the regression. 
#' @param predict_all logical, if \code{TRUE} all covariate combinations are used for prediction (default). Otherwise, only covariate combinations with more than one participant are used.
#' @param norm_test logical, if \code{TRUE} a normality test is performed on the transformed response variable.
#' @param plot logical, if \code{TRUE} several check are plotted. All these plots are also returned.
#' @note This function test successively a linear model (doesn't account for random effect at participant level), then a linear mixed effect model (with random effect at participant level) and finally add heteroscedasticity if passed as argument. Only the best model is returned (based on likelihood ratio test).
#' @export
#' @import dplyr ggplot2
#' @importFrom car powerTransform bcPower
#' @importFrom plyr dlply llply ldply
#' @importFrom nortest sf.test
#' @importFrom gridExtra marrangeGrob
#' @importFrom nlme gls lme varIdent
#' @return A list of 7 elements:
#' \itemize{
#' \item \code{model} either a \code{gls} or \code{lme} object
#' \item \code{data} data used for regression, include residual. Response is not transformed.
#' \item \code{bc_coef} boxcox coefficient
#' \item \code{prediction} predicted median, confidence and predictive intervals for all covariate combinations
#' \item \code{plot_heteroscedasticity} plot to check heteroscedasticity (only if \code{heteroscedasticity} is provided)
#' \item \code{plot_residuals} plot to check the residuals
#' \item \code{norm_test} A list of 2 elements (only if \code{norm_test} is provided):
#' 	\itemize{
#' 		\item \code{df} data frame with normality test results for all covariate group
#' 		\item \code{plot} plot to check normality of the boxcox transformed response			
#' 	}
#' }
lme_boxcox <- function(df_reg, form, heteroscedasticity=NULL, range_response=c(-Inf, Inf), predict_all = TRUE, norm_test = TRUE, plot = TRUE) {

	# extract response vs explanatory
	my_formula <- formula(form)
	response <- as.character(my_formula[[2]])
	explanatory <- setdiff(all.vars(my_formula), response)

	# select range of response
	call <- substitute(filter(df_reg,response >= min_range & response <= max_range),list(response=as.name(response),min_range=min(range_response),max_range=max(range_response)))
	df_reg <- eval(call)

	# remove response and explanatory with NA
	df_reg <- na.omit(df_reg[c("person_id",response,explanatory)])

	# transform response
	lm_bc <- powerTransform(formula(form), data = df_reg)
	bc_coef <- lm_bc$roundlam[[1]]
	df_reg[response] <- bcPower(df_reg[response], bc_coef)
	inverse_transform <- inverse_bcPower(bc_coef)


	# test normality
	if(norm_test){
		# test of distribution
		n_var <- length(explanatory)
		n_facet_x <- floor(n_var/2)
		facet_x <- ifelse(n_facet_x,paste(explanatory[1:n_facet_x],collapse="+"),".")
		facet_y <- paste(explanatory[(n_facet_x+1):n_var],collapse="+")
		
		dist_test <- dlply(df_reg, explanatory, function(df) {

			x <- df[[response]]
			p_val <- try(sf.test(x)$p)

			best_fit <- data.frame(mean = mean(x), sd = sd(x),p_val=ifelse(inherits(p_val,"try-error"),NA,p_val) )
			best_fit$accept_normality <- (best_fit$p_val>0.05)
			best_fit$sample_size <- nrow(df)

			p <- ggplot(df, aes_string(x = response))
			p <- p + facet_grid(paste(facet_x,facet_y,sep="~"), scales = "free")				
			p <- p + geom_histogram(aes(y = ..density..), position = "identity", binwidth = 0.25, alpha = 0.5)
			p <- p + geom_density(alpha = 0.25)
			p <- p + stat_function(fun = dnorm, args = list(mean = best_fit$mean, sd = best_fit$sd), colour = "red")
			p <- p + xlab("") + ylab("") + theme_bw()

			return(list(best_fit = best_fit, plot = p))
			

		}, .progress = "none")


		plots <- llply(dist_test,function(x) {x$plot})	
		plot_norm_test <- do.call(marrangeGrob,c(plots,list(nrow=2,ncol=2)))
		if(plot){
			print(plot_norm_test)
		}

		df_norm_test <- ldply(dist_test, function(x) x$best_fit)
	} else {
		plot_norm_test <- df_norm_test <- NULL
	}


	# regression
	lm_reg <- gls(model=formula(form), data = df_reg)
	lme_reg <- lme(fixed=formula(form),random=~1|person_id, data = df_reg) #,control=list(opt="optim")

	# comparaison
	print(x <- anova(lme_reg,lm_reg))
	if(x[["p-value"]][2]<0.05){
		model_reg <- lme_reg
	} else {
		model_reg <- lm_reg
	}

	# 
	if(!is.null(heteroscedasticity)){
		# lme_reg_hs <- update(model_reg,weights=varIdent(form=formula(paste0("~1|",heteroscedasticity))), data = df_reg)
		lme_reg_hs <- lme(fixed=formula(form),random=~1|person_id,weights=varIdent(form=formula(paste0("~1|",heteroscedasticity))), data = df_reg)
		print(x <- anova(lme_reg,lme_reg_hs))
		if(x[["p-value"]][2]<0.05){
			model_reg <- lme_reg_hs
		}
		# heteroscedasticity
		p_hs <- plot(x=lme_reg_hs,form=formula(paste0("resid(.,type=\"p\")~fitted(.)|",heteroscedasticity)),id=0.05,adj=-0.1)	
		if(plot){
			print(p_hs)
		}
		p_residuals <- plot(x=lme_reg_hs,form=formula(paste0("resid(.,type=\"p\")~fitted(.)|",paste(explanatory,collapse="*"))),id=0.05,adj=-0.1)
		p_qqnorm <- qqnorm(y=lme_reg_hs,form=formula(paste0("~resid(.)|",paste(explanatory,collapse="*"))),id=0.05,adj=-0.1)	


	} else {
		p_hs <- NULL
		p_residuals <- plot(x=lme_reg,form=formula(paste0("resid(.,type=\"p\")~fitted(.)|",paste(explanatory,collapse="*"))),id=0.05,adj=-0.1)
		p_qqnorm <- qqnorm(y=lme_reg,form=formula(paste0("~resid(.)|",paste(explanatory,collapse="*"))),id=0.05,adj=-0.1)	
	}

	plot_residuals <- do.call(marrangeGrob,list(p_residuals,p_qqnorm,nrow=1,ncol=1))

	if(plot){
		print(plot_residuals)
	}

	# outliers
	df_reg$pearson_residuals <- resid(model_reg,type="p")
	df_reg[[response]] <- inverse_transform(df_reg[[response]])

	# prediction
	new_data <- expand.grid(lapply(df_reg[explanatory],unique))
	new_data$prediction <- predict(model_reg,new_data,level=0)		

	design_matrix <- model.matrix(my_formula[-2], new_data) 
	sd_mat <- model_reg[[ifelse(class(model_reg)=="gls","varBeta","varFix")]]
	design_matrix <- design_matrix[,colnames(sd_mat)]
	var_pred <- diag(design_matrix %*% sd_mat %*% t(design_matrix)) 

	new_data$SE <- sqrt(var_pred) 
	new_data$SE2 <- sqrt(var_pred + model_reg$sigma^2)
	Q95 <- qnorm(0.975)
	new_data_inverse_trans <- mutate(new_data,lower_conf= as.numeric(inverse_transform(prediction-Q95*SE)),upper_conf= as.numeric(inverse_transform(prediction+Q95*SE)),lower_pred= as.numeric(inverse_transform(prediction-Q95*SE2)),upper_pred= as.numeric(inverse_transform(prediction+Q95*SE2)),prediction=as.numeric(inverse_transform(prediction)))	

	if(!predict_all){
		df_pop_size <- unique(df_reg[explanatory])
		df_pop_size$freq <- 1
		new_data_inverse_trans <- left_join(new_data_inverse_trans, df_pop_size, by=explanatory)
		new_data_inverse_trans[is.na(new_data_inverse_trans$freq),setdiff(names(new_data_inverse_trans),explanatory)] <- 0		
		new_data_inverse_trans$freq <- NULL
	}

	return(list(model=model_reg,data=df_reg,bc_coef=bc_coef,prediction=new_data_inverse_trans,plot_heteroscedasticity=p_hs,plot_residuals=plot_residuals,norm_test=list(df=df_norm_test,plot=plot_norm_test)))
}



