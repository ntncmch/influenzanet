% Generated by roxygen2 (4.0.1): do not edit by hand
\name{lme_transform}
\alias{lme_transform}
\title{Linear mixed effect model}
\usage{
lme_transform(df_reg, form, trans = c("none", "boxcox", "logit"),
  heteroscedasticity = NULL, range_response = c(-Inf, Inf),
  predict_all = TRUE, norm_test = TRUE, plot = TRUE)
}
\arguments{
\item{df_reg}{\code{data.frame}.}

\item{form}{character, a formula.}

\item{trans}{character, the trnasformation used to normalize the response variable. Default to "none" but "boxcox" and "logit" are available.}

\item{heteroscedasticity}{character, name of the variable used to adjust heteroscedasticity.}

\item{range_response}{numeric vector, range of the response variable (before transformation) to include in the regression. Bounds are not included.}

\item{predict_all}{logical, if \code{TRUE} all covariate combinations are used for prediction (default). Otherwise, only covariate combinations with more than one participant are used.}

\item{norm_test}{logical, if \code{TRUE} a normality test is performed on the transformed response variable.}

\item{plot}{logical, if \code{TRUE} several check are plotted. All these plots are also returned.}
}
\value{
A list of 7 elements:
\itemize{
\item \code{model} either a \code{gls} or \code{lme} object
\item \code{data} data used for regression, include residual. Response is not transformed.
\item \code{fun_transform} function used to transform the response
\item \code{fun_inverse_transform} function used to inverse the transformed response
\item \code{bc_coef} boxcox coefficient (only if \code{trans=="boxcox"})
\item \code{prediction} predicted median, confidence and predictive intervals for all covariate combinations
\item \code{plot_heteroscedasticity} plot to check heteroscedasticity (only if \code{heteroscedasticity} is provided)
\item \code{plot_residuals} plot to check the residuals
\item \code{norm_test} A list of 2 elements (only if \code{norm_test} is provided):
	\itemize{
		\item \code{df} data frame with normality test results for all covariate group
		\item \code{plot} plot to check normality of the boxcox transformed response
	}
}
}
\description{
This function runs a linear mixed effect model on a boxcox normalised response value. It returns the fitted values (with confidence intervals) as well as several plots and data frames to check normality of the transformed variable as well as of the residuals.
}
\note{
This function test successively a linear model (doesn't account for random effect at participant level), then a linear mixed effect model (with random effect at participant level) and finally add heteroscedasticity if passed as argument. Only the best model is returned (based on likelihood ratio test).
}

