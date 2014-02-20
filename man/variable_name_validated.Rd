\name{variable_name_validated}
\alias{variable_name_validated}
\title{Test validity of variable names}
\usage{
variable_name_validated(var_name = NULL, survey = NULL)
}
\arguments{
  \item{var_name}{character vector}

  \item{survey}{character, the survey where to look at the
  variable name}
}
\value{
\code{TRUE} or \code{FALSE}
}
\description{
This function test whether all variable names are valid and
throw a warning with unmatched names if any.
}
\examples{
\dontrun{
is_variable_name_OK(c("not_valid","person_id"))
}
}
\seealso{
\link{\code{valid_variable_names}}
}

