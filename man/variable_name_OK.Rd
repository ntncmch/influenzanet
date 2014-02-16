\name{variable_name_OK}
\alias{variable_name_OK}
\title{Test validity of variable names}
\usage{
variable_name_OK(var_name = NULL)
}
\arguments{
  \item{var_name}{character vector}
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
variable_name_OK(c("not_valid","person_id"))
}
}
\seealso{
\link{\code{valid_variable_names}}
}

