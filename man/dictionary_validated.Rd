\name{dictionary_validated}
\alias{dictionary_validated}
\alias{is_valid_dico_survey}
\title{Validate dictionary}
\usage{
is_valid_from_names(x = NULL)

is_valid_to_names(x = NULL, survey = NULL)

is_valid_format(x = NULL)

is_valid_dico_survey(dico = NULL, survey = NULL)

dictionary_validated(dico)
}
\arguments{
  \item{survey}{character, name of the survey}

  \item{dico}{flunet dictionary}
}
\value{
\code{TRUE} if valid, \code{FALSE} otherwise.
}
\description{
These functions check and validate the fields of the
dictionary, the names of the surveys and the names of the
survey variables.
}
\seealso{
\code{\link{create_dictionnary_flusurvey_201213}} for an
example of how to create a flunet dictionary,
\code{\link{variable_dico}} for how to define a variable in
the dictionary.
}

