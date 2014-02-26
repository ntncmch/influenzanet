\name{clean_weekly_survey}
\alias{clean_weekly_survey}
\title{Clean weekly survey}
\usage{
clean_weekly_survey(x, with_symptom = NULL, lag_symptom_start = 2,
  delay_in_reporting = 10, CR_as_TRUE = FALSE, plot_check = FALSE,
  debug = FALSE)
}
\arguments{
  \item{x}{a \code{flunet} object}

  \item{with_symptom}{character, name of symptom (e.g.
  ARI_ecdc etc). If present, the cleaning is restricted to
  episodes of illness with}

  \item{lag_symptom_start}{numeric, maximum number of days
  between two symptom start dates of different reports.
  Below this threshold, reports are considered to belong to
  the same episode of illness.}

  \item{delay_in_reporting}{maximum number of days to
  report a date of symptom_start and symptom_end.}

  \item{CR_as_TRUE}{logical, CR (can't remember) is
  replaced by \code{TRUE} when participants are asked
  whether current illness is the same bout as the one
  reported the previous time.}

  \item{plot_check}{logical, if \code{TRUE}: plot several
  checks}

  \item{debug}{logical, if \code{TRUE}: verbose checks}
}
\value{
a \code{flunet} object
}
\description{
This function peforms several checks, resolves them when
possible and otherwise flags informative warnings on
corresponding reports.
}
\note{
The option \code{lag_symptom_start} allows us to group
reports belonging to the same bout but having different
symptom start dates. This happens when participant change
their mind from one report to the next.
}

