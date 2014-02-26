\name{define_same_bout}
\alias{define_same_bout}
\title{This function identifies reports belonging to the same episode of illness.}
\usage{
define_same_bout(df_weekly, with_symptom = NULL, lag_symptom_start = 2,
  delay_in_reporting = 10, CR_as_TRUE = FALSE,
  my_warning = "W_same_S_start_diff_bout", debug = FALSE, debug_id = NULL)
}
\arguments{
  \item{df_weekly}{\link{data.frame} containing weekly
  survey data}

  \item{my_warning}{character, warning name}

  \item{debug_id}{character, person_id of participant to
  debug}

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

  \item{debug}{logical, if \code{TRUE}: verbose checks}
}
\description{
This function identifies reports belonging to the same
episode of illness.
}
\note{
This function adds a warning when different bouts are
considered to belong to the same episode of illness
}

