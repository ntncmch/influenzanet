\name{resolve_missing_still_ill}
\alias{resolve_missing_still_ill}
\title{Resolve NA in still_ill}
\usage{
resolve_missing_still_ill(df_weekly, my_warning = "W_same_S_start_diff_bout",
  delay_in_reporting = 10, debug = FALSE)
}
\arguments{
  \item{df_weekly}{\code{data.frame} containing weekly
  survey data for a single participant.}

  \item{my_warning}{character, warning name}
}
\description{
Resolve missing answer to the question "Are you still ill?"
}

