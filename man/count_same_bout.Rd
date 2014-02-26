\name{count_same_bout}
\alias{count_same_bout}
\title{Group reports by same bout}
\usage{
count_same_bout(df_weekly, CR_as_TRUE = FALSE, give_position = FALSE,
  give_length = FALSE, with_symptom = NULL)
}
\arguments{
  \item{df_weekly}{\code{data.frame} containing weekly
  survey data for a single participant.}

  \item{give_position}{logical, if TRUE the position within
  each bout is returned in the column "position_bout"}

  \item{give_length}{logical, if TRUE the length of each
  bout is returned in the column "length_bout"}

  \item{CR_as_TRUE}{logical, CR (can't remember) is
  replaced by \code{TRUE} when participants are asked
  whether current illness is the same bout as the one
  reported the previous time.}

  \item{with_symptom}{character, name of symptom (e.g.
  ARI_ecdc etc). If present, the cleaning is restricted to
  episodes of illness with}
}
\description{
Group reports by same bout
}
\note{
this function account for multiple report on the same date
by grouping them in the same bout.
}

