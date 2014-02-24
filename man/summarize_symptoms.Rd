\name{summarize_symptoms}
\alias{summarize_symptoms}
\title{Summarize symptoms}
\usage{
summarize_symptoms(x, definitions = c("ARI_ecdc", "ILI_ecdc", "ILI_fever"),
  CR_as_TRUE = c("FALSE", "TRUE"), keep_bool = c("FALSE", "TRUE"))
}
\arguments{
  \item{x}{a \code{\link{flunet}} object}

  \item{definitions}{character vector, one or more
  definitions to summarize symptoms}

  \item{CR_as_TRUE}{logical, if \code{TRUE}, answers as
  "can't remember" are considered as "yes", "no" otherwise.
  This is needed to resolve questions like "did your
  symptoms developed over a few hours?".}

  \item{keep_bool}{logical, if \code{TRUE}, all
  intermediate, logical, variables created by the function
  are kept.}
}
\value{
a \code{\link{flunet}} object. The weekly survey contains a
logical column per symptom definitions.
}
\description{
This function summarizes weekly reported symptoms as acute
respiratory infection (ARI) and influenza-lile illness
(ILI). It makes use of the definitions for ARI and ILI
provided by the European Center for Disease Control (ECDC).
}
\note{
the symptoms definitions for ARI and ILI are taken from the
ECDC:
\url{http://ecdc.europa.eu/en/activities/surveillance/eisn/surveillance/pages/influenza_case_definitions.aspx}
}

