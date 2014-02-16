\name{variable_dico}
\alias{variable_dico}
\title{Dictionary of flunet variable}
\usage{
variable_dico(rename = NULL, revalue = NULL, is_factor = FALSE,
  is_ordered = FALSE, is_logical = FALSE, is_date = !is_factor &
  !is_logical, order_date = "ymd")
}
\arguments{
  \item{rename}{named character vector, with new names as
  values, and old names as names.}

  \item{revalue}{named character vector, with new values as
  values, and old values as names}

  \item{is_factor}{logical, if \code{TRUE} the variable
  will be considered as a factor}

  \item{is_ordered}{if \code{TRUE} the variable will be
  considered as an ordered factor}

  \item{is_logical}{if \code{TRUE} the variable will be
  considered as logical (\code{TRUE}, \code{FALSE})}

  \item{is_date}{if \code{TRUE} the variable will be
  considered as a date}

  \item{order_date}{date format orders to look for ("ymd"
  by default). See \link{\code{lubridate::guess_formats}}.}
}
\value{
a list
}
\description{
Create a dictionary for a variable with all necessary
information to convert it into a flunet object
}

