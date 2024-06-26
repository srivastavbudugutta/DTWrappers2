% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DTWrapper2.R
\name{dt.format.numerics}
\alias{dt.format.numerics}
\title{dt.format.numerics}
\usage{
dt.format.numerics(
  dt.name,
  digits,
  the.variables = ".",
  the.filter = NULL,
  grouping.variables = NULL,
  add.function.name = FALSE,
  return.as = "result",
  big.mark = "",
  big.interval = 3L,
  small.mark = "",
  small.interval = 5L,
  decimal.mark = getOption("OutDec"),
  input.d.mark = decimal.mark,
  preserve.width = c("common", "individual", "none"),
  envir = parent.frame(),
  ...
)
}
\arguments{
\item{dt.name}{a character value specifying the name of a data.frame or data.table object.}

\item{digits}{the number of digits to round to.  This number will be exact, in that there will be exactly k decimal places listed even if this includes lagging zeros.  For instance, setting k = 5 for x = 2.54 would result in 2.54000}

\item{the.variables}{a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).}

\item{the.filter}{a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.}

\item{grouping.variables}{a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.}

\item{add.function.name}{a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.}

\item{return.as}{describes whether return should be result, code or mixture of both}

\item{big.mark}{big mark}

\item{big.interval}{big.interval}

\item{small.mark}{small mark}

\item{small.interval}{small interval}

\item{decimal.mark}{decimal mark}

\item{input.d.mark}{input d mark}

\item{preserve.width}{preserve width}

\item{envir}{the environment in which the code would be evaluated; parent.frame() by default.}

\item{...}{additional arguments to be passed}
}
\value{
Depending on the value of 'return.as', this function returns different outputs:
- If 'return.as' is "result", it returns a data frame or data table with the specified numeric columns formatted according to the provided parameters. This includes adjustments to decimal places, digit grouping, and the inclusion of specified marks for readability.
- If 'return.as' is "code", it might return the R code or expressions that would result in the formatted data, allowing users to review or execute the formatting commands separately.
- If 'return.as' is a mixture of both or another specified return type, the output may combine both the formatted data and the corresponding R code or expressions.

The function is designed to apply numeric formatting like rounding to a specified number of digits, adding thousand separators, and adjusting decimal marks, according to the parameters provided by the user. The exact nature of the returned object is determined by the function's settings and the input data.
}
\description{
wrapper of the format function that is only applied to numeric inputs
}
