#' dt.character.coercion.culprits
#'
#' @description a wrapper function to determine if a character variable that might reasonably be reformatted as numeric


#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param threshold.for.numeric  a value between 0 and 1 specifying the maximum proportion of x that does not "look" numeric, e.g. "2.154" is a character value that can be converted to a numeric value.. If threshold.for.numeric = 0.1, then no more than 10 percent of the values in x can be values that do not "look" numeric.

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).
#'
#'
#' @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.
#'
#' @param grouping.variables a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

#' @param grouping.type a character value specifying whether the grouping should be sorted (keyby) or as is (by).  Defaults to keyby unless "by" is specified.
#'
#' @param add.function.name  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.
#'
#' @param ... additional arguments to be passed
#' @return Returns a data table object resulting from the application of the 'character coercion culprits' analysis on the specified data frame or data table (dt).
#' This function identifies character variables within the specified columns (the.variables) of the data table 'dt.name' that could potentially be converted to numeric based on the specified 'threshold.for.numeric'.
#' It applies the given logical filter (if any) before the analysis and groups the results based on 'grouping.variables' and 'grouping.type' parameters.
#' If 'add.function.name' is TRUE, the name of the function is appended to the column names in the resultant table.
#' The output will contain columns corresponding to the analyzed variables, indicating the proportion of values in each that can potentially be converted to numeric, respecting the specified threshold.
#' @import DTwrappers
#' @import data.table
#'
#' @export



dt.character.coercion.culprits <- function(dt.name, threshold.for.numeric = 0.5, the.variables = ".", the.filter = NULL, grouping.variables = NULL, grouping.type = "keyby", add.function.name = FALSE, ...){
  ##require(DTwrappers)
  other.params <- sprintf("threshold.for.numeric = %s", threshold.for.numeric)
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }



  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables, the.functions = "character.coercion.culprits", the.filter = the.filter, grouping.variables = grouping.variables, grouping.type = grouping.type, add.function.name = add.function.name, other.params = other.params, ...))
}


#' dt.format.numerics
#'
#' @description wrapper of the format function that is only applied to numeric inputs
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object.
#'
#'
#' @param digits the number of digits to round to.  This number will be exact, in that there will be exactly k decimal places listed even if this includes lagging zeros.  For instance, setting k = 5 for x = 2.54 would result in 2.54000

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter  a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param grouping.variables  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

#' @param add.function.name  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.
#'
#' @param return.as describes whether return should be result, code or mixture of both
#'
#' @param big.mark big mark
#'
#' @param big.interval big.interval
#'
#' @param small.mark small mark
#'
#' @param small.interval small interval
#'
#' @param decimal.mark decimal mark
#'
#' @param input.d.mark input d mark
#'
#' @param preserve.width preserve width
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.
#'
#' @param ... additional arguments to be passed
#' @return Depending on the value of 'return.as', this function returns different outputs:
#' - If 'return.as' is "result", it returns a data frame or data table with the specified numeric columns formatted according to the provided parameters. This includes adjustments to decimal places, digit grouping, and the inclusion of specified marks for readability.
#' - If 'return.as' is "code", it might return the R code or expressions that would result in the formatted data, allowing users to review or execute the formatting commands separately.
#' - If 'return.as' is a mixture of both or another specified return type, the output may combine both the formatted data and the corresponding R code or expressions.
#'
#' The function is designed to apply numeric formatting like rounding to a specified number of digits, adding thousand separators, and adjusting decimal marks, according to the parameters provided by the user. The exact nature of the returned object is determined by the function's settings and the input data.

#' @export
#'

# All other inputs:  see help(prettyNum)

dt.format.numerics <- function(dt.name, digits, the.variables = ".", the.filter = NULL, grouping.variables = NULL, add.function.name = FALSE, return.as = "result", big.mark = "", big.interval = 3L, small.mark  = "", small.interval = 5L, decimal.mark = getOption("OutDec"), input.d.mark = decimal.mark, preserve.width = c("common", "individual", "none"), envir = parent.frame(), ...){

  other.params <- sprintf("digits = %d, big.mark = '%s', big.interval = %s, small.mark  = '%s', small.interval = '%s', decimal.mark = '%s', input.d.mark = '%s', preserve.width = c(%s)", digits, big.mark, big.interval, small.mark, small.interval, decimal.mark, input.d.mark, paste(sprintf("'%s'", preserve.width), collapse = ","))
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.functions = "format.numerics", the.variables = the.variables, the.filter = the.filter, grouping.variables = grouping.variables, sortby.group = TRUE, other.params = other.params, table.format = "wide", add.function.name = FALSE, individual.variables = TRUE, return.as = return.as, envir = envir))

}


#' dt.max.numerics
#'
#' @description wrapper function that computes the maximal value for each selected quantitative variable in each group after applying a filter
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param the.variables  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param grouping.variables a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

#' @param add.function.name  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.

#' @param non.numeric.value if "missing", returns NA for variables that are not numeric, integer, logical, or complex.  Otherwise returns first entry of the vector.
#'
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param table.format a character vector soecifying if table should be in a wide format or a tall format
#'
#' @param return.as describes whether return should be result, code or mixture of both
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.
#'
#' @param  ... additional arguments to be passed
#'
#' @param na.rm a logical value specifying whether missing values should be removed from the calculations specified by the.functions.
#' @return The function returns a data frame or data table, depending on the 'return.as' parameter:
#' - If 'return.as' is "result", it returns a modified version of the input data frame or data table with the maximum values computed for the specified numeric variables, after applying any filters and grouping as specified. The function respects the 'na.rm' parameter to handle missing values and the 'non.numeric.value' setting for non-numeric columns.
#' - If 'return.as' is "code", it provides the R code or expressions intended to generate the result, allowing the user to evaluate or inspect the logic separately.
#' - If 'return.as' specifies a mixture or an alternative option, the output may include both the calculated maximum values and the corresponding R code or expressions.
#'
#' The function adapts to the 'table.format' parameter, offering the results in either a wide or tall format, and can also sort the results by group if 'sortby.group' is set to TRUE. The inclusion of the function name in the output column names is controlled by 'add.function.name'.

#' @export


dt.max.numerics <- function(dt.name, the.variables = ".", the.filter = NULL,
                            grouping.variables = NULL, sortby.group = TRUE,
                            table.format = "wide", add.function.name = FALSE,
                            return.as = "result", envir = parent.frame(),
                            na.rm = TRUE, non.numeric.value = "missing", ...){

  other.params <- sprintf("na.rm = %s, non.numeric.value = '%s'", na.rm, non.numeric.value)
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables,
                      the.functions = "max.numerics", the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      other.params = other.params, table.format = table.format,
                      add.function.name = add.function.name, return.as = return.as,
                      envir = envir, ...))
}

#' dt.mean.measured
#'
#' @description Calculates the proportion of measured values for each specified variable in each group after applying a filter.

#' @param  dt.name a character value specifying the name of a data.frame or data.table object.

#' @param the.variables  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter  a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param  grouping.variables a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

#' @param add.function.name a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.
#'
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param table.format a character vector specifying if table should be in a wide format or a tall format
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.
#'
#' @param return.as describes whether return should be result, code or mixture of both
#'
#' @param  ... additional arguments to be passed
#' @return The function returns an object based on the 'return.as' parameter:
#' - If 'return.as' is "result", it outputs a modified version of the input data frame or data table, showing the proportion of measured (non-missing) values for each specified variable, potentially grouped and sorted as defined by the user. The results are presented in the format (wide or tall) specified by the 'table.format' parameter.
#' - If 'return.as' is "code", the function provides the R code or expressions that would generate the aforementioned results, allowing users to evaluate or review the logic independently.
#' - For other values of 'return.as', the output may include both the computed proportions and the corresponding R code, depending on the function's implementation.
#'
#' The function is designed to facilitate an understanding of data completeness, providing insights into the proportion of actual measurements available within the dataset, after any specified subgrouping and filtering.


#' @export

dt.mean.measured <- function(dt.name, the.variables = ".", the.filter = NULL,
                             grouping.variables = NULL, sortby.group = TRUE,
                             table.format = "wide", add.function.name = FALSE,
                             return.as = "result", envir = parent.frame(), ...){
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables,
                      the.functions = "mean.measured", the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      table.format = table.format, add.function.name = add.function.name,
                      return.as = return.as, envir = envir, ...))
}



#' dt.mean.missing
#'
#' @description Calculates the proportion of measured values for each specified variable in each group after applying a filter.
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param grouping.variables a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

#' @param add.function.name a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.
#'
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param table.format a character vector specifying if table should be in a wide format or a tall format
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.
#'
#' @param return.as describes whether return should be result, code or mixture of both
#'
#' @param  ... additional arguments to be passed
#' @return The function outputs an object based on the specified 'return.as' parameter:
#' - If 'return.as' is "result", it returns a data frame or data table modified to include the proportion of missing values for each specified variable, after the data has been filtered and potentially grouped according to the parameters provided. The results are formatted according to the 'table.format' parameter, in either wide or tall form, and can be sorted by groups if 'sortby.group' is enabled.
#' - If 'return.as' is "code", the function will return the R code or expressions designed to calculate these proportions, allowing the user to review or execute the code independently.
#' - If 'return.as' encompasses other values, the output may combine the computed results and the R code, varying with the function's implementation and user specifications.
#'
#' This function is tailored for analyzing data completeness, specifically by quantifying the missingness in the dataset, facilitating detailed examination and understanding of the data's integrity, especially after applying any specified filters and groupings.

#' @export

dt.mean.missing <- function(dt.name, the.variables = ".", the.filter = NULL,
                            grouping.variables = NULL, sortby.group = TRUE,
                            table.format = "wide", add.function.name = FALSE,
                            return.as = "result", envir = parent.frame(), ...){
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables,
                      the.functions = "mean.missing", the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      table.format = table.format, add.function.name = add.function.name,
                      return.as = return.as, envir = envir, ...))
}




#' dt.mean.numerics
#'
#' @description wrapper function that computes the mean value for each selected quantitative variable in each group after applying a filter.

#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param grouping.variables  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

#'
#' @param na.rm a logical value specifying whether missing values should be removed from the calculations specified by the.functions.
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.

#' @param add.function.name a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.

#' @param non.numeric.value if "missing", returns NA for variables that are not numeric, integer, logical, or complex.  Otherwise returns first entry of the vector.
#'
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param table.format a character vector specifying if table should be in a wide format or a tall format
#'
#' @param return.as describes whether return should be result, code or mixture of both
#'
#' @param  ... additional arguments to be passed
#' @return The function returns an object determined by the 'return.as' parameter:
#' - If 'return.as' is "result", it outputs a modified version of the input data frame or data table, presenting the mean values for each selected numeric variable, adjusted for any applied filters and groupings. The results are formatted according to the user's preference (wide or tall format) and can incorporate sorting by groups if specified.
#' - If 'return.as' is "code", it provides the R code or expressions that would result in the calculation of these means, which allows the user to review or manually execute the code.
#' - If 'return.as' includes other values, the output might combine both the calculated means and the R code, depending on the function's implementation.
#'
#' The function efficiently aggregates the mean values, considering the handling of missing values as specified by 'na.rm' and adjusting for non-numeric values based on 'non.numeric.value'. This enables a detailed analysis of the dataset's quantitative aspects, especially after subgrouping and applying specific filters.

#' @export

dt.mean.numerics <- function(dt.name, the.variables = ".", the.filter = NULL,
                             grouping.variables = NULL, sortby.group = TRUE,
                             table.format = "wide", add.function.name = FALSE,
                             return.as = "result", envir = parent.frame(),
                             na.rm = TRUE, non.numeric.value = "missing", ...){

  other.params <- sprintf("na.rm = %s, non.numeric.value = '%s'", na.rm, non.numeric.value)
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables,
                      the.functions = "mean.numerics", the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      other.params = other.params, table.format = table.format,
                      add.function.name = add.function.name, return.as = return.as,
                      envir = envir, ...))
}



#' dt.median.numerics
#'
#' @description wrapper function that computes the median value for each selected quantitative variable in each group after applying a filter.
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param  the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param  grouping.variables a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

#' @param add.function.name  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.

#' @param non.numeric.value if "missing", returns NA for variables that are not numeric, integer, logical, or complex.  Otherwise returns first entry of the vector.
#'
#'@param  na.rm a logical value specifying whether missing values should be removed from the calculations specified by the.functions.
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.
#'
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param table.format a character vector specifying if table should be in a wide format or a tall format
#'
#' @param return.as describes whether return should be result, code or mixture of both
#'
#' @param  ... additional arguments to be passed
#' @return The function returns an output based on the 'return.as' parameter:
#' - If 'return.as' is "result", it provides a modified version of the input data frame or data table, showing the median values for the specified numeric variables, after applying the set filters and groupings. The data is presented in the format specified by 'table.format', either wide or tall, and it reflects any sorting by group as dictated by 'sortby.group'.
#' - If 'return.as' is "code", the function will return the R code or expressions that would generate the calculated medians, offering users the opportunity to inspect or execute the code separately.
#' - If 'return.as' incorporates other options, the output may consist of both the calculated medians and the R code, varying according to the function’s implementation.
#'
#' The function aims to aggregate median values, taking into account the handling of missing values as specified by 'na.rm' and adapting for non-numeric values as determined by 'non.numeric.value'. This functionality allows for a nuanced analysis of the central tendency within the dataset, particularly after subgrouping and applying specified filters.

#' @export

dt.median.numerics <- function(dt.name, the.variables = ".", the.filter = NULL,
                               grouping.variables = NULL, sortby.group = TRUE,
                               table.format = "wide", add.function.name = FALSE,
                               return.as = "result", envir = parent.frame(),
                               na.rm = TRUE, non.numeric.value = "missing", ...){

  other.params <- sprintf("na.rm = %s, non.numeric.value = '%s'", na.rm, non.numeric.value)
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables,
                      the.functions = "median.numerics", the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      other.params = other.params, table.format = table.format,
                      add.function.name = add.function.name, return.as = return.as,
                      envir = envir, ...))
}




#' dt.min.numerics
#'
#' @description wrapper function that computes the minimal value for each selected quantitative variable in each group after applying a filter.
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param grouping.variables  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.
#'
#' @param na.rm a logical value specifying whether missing values should be removed from the calculations specified by the.functions.

#' @param add.function.name a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.

#' @param non.numeric.value  if "missing", returns NA for variables that are not numeric, integer, logical, or complex.  Otherwise returns first entry of the vector.
#'
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param table.format a character vector specifying if table should be in a wide format or a tall format
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.
#'
#' @param return.as describes whether return should be result, code or mixture of both
#'
#' @param  ... additional arguments to be passed
#' @return The function's output depends on the 'return.as' parameter:
#' - If 'return.as' is "result", it generates a modified version of the input data frame or data table, detailing the minimum values for each specified numeric variable, adjusted for any applied filters and groupings. The data is structured according to the specified 'table.format' (wide or tall) and reflects sorting by group if 'sortby.group' is enabled.
#' - If 'return.as' is "code", the function returns the R code or expressions that would compute these minimum values, giving the user the ability to inspect or execute the code separately.
#' - For other values of 'return.as', the function may produce both the calculated minimum values and the R code, depending on the function’s specifics.
#'
#' This function is designed to compute and aggregate minimum values, accounting for the handling of missing data as dictated by 'na.rm' and addressing non-numeric values as per 'non.numeric.value'. It provides insightful analysis into the lower bounds of the dataset’s quantitative variables, particularly after implementing specified subgroupings and filters.

#' @export

dt.min.numerics <- function(dt.name, the.variables = ".", the.filter = NULL,
                            grouping.variables = NULL, sortby.group = TRUE,
                            table.format = "wide", add.function.name = FALSE,
                            return.as = "result", envir = parent.frame(),
                            na.rm = TRUE, non.numeric.value = "missing", ...){

  other.params <- sprintf("na.rm = %s, non.numeric.value = '%s'", na.rm, non.numeric.value)
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables,
                      the.functions = "min.numerics", the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      other.params = other.params, table.format = table.format,
                      add.function.name = add.function.name, return.as = return.as,
                      envir = envir, ...))
}


#' dt.quantile.numerics
#'
#' @description wrapper function that computes the quantiles for each selected quantitative variable in each group after applying a filter.
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param probs the range specifying the upper and lower quartiles
#'
#' @param grouping.variables  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.

#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param table.format a character vector specifying if table should be in a wide format or a tall format
#'
#' @param return.as describes whether return should be result, code or mixture of both
#'
#' @param  ... additional arguments to be passed
#' @return The function outputs depend on the specified 'return.as' parameter:
#' - If 'return.as' is "result", it returns a modified version of the input data frame or data table, including the calculated quantiles for each specified numeric variable. These quantiles are computed based on the provided 'probs' array, after applying any specified filters and subgroupings. The results are structured according to the 'table.format', which can be wide or long, reflecting any group-based sorting if 'sortby.group' is enabled.
#' - If 'return.as' is "code", the function will return the R code or expressions capable of computing the quantiles, allowing users to review or execute the calculations separately.
#' - For other specified values of 'return.as', the function may provide both the quantile calculations and the corresponding R code, depending on how the function is implemented.
#'
#' This function facilitates a comprehensive analysis of the data's distribution by calculating specific quantiles, aiding in the statistical examination of the dataset's quantitative attributes, particularly after implementing specific filters and groupings.

#' @export



dt.quantile.numerics <- function(dt.name, the.variables = ".", probs = c(0.25, 0.75),
                                 the.filter = NULL, grouping.variables = NULL,
                                 sortby.group = TRUE, table.format = "long",
                                 return.as = "result", envir = parent.frame(), ...){

  other.params = sprintf("probs = c(%s)", paste(sort(unique(probs)), collapse = ", "))
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.functions = "quantile.numerics",
                      the.variables = the.variables, the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      other.params = other.params, table.format = table.format,
                      add.function.name = FALSE, individual.variables = TRUE,
                      output.as.table = TRUE, return.as = return.as, envir = envir))
}



#' dt.remove.erroneous.characters
#'
#' @description removes erroneous characters
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param threshold.for.numeric  a value between 0 and 1 specifying the maximum proportion of x that does not "look" numeric, e.g. "2.154" is a character value that can be converted to a numeric value.. If threshold.for.numeric = 0.1, then no more than 10 percent of the values in x can be values that do not "look" numeric.

#' @param the.variables  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.
#'
#' @param variable.should.be a character vector specifying whether variable should be numeric or text or something else
#'
#' @param value.for.missing a character value, logical value, or expression stating the logical operations to be performed in stating the missing value
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.
#'
#' @param return.as describes whether return should be result, code or mixture of both
#'
#' @param  ... additional arguments to be passed
#' @return The output of the function is contingent upon the 'return.as' parameter:
#' - If 'return.as' is "result", it returns a modified version of the input data frame or data table where erroneous characters in the specified variables are removed based on the given threshold. The function ensures that only values fitting the specified 'variable.should.be' criteria are retained, converting or imputing others as necessary, guided by the 'threshold.for.numeric' and 'value.for.missing' parameters.
#' - If 'return.as' is "code", it provides the R code or expressions that would perform this character removal and data cleaning, allowing users to review or manually execute the modifications.
#' - If another option is specified for 'return.as', the output may include both the cleaned data and the corresponding R code, depending on the specifics of the function’s implementation.
#'
#' The function aims to sanitize data by eliminating or correcting values in specified variables that do not conform to the expected numeric or text formats, thereby enhancing data quality and consistency.

#' @export

dt.remove.erroneous.characters <- function(dt.name, threshold.for.numeric = 0.8,
                                           the.variables = ".", the.filter = NULL,
                                           variable.should.be = "numeric",
                                           value.for.missing = NULL, return.as = "result",
                                           envir = parent.frame(), ...){

  if(is.null(value.for.missing)){
    value.for.missing <- "NULL"
  }
  other.params <- sprintf("threshold.for.numeric = %s, variable.should.be = '%s', value.for.missing = %s",
                          threshold.for.numeric, variable.should.be, value.for.missing)
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.functions = "remove.erroneous.characters",
                      the.variables = the.variables, the.filter = the.filter,
                      grouping.variables = NULL, sortby.group = TRUE, other.params = other.params,
                      table.format = "wide", add.function.name = FALSE, individual.variables = TRUE,
                      output.as.table = TRUE, return.as = return.as, envir = envir))
}



#' dt.round.exactly
#'
#' @description rounds the number to exactly desired decimals
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param digits  the number of digits to round to.  This number will be exact, in that there will be exactly k decimal places listed even if this includes lagging zeros.  For instance, setting k = 5 for x = 2.54 would result in 2.54000

#' @param decimal  The character specifying the decimal, which splits between whole numbers (greater than 1) and the fractional component (less than 1).

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter  a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.
#'
#' @param return.as describes whether return should be result, code or mixture of both
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.
#'
#' @param  ... additional arguments to be passed
#' @return The function's output depends on the 'return.as' parameter:
#' - If 'return.as' is "result", it returns a modified version of the input data frame or data table with the specified variables rounded to the exact number of decimal places as defined by 'digits'. The rounding is performed even if this results in trailing zeros, ensuring that each value has precisely the specified number of decimal places.
#' - If 'return.as' is "code", the function provides the R code or expressions that would perform this precise rounding, allowing users to review or manually execute the code as needed.
#' - If 'return.as' specifies another option, the output might include both the rounded data and the corresponding R code, depending on the function's implementation.
#'
#' The function facilitates precise numerical formatting, enhancing data readability and consistency, especially useful in scenarios where a specific decimal precision is required for subsequent analyses or reporting.

#' @export

dt.round.exactly <- function(dt.name, the.variables = ".", digits = 0,
                             the.filter = NULL, decimal = ".", return.as = "result",
                             envir = parent.frame(), ...){

  other.params <- sprintf("digits = %d, decimal = '%s'", digits, decimal)
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.functions = "round_exactly",
                      the.variables = the.variables, the.filter = the.filter,
                      other.params = other.params, add.function.name = FALSE,
                      table.format = "wide", return.as = return.as, envir = envir))
}



#' dt.round.numerics
#'
#' @description rounds the number to desired decimal digits
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param digits  the number of digits to round to.  This number will be exact, in that there will be exactly k decimal places listed even if this includes lagging zeros.  For instance, setting k = 5 for x = 2.54 would result in 2.54000

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param grouping.variables  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

#' @param envir the environment in which the code would be evaluated; parent.frame() by default.
#'
#' @param  ... additional arguments to be passed
#'

#' @param add.function.name  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#'
#' @param return.as describes whether return should be result, code or mixture of both
#' @return The function's output varies based on the 'return.as' parameter:
#' - If 'return.as' is "result", it returns the input data frame or data table with specified numeric variables rounded to the defined number of decimal places. The rounding is applied even if it results in trailing zeros, ensuring a consistent number of decimal places across the data.
#' - If 'return.as' is "code", the function provides the R code or expressions designed to perform this rounding, allowing users to examine or execute the rounding logic independently.
#' - If another option is specified for 'return.as', the output may include both the rounded data and the corresponding R code, varying with the function’s setup.
#'
#' The function is crafted to modify numerical data by rounding it to a specified number of decimal places, aiding in data standardization and precision control, especially beneficial for detailed numerical analysis or reporting.

#' @export

dt.round.numerics <- function(dt.name, digits, the.variables = ".", the.filter = NULL,
                              grouping.variables = NULL, sortby.group = TRUE,
                              add.function.name = FALSE, return.as = "result",
                              envir = parent.frame(), ...){

  other.params <- sprintf("digits = %d", floor(digits))
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables,
                      the.functions = "round.numerics", the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      other.params = other.params, table.format = "wide",
                      add.function.name = add.function.name, return.as = return.as, envir = envir))
}


#' dt.sd.numerics
#'
#' @description wrapper function that computes the standard deviation for each selected quantitative variable in each group after applying a filter.

#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter  a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param grouping.variables a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

#'
#'@param na.rm a logical value specifying whether missing values should be removed from the calculations specified by the.functions.

#' @param add.function.name  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.
#'
#' @param  ... additional arguments to be passed
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.

#' @param non.numeric.value if "missing", returns NA for variables that are not numeric, integer, logical, or complex.  Otherwise returns first entry of the vector.
#'
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param table.format a character vector specifying if table should be in a wide format or a tall format
#'
#' @param return.as describes whether return should be result, code or mixture of both
#' @return The output of the function varies based on the 'return.as' parameter:
#' - If 'return.as' is "result", it provides a modified version of the input data frame or data table, incorporating the standard deviation values for the specified numeric variables, computed post any filtering and grouping operations. These standard deviations are calculated considering the 'na.rm' parameter, which dictates the handling of missing values.
#' - If 'return.as' is "code", the function returns the R code or expressions that would compute the standard deviations, allowing the user to inspect or run the calculations separately.
#' - If 'return.as' is set to another value, the output might include both the computed standard deviations and the R code, depending on the function's design.
#'
#' This function is designed to provide a statistical summary, particularly the standard deviation, which is a measure of the amount of variation or dispersion of a set of values. It's particularly useful for data analysis and understanding the variability of the dataset.

#' @export

dt.sd.numerics <- function(dt.name, the.variables = ".", the.filter = NULL,
                           grouping.variables = NULL, sortby.group = TRUE,
                           table.format = "wide", add.function.name = FALSE,
                           return.as = "result", envir = parent.frame(),
                           na.rm = TRUE, non.numeric.value = "missing", ...){

  other.params <- sprintf("na.rm = %s, non.numeric.value = '%s'", na.rm, non.numeric.value)
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables,
                      the.functions = "sd.numerics", the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      other.params = other.params, table.format = table.format,
                      add.function.name = add.function.name, return.as = return.as,
                      envir = envir, ...))
}


#' dt.summarize
#'
#' @description summarizes the dataset
#'
#' @param dt.name a character value specifying the name of a data.frame or data.table object.
#'
#' @param the.variables  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.functions  a character vector or list specifying the name of the function to apply to the variables.  This may either be specified by the name of the function as a character (e.g. "mean") or by defining a function;

#' @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param grouping.variables a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.

#' @param add.function.name  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.

#'
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param  ... additional arguments to be passed
#'
#' @param other.params additional parameters to be passed
#'
#' @param table.format a character vector specifying if table should be in a wide format or a tall format
#'
#' @param return.as describes whether return should be result, code or mixture of both
#' @return The output of the function is determined by the 'return.as' parameter:
#' - If 'return.as' is "result", it returns a data frame or data table that summarizes the specified variables using the functions listed in 'the.functions'. The summary might include statistics like minimum, maximum, mean, median, standard deviation, and other specified measures, applied after any set filtering and grouping.
#' - If 'return.as' is "code", the function will return the R code or expressions that generate the summary, allowing users to inspect or execute the code independently.
#' - If 'return.as' specifies a different option, the output may include both the summary statistics and the corresponding R code, varying with the function's implementation.
#'
#' This function is intended to provide a comprehensive summary of the dataset, offering insights into each selected variable's distribution and central tendencies, facilitating a thorough understanding of the dataset's characteristics.

#' @export

dt.summarize <- function(dt.name,
                         the.functions = c("min", "lower.quartile", "median", "mean", "upper.quartile", "max", "sd", "num.records", "total.missing"),
                         the.variables = ".",
                         the.filter = NULL,
                         grouping.variables = NULL,
                         sortby.group = TRUE,
                         other.params = "",
                         table.format = "long",
                         add.function.name = TRUE,
                         return.as = "result",
                         envir = parent.frame(),
                         ...){
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.functions = the.functions, the.variables = the.variables, the.filter = the.filter, grouping.variables = grouping.variables, sortby.group = sortby.group, other.params = other.params, table.format = table.format, add.function.name = add.function.name, return.as = return.as, envir = envir, ...))
}






#' dt.total.measured
#'
#' @description Calculates the proportion of measured values for each specified variable in each group after applying a filter.


#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter  a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param grouping.variables  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.

#' @param add.function.name  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.
#'
#' @param  ... additional arguments to be passed
#'
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param table.format a character vector soecifying if table should be in a wide format or a tall format
#'
#' @param return.as describes whether return should be result, code or mixture of both
#' @return The function's output varies based on the 'return.as' parameter:
#' - If 'return.as' is "result", it returns a data frame or data table with the calculated proportions of measured (non-missing) values for each specified variable, adjusted for any applied filtering and subgrouping. The data is formatted as specified, in either a wide or tall format, depending on the 'table.format' setting.
#' - If 'return.as' is "code", it provides the R code or expressions intended to generate these calculations, allowing the user to review or execute the code independently.
#' - If another option is specified for 'return.as', the output may include both the calculated proportions and the corresponding R code, contingent on the function’s design.
#'
#' This function is particularly useful for assessing data completeness, providing insights into the proportion of actual measurements available within the dataset, especially after subgrouping and applying specific filters.

#' @export

dt.total.measured <- function(dt.name, the.variables = ".", the.filter = NULL,
                              grouping.variables = NULL, sortby.group = TRUE,
                              table.format = "wide", add.function.name = FALSE,
                              return.as = "result", envir = parent.frame(), ...){
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables,
                      the.functions = "total.measured", the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      table.format = table.format, add.function.name = add.function.name,
                      return.as = return.as, envir = envir, ...))
}



#' dt.total.missing
#'
#' @description Calculates the proportion of missing values for each specified variable in each group after applying a filter.


#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param the.variables  a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter  a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param grouping.variables  a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.

#' @param add.function.name  a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.
#'
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param table.format a character vector soecifying if table should be in a wide format or a tall format
#'
#' @param  ... additional arguments to be passed
#'
#' @param return.as describes whether return should be result, code or mixture of both
#' @return The function's output is determined by the 'return.as' parameter:
#' - If 'return.as' is "result", it provides a modified data frame or data table displaying the proportions of missing values for each specified variable. These calculations are performed after considering any filters applied and subgrouping defined, presented in the format specified by 'table.format'.
#' - If 'return.as' is "code", the function outputs the R code or expressions designed to calculate these proportions, allowing users to inspect or execute the code independently.
#' - If a different value is specified for 'return.as', the output might include both the calculated proportions of missing values and the corresponding R code, depending on the specifics of the function's implementation.
#'
#' This function is essential for data quality assessment, offering insights into the extent of missing data within each variable of the dataset, especially useful after applying subgrouping and specific filters.

#' @export


dt.total.missing <- function(dt.name, the.variables = ".", the.filter = NULL,
                             grouping.variables = NULL, sortby.group = TRUE,
                             table.format = "wide", add.function.name = FALSE,
                             return.as = "result", envir = parent.frame(), ...){
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables,
                      the.functions = "total.missing", the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      table.format = table.format, add.function.name = add.function.name,
                      return.as = return.as, envir = envir, ...))
}


#' dt.trimws.character
#'
#' @description wrapper function


#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).

#' @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.

#' @param grouping.variables a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

#' @param add.function.name a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.
#'
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param  ... additional arguments to be passed
#'
#' @param table.format a character vector specifying if table should be in a wide format or a tall format
#'
#' @param which both, left or right
#'
#' @param whitespace encoded whitespace
#'
#' @param convert.factor logical value specifying if variable needs to be converted to a factor before calculations
#'
#' @param return.as describes whether return should be result, code or mixture of both
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.
#' @return The function's output varies based on the 'return.as' parameter:
#' - If 'return.as' is "result", it returns a modified version of the input data frame or data table with specified variables having their leading, trailing, or both types of whitespace trimmed according to the 'which' parameter. This is performed after any set filtering and subgrouping, with options for trimming specified by 'whitespace' and 'convert.factor'.
#' - If 'return.as' is "code", it provides the R code or expressions designed to execute this whitespace trimming, allowing users to review or implement the modifications independently.
#' - If another value is specified for 'return.as', the output might include both the adjusted data and the corresponding R code, depending on the function’s configuration.
#'
#' This function is crucial for cleaning character data, ensuring consistency, and preparing data for analysis by removing unwanted whitespace from the specified variables.

#' @export

dt.trimws.character <- function(dt.name, the.variables = ".", the.filter = NULL,
                                grouping.variables = NULL, sortby.group = TRUE,
                                table.format = "wide", add.function.name = FALSE,
                                return.as = "result", envir = parent.frame(),
                                which = c("both", "left", "right"),
                                whitespace = "[ \t\r\n]",
                                convert.factor = FALSE, ...){

  value.which <- NULL
  if(is.character(which) && which[1] != "both"){
    value.which <- sprintf("which = c('%s')", paste(which))
  }

  value.whitespace <- NULL
  if(is.character(whitespace) && whitespace[1] != "[ \t\r\n]"){
    value.whitespace <- as.character(whitespace[1])
  }

  value.convert.factor <- NULL
  if(convert.factor == TRUE){
    value.convert.factor <- "convert.factor = TRUE"
  }

  other.params <- paste(value.which, value.whitespace, value.convert.factor, collapse = ", ")
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables,
                      the.functions = "trimws.character", the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      other.params = other.params, table.format = table.format,
                      add.function.name = add.function.name, return.as = return.as, envir = envir, ...))
}



#' dt.var.numerics

#' @description wrapper function that computes the variance for each selected quantitative variable in each group after applying a filter


#' @param dt.name a character value specifying the name of a data.frame or data.table object.

#' @param the.variables a character vector specifying the variables that we want to apply a function to.  Only values that exist in names(dat) will be used; other values in the.variables will be excluded from the calculation.  When the.variables includes ".", then all values in names(dat) will be selected.  Values of the.variables that also exist in grouping.variables will be excluded from the.variables (but grouped by these values).
#'
#' @param grouping.variables a character vector specifying variables to group by in performing the computation.  Only values that exist in names(dat) will be used.

#' @param add.function.name a logical value specifying whether the name of the function applied should be appended to the column names in the resulting table.
#'
#' @param na.rm a logical value specifying whether missing values should be removed from the calculations specified by the.functions.
#'
#' @param non.numeric.value if "missing", returns NA for variables that are not numeric, integer, logical, or complex.  Otherwise returns first entry of the vector.
#'
#' @param the.filter a character value, logical value, or expression stating the logical operations to be performed in filtering the data prior to calculating the.function.
#'
#' @param envir the environment in which the code would be evaluated; parent.frame() by default.

#'
#' @param sortby.group a logical value to specify if the sorting functionality needs to be applied or not
#'
#' @param table.format a character vector specifying if table should be in a wide format or a tall format
#'
#' @param return.as describes whether return should be result, code or mixture of both
#'
#' @param  ... additional arguments to be passed
#' @return The function's output varies based on the 'return.as' parameter:
#' - If 'return.as' is "result", it returns a modified data frame or data table that includes the variance calculations for each specified quantitative variable, adjusted post any filtering and subgrouping. The variance is computed considering the 'na.rm' parameter to handle missing values and is presented in the specified 'table.format'.
#' - If 'return.as' is "code", it provides the R code or expressions designed to perform these variance computations, allowing users to inspect or execute the code independently.
#' - If 'return.as' specifies another option, the output may include both the calculated variance values and the corresponding R code, depending on the function's setup.
#'
#' This function is particularly valuable for statistical analysis, providing insights into the variability of each variable within the dataset, which is crucial for understanding the data distribution and variability after subgrouping and applying specific filters.

#' @export

dt.var.numerics <- function(dt.name, the.variables = ".", the.filter = NULL,
                            grouping.variables = NULL, sortby.group = TRUE,
                            table.format = "wide", add.function.name = FALSE,
                            return.as = "result", envir = parent.frame(),
                            na.rm = TRUE, non.numeric.value = "missing", ...){

  other.params <- sprintf("na.rm = %s, non.numeric.value = '%s'", na.rm, non.numeric.value)
  if (!requireNamespace("DTwrappers", quietly = TRUE)) {
    stop("DTwrappers package is required but not installed.")
  }


  return(DTwrappers::dt.calculate(dt.name = dt.name, the.variables = the.variables,
                      the.functions = "var.numerics", the.filter = the.filter,
                      grouping.variables = grouping.variables, sortby.group = sortby.group,
                      other.params = other.params, table.format = table.format,
                      add.function.name = add.function.name, return.as = return.as, envir = envir, ...))
}



#' character.coercion.culprits

#' This function identifies which character values in a vector are preventing it from being converted to a numeric vector.
#'
#' @param x A character vector of values that should be a numeric vector but was coerced to a character due to a small number of non-numeric entries.
#' @param threshold.for.numeric A value between 0 and 1 specifying the maximum proportion of x that does not "look" numeric. If threshold.for.numeric = 0.1, then no more than 10 percentage of the values in x can be values that do not "look" numeric.
#' @param ... Additional arguments.
#' @return A character vector of values that are preventing x from being converted to numeric. Returns NA if the proportion of non-numeric values exceeds the threshold.
#' @importFrom stats mean
#' @export

character.coercion.culprits <- function(x, threshold.for.numeric = 0.5, ...) {
  w1 <- which(is.na(x))

  # Using suppressWarnings to locally suppress warnings generated by as.numeric
  y <- suppressWarnings(as.numeric(x))
  if (mean(is.na(y)) > threshold.for.numeric) {
    return(NA)
  }

  w2 <- which(is.na(y))

  the.indices <- w2[!(w2 %in% w1)]
  the.culprits <- unique(x[the.indices])
  return(the.culprits)
}


#' This function formats numeric values with specified rounding and marking options.
#'
#' @param x A numeric, integer, or logical vector to be formatted.
#' @param digits The number of digits to round to. Defaults to 0.
#' @param big.mark A character string used as a mark for thousands. Defaults to "".
#' @param big.interval An integer specifying the interval for the big mark. Defaults to 3.
#' @param small.mark A character string used as a mark for small intervals. Defaults to "".
#' @param small.interval An integer specifying the interval for the small mark. Defaults to 5.
#' @param decimal.mark A character string used as a decimal mark. Defaults to the value of getOption("OutDec").
#' @param input.d.mark The decimal mark to be used for input. Defaults to the value of \code{decimal.mark}.
#' @param preserve.width A character string specifying how to preserve the width of the output. Can be "common", "individual", or "none". Defaults to "common".
#' @param zero.print A character string to be used for printing zero. Defaults to NULL.
#' @param replace.zero A logical value indicating whether to replace zeros with the \code{zero.print} value. Defaults to FALSE.
#' @param drop0trailing A logical value indicating whether to drop trailing zeros. Defaults to FALSE.
#' @param is.cmplx A logical value indicating whether the input is complex. Defaults to NA.
#' @param ... Additional arguments passed to prettyNum.
#' @return A character vector with formatted numeric values.
#' @export format.numerics
#' @export

format.numerics <- function(x, digits = 0, big.mark = "",   big.interval = 3L, small.mark  = "", small.interval = 5L, decimal.mark = getOption("OutDec"), input.d.mark = decimal.mark, preserve.width = c("common", "individual", "none"), zero.print = NULL, replace.zero = FALSE, drop0trailing = FALSE, is.cmplx = NA, ...){
  if(is.numeric(x) | is.integer(x) | is.logical(x)){
    x <- prettyNum(x = round(x = x, digits = digits), big.mark = big.mark, big.interval = big.interval, small.mark  = small.mark, small.interval = small.interval, decimal.mark = decimal.mark, input.d.mark = input.d.mark, preserve.width = preserve.width, zero.print = zero.print, replace.zero = replace.zero, drop0trailing = drop0trailing, is.cmplx = is.cmplx, ...)
  }
  return(x)
}


#' lower.quartile
#'
#' @description value that cuts off the first 25% of the data when it is sorted in ascending order.
#'
#' @param x a vector
#'
#' @param na.rm a logical value specifying whether missing values should be removed from the calculations specified by the.functions.
#'
#' @param ... additional arguments to be passed
#' @return Returns a numeric value representing the lower quartile (25th percentile) of the given vector 'x'.
#' The lower quartile is the value below which 25% of the data points in the sorted vector fall.
#' If 'na.rm' is TRUE, any missing values (NA) are removed before the calculation.
#' The function utilizes the 'quantile' function internally to compute this statistic, and any additional arguments provided (...) are passed along to this underlying function.
#' @export

# x:  a vector

# na.rm:  a logical value specifying whether missing values should be removed from the calculations specified by the.functions.

lower.quartile <- function(x, na.rm = TRUE, ...){
  return(as.numeric(quantile(x = x, probs = 0.25, na.rm = na.rm)))
}


#' max.numerics
#'
#' This function computes the maximal value of a numeric, integer, logical, or complex vector. If the vector is not one of these types, the function returns either NA or the first entry of the vector.
#'
#' @param x A vector.
#' @param na.rm A logical value specifying whether missing values should be removed from the calculations specified by the function. Defaults to TRUE.
#' @param non.numeric.value If "missing", returns NA for variables that are not numeric, integer, logical, or complex. Otherwise, returns the first entry of the vector. Defaults to "missing".
#' @param ... Additional arguments .
#' @return If x is numeric, integer, logical, or complex, the maximal value will be computed. Otherwise, the first value of x will be returned untouched or NA based on non.numeric.value.
#' @export max.numerics
#' @export

max.numerics <- function(x, na.rm = TRUE, non.numeric.value = "missing", ...){
  if(is.numeric(x) | is.complex(x)){
    return(max(x = x, na.rm = na.rm))
  }
  if(non.numeric.value == "missing"){
    return(NA)
  }
  return(x[1])
}


#' mean.measured
#'
#' This function calculates the proportion of non-NA values in a vector.
#'
#' @param x A vector.
#' @param ... Additional arguments.
#' @return A numeric value representing the proportion of non-NA values in the vector.
#' @importFrom stats mean
#' @export mean.measured
#' @export

mean.measured <- function(x, ...){
  return(mean(!is.na(x)))
}


#' Calculate the Proportion of NA Values
#'
#' This function calculates the proportion of NA values in a vector.
#'
#' @param x A vector.
#' @param ... Additional arguments.
#' @return A numeric value representing the proportion of NA values in the vector x.
#' @importFrom stats mean
#' @export mean.missing
#' @export

mean.missing <- function(x, ...){
  return(mean(is.na(x)))
}


#' mean.numerics
#'
#' This function computes the mean value of a numeric, integer, logical, or complex vector. If the vector is not one of these types, the function returns either NA or the first entry of the vector.
#'
#' @param x A vector.
#' @param na.rm A logical value specifying whether missing values should be removed from the calculations specified by the function. Defaults to TRUE.
#' @param non.numeric.value If "missing", returns NA for variables that are not numeric, integer, logical, or complex. Otherwise, returns the first entry of the vector. Defaults to "missing".
#' @param ... Additional arguments.
#' @return If x is numeric, integer, logical, or complex, the mean value will be computed. Otherwise, the first value of x will be returned untouched or NA based on non.numeric.value.
#' @export mean.numerics
#' @export

mean.numerics <- function(x, na.rm = TRUE, non.numeric.value = "missing", ...){


  if(is.numeric(x) | is.complex(x)){
    return(mean(x = x, na.rm = na.rm))
  }
  if(non.numeric.value == "missing"){
    return(NA)
  }
  return(x[1])
}


#' median.numerics
#'
#' This function computes the median value of a numeric, integer, logical, or complex vector. If the vector is not one of these types, the function returns either NA or the first entry of the vector.
#'
#' @param x A vector.
#' @param na.rm A logical value specifying whether missing values should be removed from the calculations specified by the function. Defaults to TRUE.
#' @param non.numeric.value If "missing", returns NA for variables that are not numeric, integer, logical, or complex. Otherwise, returns the first entry of the vector. Defaults to "missing".
#' @param ... Additional arguments .
#' @return If x is numeric, integer, logical, or complex, the median value will be computed. Otherwise, the first value of x will be returned untouched or NA based on non.numeric.value.
#' @importFrom stats median
#' @export median.numerics
#' @export

median.numerics <- function(x, na.rm = TRUE, non.numeric.value = "missing", ...){

  if(is.numeric(x) | is.complex(x)){
    return(median(x = x, na.rm = na.rm))
  }
  if(non.numeric.value == "missing"){
    return(NA)
  }
  return(x[1])
}


#' min.numerics
#'
#' This function computes the minimal value of a numeric, integer, logical, or complex vector. If the vector is not one of these types, the function returns either NA or the first entry of the vector.
#'
#' @param x A vector.
#' @param na.rm A logical value specifying whether missing values should be removed from the calculations specified by the function. Defaults to TRUE.
#' @param non.numeric.value If "missing", returns NA for variables that are not numeric, integer, logical, or complex. Otherwise, returns the first entry of the vector. Defaults to "missing".
#' @param ... Additional arguments .
#' @return If x is numeric, integer, logical, or complex, the minimal value will be computed. Otherwise, the first value of x will be returned untouched or NA based on non.numeric.value.
#' @export min.numerics
#' @export

min.numerics <- function(x, na.rm = TRUE, non.numeric.value = "missing", ...){
  if(is.numeric(x) | is.complex(x)){
    return(min(x = x, na.rm = na.rm))
  }
  if(non.numeric.value == "missing"){
    return(NA)
  }
  return(x[1])
}

#' This internal function calculates the quantiles for a numeric or complex vector. If the input vector is not numeric or complex, it returns a vector of NA of the same length as the provided probabilities.
#'
#' @param x A vector to calculate quantiles for.
#' @param probs A numeric vector of probabilities with values in [0,1].
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds. Defaults toTRUE.
#' @return A vector of quantiles corresponding to the specified probabilities if x is numeric or complex. If x is not numeric or complex, returns a vector of NA.
#' @keywords internal

quantile.numerics <- function(x, probs, na.rm = TRUE){
  will.calculate.quantiles <- is.numeric(x) | is.complex(x)
  probs <- sort(unique(probs))
  if(will.calculate.quantiles == TRUE){
    the.quantiles <- quantile(x = x, probs = probs, na.rm = na.rm)
  }
  if(will.calculate.quantiles == FALSE){
    the.quantiles <- rep.int(x = NA_character_, times = length(probs))
  }
  return(the.quantiles)
}


#' remove.erroneous.characters
#'
#' This function attempts to convert a character vector to a numeric or complex vector, replacing erroneous values based on a specified threshold.
#'
#' @param x A character vector of values that should be a numeric vector but was coerced to a character due to a small number of entries.
#' @param threshold.for.numeric A value between 0 and 1 specifying the maximum proportion of x that does not "look" numeric. If threshold.for.numeric = 0.1, then no more than 10 percentage of the values in x can be values that do not "look" numeric.
#' @param variable.should.be A character string specifying the target variable type ("numeric" or "complex"). Defaults to "numeric".
#' @param value.for.missing The value to replace missing or erroneous entries with. Defaults to NA_real_ for numeric and NA_complex_ for complex.
#' @param ... Additional arguments .
#' @return A numeric or complex vector with erroneous entries replaced, or the original character vector if the proportion of erroneous values exceeds the threshold.
#' @importFrom stats mean
#' @export

remove.erroneous.characters <- function(x, threshold.for.numeric = 0.8, variable.should.be = "numeric", value.for.missing = NULL, ...){
  w <- is.na(x)

  complex.variable <- (variable.should.be[1] == "complex")

  if(complex.variable){
    y <- suppressWarnings(as.complex(x))
    if(is.null(value.for.missing)){
      value.for.missing <- NA_complex_
    }
  } else {
    y <- suppressWarnings(as.numeric(x))
    if(is.null(value.for.missing)){
      value.for.missing <- NA_real_
    }
  }
  if(mean(!is.na(y[!w])) < threshold.for.numeric){
    return(x)
  }

  w2 <- which(is.na(y))
  the.indices <- w2[!(w2 %in% w)]
  y[the.indices] <- value.for.missing

  return(y)
}


#' round_exactly_one_value
#' This internal function rounds exactly one value to the specified number of digits.
#' @param x A character vector where the first element is the integer part and the second element is the decimal part.
#' @param digits The number of digits to round to.
#' @return A character string representing the rounded value.
#' @export round_exactly_one_value
#' @export

round_exactly_one_value <- function(x, digits){

  if(digits == 0){
    return(x[1])
  }
  if(digits > 0){
    nc <- max(0, nchar(x[2]), na.rm = TRUE)

    if(nc == 0){
      this.result <- sprintf("%s.%s", x[1], paste(rep.int(x = 0, times = digits), collapse = ""))
    }
    if(nc < digits & nc > 0){
      this.result <- sprintf("%s.%s%s", x[1], x[2], paste(rep.int(x = 0, times = digits - nc), collapse = ""))
    }
    if(nc >= digits){
      this.result <- sprintf("%s.%s", x[1], substring(text = x[2], first = 1, last = digits))
    }
  }
  return(this.result)
}

#' round_exactly
#'
#' This function rounds numeric values to a specified number of decimal places. The rounding is exact, meaning there will be exactly the specified number of decimal places even if this includes trailing zeros.
#'
#' @param x A numeric vector.
#' @param digits The number of digits to round to. This number will be exact, meaning there will be exactly this number of decimal places listed even if this includes trailing zeros. For instance, setting digits = 5 for x = 2.54 would result in 2.54000.
#' @param decimal The character specifying the decimal, which splits between whole numbers and the fractional component. Defaults to ".".
#' @param ... Additional arguments .
#' @return A character vector of rounded numeric values with exactly the specified number of decimal places.
#' @export round_exactly
#' @export



round_exactly <- function(x, digits = 0, decimal = ".", ...){

  x.is.numeric <- is.numeric(x)
  digits <- floor(digits)

  if(x.is.numeric == TRUE){

    the.pieces <- strsplit(x = as.character(x), split = decimal, fixed = TRUE)

    rounded.values <- unlist(sapply(X = the.pieces, FUN = "round_exactly_one_value", digits = digits))

    return(rounded.values)
  }
  if(x.is.numeric == FALSE){
    return(x)
  }
}


#' round.numerics
#'
#' This function rounds numeric or complex values to the specified number of digits. If the input is not numeric or complex, the values are returned untouched.
#'
#' @param x A vector.
#' @param digits The number of digits to round to. Defaults to 0.
#' @param ... Additional arguments.
#' @return If x is numeric or complex, the values will be rounded to the specified number of digits. Otherwise, the values of x will be returned untouched.
#' @export
#' @export round.numerics

round.numerics <- function(x, digits = 0, ...){
  if(is.numeric(digits) == FALSE){
    digits <- 0
  }
  if(is.numeric(x) | is.complex(x)){
    x <- round(x = x, digits = floor(digits))
  }
  return(x)
}


#' sd.numerics
#'
#' This function computes the standard deviation of a numeric, integer, logical, or complex vector. If the vector is not one of these types, the function returns either NA or the first entry of the vector.
#'
#' @param x A vector.
#' @param na.rm A logical value specifying whether missing values should be removed from the calculations specified by the function. Defaults to TRUE.
#' @param non.numeric.value If "missing", returns NA for variables that are not numeric, integer, logical, or complex. Otherwise, returns the first entry of the vector. Defaults to "missing".
#' @param ... Additional arguments.
#' @return If x is numeric, integer, logical, or complex, the standard deviation will be computed. Otherwise, the first value of x will be returned untouched or NA based on non.numeric.value.
#' @importFrom stats sd
#' @export

sd.numerics <- function(x, na.rm = TRUE, non.numeric.value = "missing", ...){

  if(is.numeric(x) | is.complex(x)){
    return(sd(x = x, na.rm = na.rm))
  }
  if(non.numeric.value == "missing"){
    return(NA)
  }
  return(x[1])
}


#' total.measured
#'
#' This function calculates the total number of non-NA values in a vector.
#'
#' @param x A vector.
#' @param ... Additional arguments.
#' @return An integer representing the total number of non-NA values in the vector x.
#' @export

total.measured <- sum.measured <- function(x, ...){
  return(sum(!is.na(x)))
}


#' total.missing
#'
#' This function calculates the total number of NA values in a vector.
#'
#' @param x A vector.
#' @param ... Additional arguments.
#' @return An integer representing the total number of NA values in the vector x.
#' @export

total.missing <- sum.missing <- function(x, ...){
  return(sum(is.na(x)))
}


trimws.character <- function(x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]", convert.factor = FALSE){
  if(is.factor(x) & convert.factor == TRUE){
    x <- as.character(x)
  }
  if(is.character(x)){
    x <- trimws(x = x, which = which, whitespace = whitespace)
  }
  return(x)
}


# internal function

# x:  a vector

# na.rm:  a logical value specifying whether missing values should be removed from the calculations specified by the.functions.

upper.quartile <- function(x, na.rm = TRUE, ...){
  return(quantile(x = x, probs = 0.75, na.rm = na.rm))
}



#' var.numerics
#'
#' This function computes the variance of a numeric, integer, logical, or complex vector. If the vector is not one of these types, the function returns either NA or the first entry of the vector.
#'
#' @param x A vector.
#' @param na.rm A logical value specifying whether missing values should be removed from the calculations specified by the function. Defaults to TRUE.
#' @param non.numeric.value If "missing", returns NA for variables that are not numeric, integer, logical, or complex. Otherwise, returns the first entry of the vector. Defaults to "missing".
#' @param ... Additional arguments.
#' @return If x is numeric, integer, logical, or complex, the variance will be computed. Otherwise, the first value of x will be returned untouched or NA based on non.numeric.value.
#' @importFrom stats var
#' @export

var.numerics <- function(x, na.rm = TRUE, non.numeric.value = "missing", ...){

  if(is.numeric(x) | is.integer(x) | is.logical(x) | is.complex(x)){
    return(var(x = x, na.rm = na.rm))
  }
  if(non.numeric.value == "missing"){
    return(NA)
  }
  return(x[1])
}


