## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  tidy = TRUE
)

## ----include=FALSE------------------------------------------------------------
devtools::load_all(".")

## ----setup--------------------------------------------------------------------
library(DTwrappers2)
library(DTwrappers)
library(data.table)
data(iris)

## -----------------------------------------------------------------------------
RNGversion(vstr = 3.6)
set.seed(47)
iris$noise <- rnorm(n = nrow(iris))
iris$noise[c(1,51)] <- c("0.13ABC", "N/A")
is.character(iris$noise)

## ----character.coercion.culprits----------------------------------------------
character.coercion.culprits(x = iris$noise, threshold.for.numeric = 0.8)

## ----charactercoercion--------------------------------------------------------
character.coercion.culprits(x = iris$Sepal.Length, threshold.for.numeric = 0.8)

## -----------------------------------------------------------------------------
dt.character.coercion.culprits(dt.name = "iris", threshold.for.numeric = 0.8, the.variables = ".")

## -----------------------------------------------------------------------------
dt.character.coercion.culprits(dt.name = "iris", threshold.for.numeric = 0.8, the.variables = ".", return.as = "code")

## -----------------------------------------------------------------------------
library(data.table)
iris <- as.data.table(iris)

iris[, .(variable = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width', 'Species', 'noise'), character.coercion.culprits = lapply(X = .SD, FUN = 'character.coercion.culprits', threshold.for.numeric = 0.8))]

## -----------------------------------------------------------------------------
dt.character.coercion.culprits(dt.name = "iris", threshold.for.numeric = 0.8, the.variables = ".", return.as = "all")

## -----------------------------------------------------------------------------
dt.character.coercion.culprits(dt.name = "iris", threshold.for.numeric = 0.8, the.variables = c("Petal.Width", "noise"), grouping.variables = "Species")

## -----------------------------------------------------------------------------
dt.character.coercion.culprits(dt.name = "iris", threshold.for.numeric = 0.8, the.filter = "Species != 'virginica' & Sepal.Width > 3.3", the.variables = c("Petal.Width", "noise"), grouping.variables = "Species")

## -----------------------------------------------------------------------------
dt.character.coercion.culprits(dt.name = "iris", threshold.for.numeric = 0.8, the.filter = expression(Species != "virginica" & Sepal.Width > 3.3), the.variables = c("Petal.Width", "noise"), grouping.variables = "Species")

## -----------------------------------------------------------------------------
dt.character.coercion.culprits(dt.name = "iris", the.filter = 1:100)
dt.character.coercion.culprits(dt.name = "iris", the.filter = "1:100")

## -----------------------------------------------------------------------------
iris$noise[1:5]
remove.erroneous.characters(x = iris$noise[1:5], threshold.for.numeric = 0.8, variable.should.be = "numeric")

## -----------------------------------------------------------------------------
dt.remove.erroneous.characters(dt.name = "iris", threshold.for.numeric = 0.8)[1:5,]

## -----------------------------------------------------------------------------
mean.numerics(x = iris$Sepal.Length)
mean.numerics(x = iris$Species)
mean.numerics(x = iris$Species, non.numeric.value = "first")

## -----------------------------------------------------------------------------
dt.mean.numerics(dt.name = "iris")

## -----------------------------------------------------------------------------
dt.mean.numerics(dt.name = "iris", the.filter = "Sepal.Length > 4 & Sepal.Length < 6", grouping.variables = "Species")

## -----------------------------------------------------------------------------
median.numerics(x = iris$Sepal.Length)
dt.median.numerics(dt.name = "iris", the.variables = c("Sepal.Length", "Sepal.Width"), grouping.variables = "Species")

sd.numerics(x = iris$Sepal.Length)
dt.sd.numerics(dt.name = "iris", the.variables = c("Sepal.Length", "Sepal.Width"), grouping.variables = "Species")

var.numerics(x = iris$Sepal.Length)
dt.var.numerics(dt.name = "iris", the.variables = c("Sepal.Length", "Sepal.Width"), grouping.variables = "Species")

min.numerics(x = iris$Sepal.Length)
dt.min.numerics(dt.name = "iris", the.variables = c("Sepal.Length", "Sepal.Width"), grouping.variables = "Species")

max.numerics(x = iris$Sepal.Length)
dt.max.numerics(dt.name = "iris", the.variables = c("Sepal.Length", "Sepal.Width"), grouping.variables = "Species")

## -----------------------------------------------------------------------------
round.numerics(x = iris$Sepal.Length[1:5], digits = 0)
round.numerics(x = iris$Species[1:5])

## -----------------------------------------------------------------------------
grouped.means <- dt.mean.numerics(dt.name = "iris", the.variables = c("Sepal.Length", "Sepal.Width"), grouping.variables = "Species")
dt.round.numerics(dt.name = "grouped.means", digits = 2)

## -----------------------------------------------------------------------------
iris$SL.1000 <- iris$Sepal.Length * 1000 + rnorm(n = nrow(iris), mean = 0, sd = 25)
format.numerics(x = iris$SL.1000[1:5], digits = 2, big.mark = ",")

## -----------------------------------------------------------------------------
grouped.means <- dt.mean.numerics(dt.name = "iris", grouping.variables = "Species", na.rm = T)

dt.format.numerics(dt.name = "grouped.means", the.variables = c("Sepal.Length", "SL.1000"), digits = 2, big.mark = ",", grouping.variables = "Species")

## -----------------------------------------------------------------------------
round_exactly(x = iris$Sepal.Length[1:5], digits = 3)
round_exactly(x = iris$Sepal.Length[1:5], digits = 5)

dt.round.exactly(dt.name = "grouped.means", the.variables = c("Sepal.Length", "Sepal.Width"), digits = 3)

## -----------------------------------------------------------------------------
iris$noise[3 + 50 * (0:2)] <- NA

total.missing(x = iris$noise)
total.measured(x = iris$noise)
mean.missing(x = iris$noise)
mean.measured(x = iris$noise)

## -----------------------------------------------------------------------------
dt.total.missing(dt.name = "iris")
dt.total.measured(dt.name = "iris")
dt.mean.missing(dt.name = "iris")
dt.mean.measured(dt.name = "iris")

## -----------------------------------------------------------------------------
dt.total.missing(dt.name = "iris", grouping.variables = "Species")
dt.total.measured(dt.name = "iris", the.filter = "Sepal.Length > 4 & Sepal.Length < 6")
dt.mean.missing(dt.name = "iris", the.filter = "Sepal.Length > 4 & Sepal.Length < 6", grouping.variables = "Species")
dt.mean.measured(dt.name = "iris", the.variables = c("noise", "Sepal.Width"), the.filter = "Sepal.Length > 4 & Sepal.Length < 6", grouping.variables = "Species", return.as = "all")

## -----------------------------------------------------------------------------
dt.summarize(dt.name = "iris", the.variables = c("Sepal.Length", "Sepal.Width"))

## -----------------------------------------------------------------------------
dt.summarize(dt.name = "iris", the.variables = c("Sepal.Length", "Sepal.Width"), the.functions = c("mean", "sd", "var"))

## -----------------------------------------------------------------------------
dt.summarize(dt.name = "iris", the.variables = c("Sepal.Length", "Sepal.Width"), the.filter = "Sepal.Length > 4 & Sepal.Length < 6", grouping.variables = "Species")

## -----------------------------------------------------------------------------
dt.summarize(dt.name = "iris", the.functions = c("mean.numerics", "sd.numerics"), the.variables = c("Sepal.Length", "Sepal.Width", "Species"))

