library(testthat)
library(data.table)
library(DTwrappers2)

# Ensure the iris dataset is in the data.table format
data("iris")
setDT(iris)

# Manipulate the iris dataset as described in your Rmd file
RNGversion(vstr = "3.6")
set.seed(47)
iris$noise <- rnorm(n = nrow(iris))
iris$noise[c(1, 51)] <- c("0.13ABC", "N/A")

describe("DTwrappers2 functionality tests", {
  describe("Tests for character coercion culprit identification", {
    it("identifies character coercion culprits accurately", {
      result <- character.coercion.culprits(x = iris$noise, threshold.for.numeric = 0.8)
      expect_type(result, "character")
      expect_length(result, 2)
      expect_true("0.13ABC" %in% result)
      expect_true("N/A" %in% result)
    })

    it("confirms no culprits in numeric variables", {
      result <- character.coercion.culprits(x = iris$Sepal.Length, threshold.for.numeric = 0.8)
      expect_type(result, "double")
      expect_length(result, 0)
    })
  })

  describe("Tests for removing erroneous characters", {
    it("removes erroneous characters and converts to numeric where possible", {
      modified_values <- remove.erroneous.characters(x = iris$noise[1:5], threshold.for.numeric = 0.8, variable.should.be = "numeric")
      expect_type(modified_values, "double")
      expect_true(is.na(modified_values[1]))
      expect_equal(modified_values[2], as.numeric(iris$noise[2]))
    })

  })
})
