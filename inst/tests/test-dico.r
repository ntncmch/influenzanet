context("dictionary")

test_that("dictionary of Flusurvey 2012/13 has valid variable names",{

	tmp <- create_dictionnary_flusurvey_201213()
	
	expect_valid_name <- function(x){expect_true(variable_name_OK(x$to_name))}	

	lapply(tmp[1],expect_valid_name)

})