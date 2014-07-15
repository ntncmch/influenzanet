context("context")

test_that("context of Flusurvey 2012/13 has valid variable names",{

	tmp <- create_context_flusurvey_201213()
	
	expect_true(is_context_valid(tmp$context_R))	

})