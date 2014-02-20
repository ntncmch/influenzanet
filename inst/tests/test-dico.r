context("dictionary")

test_that("dictionary of Flusurvey 2012/13 has valid variable names",{

	tmp <- create_dictionnary_flusurvey_201213()
	
	expect_true(dictionary_validated(tmp$dico_R))	

})