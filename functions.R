# start out with a number to test
x <- 53
# you'll want your function to return this number
x^2
square <- function(x) {
	n <- length(x)
	squared_val <- (x*x)
	return(squared_val)
}

# test it out
square(x)
square(53)
53^2 # does this match?

raise <- function(x, power=2) {
	result <- x^power
	return(result)
}

# Test with
raise(x = 2, power = 4)
#Should give you
2^4

raise(x=5)


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial()
)
poisson_model <- glm(nsibs ~ eyesight_cat + sex_cat + income,
										 data = nlsy, family = poisson()
)
logbinomial_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
												 data = nlsy, family = binomial(link = "log")
)



new_table_function <- function(model) {
	tbl_regression(
		model,
		exponentiate = TRUE,
		label = list(
			sex_cat ~ "Sex",
			eyesight_cat ~ "Eyesight",
			income ~ "Income"
		)
	)
}

new_table_function(logistic_model)
new_table_function(poisson_model)
new_table_function(logbinomial_model)
