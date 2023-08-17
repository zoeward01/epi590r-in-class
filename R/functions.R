# start out with a number to test
x <- 3
# you'll want your function to return this number
x^2
square <- function(x) {
	squared_value <- x^2
	return(squared_value)
}
# test it out
square(x) # 9
square(53)
53^2 # does this match?

raise <- function(x, power) {
	value <- x^power
	return(value)
}
raise(x = 5, power = 3)

# set a default value of power
raise <- function(x, power = 2) {
	value <- x^power
	return(value)
}
raise(x = 5)
raise(x = 5, power = 2)
raise(x = 5, power = 3)

