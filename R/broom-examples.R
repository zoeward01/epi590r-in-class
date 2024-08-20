library(tidyverse)
library(broom)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")))

mod_sex_cat <- lm(income ~ sex_cat, data = nlsy)
mod_race_eth_cat <- lm(income ~ race_eth_cat, data = nlsy)
mod_eyesight_cat <- lm(income ~ eyesight_cat, data = nlsy)
mod_age_bir <- lm(income ~ age_bir, data = nlsy)

tidy_sex_cat <- tidy(mod_sex_cat, conf.int = TRUE)
tidy_race_eth_cat <- tidy(mod_race_eth_cat, conf.int = TRUE)
tidy_eyesight_cat <- tidy(mod_eyesight_cat, conf.int = TRUE)
tidy_age_bir <- tidy(mod_age_bir, conf.int = TRUE)

bind_rows(
	sex_cat = tidy_sex_cat,
	race_eth_cat = tidy_race_eth_cat,
	eyesight_cat = tidy_eyesight_cat,
	age_bir = tidy_age_bir, .id = "model") |>
	mutate(
		term = str_remove(term, model),
		term = ifelse(term == "", model, term))

logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial())

tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) |>
	tidycat::tidy_categorical(logistic_model, exponentiate = TRUE) |>
	# remove standard error, z stat, p-value to make for cleaner output
	select(-c(3:5))

tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) |>
	tidycat::tidy_categorical(logistic_model, exponentiate = TRUE) |>
	slice(-1) |> # remove intercept
	ggplot(mapping = aes(x = level, y = estimate,
											 ymin = conf.low, ymax = conf.high)) +
	geom_point() +
	geom_errorbar() +
	facet_grid(cols = vars(variable), scales = "free", space = "free") +
	scale_y_log10()


#In the last set of exercises, you compared a log-binomial model to a log-Poisson model with robust
#standard errors using {gtsummary}. Your job now is to do the same using broom::tidy(). You’ll need some
#extra packages, though, because if you look at the broom::tidy() documentation, it doesn’t say anything
#about adding robust standard errors. The answer, then, is usually to start Googling. I’ve done so for you
#and found this post (I highly recommend all of Andrew Heiss’s R and statistics explanations!). Use this as
#a guide to create a dataframe with the results from both models (you can use bind_rows() to combine them,
#as in the example).Return to your final project. You are going to need to do some type of modeling and
#create some tables and figures, so this is a good time to start playing around!

logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy_cc, family = binomial(link = "log"))
log_binomial <- tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"
	))


#Since family = binomial(link = "log") often doesn’t converge, we often use Poisson regression with robust
#standard errors to estimate risk ratios. Fit a Poisson regression instead of the log-binomial regression in
#the last question. Then create a table using tidy_fun = partial(tidy_robust, vcov = "HC1"). It will prompt
#you to install new package(s) (yes!). See this page for more on custom tidiers.
logistic_model1 <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy_cc, family = binomial(link = "log"))
log_binomial <- tbl_regression(
	logistic_model1,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"
	))

logistic_model2 <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy_cc, family = poisson())
log_poisson <- tbl_regression(
	logistic_model2,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight"),
	tidy_fun = partial(tidy_robust, vcov = "HC1"))

comparison <- tbl_merge(list(log_binomial, log_poisson),
					tab_spanner = c("**Binomial**", "**Poisson**"))
comparison
dplyr::bind_rows(
	sex_cat = tidy_sex_cat,
	race_eth_cat = tidy_race_eth_cat,
	eyesight_cat = tidy_eyesight_cat,
	age_bir = tidy_age_bir, .id = "model") |>
	dplyr::mutate(
		term = stringr::str_remove(term, model),
		term = ifelse(term == "", model, term))










