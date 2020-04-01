library(tidyverse)
library(rstanarm)
library(titanic)

# View(head(titanic_train, 10))

# titanic_train %>% 
#   glimpse

titanic_summary <- titanic_train %>%
#  mutate(children_flag = ifelse(Age <= 16, 1, 0)) %>%
  group_by(Pclass, Sex) %>%
  summarise(passengers = n(),
            survived = sum(Survived, na.rm = TRUE),
            pct_survived = survived / passengers,
            med_fare = median(Fare, na.rm = TRUE),
            med_age  = median(Age, na.rm = TRUE))

View(titanic_summary)

## Set the priors
# using the training data, find the mean and std deviation of the survival rate
titanicMean <- mean(titanic_train$Survived)
titanicStd_dev <- sqrt(var(titanic_train$Survived))

## fit a standard logistic regression model
hfit_survived = stan_glm(cbind(survived, passengers - survived) ~ Sex,
                        family = binomial(), chains = 1, 
                        iter = 4000, warmup = 2000,
                        prior_intercept = normal(titanicMean, titanicStd_dev),
                        prior = normal(titanicMean, titanicStd_dev),
                        adapt_delta = 0.9,
                        data = titanic_summary)

summary(hfit_survived, digits = 3)

View(as.data.frame(hfit_survived))

hfit_survived_samples <- as.data.frame(hfit_survived) %>%
  set_names(c("intercept",
              "sex_male"))

View(hfit_survived_samples)

## get likelihood of condition from the posterior properties
hfit_survived_samples %>%
  summarise(`female > male` = mean(intercept > intercept + sex_male)) %>%
  t %>%
  as.data.frame %>%
  rownames_to_column() %>%
  mutate_if(is.numeric, scales::percent) %>%
  set_names(c("Condition", "Probability of Condition"))

## Plot the posterior distributions of each group
hfit_survived_samples %>%
  mutate(female = intercept,
         male = intercept + sex_male) %>%
  tidyr::gather(type, estimate, -sex_male, -intercept) %>%
  ggplot(aes(estimate, fill = type)) +
  geom_density(alpha = 0.5)

########## hierarchical modeling

titanic_summary2 <- titanic_train %>%
  mutate(Pclass = case_when(Pclass == 1 ~ 'First',
                            Pclass == 2 ~ 'Second',
                            Pclass == 3 ~ 'Third',
                            TRUE ~ 'error')) %>%
  group_by(Pclass, Sex) %>%
  summarise(passengers = n(),
            survived = sum(Survived, na.rm = TRUE),
            pct_survived = survived / passengers,
            med_fare = median(Fare, na.rm = TRUE))

View(titanic_summary2)

## fit a hierarchical logistic regression model
hfit_survived_hier = stan_glmer(cbind(survived, passengers - survived) ~ (1 | Sex / Pclass),
                              family = binomial(), chains = 1, 
                              iter = 4000, warmup = 2000,
                              prior_intercept = normal(titanicMean, titanicStd_dev),
                              prior = normal(titanicMean, titanicStd_dev),
                              adapt_delta = 0.999,
                              data = titanic_summary2)

summary(hfit_survived_hier, digits = 3)

View(as.data.frame(hfit_survived_hier))

hfit_survived_hier_samples <- as.data.frame(hfit_survived_hier) %>%
  set_names(c("intercept",
              "first_class_women",
              "first_class_men",
              "second_class_women",
              "second_class_men",
              "third_class_women",
              "third_class_men",
              "women",
              "men",
              "sigma1",
              "sigma2"))

View(hfit_survived_hier_samples)

## get likelihood of condition from the posterior properties
hfit_survived_hier_samples %>%
  summarise(`first class > second class` = 
              mean((first_class_men + first_class_women) > (second_class_men + second_class_women)),
            `second class > third class` = 
              mean((second_class_men + second_class_women) > (third_class_men + third_class_women)),
            `first class men > second class men` = 
              mean((first_class_men) > (second_class_men)),
            `second class men > third class men` = 
              mean((second_class_men) > (third_class_men)),
            `first class women > second class women` =
              mean((first_class_women > second_class_women)),
            `second class women > third class women` =
              mean((second_class_women > third_class_women)),
            `female > male` = mean(women > men),
            `first class female > first class male` = 
              mean((women + first_class_women) > (men + first_class_men)),
            `second class female > second class male` = 
              mean((women + second_class_women) > (men + second_class_men)),
            `third class female > third class male` = 
              mean((women + third_class_women) > (men + third_class_men))) %>%
  t %>%
  as.data.frame %>%
  rownames_to_column() %>%
  mutate_if(is.numeric, scales::percent) %>%
  set_names(c("Condition", "Probability of Condition"))

## Plot the posterior distributions of Male vs Female
hfit_survived_hier_samples %>%
  mutate(female = intercept + women,
         male = intercept + men) %>%
  tidyr::gather(type, estimate, -women, -men, -sigma1, -sigma2, -first_class_women, -first_class_men, 
                -second_class_women, -second_class_men, 
                -third_class_women, -third_class_men, -intercept) %>%
  ggplot(aes(estimate, fill = type)) +
  geom_density(alpha = 0.5)

## Plot the posterior distributions of First - Third classes
hfit_survived_hier_samples %>%
  mutate(first_class  = first_class_women + first_class_men,
         second_class = second_class_women + second_class_men,
         third_class  = third_class_women + third_class_men) %>%
  tidyr::gather(type, estimate, -women, -men, -sigma1, -sigma2, -first_class_women, -first_class_men, 
                -second_class_women, -second_class_men, 
                -third_class_women, -third_class_men, -intercept) %>%
  ggplot(aes(estimate, fill = type)) +
  geom_density(alpha = 0.5)

## Plot the posterior distributions of Male and Female grouped by First - Third classes
hfit_survived_hier_samples %>%
  mutate(first_class  = first_class_women + first_class_men,
         second_class = second_class_women + second_class_men,
         third_class  = third_class_women + third_class_men) %>%
  tidyr::gather(type, estimate, -women, -men, -sigma1, -sigma2, -first_class_women, -first_class_men, 
                -second_class_women, -second_class_men, 
                -third_class_women, -third_class_men, -intercept) %>%
  ggplot(aes(estimate, fill = type)) +
  geom_density(alpha = 0.5)








hfit_samples_opens %>%
  # confirm we don't need to include 'content_match_shopper' when calculating these values
  summarise(`test > control` = mean(test > control),
            `mens_test > control ` = mean((mens_test + test) > (mens_control + control)),
            `womens_test > control ` = mean((womens_test + test) > (womens_control + control))) %>%
  t %>%
  as.data.frame %>%
  rownames_to_column() %>%
  mutate_if(is.numeric, scales::percent) %>%
  set_names(c("Condition", "Probability of Condition"))



