library(tidyverse)
library(rstanarm)
library(ggplot2)
library(kableExtra)

combined <- readRDS("data.RDS") %>%
  mutate(group = ifelse(group == 'A', 'Designer', 'In-House'))

combined_summary <- combined %>%
  group_by(group) %>%
  summarise(customers = n(),
            conversions = sum(conversion, na.rm = T),
            conversion_rate = scales::percent(mean(conversion, na.rm = T), accuracy = 0.01),
            spend = scales::dollar(mean(spend, na.rm = T)))

combined_summary %>%
  kable() %>%
  kable_styling(full_width = F)

### Frequentist regression

conv_fit_freq <- glm(conversion ~ group,
                     data = combined,
                     family = binomial())

summary(conv_fit_freq)

spend_fit_freq <- glm(spend ~ group,
                      data = combined)

summary(spend_fit_freq)

### Bayesian Conversion Regression

conv_fit_bayes = stan_glm(cbind(conversions, customers - conversions) ~ group,
                          family = binomial(link = logit), chains = 4, 
                          iter = 4000, warmup = 2000,
                          #                         prior_intercept = binomial(conv_mean, conv_std),
                          #                         prior = normal(titanicMean, titanicStd_dev),
                          #                         adapt_delta = 0.9,
                          data = combined_summary)

summary(conv_fit_bayes, digits = 3)
prior_summary(conv_fit_bayes)

plot(conv_fit_bayes, "trace")

ci85 <- posterior_interval(conv_fit_bayes, prob = 0.85, pars = "groupIn-House")
round(ci85, 2)

View(as.data.frame(conv_fit_bayes))

conv_fit_samples <- as.data.frame(conv_fit_bayes) %>%
  set_names(c("intercept_conv",
              "In_House_Flag_conv"))

View(conv_fit_samples)

## get likelihood of condition from the posterior properties
conv_fit_samples %>%
  summarise(`Designer > In-House` = mean(0 > In_House_Flag_conv)) %>%
  t %>%
  as.data.frame %>%
  rownames_to_column() %>%
  mutate_if(is.numeric, scales::percent) %>%
  set_names(c("Condition", "Probability of Condition"))


## Plot the posterior distributions of each group
conv_fit_samples %>%
  mutate(A = intercept_conv,
         B = intercept_conv + In_House_Flag_conv) %>%
  tidyr::gather(type, estimate, -In_House_Flag_conv, -intercept_conv) %>%
  ggplot(aes(estimate, fill = type)) +
  geom_density(alpha = 0.5)


##### Bayesian Spend Regression
spend_fit_bayes = stan_glm(spend ~ group,
                           chains = 4,
                           iter = 6000, warmup = 4000,
                           #                            adapt_delta = 0.99,
                           data = combined)

summary(spend_fit_bayes, digits = 3)
prior_summary(spend_fit_bayes)

ci85 <- posterior_interval(spend_fit_bayes, prob = 0.85, pars = "groupIn-House")
round(ci85, 2)

plot(spend_fit_bayes, "trace")

spend_fit_samples <- as.data.frame(spend_fit_bayes) %>%
  set_names(c("intercept_spend",
              "In_House_Flag_spend",
              "Sigma"))

#View(spend_fit_samples)

spend_fit_samples %>%
  summarise(`Group A > Group B` = mean(0 > In_House_Flag_spend)) %>%
  t %>%
  as.data.frame %>%
  rownames_to_column() %>%
  mutate_if(is.numeric, scales::percent) %>%
  set_names(c("Condition", "Probability of Condition"))

spend_fit_samples %>%
  summarise(`Group A - Group B` = mean((intercept_spend) - (intercept_spend + In_House_Flag_spend)),
            `Group B - Group A` = mean((intercept_spend + In_House_Flag_spend) - intercept_spend))


##### Combined overall results

overall_results <- conv_fit_samples %>%
  bind_cols(spend_fit_samples)

overall_results %>%
  summarise(`Group A > Group B` = mean((plogis(intercept_conv)*(intercept_spend)) > 
                                         (plogis(intercept_conv + In_House_Flag_conv) * (intercept_spend + In_House_Flag_spend)))) %>%
  mutate_if(is.numeric, scales::percent)

overall_results %>%
  summarise(`Group A - Group B` = mean((plogis(intercept_conv) * (intercept_spend)) - 
                                         (plogis(intercept_conv + In_House_Flag_conv) * (intercept_spend + In_House_Flag_spend)))) %>%
  mutate_if(is.numeric, scales::dollar)

overall_results %>%
  mutate(A = (plogis(intercept_conv) * (intercept_spend)),
         B = (plogis(intercept_conv + In_House_Flag_conv) * (intercept_spend + In_House_Flag_spend))) %>%
  tidyr::gather(type, estimate, -In_House_Flag_conv, -intercept_conv, -intercept_spend, -In_House_Flag_spend, -Sigma) %>%
  ggplot(aes(estimate, fill = type)) +
  geom_density(alpha = 0.5)




