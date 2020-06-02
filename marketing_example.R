library(tidyverse)
library(dplyr)
library(rstanarm)
library(ggplot2)
library(kableExtra)

combined <- readRDS("data.RDS") %>%
  mutate(group = ifelse(group == 'A', 'Designer', 'In-House'))

#write_csv(combined, 'data.csv')

combined_summary <- combined %>%
  group_by(group) %>%
  summarise(customers = n(),
            conversions = sum(conversion, na.rm = T),
            conversion_rate = scales::percent(mean(conversion, na.rm = T), accuracy = 0.01),
            conversion_var = var(conversion, na.rm = T),
            spend = scales::dollar(mean(spend, na.rm = T)))

combined_summary %>%
  kable() %>%
  kable_styling(full_width = F)

## calculate variance of spend 
combined %>%
  filter(conversion == 1) %>%
  summarise(min_spend = min(spend, na.rm = T),
            max_spend = max(spend, na.rm = T),
            spend_var = var(spend, na.rm = TRUE),
            spend_sd = sd(spend, na.rm = TRUE))

## plot the distribution of spend
ggplot(combined, aes(x = spend)) +
  geom_histogram()

# subset conversion and spend to perform F and t tests
spend_designer <- combined %>%
  filter(conversion == 1,
         group == "Designer") %>%
  select(spend)

spend_house <- combined %>%
  filter(conversion == 1,
         group == "In-House") %>%
  select(spend)

conv_designer <- combined %>%
  filter(group == "Designer")

conv_house <- combined %>%
  filter(group == "In-House")

## F test to compare variance between groups
var.test(spend_designer$spend, spend_house$spend,  alternative = c("two.sided"), conf.level = 0.95)
var.test(conv_designer$conversion, conv_house$conversion,  alternative = c("two.sided"), conf.level = 0.95)

## t-test conversion
t.test(conversion~group, data = combined, var.equal = FALSE)

## t-test spend
spend <- combined %>%
  filter(conversion == 1)

t.test(spend~group, data = spend, var.equal = TRUE)

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




