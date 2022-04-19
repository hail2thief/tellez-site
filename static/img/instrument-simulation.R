library(tidyverse)
library(broom)

fake = tibble(instrument = rnorm(500), 
       confound = rnorm(500)) %>% 
  mutate(treatment = 1 * instrument + 4 * confound) %>% 
  mutate(outcome = rnorm(500) + 3*treatment - 2 * confound)


lm(outcome ~ treatment, data = fake) %>% summary()
lm(outcome ~ treatment + confound, data = fake) %>% summary()


first_stage = lm(treatment ~ instrument, data = fake)
first_stage_fit = augment(first_stage, fake)
first_stage_fit = fake %>% 
  mutate(.fitted = coef(first_stage)[1] + 
           coef(first_stage)[2]*instrument)

lm(outcome ~ .fitted, data = first_stage_fit)


estimate = AER::ivreg(outcome ~ treatment | instrument, data = fake)
estimate


# repeat simulations
simulate_instrument = function(x)
{
  fake = tibble(instrument = rnorm(500), 
                unobserv = rnorm(500)) %>% 
    mutate(treatment = instrument + unobserv) %>% 
    mutate(outcome = rnorm(500) + 3*treatment - unobserv)
  
  estimate = coef(AER::ivreg(outcome ~ treatment | instrument, 
                             data = fake))[[2]]
  
  return(estimate)

}


simulations = tibble(reps = 1:1e3) %>% 
  mutate(estimate = map_dbl(.x = reps, .f = simulate_instrument))


ggplot(simulations, aes(x = estimate)) + 
  geom_density(fill = "orange", alpha = .7, color = "white") + 
  geom_vline(xintercept = 3, lty = 2, size = 2, color = "red") + 
  theme_minimal() + 
  labs(x = "Estimated coefficient on treatment")
