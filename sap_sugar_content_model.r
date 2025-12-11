mod <- brms::brm(formula = ssc ~ year_c + (1 | ID),
                 data = data %>% select(ID, year, ssc) %>% 
                   mutate(year_c = year - mean (year, na.rm = TRUE)) %>%
                   select(ssc, ID, year_c),
                 family = gaussian(),
                 prior = c(set_prior("normal(5, 3)", class = "Intercept"), # Intercept prior: sap sugar content typically bounded (e.g., 0–10+), center at a reasonable value
                            set_prior("normal(0, 1)", class = "b", coef = "year_c"), # Slope prior: modest changes per year; adjust scale to your context
                            set_prior("exponential(1)", class = "sd", group = "ID"), # Random intercept SD across tree
                            set_prior("exponential(1)", class = "sigma")), # Residual SD
                 cores = 4,
                 seed = 42)
plot(mod)
summary(mod)
coef(mod)
