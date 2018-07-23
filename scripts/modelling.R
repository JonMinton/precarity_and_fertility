# Modelling 

all_females_in_bhps <- read_rds("data/all_females_in_bhps.rData")

# Analyses  ---------------------------------------------------------------

# 1) Find people who were couple no children at wave 1, 
# 2) find first wave where their status changes to couple dep children 

all_females_in_bhps %>% 
  filter(wave == 1, HHTYPE %in% c(3,1)) %>% 
  pull(PID) -> init_no_child

all_females_in_bhps %>% 
  filter(PID %in% init_no_child) %>% 
  filter(AGE < 50) %>% 
  mutate(now_has_child = HHTYPE %in% c(4, 6)) %>% 
  group_by(PID) %>% 
  arrange(wave) %>% 
  mutate(child_last = lag(now_has_child)) %>% 
  mutate(is_mother = case_when(
    now_has_child == 1 & child_last == 0 ~ 1,
    now_has_child == 0 & child_last == 0 ~ 0,
    now_has_child == 1 & child_last == 1 ~ 1,
    now_has_child == 0 & child_last == 1 ~ 1,
    is.na(child_last) ~ 0
  )
  )  %>% 
  ungroup() %>% 
  select(age = AGE, wave, tenure, is_mother) -> mother_df

# Visualise the 'survival' curve 

mother_df %>% 
  group_by(wave) %>% 
  summarise(mean_mother = mean(is_mother)) %>% 
  ungroup() %>% 
  ggplot(aes(x = wave, y = 1 - mean_mother)) + 
  geom_line() + 
  ylim(c(0, 1)) + 
  labs(x = "Wave", y = "Proportion not mothers")


mother_df %>% 
  mutate(simple_tenure = car::recode(
    tenure,
    "
    c('Housing Assoc. rented', 'Local Authority rented') = 'Social';
    c('Other rented', 'Rented from Employer', 'Rented Private furnished', 'Rented private unfurnished') = 'Rented';
    c('Owned Outright', 'Owned with Mortgage') = 'Owned';
    else = NA
    "
  )
  ) -> mother_df

# Now by tenure (not really following the same people)
mother_df %>% 
  filter(!is.na(simple_tenure)) %>% 
  group_by(wave, simple_tenure) %>% 
  summarise(mean_mother = mean(is_mother)) %>% 
  ungroup() %>% 
  ggplot(aes(x = wave, y = 1 - mean_mother, colour = simple_tenure, linetype = simple_tenure)) + 
  geom_line() + 
  ylim(c(0, 1)) + 
  labs(x = "Wave", y = "Proportion not mothers")

# To do: Survival modelling 
# https://www.datacamp.com/community/tutorials/survival-analysis-R

surv_object <- Surv(time = mother_df$wave, event = mother_df$is_mother)

fit1 <- survfit(surv_object ~ simple_tenure, data = mother_df)
summary(fit1)

ggsurvplot(fit1, data = mother_df, pval = TRUE)

fit2 <- survfit(surv_object ~ simple_tenure + age, data = mother_df)

summary(fit1)
summary(fit2)

fit1_cox <- coxph(surv_object ~ simple_tenure , data = mother_df)
fit2_cox <- coxph(surv_object ~ simple_tenure + age , data = mother_df)
fit3_cox <- coxph(surv_object ~ simple_tenure + age + I(age^2) , data = mother_df)

ggforest(fit3_cox, data = mother_df)
