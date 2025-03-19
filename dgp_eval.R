library(dplyr)
library(ggplot2)
library(ggpubr)

# Seed
set.seed(03022025)

# Generate covariate data
simulate_covariates <- function(n) {
  tibble(
    age = rnorm(n, mean = 80, sd = 10),
    BMI = rnorm(n, mean = 27, sd = 4),
    hypertension = rbinom(n, 1, prob = 0.3),
    smoking_status = sample(c("never", "past", "current"), n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
    gender = sample(c("male", "female"), n, replace = TRUE, prob = c(0.5, 0.5)),
    aspirin = rbinom(n, 1, prob = 0.2),
    alcohol_history = rbinom(n, 1, prob = 0.1),
    depression_history = rbinom(n, 1, prob = 0.15),
    afib_history = rbinom(n, 1, prob = 0.1)
  )
}

# Parameter choice
n = 1000000

# Effects
beta_age = 0.04
beta_bmi = 0.01 
beta_smoking_current = 0.02
beta_smoking_past = 0.005
beta_female = 0.1 # Females have higher baseline risk
gamma_treatment = -0.2 # Main treatment effect
gamma_female_treatment = -0.1
baseline_log_odds = -4.6

input_data <- simulate_covariates(n)

# Treatment assignment
assign_treatment <- function(data, age_cutoff = 81, always_taker_prob = 0.03, complier_prob = 0.40) {
  data <- data %>%
    mutate(
      eligible = ifelse(age <= age_cutoff, 1, 0),  # Eligibility based on age
      always_taker = ifelse(eligible == 0 & runif(n()) < always_taker_prob, 1, 0),  # Always-takers
      complier = ifelse(eligible == 1 & runif(n()) < complier_prob, 1, 0),  # Compliers among eligible
      treated = ifelse(always_taker == 1 | complier == 1, 1, 0)  # Final treatment assignment
    ) #%>%
    #select(-always_taker, -complier)  # Remove intermediate columns
  
  return(data)
}

input_data_treatment <- assign_treatment(input_data)
input_data_treatment %>% summary()

# Simulating dementia risk
simulate_dementia_risk <- function(data, 
                                   beta_age , 
                                   beta_bmi, 
                                   beta_smoking_current,
                                   beta_smoking_past,
                                   beta_female,  # Females have higher baseline risk
                                   gamma_treatment,  # Main treatment effect
                                   gamma_female_treatment,
                                   baseline_log_odds) {  # Extra reduction for females
  
  data <- data %>%
    mutate(
      logit_p_dementia = baseline_log_odds +  # Baseline log-odds
        beta_age * age + 
        beta_bmi * BMI +
        beta_smoking_current * (smoking_status == "current") + 
        beta_smoking_past * (smoking_status == "past") + 
        beta_female * (gender == "female") + 
        gamma_treatment * treated + 
        gamma_female_treatment * treated * (gender == "female"),
      
      p_dementia = plogis(logit_p_dementia),  # Convert log-odds to probability
      
      dementia = rbinom(n(), 1, p_dementia)  # Simulate binary outcome
    ) 
  
  return(data)
}

data_treated <- simulate_dementia_risk(input_data_treatment, gamma_treatment = gamma_treatment, gamma_female_treatment = gamma_female_treatment, beta_age = beta_age,
                                       beta_bmi = beta_bmi, beta_smoking_current = beta_smoking_current, beta_smoking_past = beta_smoking_past, beta_female = beta_female,
                                       baseline_log_odds = baseline_log_odds)

data_treated %>% filter(eligible == 1) %>% group_by(treated, gender) %>% summarise(mean_p_dementia = mean(p_dementia),
                                                                                   mean_age = mean(age))

# Calculate true LATE
calculate_true_LATE <- function(data, gamma_treatment, gamma_female_treatment, beta_age, beta_bmi, beta_smoking_current, beta_smoking_past, beta_female, baseline_log_odds) {
  
  # Identify compliers (eligible and treated OR ineligible and not treated)
  data <- data %>%
    mutate(complier = (eligible & treated) | (!eligible & !treated))
  
  # Compute dementia probability for compliers when untreated
  logit_untreated <- with(data, baseline_log_odds +  
                            beta_age * age +  
                            beta_bmi * BMI +  
                            beta_smoking_current * (smoking_status == "current") + 
                            beta_smoking_past * (smoking_status == "past") +  
                            beta_female * (gender == "female"))
  
  p_dementia_untreated <- plogis(logit_untreated)
  
  # Compute dementia probability for compliers when treated
  logit_treated <- logit_untreated + gamma_treatment + gamma_female_treatment * (data$gender == "female")
  
  p_dementia_treated <- plogis(logit_treated)
  
  # Compute LATE
  LATE <- mean(p_dementia_treated[data$complier]) - mean(p_dementia_untreated[data$complier])
  
  return(LATE)
}

true_LATE <- calculate_true_LATE(data_treated, gamma_treatment = gamma_treatment, gamma_female_treatment = gamma_female_treatment, beta_age = beta_age,
                                 beta_bmi = beta_bmi, beta_smoking_current = beta_smoking_current, beta_smoking_past = beta_smoking_past, beta_female = beta_female,
                                 baseline_log_odds = baseline_log_odds)
print(true_LATE)

# Vizualise LATE

visualize_LATE <- function(data, gamma_treatment, gamma_female_treatment, beta_age, beta_bmi, beta_smoking_current, beta_smoking_past, beta_female, baseline_log_odds) {
  
  # Identify compliers
  data <- data %>%
    mutate(complier = (eligible & treated) | (!eligible & !treated))
  
  # Compute dementia probability for compliers when untreated
  logit_untreated <- with(data, baseline_log_odds +  
                            beta_age * age +  
                            beta_bmi * BMI +  
                            beta_smoking_current * (smoking_status == "current") + 
                            beta_smoking_past * (smoking_status == "past") +  
                            beta_female * (gender == "female"))
  
  data$p_dementia_untreated <- plogis(logit_untreated)
  
  # Compute dementia probability for compliers when treated
  logit_treated <- logit_untreated + gamma_treatment + gamma_female_treatment * (data$gender == "female")
  
  data$p_dementia_treated <- plogis(logit_treated)
  
  # Prepare data for plotting
  plot_data <- data %>%
    filter(complier) %>%
    mutate(p_dementia_untreated = p_dementia_untreated,
           p_dementia_treated = p_dementia_treated) %>%
    select(age, gender, p_dementia_untreated, p_dementia_treated) %>%
    tidyr::pivot_longer(cols = starts_with("p_dementia"), 
                        names_to = "Treatment_Status", 
                        values_to = "Dementia_Probability") %>%
    mutate(Treatment_Status = ifelse(Treatment_Status == "p_dementia_untreated", "Untreated", "Treated"))
  
  # 1. Density Plot
  density_plot <- ggplot(plot_data, aes(x = Dementia_Probability, fill = Treatment_Status)) +
    geom_density(alpha = 0.5) +
    labs(title = "Distribution of Dementia Probability: Treated vs. Untreated Compliers",
         x = "Dementia Probability", y = "Density") +
    theme_minimal() +
    scale_fill_manual(values = c("red", "blue"))
  
  # 2. Box Plot
  box_plot <- ggplot(plot_data, aes(x = Treatment_Status, y = Dementia_Probability, fill = Treatment_Status)) +
    geom_boxplot(alpha = 0.5) +
    labs(title = "Dementia Probability by Treatment Status",
         x = "Treatment Status", y = "Dementia Probability") +
    theme_minimal() +
    scale_fill_manual(values = c("red", "blue"))
  
  # 3. Scatter Plot: Age vs. Dementia Probability
  scatter_plot <- ggplot(plot_data, aes(x = age, y = Dementia_Probability, color = Treatment_Status)) +
    geom_point(alpha = 0.3) +
    #geom_smooth(method = "loess") +
    labs(title = "COUNTERFACTUAL FOR COMPLIERS: \nDementia Probability by Age: Treated vs. Untreated",
         x = "Age", y = "Dementia Probability") +
    theme_minimal() +
    scale_color_manual(values = c("red", "blue"))
  
g1 <- ggarrange(density_plot, box_plot)
ggarrange(g1, scatter_plot, nrow = 2)
}

# Run the visualization function
visualize_LATE(data_treated, gamma_treatment = gamma_treatment, gamma_female_treatment = gamma_female_treatment, beta_age = beta_age,
               beta_bmi = beta_bmi, beta_smoking_current = beta_smoking_current, beta_smoking_past = beta_smoking_past, beta_female = beta_female,
               baseline_log_odds = baseline_log_odds)

# Inference
data_treated <- data_treated %>%
  mutate(run_var = 81 - age,
         tria_weights = pmax(0, 1 - abs(run_var)/max(abs(run_var)))
         )

# Continuity based RD
library(rdrobust)
rdplot(y        = data_treated$dementia,
       x        = data_treated$run_var,
       c        = 0,
       nbins    = c(200, 200),
       x.lim    = c(-20, 20),
       p        = 1)

rd1 <- rdrobust(y      = data_treated$dementia,
                x      = data_treated$run_var,
                c      = 0,
                fuzzy  = data_treated$treated,
                p = 1)

summary(rd1)

rdplot(y        = data_treated$dementia,
       x        = data_treated$run_var,
       c        = 0,
       nbins    = c(200, 200),
       x.lim    = c(-10, 10),
       y.lim    = c(0.1, 0.4),
       h        = rd1$bws["h",1],
       p        = 1)

# Local randomisation based RD
library(rdlocrand)
data_treated <- data_treated %>%
  mutate(age_discrete = round(age, digits = 0),
         run_var_discrete = 81 - age_discrete)

rdrandinf(Y      = data_treated$dementia,
          R      = data_treated$run_var_discrete,
          cutoff = 0,
          fuzzy  = data_treated$treated,
          p      = 1,
          covariates = cbind(data_treated$BMI,
                             data_treated$hypertension,
                             #data_treated$smoking_status,
                             data_treated$aspirin,
                             data_treated$alcohol_history,
                             data_treated$depression_history,
                             data_treated$afib_history))

# Instrumental variable fixed effect model
library(fixest)

feols(dementia ~ 1 | treated*run_var ~ eligible*run_var,
      vcov = "hetero",
      data = data_treated)

feols(dementia ~ 1 | treated*run_var ~ eligible*run_var,
      vcov = "hetero",
      weights = ~tria_weights,
      data = data_treated)

# Modified Poisson Regression for Binary Outcome
library(rqlm)

rqlm(dementia ~ eligible*run_var + BMI + hypertension + aspirin + alcohol_history + depression_history + afib_history,
     data = data_treated,
     family = "poisson",
     cl = 0.95,
     digits = 5)

# Two-Stage Residual Inclusion (2SRI)
library(fixest)

first_stage <- feols(treated ~ eligible + BMI + hypertension + aspirin + alcohol_history + depression_history + afib_history,
                     data = data_treated)

data_treated[, "fs_residuals"] <- first_stage$residuals

second_stage <- fepois(dementia ~ treated*run_var + BMI + hypertension + aspirin + alcohol_history + depression_history + afib_history + fs_residuals,
                       data = data_treated,
                       weights = ~tria_weights)

summary(second_stage)

# Percentage effect of treatment vis-a-vis baseline risk
(percentage_treatment_effect <- (exp(second_stage$coeftable["treated", "Estimate"])-1))

# Percentage points the treatment effect is
baseline_risk <- data_treated %>% filter(treated == 0) %>% summarise(dem_mean = mean(dementia)) %>% as.numeric()
baseline_risk*percentage_treatment_effect


second_stage_logit <- feglm(dementia ~ treated*run_var + BMI + hypertension + aspirin + alcohol_history + depression_history + afib_history + fs_residuals,
                            data = data_treated,
                            weights = ~tria_weights,
                            family = binomial("logit"))

summary(second_stage_logit)
(OR <- exp(second_stage_logit$coeftable["treated", "Estimate"]))
probability <- (OR*baseline_risk) / (1+(OR-1)*baseline_risk)

# Percentage points the treatment effect is
probability - baseline_risk

