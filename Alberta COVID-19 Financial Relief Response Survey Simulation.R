library(tidyverse)
library(readr)
library(dplyr)
library(knitr)
library(ggplot2)

# The following are a series of simulations in relation to a survey which /
# investigates how residents of Alberta  feel about the Liberal government's /
# pandemic financial relief efforts.

# A seed is set so the simulation is reproducible.
set.seed(123)

# It is expected that we will receive 400 responses so the survey size is set /
# to 400.
survey_size = 400

# The following is a simulation the question "How old are you?". The multiple /
# choice options for this question are: a) 18-29, b) 30-39, c)40-49, d) 50-59, /
# e) 60+.

# We assume that the frame is indicative of the population of Alberta and the /
# probabilities are adjusted to convey Alberta's demographic data.

age = sample(x = c("18-29", "30-39", "40-49", "50-59", "60+"), 
             size =  survey_size, replace = TRUE, 
             prob = c(0.21,0.2,0.2,0.17,0.22))

# The following is a simulation for the question "What is your employment 
# status before March?". The multiple choice options for this question are: /
# a) Unemployed, b) Employed/Self-employed.

# The probability of the respective options were determined using the /
# unemployment rate in the province of Alberta in February 2020 as this is /
# when the pandemic began to take its effect. The unemployment rate in /
# February 2020 was 7.2%.

pre_employment_status = sample(x = c("Unemployed", "Employed/Self-Employed"), 
                               size = survey_size, replace = TRUE, 
                               prob = c(0.072, 0.928))

# The following is a simulation for the question "What is your current /
# employment status?". The multiple choice options for this question are: /
# a) Unemployed, b) Employed/Self-employed.

# We assume the employment data based on the labour force characteristics /
# of Alberta.

curr_employment_status = sample(x= c("Employed/Self-employed", "Unemployed"), 
                                size = survey_size, replace = TRUE, 
                                prob = c(0.12,0.88))

# The following is a simulation for the question "Are you currently a student?". 
# The multiple choice options for this question are : a) Yes, b) No.

# The probability of the options was determined using the latest information /
# on the amount of people enrolled in post-secondary institutions and the /
# population of Alberta. With 194,010 people enrolled in post-secondary and a /
# population of 4,421,876 this means 4.39% of people are students in /
# post-secondary.

student_var = sample(x = c("Yes", "No"), 
                     size = survey_size, replace = TRUE, 
                     prob = c(0.0439, 0.9561))

# The following is a simulation for the question "Were you eligible for Canada /
# Emergency Student Benefit (CESB)?". The multiple choice options for this /
# question are : a) Yes, b) No.

# The probabilities were set based on information released by the Canadian /
# federal government in relation to application and eligibility for CESB.

CESB_eligibility = sample(x= c("Yes", "No"), size = survey_size,
                          replace = TRUE, prob = c(0.0439,0.9561))

# The following is a simulation for the question "Were you eligible for Canada /
# Emergency Relief Benefit (CERB)?". The multiple choice options for this /
# question are : a) Yes, b) No.

# The probabilities were set based on information released by the Canadian /
# federal government in relation to application and eligibility for CERB.

CERB_eligibility = sample(x= c("Yes", "No"), size = survey_size,
                          replace = TRUE, prob = c(0.241,0.759))

# The following is a simulation for the question "If you applied, how many /
# periods did you apply for CESB?". The multiple choice options for this /
# question are: a) N/A, b) 1, c) 2, d) 3, e) 4

# The probability of each option was determined using statistics released in /
# relation to CESB. The percentage of people who are not students was used for /
# the N/A option and the rest of the option probabilities were allocated /
# proportionally to how many applications were approved per period.

peri_CESB_app = sample(x = c(0:4), size = survey_size, replace = TRUE, 
                       prob = c(0.9561, 0.0119, 0.0108, 0.0110, 0.0102))

# The following is a simulation for the question " If you applied, how many /
# periods did you apply for CERB?". The multiple choice options for this /
# question are: a) N/A, b) 1, c) 2, d) 3, e) 4, f) 5, g) 6, h) 7

# The probability of each option was determined using statistics released in /
# relation to CERB. The percentage of the population that did not apply to /
# CERB was used for the zero option and the rest of the option probabilities /
# were allocated proportionally to how many applications were approved per /
# period.

peri_CERB_app = sample(x = c(0:7), size = survey_size, replace = TRUE, 
                       prob = c(0.759, 0.039, 0.039, 0.036, 0.032, 0.036, 
                                0.030, 0.029))

# The following is a simulation for the question "Do you believe that $1250 /
# payment of CESB a month is sustainable for students?". The multiple choice /
# options for this question are: a) Too little, b) Adequate, c) Too much.

# The probabilities of the options were determined using statistics collected /
# from an Ipsos survey done on behalf of Global News on public opinions about /
# COVID-19 financial relief response.

CESB_sustainability = sample(x= c("Too little", "Adequate", "Too much"),
                             size = survey_size, replace = TRUE, 
                             prob = c(0.3,0.5,0.2))

# The following is a simulation for the question "Do you believe that $2000 /
# payment of CERB a month is sustainable?". The multiple choice options for /
# this question are: a) Too little, b) Adequate, c) Too much.

# The probabilities of the options were determined using statistics collected /
# from an Ipsos survey done on behalf of Global News on public opinions about /
# COVID-19 financial relief response.

CERB_sustainability = sample(x= c("Too little", "Adequate", "Too much"),
                             size = survey_size, replace = TRUE, 
                             prob = c(0.4,0.35,0.25))


# The following is a simulation for the question "Do you believe the 16 weeks /
# period for CESB was long enough?". The multiple choice options for this /
# question are: a) Too short,  b) Adequate, c) Too long.

# The probabilities of the options were determined using statistics collected /
# from an Ipsos survey done on behalf of Global News on public opinions about /
# COVID-19 financial relief response.

peri_CESB_length = sample(x = c("Too Short", "Adequate", "Too Long"), 
                          size = survey_size, replace = TRUE, 
                          prob = c(0.52, 0.28, 0.20))

# The following is a simulation for the question "Do you believe the 28 weeks /
# period for CERB was long enough?". The multiple choice options for this /
# question are: a) Too short,  b) Adequate, c) Too long

# The probabilities of the options were determined using statistics collected /
# from an Ipsos survey done on behalf of Global News on public opinions about /
# COVID-19 financial relief response.

peri_CERB_length = sample(x = c("Too Short", "Adequate", "Too Long"), 
                          size = survey_size, replace = TRUE, 
                          prob = c(0.52, 0.28, 0.20))

# The following is a simulation for the question "On a scale of 1 to 5 how /
# satisfied are you with the 4 weeks extension of CERB?". The multiple choice /
# options for this question are: a) 1 (Highly unsatisfied), b) 2 (Unsatisfied) /
# c) 3 (Moderate), d) 4 (Satisfied), e) 5 (Highly unsatisfied).

# The probabilities of the options were determined using statistics collected /
# from an Ipsos survey done on behalf of Global News on public opinions about /
# COVID-19 financial relief response.

CERB_ext = sample(x= c("1 (Highly unsatisfied)", "2 (Unsatisfied)", 
                       "3 (Neutral)", "4 (Satisfied)", "5 (Highly satisfied)"),
                  size = survey_size, 
                  replace = TRUE, prob = c(0.03,0.05,0.2,0.22,0.1))

# The following is a simulation for the question "Do you believe that the 4 /
# weeks extension should have been applied to CESB as well?". The multiple /
# choice options for this question are: a) Yes, b) No.

# The probabilities of the options were determined using statistics collected /
# from an Ipsos survey done on behalf of Global News on public opinions about /
# COVID-19 financial relief response.

CESB_ext = sample(x= c("Yes", "No"), size = survey_size,
                  replace = TRUE, prob = c(0.75,0.25))

# The following is a simulation for question "What are your thoughts on /
# income restriction (earning less than $1000 a month) for CERB?". The multiple /
# choice options for this question are: a) Too restrictive, b) Fair, /
# c) Too lenient.

# The probabilities of the options were determined using statistics collected /
# from an Ipsos survey done on behalf of Global News on public opinions about /
# COVID-19 financial relief response.

inc_restr_CERB = sample(x = c("Too Restrictive", "Fair", "Too Lenient"), 
                        size = survey_size, replace = TRUE, 
                        prob = c(0.52, 0.28, 0.20))

# The following is a simulation for question "What are your thoughts on /
# income restriction (less than $1000 a month) for CESB?". The multiple choice /
# options for this question are: a) Too restrictive, b) Fair, c) Too lenient.

# The probabilities of the options were determined using statistics collected /
# from an Ipsos survey done on behalf of Global News on public opinions about /
# COVID-19 financial relief response.

inc_restr_CESB = sample(x = c("Too Restrictive", "Fair", "Too Lenient"), 
                        size = survey_size, replace = TRUE, 
                        prob = c(0.52, 0.28, 0.20))

# The following is a table which combines all of the simulated variables

survey_data <- tibble(age, pre_employment_status, curr_employment_status,
                      student_var, CESB_eligibility, CERB_eligibility, 
                      peri_CESB_app, peri_CERB_app, CESB_sustainability,
                      CERB_sustainability, peri_CESB_length, peri_CERB_length,
                      CERB_ext, CESB_ext, inc_restr_CERB, inc_restr_CESB)
survey_data

# The following are a series of graphs used for the analysis of the survey data:

# The following is a graph visualizing the distribution of responses to the/
# question "How old are you?".

age_graph <- 
  survey_data %>%
  select(age) %>%
  ggplot(aes(x=age)) +
  geom_bar() +
  labs(
    title = "Figure 1: Distribution of the Age of Respondents",
    x = "Age groups",
    y = "Number of Respondents",
    caption = "Source = Simulated Dataset"
  )

age_graph

# The following is a graph visualizing the distribution of responses to the/
# question "Do you believe that $1250 payment of CESB a month is sustainable /
# for students?".

CESB_sustainability_graph <- 
  survey_data %>%
  select(CESB_sustainability) %>%
  ggplot(aes(CESB_sustainability)) +
  geom_bar() +
  labs(
    title = "Figure 2: Distribution of Opinions on the Sustainability of CESB
    Payments",
    x = "Opinions on the sustainability of CESB payments",
    y = "Number of Respondents",
    caption = "Source = Simulated Dataset"
  ) 

CESB_sustainability_graph


# The following is a graph visualizing the distribution of responses to the/
# question "Do you believe that $2000 payment of CERB a month is sustainable?".

CERB_sustainability_graph <- 
  survey_data %>%
  select(CERB_sustainability) %>%
  ggplot(aes(CERB_sustainability)) +
  geom_bar() +
  labs(
    title = "Figure 3: Distribution of Opinions on the Sustainability of CERB
    Payments",
    x = "Opinions on the sustainability of CERB payments",
    y = "Number of Respondents",
    caption = "Source = Simulated Dataset"
  ) 

CERB_sustainability_graph

# The following is a graph visualizing the distribution of responses to the/
# question "If you applied, how many periods did you apply for CESB?".

peri_CESB_app_graph <- 
  survey_data %>%
  select(peri_CESB_app) %>%
  mutate(peri_CESB_app = as.factor(peri_CESB_app)) %>%
  ggplot(aes(x=peri_CESB_app)) +
  geom_bar() +
  labs(
    title = "Figure 4: Distribution of the Amount of Periods Respondents 
    Applied to CESB",
    x = "Number of CESB periods applied",
    y = "Number of Respondents",
    caption = "Source = Simulated Dataset"
  ) 

peri_CESB_app_graph

# The following is a graph visualizing the distribution of responses to the/
# question "If you applied, how many periods did you apply for CERB?".

peri_CERB_app_graph <- 
  survey_data %>%
  select(peri_CERB_app) %>%
  mutate(peri_CERB_app = as.factor(peri_CERB_app)) %>%
  ggplot(aes(x=peri_CERB_app)) +
  geom_bar() +
  labs(
    title = "Figure 5: Distribution of the Amount of Periods Respondents 
    Applied to CERB",
    x = "Number of CERB periods applied",
    y = "Number of Respondents",
    caption = "Source = Simulated Dataset"
  ) 

peri_CERB_app_graph

# The following is a graph visualizing the distribution of responses to the/
# question "What are your thoughts on income restriction (earning less than /
# $1000 a month) for CERB?".

inc_restr_CERB_graph <- 
  survey_data %>%
  select(inc_restr_CERB) %>%
  ggplot(aes(x=inc_restr_CERB)) +
  geom_bar() +
  labs(
    title = "Figure 6: Distribution of Opinions on the Income Restriction for
    CERB",
    x = "Opinions on the income restriction for CERB",
    y = "Number of Respondents",
    caption = "Source = Simulated Dataset"
  ) 

inc_restr_CERB_graph

# The following is a graph visualizing the distribution of responses to the/
# question "What are your thoughts on income restriction (earning less than /
# $1000 a month) for CESB?".

inc_restr_CESB_graph <- 
  survey_data %>%
  select(inc_restr_CESB) %>%
  
  ggplot(aes(x=inc_restr_CESB)) +
  geom_bar() +
  labs(
    title = "Figure 7: Distribution of Opinions on the Income Restriction for
    CESB",
    x = "Opinions on the income restriction for CESB",
    y = "Number of Respondents",
    caption = "Source = Simulated Dataset"
  ) 

inc_restr_CESB_graph


# The following is a graph visualizing the distribution of responses to the/
# question "Do you believe that $1250 payment of CESB a month is sustainable /
# for students?" across the various age groups.

age_CESB_sustainability_graph <- 
  survey_data %>%
  ggplot( aes(x = age, fill = CESB_sustainability)) + 
  geom_bar(position = "dodge") +
  labs(
    title = "Figure 8: Distribution of Opinions on the Sustainability of CESB
    Payments Across Age Groups",
    x = "Age Groups",
    y = "Number of Respondents",
    fill = "Opinions on sustainability of CESB payments",
    caption = "Source = Simulated Dataset")

age_CESB_sustainability_graph

# The following is a graph visualizing the distribution of responses to the/
# question "Do you believe that $2000 payment of CERB a month is sustainable?" /
# across the various age groups.

age_CERB_sustainability_graph <- 
  survey_data %>%
  ggplot( aes(x = age, fill = CERB_sustainability)) + 
  geom_bar(position = "dodge") +
  labs(
    title = "Figure 9: Distribution of Opinions on the Sustainability of CERB
    Payments Across Age Groups",
    x = "Age Groups",
    y = "Number of Respondents",
    fill = "Opinions on sustainability of CERB payments",
    caption = "Source = Simulated Dataset")

age_CERB_sustainability_graph

# The following is a graph visualizing the distribution of responses to the/
# question "What are your thoughts on income restriction (earning less than /
# $1000 a month) for CERB?" in relation to how many periods a respondent /
# applied for.

CERB_restr_peri_graph <- 
  survey_data %>%
  mutate(peri_CERB_app = as.factor(peri_CERB_app)) %>%
  ggplot( aes(x = peri_CERB_app, fill = inc_restr_CERB)) + 
  geom_bar(position = "dodge") +
  labs(
    title = "Figure 10: Distribution of Opinions on the Income Restriction for
    CERB Over Number of Periods Applied",
    x = "CERB Periods Applied",
    y = "Number of Respondents",
    fill = "Opinions on CERB income restriction",
    caption = "Source = Simulated Dataset")

CERB_restr_peri_graph

# The following is a graph visualizing the distribution of responses to the/
# question "What are your thoughts on income restriction (earning less than /
# $1000 a month) for CSRB?" in relation to how many periods a respondent /
# applied for.

CESB_restr_peri_graph <- 
  survey_data %>%
  mutate(peri_CESB_app = as.factor(peri_CESB_app)) %>%
  ggplot( aes(x = peri_CESB_app, fill = inc_restr_CESB)) + 
  geom_bar(position = "dodge") +
  labs(
    title = "Figure 11: Distribution of Opinions on the Income Restriction for
    CESB Over Number of Periods Applied",
    x = " CESB Periods Applied",
    y = "Number of Respondents",
    fill = "Opinions on CESB income restriction",
    caption = "Source = Simulated Dataset")

CESB_restr_peri_graph







