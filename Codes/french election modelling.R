library(tidyverse)
library(readxl)
library(plotly)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(purrr)
library(sf)
library(INLA)
library(spdep) # adjacency matrix
rm(list = ls())

# ========================================================================================================
# ========================================= Using updated dataset ========================================
# ========================================================================================================
# reading the dataset
df_merged <- readRDS("df_merged.rds")
deps <- readRDS("departments.rds")

# Extracting adjacency matrix
g <- poly2nb(deps)

# Visualising Adjacency matrix based on the row's number
image(inla.graph2matrix(g), xlab = NULL, ylab = NULL, main = "Adjacency Matrix")

# adding a column for row's number
df_merged <- df_merged %>%
  ungroup() %>% 
  mutate(code = 1:nrow(.)) %>%
  select(-nom) 

# data preparation for converting multinomial into poisson dist.
df_final <- df_merged %>%
  #st_drop_geometry() %>%
  select(-geometry) %>%
  pivot_longer(
    names_to = "candidate", values_to = "vote",
    -c(code, white_collar_rate:life_expectancy,im_rate)
  ) #%>% 
  #filter(candidate %in% c('LE PEN','MACRON', 'MÉLENCHON'))

# preparing a dictionary for recognizing the candidates
cand_code <- unique(df_final$candidate)
candidate_code <- data.frame(candidate = cand_code, id = seq(1, length(cand_code), 1))

df_final <- left_join(df_final, candidate_code)


# adding spatial effects to the dataset
df_final$sp_effect1 <- NA # macron
df_final$sp_effect2 <- NA  # Lepen
df_final$sp_effect3 <- NA # melenchon

df_final <- df_final %>%
  mutate(
    sp_effect1 = ifelse(id == 3, code, sp_effect1),
    sp_effect2 = ifelse(id == 5, code, sp_effect2),
    sp_effect3 = ifelse(id == 7, code, sp_effect3))

# Checking correlations
# df_final1 <- df_final %>% 
#   mutate(white_collar_rate = as.numeric(white_collar_rate),
#          unemployment_rate = as.numeric(unemployment_rate),
#          poverty_rate = as.numeric(poverty_rate),
#          life_expectancy = as.numeric(life_expectancy),
#          higher_education_rate = as.numeric(higher_education_rate),
#          im_rate = as.numeric(im_rate)
#          )
# 
# corrr <- df_final1 %>%
#   select(white_collar_rate, unemployment_rate, poverty_rate, life_expectancy, higher_education_rate, im_rate) 
# 
# corrplot(cor(corrr), order = "hclust", type = "upper")

# creating id for random effects(bcz inla works this way)
df_final <- df_final %>% mutate(
  id_wh = id,
  id_un = id,
  id_po = id,
  id_li = id,
  id_ed = id,
  id_im = id
)


# har kudum az in f ha ye random effecte age dakhele f chizi nanvisi fek mikone iid hast
# fixed=t yani estimate nashe un hyperparameter, constr=t yani sum coef ha 0 she (sum to zero constraint for this model)
# formula.spatial = vote ~ -1 +
#  f(code, initial = -10, fixed = T) +
#  f(id_im, percent_imm_dep, fixed = T, constr = T) +
#  f(id_po, poverty_rate, fixed = T, constr = T) +
#  f(id_ex, avg_life, fixed = T, constr = T) +
#  f(id_ed, people_education, fixed = T, constr = T)  +
#  f(id_wa, SNHM14, fixed = T, constr = T) +
#  f(id_un, unemployment_rate, fixed = T, constr = T) +
#  f(id_el, elderly_rate, fixed = T, constr = T) +
#  f(id_wh, whitecol_rate, fixed = T, constr = T) +
#  f(id_ag, agri_rate, fixed = T, constr = T) +
#  f(sp_effect, model = 'besag', graph = g) + #spatial random effect
#  f(sp_effect2, model = 'besag', graph = g) #+
# f(sp_effect3, model = 'besag', graph = g) #+
# f(sp_effect4, model = 'besag', graph = g)
# f(id_ed, people_education, fixed = T, constr = T)


formula.spatial <- vote ~ -1 +
  f(code, initial = -10, fixed = T) +
  f(id_wh, white_collar_rate, fixed = T, constr = T) +
  f(id_un, unemployment_rate, fixed = T, constr = T) +
  f(id_po, poverty_rate, fixed = T, constr = T) +
  f(id_li, life_expectancy, fixed = T, constr = T) +
  f(id_ed, higher_education_rate, fixed = T, constr = T) +
  f(id_im, im_rate, fixed = T, constr = T) +
  f(sp_effect1, model = "besag", graph = g) + # spatial random effect
  f(sp_effect2, model = "besag", graph = g) #+ 
  # f(sp_effect3, model = "besag", graph = g)


######################## Training fit ##################

# Fit the model
model <- inla(formula.spatial,
              family = "poisson",
              data = df_final,
              control.predictor = list(compute = TRUE) #link = 1, 
)

# Checking model fit on training data, lambda=unility func
df_accuracy <- df_final %>%
  mutate(lambda = model$summary.fitted.values[1:nrow(.), "mode"],
         lambda_low = model$summary.fitted.values[1:nrow(.), "0.025quant"],
         lambda_high = model$summary.fitted.values[1:nrow(.), "0.975quant"]) #%>%
  # group_by(code) %>%
  # mutate(
  #   total_votes = sum(vote),
  #   total_lambda = sum(lambda),
  #   real_probs = vote / total_votes,
  #   pred_probs = lambda / total_lambda
  # ) %>%
  # ungroup()

plt_theme = theme(
  axis.text.x = element_text(size = 15, angle = 0, margin = margin(t = 6)),
  axis.text.y = element_text(size = 15, margin = margin(t = 6)),
  legend.text = element_text(family = "Arial",size = 15),
  legend.title = element_text(size = 15, face = "bold"),
  legend.spacing.x = unit(1,"cm"),
  axis.title.y = element_text(family = "Arial",size = 12, face = "bold"),
  axis.title.x = element_text(family = "Arial",size = 12, face = "bold", 
                              margin = margin(t = 10, r = 0, b = 0, l = 0)),
  title = element_text(face = 'bold'))


up_lim = max(df_accuracy[,c("vote", "lambda")], na.rm = T) 
low_lim = min(df_accuracy[,c("vote", "lambda")], na.rm = T) 

# df_accuracy %>% 
#   #mutate(across(c(vote,lambda, lambda_low, lambda_high), function(x) log(x))) %>% 
#   ggplot(aes(vote, lambda)) +
#   #geom_point() +
#   geom_linerange(aes(ymin=lambda_low, ymax=lambda_high),color = '#112f5f') + 
#   geom_abline(slope=1, color = "darkblue") +
#   #coord_cartesian(ylim = c(low_lim,up_lim), xlim = c(low_lim,up_lim)) + 
#   labs(x = 'Observed', y = 'Fitted') + 
#   theme_bw() +
#   plt_theme

summary(df_accuracy$real_probs - df_accuracy$pred_probs)



# 
# test = data.frame(x = x , x_low = x_low, x_high = x_high, y = y)
# test %>% 
#   ggplot(aes(y, x)) +
#   geom_point() +
#   geom_linerange(aes(ymin=x_low, ymax=x_high),color = '#112f5f') + 
#   geom_abline(slope=1, color = "darkblue") +
#   theme_bw() +
#   plt_theme


# df_accuracy %>% 
#   mutate(across(c(vote,lambda, lambda_low, lambda_high), function(x) log(x))) %>% 
#   mutate(diff = lambda_high - lambda_low) %>% 
#   pull(diff) %>% 
#   summary()

# 
# test = df_accuracy %>%
#   #as_tibble() %>%
#   mutate(across(c(vote,lambda, lambda_low, lambda_high), function(x) log(x)))

# Confusion matrix on training data
df_conf_train <- df_accuracy %>%
  group_by(code) %>%
  summarise(
    winner_real = .$candidate[which(vote == max(vote))],
    winner_pred = .$candidate[which(lambda == max(lambda))]
  )



train_conf_tabl <- table(df_conf_train$winner_real, df_conf_train$winner_pred)
train_conf_tabl
sum(diag(train_conf_tabl)) / sum(train_conf_tabl) # Accuracy, sum of diagonal values / total


# =============================================TEST SET=======================================================

# Evaluating model performance on test data
set.seed(1)
samp <- sample(1:length(unique(df_final$code)), 30) # Selecting 30 departments at random

df_test <- df_final %>% # NA bezar tuye departemanayi ke tu sample hastan
  mutate(
    department = code,
    vote = ifelse(code %in% samp, NA, vote),
    code = ifelse(code %in% samp, NA, code)
  )


model.test <- inla(formula.spatial,
                   family = "poisson",
                   data = df_test,
                   control.predictor = list(link = 1, compute = TRUE)
)


df_pred <- df_test %>%
  mutate(lambda = model.test$summary.fitted.values[1:nrow(.), "mode"]) %>%
  group_by(department) %>%
  mutate(
    total_lambda = sum(lambda),
    pred_probs = round(lambda / total_lambda, 4)
  ) %>%
  ungroup() %>%
  select(department, id, pred_probs) %>%
  filter(department %in% samp)


df_real <- df_final %>%
  group_by(code) %>%
  mutate(
    total_votes = sum(vote),
    true_probs = round(vote / total_votes, 4),
    department = code
  ) %>%
  ungroup() %>%
  select(department, id, true_probs) %>%
  filter(department %in% samp)


test2 <- left_join(df_real, df_pred) %>% mutate(diff = (true_probs - pred_probs)^2)
summary(test2$diff)
sqrt(mean(test2$diff))

# Accuracy by confusion matrix
real_winners <- df_final %>%
  filter(code %in% samp) %>%
  select(code, candidate, vote) %>%
  group_by(code) %>%
  summarise(winner = .$candidate[which(vote == max(vote))]) %>%
  rename(department = code) %>%
  ungroup()


test <- left_join(df_pred, candidate_code) %>%
  group_by(department) %>%
  summarise(winner_pred = as.character(.$candidate[which(pred_probs == max(pred_probs))])) %>%
  ungroup()

t <- full_join(real_winners, test)

sum(diag(table(t$winner, t$winner_pred))) / sum(table(t$winner, t$winner_pred))
t1 <- table(t$winner, t$winner_pred)
t1


#############################################################################
# interpretation of results
#############################################################################
candid_pairs = c( 'Macron / Le Pen', 'Melenchon / Le Pen', 'Melenchon / Macron')

# immigration

E_ML_im <- model$summary.random$id_im$`0.5quant`[3] - model$summary.random$id_im$`0.5quant`[5] # macron - lepen
lower_ML_im <- E_ML_im - 2 * sqrt(model$summary.random$id_im$sd[3]^2 + model$summary.random$id_im$sd[5]^2)
upper_ML_im <- E_ML_im + 2 * sqrt(model$summary.random$id_im$sd[3]^2 + model$summary.random$id_im$sd[5]^2)

E_MeL_im <- model$summary.random$id_im$`0.5quant`[7] - model$summary.random$id_im$`0.5quant`[5] # melenchon - lepen
lower_MeL_im <- E_MeL_im - 2 * sqrt(model$summary.random$id_im$sd[7]^2 + model$summary.random$id_im$sd[5]^2)
upper_MeL_im <- E_MeL_im + 2 * sqrt(model$summary.random$id_im$sd[7]^2 + model$summary.random$id_im$sd[5]^2)

E_MeM_im <- model$summary.random$id_im$`0.5quant`[7] - model$summary.random$id_im$`0.5quant`[3] # melenchon - macron
lower_MeM_im <- E_MeM_im - 2 * sqrt(model$summary.random$id_im$sd[7]^2 + model$summary.random$id_im$sd[3]^2)
upper_MeM_im <- E_MeM_im + 2 * sqrt(model$summary.random$id_im$sd[7]^2 + model$summary.random$id_im$sd[3]^2)

im_expected_effect = c(E_ML_im, E_MeL_im, E_MeM_im)
im_lower_effect = c(lower_ML_im, lower_MeL_im, lower_MeM_im)
im_upper_effect = c(upper_ML_im, upper_MeL_im, upper_MeM_im)


df_im_effect = data.frame(cand_pairs = candid_pairs, 
                          expected = exp(im_expected_effect), 
                          lower_bound = exp(im_lower_effect), 
                          upper_bound = exp(im_upper_effect)) %>% 
  mutate(percent_expected = (expected-1)*100,
         percent_lwoer = (lower_bound-1)*100,
         percent_upper = (upper_bound-1)*100)

plot1 <- df_im_effect %>% 
  ggplot(aes(cand_pairs, percent_expected)) + #,  label = round(expected, 2)
  geom_linerange(aes(ymin = percent_lwoer, ymax = percent_upper), color = "cyan4", alpha = 0.7, size = 1.5) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_point(color = "orange", size = 2) +
  #scale_y_continuous(limits = c(-1, 18.62)) +
  labs( x = NULL, y = "Immigration Rate Effect (%)") +
  theme_bw() + #geom_text(hjust= 1.2) +
  theme(axis.text.x = element_text(angle = 60, margin = margin(t = 35), face = 'bold'),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"))


##############################################################
# life expectancy

E_ML_li <- model$summary.random$id_li$`0.5quant`[3] - model$summary.random$id_li$`0.5quant`[5] # macron - lepen
lower_ML_li <- E_ML_li - 2 * sqrt(model$summary.random$id_li$sd[3]^2 + model$summary.random$id_li$sd[5]^2)
upper_ML_li <- E_ML_li + 2 * sqrt(model$summary.random$id_li$sd[3]^2 + model$summary.random$id_li$sd[5]^2)

E_MeL_li <- model$summary.random$id_li$`0.5quant`[7] - model$summary.random$id_li$`0.5quant`[5] # melenchon - lepen
lower_MeL_li <- E_MeL_li - 2 * sqrt(model$summary.random$id_li$sd[7]^2 + model$summary.random$id_li$sd[5]^2)
upper_MeL_li <- E_MeL_li + 2 * sqrt(model$summary.random$id_li$sd[7]^2 + model$summary.random$id_li$sd[5]^2)

E_MeM_li <- model$summary.random$id_li$`0.5quant`[7] - model$summary.random$id_li$`0.5quant`[3] # melenchon - macron
lower_MeM_li <- E_MeM_li - 2 * sqrt(model$summary.random$id_li$sd[7]^2 + model$summary.random$id_li$sd[3]^2)
upper_MeM_li <- E_MeM_li + 2 * sqrt(model$summary.random$id_li$sd[7]^2 + model$summary.random$id_li$sd[3]^2)

li_expected_effect = c(E_ML_li, E_MeL_li, E_MeM_li)
li_lower_effect = c(lower_ML_li, lower_MeL_li, lower_MeM_li)
li_upper_effect = c(upper_ML_li, upper_MeL_li, upper_MeM_li)


df_li_effect = data.frame(cand_pairs = candid_pairs, 
                          expected = exp(li_expected_effect), 
                          lower_bound = exp(li_lower_effect), 
                          upper_bound = exp(li_upper_effect)) %>% 
  mutate(percent_expected = (expected-1)*100,
         percent_lwoer = (lower_bound-1)*100,
         percent_upper = (upper_bound-1)*100)

plot2 <- df_li_effect %>% 
  ggplot(aes(cand_pairs, percent_expected)) + #,  label = round(expected, 2)
  geom_linerange(aes(ymin = percent_lwoer, ymax = percent_upper), color = "cyan4", alpha = 0.7, size = 1.5) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_point(color = "orange", size = 2) +
  #scale_y_continuous(limits = c(-1, 18.62)) +
  labs( x = NULL, y = "Life Expectancy (year) Effect") +
  theme_bw() + #geom_text(hjust= 1.2) +
  theme(axis.text.x = element_text(angle = 60, margin = margin(t = 35), face = 'bold'),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"))
##############################################################
# poverty rate

E_ML_po <- model$summary.random$id_po$`0.5quant`[3] - model$summary.random$id_po$`0.5quant`[5] # macron - lepen
lower_ML_po <- E_ML_po - 2 * sqrt(model$summary.random$id_po$sd[3]^2 + model$summary.random$id_po$sd[5]^2)
upper_ML_po <- E_ML_po + 2 * sqrt(model$summary.random$id_po$sd[3]^2 + model$summary.random$id_po$sd[5]^2)

E_MeL_po <- model$summary.random$id_po$`0.5quant`[7] - model$summary.random$id_po$`0.5quant`[5] # melenchon - lepen
lower_MeL_po <- E_MeL_po - 2 * sqrt(model$summary.random$id_po$sd[7]^2 + model$summary.random$id_po$sd[5]^2)
upper_MeL_po <- E_MeL_po + 2 * sqrt(model$summary.random$id_po$sd[7]^2 + model$summary.random$id_po$sd[5]^2)

E_MeM_po <- model$summary.random$id_po$`0.5quant`[7] - model$summary.random$id_po$`0.5quant`[3] # melenchon - macron
lower_MeM_po <- E_MeM_po - 2 * sqrt(model$summary.random$id_po$sd[7]^2 + model$summary.random$id_po$sd[3]^2)
upper_MeM_po <- E_MeM_po + 2 * sqrt(model$summary.random$id_po$sd[7]^2 + model$summary.random$id_po$sd[3]^2)


po_expected_effect = c(E_ML_po, E_MeL_po, E_MeM_po)
po_lower_effect = c(lower_ML_po, lower_MeL_po, lower_MeM_po)
po_upper_effect = c(upper_ML_po, upper_MeL_po, upper_MeM_po)


df_po_effect = data.frame(cand_pairs = candid_pairs, 
                          expected = exp(po_expected_effect), 
                          lower_bound = exp(po_lower_effect), 
                          upper_bound = exp(po_upper_effect)) %>% 
  mutate(percent_expected = (expected-1)*100,
         percent_lwoer = (lower_bound-1)*100,
         percent_upper = (upper_bound-1)*100)

plot3 <- df_po_effect %>% 
  ggplot(aes(cand_pairs, percent_expected)) + #,  label = round(expected, 2)
  geom_linerange(aes(ymin = percent_lwoer, ymax = percent_upper), color = "cyan4", alpha = 0.7, size = 1.5) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_point(color = "orange", size = 2) +
  #scale_y_continuous(limits = c(-1, 18.62)) +
  labs( x = NULL, y = "Poverty Rate Effect (%)") +
  theme_bw() + #geom_text(hjust= 1.2) +
  theme(axis.text.x = element_text(angle = 60, margin = margin(t = 35), face = 'bold'),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"))
##############################################################
# education

E_ML_ed <- model$summary.random$id_ed$`0.5quant`[3] - model$summary.random$id_ed$`0.5quant`[5] # macron - lepen
lower_ML_ed <- E_ML_ed - 2 * sqrt(model$summary.random$id_ed$sd[3]^2 + model$summary.random$id_ed$sd[5]^2)
upper_ML_ed <- E_ML_ed + 2 * sqrt(model$summary.random$id_ed$sd[3]^2 + model$summary.random$id_ed$sd[5]^2)

E_MeL_ed <- model$summary.random$id_ed$`0.5quant`[7] - model$summary.random$id_ed$`0.5quant`[5] # melenchon - lepen
lower_MeL_ed <- E_MeL_ed - 2 * sqrt(model$summary.random$id_ed$sd[7]^2 + model$summary.random$id_ed$sd[5]^2)
upper_MeL_ed <- E_MeL_ed + 2 * sqrt(model$summary.random$id_ed$sd[7]^2 + model$summary.random$id_ed$sd[5]^2)

E_MeM_ed <- model$summary.random$id_ed$`0.5quant`[7] - model$summary.random$id_ed$`0.5quant`[3] # melenchon - macron
lower_MeM_ed <- E_MeM_ed - 2 * sqrt(model$summary.random$id_ed$sd[7]^2 + model$summary.random$id_ed$sd[3]^2)
upper_MeM_ed <- E_MeM_ed + 2 * sqrt(model$summary.random$id_ed$sd[7]^2 + model$summary.random$id_ed$sd[3]^2)

ed_expected_effect = c(E_ML_ed, E_MeL_ed, E_MeM_ed)
ed_lower_effect = c(lower_ML_ed, lower_MeL_ed, lower_MeM_ed)
ed_upper_effect = c(upper_ML_ed, upper_MeL_ed, upper_MeM_ed)


df_ed_effect = data.frame(cand_pairs = candid_pairs, 
                          expected = exp(ed_expected_effect), 
                          lower_bound = exp(ed_lower_effect), 
                          upper_bound = exp(ed_upper_effect)) %>% 
  mutate(percent_expected = (expected-1)*100,
         percent_lwoer = (lower_bound-1)*100,
         percent_upper = (upper_bound-1)*100)

plot4 <- df_ed_effect %>% 
  ggplot(aes(cand_pairs, percent_expected)) + #,  label = round(expected, 2)
  geom_linerange(aes(ymin = percent_lwoer, ymax = percent_upper), color = "cyan4", alpha = 0.7, size = 1.5) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_point(color = "orange", size = 2) +
  #scale_y_continuous(limits = c(-1, 18.62)) +
  labs( x = NULL, y = "Higher Education Rate Effect (%)") +
  theme_bw() + #geom_text(hjust= 1.2) +
  theme(axis.text.x = element_text(angle = 60, margin = margin(t = 35), face = 'bold'),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"))
##############################################################

# unemployment rate

E_ML_un <- model$summary.random$id_un$`0.5quant`[3] - model$summary.random$id_un$`0.5quant`[5] # macron - lepen
lower_ML_un <- E_ML_un - 2 * sqrt(model$summary.random$id_un$sd[3]^2 + model$summary.random$id_un$sd[5]^2)
upper_ML_un <- E_ML_un + 2 * sqrt(model$summary.random$id_un$sd[3]^2 + model$summary.random$id_un$sd[5]^2)


E_MeL_un <- model$summary.random$id_un$`0.5quant`[7] - model$summary.random$id_un$`0.5quant`[5] # melenchon - lepen
lower_MeL_un <- E_MeL_un - 2 * sqrt(model$summary.random$id_un$sd[7]^2 + model$summary.random$id_un$sd[5]^2)
upper_MeL_un <- E_MeL_un + 2 * sqrt(model$summary.random$id_un$sd[7]^2 + model$summary.random$id_un$sd[5]^2)


E_MeM_un <- model$summary.random$id_un$`0.5quant`[7] - model$summary.random$id_un$`0.5quant`[3] # melenchon - macron
lower_MeM_un <- E_MeM_un - 2 * sqrt(model$summary.random$id_un$sd[7]^2 + model$summary.random$id_un$sd[3]^2)
upper_MeM_un <- E_MeM_un + 2 * sqrt(model$summary.random$id_un$sd[7]^2 + model$summary.random$id_un$sd[3]^2)


un_expected_effect = c(E_ML_un, E_MeL_un, E_MeM_un)
un_lower_effect = c(lower_ML_un, lower_MeL_un, lower_MeM_un)
un_upper_effect = c(upper_ML_un, upper_MeL_un, upper_MeM_un)


df_un_effect = data.frame(cand_pairs = candid_pairs, 
                          expected = exp(un_expected_effect), 
                          lower_bound = exp(un_lower_effect), 
                          upper_bound = exp(un_upper_effect)) %>% 
  mutate(percent_expected = (expected-1)*100,
         percent_lwoer = (lower_bound-1)*100,
         percent_upper = (upper_bound-1)*100)

plot5 <- df_un_effect %>% 
  ggplot(aes(cand_pairs, percent_expected)) + #,  label = round(expected, 2)
  geom_linerange(aes(ymin = percent_lwoer, ymax = percent_upper), color = "cyan4", alpha = 0.7, size = 1.5) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_point(color = "orange", size = 2) +
  #scale_y_continuous(limits = c(-1, 18.62)) +
  labs( x = "Vote Ratio", y = "Unemployment Rate Effect (%)") +
  theme_bw() + #geom_text(hjust= 1.2) +
  theme(axis.text.x = element_text(angle = 60, margin = margin(t = 35), face = 'bold'),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"))
######################################################################
# white collar

E_ML_wh <- model$summary.random$id_wh$`0.5quant`[3] - model$summary.random$id_wh$`0.5quant`[5] # macron - lepen
lower_ML_wh <- E_ML_wh - 2 * sqrt(model$summary.random$id_wh$sd[3]^2 + model$summary.random$id_wh$sd[5]^2)
upper_ML_wh <- E_ML_wh + 2 * sqrt(model$summary.random$id_wh$sd[3]^2 + model$summary.random$id_wh$sd[5]^2)

E_MeL_wh <- model$summary.random$id_wh$`0.5quant`[7] - model$summary.random$id_wh$`0.5quant`[5] # melenchon - lepen
lower_MeL_wh <- E_MeL_wh - 2 * sqrt(model$summary.random$id_wh$sd[7]^2 + model$summary.random$id_wh$sd[5]^2)
upper_MeL_wh <- E_MeL_wh + 2 * sqrt(model$summary.random$id_wh$sd[7]^2 + model$summary.random$id_wh$sd[5]^2)

E_MeM_wh <- model$summary.random$id_wh$`0.5quant`[7] - model$summary.random$id_wh$`0.5quant`[3] # melenchon - macron
lower_MeM_wh <- E_MeM_wh - 2 * sqrt(model$summary.random$id_wh$sd[7]^2 + model$summary.random$id_wh$sd[3]^2)
upper_MeM_wh <- E_MeM_wh + 2 * sqrt(model$summary.random$id_wh$sd[7]^2 + model$summary.random$id_wh$sd[3]^2)


wh_expected_effect = c(E_ML_wh, E_MeL_wh, E_MeM_wh)
wh_lower_effect = c(lower_ML_wh, lower_MeL_wh, lower_MeM_wh)
wh_upper_effect = c(upper_ML_wh, upper_MeL_wh, upper_MeM_wh)


df_wh_effect = data.frame(cand_pairs = candid_pairs, 
                          expected = exp(wh_expected_effect), 
                          lower_bound = exp(wh_lower_effect), 
                          upper_bound = exp(wh_upper_effect)) %>% 
  mutate(percent_expected = (expected-1)*100,
         percent_lwoer = (lower_bound-1)*100,
         percent_upper = (upper_bound-1)*100)

plot6 <- df_wh_effect %>% 
  ggplot(aes(cand_pairs, percent_expected)) + #,  label = round(expected, 2)
  geom_linerange(aes(ymin = percent_lwoer, ymax = percent_upper), color = "cyan4", alpha = 0.7, size = 1.5) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_point(color = "orange", size = 2) +
  #scale_y_continuous(limits = c(-1, 18.62)) +
  labs( x = "Vote Ratio", y = "White-Collar Rate Effect (%)") +
  theme_bw() + #geom_text(hjust= 1.2) +
  theme(axis.text.x = element_text(angle = 60, margin = margin(t = 35), face = 'bold'),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"))


df_wh_effect %>% 
  ggplot(aes(percent_expected, cand_pairs)) + #,  label = round(expected, 2)
  geom_linerange(aes(xmin = percent_lwoer, xmax = percent_upper), color = "cyan4", alpha = 0.7, size = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(color = "orange", size = 2) +
  #scale_y_continuous(limits = c(-1, 18.62)) +
  labs( x = "Vote Ratio", y = "White-Collar Rate Effect (%)") +
  theme_bw() + #geom_text(hjust= 1.2) +
  theme(axis.text.x = element_text( face = 'bold'),
        axis.text.y = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"))
####################################################

# saving all plots as one

totalplot <- ggarrange(plot1 , plot2 , plot3, plot4,plot5, plot6, nrow = 3, ncol = 2, common.legend = FALSE)
#ggsave("plottotal.png", height = 8, width =10)


##########################################################################
#  visualization of the random effects on the map
##########################################################################
library(leaflet)

# Mapping Spatial Effects for lepen
sp1 <- model$summary.random$sp_effect$`0.5quant`
df_merged$sp1 <- sp1


scale_range <- c(-0.7, 0.7)

pal <- colorNumeric("RdBu", domain = scale_range)

df_merged = st_as_sf(df_merged)
leaflet() %>%
  addTiles() %>% setView(2.853, 47.047,zoom = 5) %>% #mape kolie kore zamin
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(data = df_merged, fillColor = ~pal(df_merged$sp1), 
              fillOpacity = 0.8,
              group = "Votes",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              popup = ~paste0(sp1),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, values = df_merged$sp1, opacity = 0.7) 

# Mapping Spatial Effects for macron
sp2 <- model$summary.random$sp_effect2$`0.5quant`
df_merged$sp2 <- sp2


scale_range <- c(-0.5, 0.5)

pal <- colorNumeric("RdBu", domain = scale_range)


leaflet() %>%
  addTiles() %>% setView(2.853, 47.047,zoom = 5) %>% #mape kolie kore zamin
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(data = df_merged, fillColor = ~pal(df_merged$sp2), 
              fillOpacity = 0.8,
              group = "Votes",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              popup = ~paste0(sp2),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, values = df_merged$sp2, opacity = 0.7) 


# Mapping first candidate predictions
df_merged <- left_join(df_merged, df_conf_train)
df_merged <- df_merged %>% 
  mutate(pred_status = ifelse(winner_real == winner_pred, 1, 0))

scale_range <- c(-0.5, 1.5)

pal <- colorNumeric("RdBu", domain = scale_range)


leaflet() %>%
  addTiles() %>% setView(2.853, 47.047,zoom = 5) %>% #mape kolie kore zamin
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(data = df_merged, fillColor = ~pal(df_merged$pred_status), 
              fillOpacity = 0.8,
              group = "Votes",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              popup = ~paste0(code),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))

df_missed <- df_merged %>% filter(pred_status == 0)
df_missed2 <- df_accuracy %>% filter(code == 13 | code == 28 | code == 31 | code == 64 | code == 93)

