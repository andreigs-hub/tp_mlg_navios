
library(tidyverse)
library(tidymodels)
library(reglin)
library(pROC)

data("penguins")
glimpse(penguins)

pinguins <- penguins |>
  drop_na()

glimpse(pinguins)

fct_count(pinguins$sex, prop = TRUE)

fit <- glm(
  sex ~ species + island + bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
  data = pinguins,
  family = "binomial"
)

probito <- glm(
  sex ~ species + island + bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
  data = pinguins,
  family = binomial(link = 'probit')
)

set.seed(123)
split <- initial_split(pinguins, prop = 0.75, strata = sex)
split

treino <- training(split)
teste <- testing(split)


# Validação cruzada repetida
set.seed(123)

vfolds <- vfold_cv(
  treino,
  v = 10,
  repeats = 5,
  strata = sex
)


# criando receitas:

receita <- recipe(
  sex ~ species + island + bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g,
  data = treino
  ) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())

# visualizando a fórmula da receita:
receita |>
  prep() |>
  formula()


# Regressão logística (default)
logit <- logistic_reg() |>
  set_engine("glm")

# Regressão probit
probit <- logistic_reg() |>
  set_engine(
    "glm",
    family = stats::binomial(link = "probit")
  )

# Regressão complemento log-log
cloglog <- logistic_reg() |>
  set_engine(
    "glm",
    family = stats::binomial(link = "cloglog")
  )



modelos <- workflow_set(
  preproc = list(
    receita = receita  # é possível considerar simulataneamente várias receitas aqui
  ),
  models = list(
    logit = logit,
    probit = probit,
    cloglog = cloglog
  )
)

modelos



# Métricas probabilísticas
metricas_prob <- metric_set(
  roc_auc,
  brier_class,
  accuracy

)

# Controle da validação cruzada
ctrl <- control_resamples(
  save_pred = TRUE
)

# Ajuste dos três modelos
set.seed(123)

resultados <- workflow_map(
  modelos,
  fn = "fit_resamples",
  resamples = vfolds,
  metrics = metricas_prob,
  control = ctrl,
  verbose = TRUE
)

resultados




metricas_cv <- collect_metrics(resultados)

metricas_cv |>
  filter(.metric == "accuracy") |>
  select(wflow_id, .metric, mean, std_err) |>
  arrange(desc(mean))

metricas_cv |>
  filter(.metric == "roc_auc") |>
  select(wflow_id, .metric, mean, std_err) |>
  arrange(desc(mean))

metricas_cv |>
  filter(.metric == "brier_class") |>
  select(wflow_id, .metric, mean, std_err) |>
  arrange(mean)



# seleção do melhor modelo (de acordo com a curva roc):
best_model <- metricas_cv |>
  filter(.metric == "roc_auc") |>
  slice_max(
    order_by = mean,
    n = 1,
    with_ties = FALSE
  )

best_model

# extraindo o identificador do melhor modelo:
best_id <- best_model |>
  pull(wflow_id)
best_id

# extraindo o melhor modelo:
best <- extract_workflow(
  resultados,
  id = best_id
)


# ajuste do modelo final:
final <- best |>
  fit(data = treino)

# sumarisando o modelo ajustado:
tidy(final)


# extraindo o ajuste na versão glm:
final_glm <- final |>
  extract_fit_parsnip() |>
  extract_fit_engine()

class(final_glm)

# análise de resíduos do modelo final:
ggresiduals(final_glm)




# plotando a curva roc com o tidymodels:

# 1. Obter as probabilidades preditas no conjunto de teste
preds_teste <- predict(final, new_data = teste, type = "prob") |> 
  bind_cols(teste |> select(sex))

head(preds_teste)

# 2. Calcular a curva ROC usando a função do pacote yardstick (parte do tidymodels)

# Por padrão, o pacote yardstick assume que a primeira categoria (nível) do fator 
# da sua coluna real (truth) é a classe positiva (event_level = "first").

dados_curva_roc <- preds_teste |> 
  roc_curve(truth = sex, .pred_male)

# 3. Plotar usando o autoplot() padrão
autoplot(dados_curva_roc)

preds_teste |> 
  roc_auc(truth = sex, .pred_male)


# forma correta:
preds_teste |> 
  roc_auc(
    truth = sex, 
    .pred_male, 
    event_level = "second"
  )

preds_teste %>%
  roc_curve(
    truth = sex,     
    .pred_male,        
    event_level = "second"       
  ) %>%
  autoplot()


# alternativamente (utilizando pacote pROC):

# 1. Extrair as probabilidades na forma de vetor numérico
prob_sim <- predict(final, new_data = teste, type = "prob")$.pred_male

# 2. Criar o objeto ROC
roc_obj <- roc(response = teste$sex, predictor = prob_sim, levels = c("female", "male"))

# 3. Plotar a curva
plot(roc_obj, col = "blue")
print(auc(roc_obj)) # Exibe a Área Sob a Curva (AUC) no console
