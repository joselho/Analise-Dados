################################################################################
#         EXERCÍCIOS DO LIVRO DO FÁVERO - CAP 13 - REGRESSÃO LOGÍSTICA         #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet","foreign")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

Qui2 <- function(x) {
  maximo <- logLik(x)
  minimo <- logLik(update(x, ~1, trace = F))
  Qui.Quadrado <- -2*(minimo - maximo)
  pvalue <- pchisq(Qui.Quadrado, df = 1, lower.tail = F)
  df <- data.frame()
  df <- cbind.data.frame(Qui.Quadrado, pvalue)
  return(df)
}

################################################################################
#
# 1. Crescimento do PIB e/r ao investimento em educação de um país (em 15 anos) 

default <- read.dta("Default.dta")
default %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)
summary(default)
glimpse(default)

modelo.default <- glm(formula =  default ~ . ,
                      data = default, family = "binomial")
summary(modelo.default)
summ(model = modelo.default, confint = T, digits = 4, ci.width = 0.95)
logLik(modelo.default)
Qui2(modelo.default)

# Modelo passa no teste Qui-quadrado e todas as variáveis passam nos testes z
# Prob(defalt) = 1 / 1 + exp(-Logito), onde
# Logito = 2.9751 -0.0243*idade + 0.7415*sexo[0,1] - 0.0003*renda

default$phat <- modelo.default$fitted.values

default %>% 
  group_by(sexo) %>% 
  summarise(n=n(), Media=mean(phat)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)
# Em média, pessoas do sexo masculino tem probabilidade maior de default,
# 79,55% em relação a pessoas do sexo feminino, de 67,96%

default %>% 
  group_by(idade) %>% 
  summarise(n=n(), Media=mean(phat)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)
# Pessoas com mais idade tem probabilidade menor de default que os de menor idade

# Predições
predict(object = modelo.default, 
        data.frame(idade = 37, sexo = "masculino", renda = 6850), 
        type = "response")

# chance(Y_i=1) = exp(Logito_i)
# Aumento de renda em R$ 1, mantido tudo o mais constante
(exp(-0.0003) - 1) * 100
# As chances diminuem em -0.03% de default

# Eficiência global, sensitividade e especificidade
confusionMatrix(table(predict(modelo.default, type = "response") >= 0.5,
                      default$default == "Não")[2:1, 2:1])

#Eficiência (acurácia) de 22,6% => (360 + 92) / 2000

data.frame(Sensitividade = confusionMatrix(table(predict(modelo.default,
                              type = "response") >= 0.5,
                              default$default == "Não")[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(modelo.default,
                                type = "response") >= 0.5,
                                default$default == "Não")[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acurácia = confusionMatrix(table(predict(modelo.default,
                              type = "response") >= 0.5,
                              default$default == "Não")[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)

################################################################################
#
# 2. Pesquisa de fidelidade em clientes de supermercado, maioria da variável são
# dummies, qualitativas de 5 categorias (de péssimo a ótimo)

fidelidade <- read.dta("Fidelidade.dta")
fidelidade %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)
summary(fidelidade)

#Modelo Dummy
fidelidade.dummies <- dummy_columns(.data = fidelidade,
                                    select_columns = c("atendimento", 
                                                       "sortimento",
                                                       "acessibilidade", 
                                                       "preço"),
                                    remove_selected_columns = T,
                                    remove_first_dummy = T)

modelo.fidelidade <- glm(formula = fidelidade ~ . -id, 
                                 data = fidelidade.dummies, 
                                 family = "binomial")
summary(modelo.fidelidade)
logLik(modelo.fidelidade)

# Fazendo o modelo Stepwise para retirar as variáveis que não passaram no teste z
step.modelo.fidelidade <- step(object = modelo.fidelidade,
                                k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
summary(step.modelo.fidelidade)
logLik(step.modelo.fidelidade)

# Eficiência do Modelo, Sensitividade e Especificidade
confusionMatrix(
  table(predict(step.modelo.fidelidade, type = "response") >= 0.5, 
        fidelidade$fidelidade == "Sim")[2:1, 2:1])

predicoes <- prediction(predictions = step.modelo.fidelidade$fitted.values, 
                        labels = fidelidade$fidelidade)
dados_curva_roc <- performance(predicoes, measure = "sens")
sensitividade <- dados_curva_roc@y.values[[1]] 
especificidade <- performance(predicoes, measure = "spec") 
especificidade <- especificidade@y.values[[1]]
cutoffs <- dados_curva_roc@x.values[[1]] 
dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)
dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"), size = 1) +
           geom_point(color = "#95D840FF", size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, 
                         color = "Sensitividade"), size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF", size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())

# De acordo com a curva de especifidade/sensitividade, um cutoff aproximado 
# de 0.56 igularia as probabilidades em aproximadamente 87.5%

# Curva ROC
ROC <- roc(response = fidelidade$fidelidade, 
           predictor = step.modelo.fidelidade$fitted.values)

ggplotly(
  ggroc(ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)

# Média das chances
fidelidade$phat <- step.modelo.fidelidade$fitted.values

fidelidade %>% 
  group_by(atendimento) %>% 
  summarise(n=n(), Chance=round(mean(phat)/mean(1-phat),2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)
fidelidade %>% 
  group_by(sortimento) %>% 
  summarise(n=n(), Chance=round(mean(phat)/mean(1-phat),2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)
fidelidade %>% 
  group_by(acessibilidade) %>% 
  summarise(n=n(), Chance=round(mean(phat)/mean(1-phat),2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)
fidelidade %>% 
  group_by(preço) %>% 
  summarise(n=n(), Chance=round(mean(phat)/mean(1-phat),2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

# Das 4 variáveis categóricas, a que apresenta maior chance de obtenção de 
# fidelidade por parte do cliente é a 'sortimento' (aumento em 7)
# Portanto, é a variável em que se deve investir

################################################################################
#
# 3. Medição de Indice de Colesterol como variável categórica, dividida em 
# 5 categorias (de muito elevado a ótimo), gerando uma regressão multinomial

colesterol <- read.dta("Colestquali.dta")
colesterol %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)
summary(colesterol)

colesterol %>% 
  group_by(colestquali) %>% 
  summarise(Qtde=n()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

# Modelo Multinomial
colesterol$colestquali <- relevel(colesterol$colestquali, 
                              ref = "muito elevado: superior a 189 mg/dL")
colesterol.multinom <- multinom(formula = colestquali ~ cigarro + esporte, 
                            data = colesterol)
summary(colesterol.multinom)
logLik(colesterol.multinom)
Qui2(colesterol.multinom)

zWald_modelo3_multinom <- (summary(colesterol.multinom)$coefficients / 
                            summary(colesterol.multinom)$standard.errors)
zWald_modelo3_multinom
round((pnorm(abs(zWald_modelo3_multinom), lower.tail = F) * 2), 4)

#Predições
predict(colesterol.multinom, 
        data.frame(cigarro = "não fuma", esporte = 1), 
        type = "probs")

#Gráfico de Probabilidades
levels(colesterol$colestquali)

colesterol$predicao <- predict(colesterol.multinom, 
                           newdata = colesterol, 
                           type = "class")
colesterol[c("muito elevado: superior a 189 mg/dL",
            "elevado: de 160 a 189 mg/dL",
            "limítrofe: de 130 a 159 mg/dL",
            "subótimo: de 100 a 129 mg/dL",
            "ótimo: inferior a 100 mg/dL")] <- colesterol.multinom$fitted.values

ggplotly(
  colesterol %>% 
    dplyr::select(-predicao, - colesterol) %>% 
    rename(y = 1) %>% 
    melt(id.vars = c("y","cigarro","esporte"),
         value.name = "probabilidades") %>% 
    rename(categorias = variable) %>%
    mutate(categorias = factor(categorias,
                               levels = c("muito elevado: superior a 189 mg/dL",
                                          "elevado: de 160 a 189 mg/dL",
                                          "limítrofe: de 130 a 159 mg/dL",
                                          "subótimo: de 100 a 129 mg/dL",
                                          "ótimo: inferior a 100 mg/dL"))) %>% 
    ggplot() +
    geom_smooth(aes(x = esporte, y = probabilidades, color = categorias), 
                method = "loess", formula = y ~ x, se = T) +
    labs(x = "Atividades por Semana",
         y = "Probabilidades",
         color = "Legenda:") +
    scale_color_viridis_d() +
    theme_bw()
)

# Média das chances
colesterol %>% 
  group_by(esporte) %>% 
  summarise(n=n(), 
            Chance=round(mean(`elevado: de 160 a 189 mg/dL`)/
                         mean(1-`elevado: de 160 a 189 mg/dL`),2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

colesterol %>% 
  group_by(cigarro) %>% 
  summarise(n=n(), 
            Chance=round(mean(`ótimo: inferior a 100 mg/dL`)/
                           mean(1-`ótimo: inferior a 100 mg/dL`),2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

# Eficácia do Modelo
attach(colesterol)

EGM <- as.data.frame.matrix(table(colestquali, predicao))
EGM %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

acuracia <- (round((sum(diag(table(colestquali, predicao))) / 
                      sum(table(colestquali, predicao))), 2))

detach(colesterol)
acuracia

# E se for modelo OLS?
colesterol.OLS <- lm(formula = colesterol ~ cigarro + esporte, 
                    data = colesterol)
summary(colesterol.OLS)
logLik(colesterol.OLS)

colesterol %>%
  mutate(residuos = colesterol.OLS$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..),
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(colesterol.OLS$residuals),
                            sd = sd(colesterol.OLS$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

colesterol %>%
  ggplot() +
  geom_point(aes(x = colesterol.OLS$fitted.values, y = colesterol.OLS$residuals),
             color = "#440154FF", size = 3) +
  labs(x = "Fitted Values do Modelo OLS",
       y = "Resíduos do Modelo OLS") +
  theme_bw()
