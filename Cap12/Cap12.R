################################################################################
#            EXERCÍCIOS DO LIVRO DO FÁVERO - CAP 12 - REGRESSÃO LINEAR         #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp","foreign","lmtest")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

################################################################################
#
# 1. Crescimento do PIB e/r ao investimento em educação de um país (em 15 anos) 

pib <- c(-1.5,-0.9,1.3,0.8,0.3,2.0,4.0,3.7,0.2,-2.0,1.0,1.1,4.0,2.7,2.5)
educacao <- c(7.0,9.0,15.0,12.0,10.0,15.0,20.0,17.0,8.0,5.0,13.0,13.0,19.0,19.0,17.0)

dados1 <- data.frame(pib, educacao)
dados1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)
summary(dados1)
ggplotly(
  ggplot(dados1, aes(x = educacao, y = pib)) +
    geom_point(color = "#39568CFF", size = 2) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", se = F, size = 1) +
    xlab("Educação (US$ Bi)") +
    ylab("Taxa Crescimento PIB (%)") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

#Modelo de Regressão
modelo1 <- lm(formula = pib ~ educacao, data = dados1)
summary(modelo1)
modelo1$coefficients
cor(dados1)[1,2]^2 #R-quadrado

#Educação se PIB = 0 ?
#0 = modelo1$coefficients[1] + modelo1$coefficients[2] * educacao
-modelo1$coefficients[1] / modelo1$coefficients[2]

#PIB se Educação = 0 ?
modelo1$coefficients[1] + modelo1$coefficients[2] * 0
#fora do intervalo de predição

#Previsão do PIB com Educação = 11
predict(object = modelo1, data.frame(educacao = 11))
predict(object = modelo1, data.frame(educacao = 11),
        interval = "confidence", level = 0.95)

################################################################################
#
# 2. Indice de percepção de corrupção em 52 países, com as variáveis dependentes
# sendo a idade média dos bilionários e qtde média horas trabalhadas nos paises

corrupcao <- foreign::read.dta("Corrupção.dta")

summary(corrupcao)
corrupcao %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)
chart.Correlation(corrupcao[,-1], histogram = TRUE)

modelo.corrupto <- lm(formula = cpi ~ idade + horas, data = corrupcao)
summary(modelo.corrupto)

# O modelo passa nos teste F e t, e o R-quadrado é de 31.77%
# cpi = 15.16 + 0.07*idade - 0.42*horas

#Resíduos
corrupcao %>%
  mutate(residuos = modelo.corrupto$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..),
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo.corrupto$residuals),
                            sd = sd(modelo.corrupto$residuals)),
                size = 1, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

corrupcao %>%
  ggplot() +
  geom_point(aes(x = modelo.corrupto$fitted.values, 
                 y = modelo.corrupto$residuals),
             color = "#440154FF", size = 3) +
  labs(x = "Fitted Values do Modelo OLS",
       y = "Resíduos do Modelo OLS") +
  theme_bw()

sf.test(modelo.corrupto$residuals)

#Heterocedasticidade
ols_test_breusch_pagan(modelo.corrupto)
ols_vif_tol(modelo.corrupto)

# Percebe-se uma presença de heterocedasticidade nos resíduos do modelo
# Talvez a ausência de variáveis dependentes seja um dos motivos

################################################################################
#
# 3. Os mesmos dados do exercício anterior, incluindo uma variável dummy,
# de país considerado emergente ou não (emergente = 0 para país desenvolvido)

corrupcao2 <- foreign::read.dta("Corrupçãoemer.dta")

summary(corrupcao2)
table(corrupcao2$emergente)
corrupcao2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

corrupcao2 %>% 
  group_by(emergente) %>% 
  summarise(n=n(), Media=mean(cpi)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

#Dummies
corrupcao.dummies <- dummy_columns(.data = corrupcao2,
                                    select_columns = "emergente",
                                    remove_selected_columns = T,
                                    remove_most_frequent_dummy = T)
corrupcao.dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

modelo.dummies <- lm(formula =  cpi ~ . - país, data = corrupcao.dummies)
summary(modelo.dummies)

# As variáveis 'idade' e 'horas' não passaram no Teste t

#Fazendo o procedimento Stepwise
#O valor de 'k' no argumento da função stepwise (significância de 10%)
qchisq(p = 0.1, df = 1, lower.tail = F)
round(pchisq(2.705543, df = 1, lower.tail = F),7)

modelo.step <- step(modelo.dummies, k = 2.705543)
summary(modelo.step)

# Ao nível de significância de 10%, apenas as variáveis dependentes 'horas'
# e a dummy 'emergente' passaram no Teste t

#variável dependente ln(horas)
summary(lm(formula =  cpi ~ idade + log(horas) + emergente_Desenvolvido, 
           data = corrupcao.dummies))

# Mesmo transformando em log a variável 'horas', a outra variável 'idade'
# também não passa no Teste t em 5% e também não passaria em 10%

################################################################################
#
# 4. Monitoramento de 48 meses do índice de colesterol (variável preditora), 
# e as variáveis dependentes o IMC e atividades físicas por semana

colesterol <- foreign::read.dta("Colesterol.dta")
summary(colesterol)

colesterol %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

modelo.colesterol <- lm(colesterol ~ imc + esporte, colesterol)
summary(modelo.colesterol)

# Todas as variáveis passaram nos Testes F e t
# colesterol = 136.72 + 1.99*imc - 5.16*esporte

colesterol %>%
  mutate(residuos = modelo.colesterol$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..),
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo.colesterol$residuals),
                            sd = sd(modelo.colesterol$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

colesterol %>%
  ggplot() +
  geom_point(aes(x = modelo.colesterol$fitted.values, 
                 y = modelo.colesterol$residuals),
             color = "#440154FF", size = 3) +
  labs(x = "Fitted Values do Modelo OLS",
       y = "Resíduos do Modelo OLS") +
  theme_bw()

#Autocorrelação dos resíduos
durbinWatsonTest(modelo.colesterol)

# A hipótese nula de não correlação entre os resíduos é rejeitada,
# pois o valor-p calculado é menor que 0.05 (significância 5%)
# O teste de Durbin-Watson é apenas de autocorrelação de 1 ordem.
# Fonte: https://www.statology.org/durbin-watson-test-r/

#Testes de Breusch-Godfrey
bgtest(colesterol ~ imc + esporte, order = 1, data = colesterol)
bgtest(colesterol ~ imc + esporte, order = 3, data = colesterol)
bgtest(colesterol ~ imc + esporte, order = 4, data = colesterol)
bgtest(colesterol ~ imc + esporte, order = 12, data = colesterol)

# Resíduos do modelo contem presença de autocorrelação, pois os testes de 
# Breusch-Godfrey mostram os p-valores menores que 0.05 (significância 5%),
# rejeitando a hipótese nula de não correlação entre os resíduos.
# Uma análise com base em séries temporais seriam mais apropriadas para os dados.
# Fonte: https://www.statology.org/breusch-godfrey-test-in-r/
