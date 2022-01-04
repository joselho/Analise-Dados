################################################################################
#      EXERCÍCIOS DO LIVRO DO FÁVERO - CAP 14 - REGRESSÃO POISSON/BIN.NEG.    #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","reshape2","ggrepel",
             "fastDummies","lmtest","splines","jtools","questionr","MASS",
             "pscl","overdisp","magick","foreign")

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
# 1. Pesquisa de incidência de compras via CDC, através da renda e idade do
# consumidor, modelagem de regressão Poisson

financiamento <- read.dta("Financiamento.dta")
financiamento %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

#Análise descritiva
summary(financiamento)
freq(financiamento$quantcompras) %>%
  kable()%>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

financiamento %>%
  summarise(Média = mean(quantcompras),
            Variância = var(quantcompras)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

ggplotly(
  financiamento %>%
    ggplot(aes(x = quantcompras,
               fill = ..count..)) +
    geom_histogram(bins = 5,
                   color = "black") +
    scale_fill_gradient("Contagem",
                        low = "#440154FF", 
                        high = "#FDE725FF") +
    labs(x = "Quantidade de compras de bens",
         y = "Frequência") +
    theme_bw()
)

#Modelagem Poisson
financiamento.poisson <- glm(formula = quantcompras ~ renda + idade,
                              data = financiamento,
                              family = "poisson")
summary(financiamento.poisson)
summ(financiamento.poisson, digits = 4, confint = T, ci.width = 0.95)
logLik(financiamento.poisson)
lrtest(financiamento.poisson)

overdisp(x = financiamento,
         dependent.position = 2,
         predictor.position = 3:4) #equidisperção dos dados: Poisson

financiamento$lambda_poisson <- financiamento.poisson$fitted.values
coefs.m1 <- financiamento.poisson$coefficients
coefs.m1

# O modelo passa no teste Qui-quadrado e as variáveis nos testes z
# com nível de significância de 5%, o modelo completo é:
# quantcompras = exp(7.0484 - 0.0011*renda - 0.0865*idade)

#Previsões
predict(object = financiamento.poisson,
        newdata = data.frame(renda = 2660,
                             idade = 47),
        type = "response")

exp(coefs.m1[2]*100) #aumenta em média R$ 100 da renda
exp(coefs.m1[3]*1) #aumenta em média 1 ano da idade

# Gráficos de previsão de incidência de uso dee CDC em compras
# pela idade ou renda cliente

ggplotly(
  financiamento %>%
    ggplot(aes(x = idade, y = lambda_poisson)) +
    geom_point(aes(y = quantcompras), color = "red") +
    geom_smooth(aes(y = lambda_poisson), se = F) +
    labs(x = "Idade",
         y = "Incidência Estimada") +
    theme_bw()
)

ggplotly(
  financiamento %>%
    ggplot(aes(x = renda, y = lambda_poisson)) +
    geom_point(aes(y = lambda_poisson), color = "red") +
    geom_smooth(se = F) +
    labs(x = "Renda",
         y = "Incidência Estimada") +
    theme_bw()
)

# Com relação a renda do cliente, quanto mais se aumenta, menos a incidência
# de compras através de CDC, o que faz sentido, já que clientes com maior renda
# tendem a pagar suas compras com recursos próprios, de que financiá-las
# Clientes de renda mais baixa podem ser mais apropriados para realizar uma
# campanha de marketing de incentivo ao financiamento por CDC

# E se for modelar numa OLS?
financiamento.OLS <- lm(formula = quantcompras ~ renda + idade,
                        data = financiamento)
summary(financiamento.OLS)
logLik(financiamento.OLS)

lrtest(financiamento.OLS, financiamento.poisson)

# O modelo Poisson possui um logLik maior que o Modelo OLS
# sendo mais adequado para fazer as previsões

################################################################################
#
# 2. Imóveis a venda em uma região determinada por quadrículas equidistantes
# (total de 276 imóveis e 100 quadrículas), e determinar se a quantidade de 
# imóveis a venda é influenciada pela distância do imóvel ao Parque Municipal
# e se na quadrícula possui centros de consumo ou shoppings

imoveis <- read.dta("Imobiliária.dta")
imoveis %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

#Análise descritiva
summary(imoveis)
freq(imoveis$quantimóveis, cum = T) %>%
  kable()%>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

imoveis %>%
  summarise(Média = mean(quantimóveis),
            Variância = var(quantimóveis)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

ggplotly(
  imoveis %>%
    ggplot(aes(x = quantimóveis,
               fill = ..count..)) +
    geom_histogram(bins = 11,
                   color = "black") +
    scale_fill_gradient("Contagem",
                        low = "#440154FF", 
                        high = "#FDE725FF") +
    labs(x = "Quantidade de imóveis",
         y = "Frequência") +
    theme_bw()
)

# Fazendo primeiro uma Modelagem Poisson
imoveis.poisson <- glm(formula = quantimóveis ~ . -quadrícula,
                       data = imoveis,
                       family = "poisson")
summary(imoveis.poisson)
summ(imoveis.poisson, digits = 4, confint = T, ci.width = 0.95)
logLik(imoveis.poisson)
lrtest(imoveis.poisson)

overdisp(x = imoveis,
         dependent.position = 2,
         predictor.position = 3:4)

# Modelo passa no teste Qui-quadrado e variáveis nos testes z (signif 5%)
# quantimoveis = exp(1.0283 + 0.0007*distparque - 0.9*Shopping[Sim=1])
# Porém, modelo não passa no teste de superdispersão (p-valor < 0.05)

# Agora uma Modelagem Binomial Negativa
imoveis.bneg <- glm.nb(formula = quantimóveis ~ distparque + shopping,
                       data = imoveis)
summary(imoveis.bneg)
summ(imoveis.bneg, digits = 4, confint = T, ci.width = 0.95)
logLik(imoveis.bneg)
1 / imoveis.bneg$theta #phi

# Modelo passa no teste Qui-quadrado e variáveis nos testes z (signif 5%)
# além do phi (1/theta) ser estatisticamente maior que zero
# quantimoveis = exp(0.6078 + 0.0012*distparque - 0.6869*Shopping[Sim=1])

#Previsões
coefs.m2p <- imoveis.poisson$coefficients
coefs.m2p
coefs.m2bn <- imoveis.bneg$coefficients
coefs.m2bn

predict(object = imoveis.bneg,
        newdata = data.frame(distparque = 820,
                             shopping = "Não"),
        type = "response")
#previsão de 5 imóveis a venda na quadrícula quando
#a distância ao parque é de 820 e não tem presença de shoppings

exp(coefs.m2bn[2]*100) #aumenta em média 100m de distância
exp(coefs.m2bn[3]*1) #aumenta se há presença de lojas ('shopping=1')

# Grafico da quantidade prevista de imóveis
imoveis %>%
  mutate(fitted_poisson = imoveis.poisson$fitted.values,
         fitted_bneg = imoveis.bneg$fitted.values) %>% 
  dplyr::select(quadrícula, quantimóveis, fitted_poisson, fitted_bneg) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

imoveis %>%
  ggplot() +
  geom_point(aes(x = distparque, y = quantimóveis, shape = shopping), 
             alpha = 0.5, size = 3) +
  geom_smooth(aes(x = distparque, y = imoveis.poisson$fitted.values,
                  color = "POISSON"), se = F, size = 1.5) +
  geom_smooth(aes(x = distparque, y = imoveis.bneg$fitted.values,
                  color = "BNEG"), se = F, size = 1.5) + 
  scale_color_manual("Estimação:",
                     values = c("orange", "#440154FF")) +
  labs(x = "Distância do Parque",
       y = "Imóveis a Venda") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

# Pelo gráfico, percebe-se que a presença de centros de consumo na quadrícula
# inibe a venda de imóveis, além de que, quanto maior a distância da quadrícula
# ao Parque Municipal, mais imóveis são colocados à venda

# Gráfico de Probabilidades
prob.poisson <- predprob(imoveis.poisson) %>% colMeans
prob.bneg <- predprob(imoveis.bneg) %>% colMeans
df.plot <- data.frame(x = 0:max(imoveis$quantimóveis), 
                      Poisson = prob.poisson, BinNeg = prob.bneg)
obs.plot <- table(imoveis$quantimóveis) %>% prop.table() %>% data.frame #Observados
names(obs.plot) <- c("x", 'Observados')
comb.tmp <- merge(obs.plot, df.plot, by = 'x', all = T)
comb.tmp[is.na(comb.tmp)] <- 0
comb.tmp <- comb.tmp[-nrow(comb.tmp),]
mm.plot <- melt(comb.tmp, 
                id.vars = 'x', value.name = 'Prob', variable.name = 'Modelo')

ggplot(mm.plot, aes(x = x, y = Prob, group = Modelo, col = Modelo)) +
  geom_line(aes(lty = Modelo), lwd = 1) +
  theme_bw() +
  labs(x = "Imóveis a Venda", y = 'Probabilidade',
       title = "Comparação de Estimação de Modelos") +
  scale_color_manual(values = c('black', 'blue', 'green')) +
  scale_linetype_manual(values = c('solid', 'solid', 'solid')) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position=c(.75, .65))

# E se for modelar numa OLS?
imoveis.OLS <- lm(formula = quantimóveis ~ distparque + shopping,
                  data = imoveis)
summary(imoveis.OLS)
logLik(imoveis.OLS)

# comparando valores previstos
imoveis %>%
  mutate(fitted_OLS = imoveis.OLS$fitted.values,
         fitted_poisson = imoveis.poisson$fitted.values,
         fitted_bneg = imoveis.bneg$fitted.values) %>% 
  dplyr::select(quadrícula, quantimóveis, 
                fitted_OLS, fitted_poisson, fitted_bneg) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

# comparando as logliks dos três modelos
data.frame(OLS = logLik(imoveis.OLS),
           Poisson = logLik(imoveis.poisson),
           BNeg = logLik(imoveis.bneg)) %>% 
  melt() %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = format(value, digts = 3)), 
            color = "black", 
            size = 4, 
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("#440154FF", "#453781FF", "orange")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_bw()
