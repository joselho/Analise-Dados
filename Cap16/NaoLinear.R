
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl","car",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools","glmmTMB",
             "lme4","caret","MASS","foreign")

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

turismo <- read.dta("Turismo.dta")
turismo %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

summary(turismo)
turismo %>% 
  group_by(país) %>% 
  summarise(Casais = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

turismo_binom <- glm(formula = turismo ~ idade + filhos, 
                      data = turismo, 
                      family = "binomial")
summary(turismo_binom)

turismo_nlme <- glmer(formula = turismo ~ idade + filhos + (1 | país),
                     data = turismo,
                     family = binomial)
summary(turismo_nlme)

turismo$binom_fitted <- turismo_binom$fitted.values
turismo$nlme_fitted <- 1 / (1 + exp(-predict(turismo_nlme, turismo)))
turismo %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

lrtest(turismo_binom, turismo_nlme)

ggplotly(
  turismo %>% 
    mutate(phat = nlme_fitted) %>% 
    ggplot() +
    geom_point(aes(x = filhos, y = phat), color = "#95D840FF", size = 2) +
    geom_smooth(aes(x = filhos, y = phat, group = país), 
                method = "glm", formula = y ~ x, 
                method.args = list(family = "binomial"), 
                se = F,
                color = "#440154FF", size = 1) +
    labs(x = "Filhos",
         y = "Prob Turismo") +
    theme_bw()
)

################################################################################

acidentes <- read.dta("Acidentes de Trânsito.dta")
acidentes %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

summary(acidentes)
acidentes %>%
  summarise(Média = mean(acidentes),
            Variância = var(acidentes)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

ggplotly(
  acidentes %>%
    ggplot(aes(x = acidentes,
               fill = ..count..)) +
    geom_histogram(bins = round(2 * nrow(acidentes) ^ (1 / 2.5)),
                   color = "black") +
    scale_fill_gradient("Contagem",
                        low = "#440154FF", 
                        high = "#FDE725FF") +
    labs(x = "Quantidade de acidentes de trânsito",
         y = "Frequência") +
    theme_bw()
)

acidentes %>%
  group_by(estado) %>%
  summarise(Municípios = n(),
            `Média de acidentes` = mean(acidentes)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

acidentes_glm.bn <- glm.nb(acidentes ~ alcool,
                           data = acidentes)
summary(acidentes_glm.bn)

acidentes_nlme.bn <- glmer.nb(acidentes ~ alcool + (1 | estado) + (1 | município),
                              data = acidentes)
summary(acidentes_nlme.bn)

lrtest(acidentes_glm.bn, acidentes_nlme.bn)

acidentes$glm.fitted <- acidentes_glm.bn$fitted.values
acidentes$nlme.fitted <- exp(predict(acidentes_nlme.bn,acidentes))
acidentes %>%
  dplyr::select(estado, município, distrito, acidentes, 
         glm.fitted, nlme.fitted) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

acidentes %>%
  ggplot() +
  geom_point(aes(x = alcool, y = acidentes), alpha = 0.5, size = 2) +
  geom_smooth(aes(x = alcool, y = glm.fitted,
                  color = "Binomial Negativo Tradicional"), se = F, size = 1.5) +
  geom_smooth(aes(x = alcool, y = nlme.fitted,
                  color = "Binomial Negativo Multinível"), se = F, size = 1.5) + 
  scale_color_manual("Estimação:",
                     values = c("orange", "#440154FF")) +
  labs(x = "Quantidade média de álcool ingerida por habitante/dia no distrito (g)",
       y = "Quantidade de acidentes de trânsito") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")
