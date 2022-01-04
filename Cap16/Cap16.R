################################################################################
#      EXERCÍCIOS DO LIVRO DO FÁVERO - CAP 16 - REGRESSÃO MULTINÍVEL LINEAR    #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl","car",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools","foreign")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

stderr_nlme <- function(model){
  if(base::class(model) != "lme"){
    base::message("Use a lme object model from nlme package")
    stop()}
  resume <- base::summary(model)
  if(base::length(base::names(model$groups))==1){
    m.type <- "HLM2"
  } else if(base::length(base::names(model$groups))==2){
    m.type <- "HLM3"
  }
  if(m.type == "HLM2"){
    vcov_matrix <- model$apVar
    logs_sd_re <- base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re)==2){
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE Components`=base::c("Var(v0j)","Var(e)"),
                                  `Variance Estimatives`= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                                  base::exp(logs_sd_re[[2]])^2),
                                  `Std Err.`=base::c(stderr_tau00,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
    else{
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau01 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(v0j)","Var(v1j)","Var(e)"),
                                  Estimatives= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                       base::exp(logs_sd_re[[2]])^2,
                                                       base::exp(logs_sd_re[[4]])^2),
                                  Std_Err=base::c(stderr_tau00,
                                                  stderr_tau01,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                            base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                                                               base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
  if(m.type == "HLM3"){
    vcov_matrix <- model$apVar
    logs_sd_re <-  base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re) == 3){
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x3)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(t00k)","Var(v0jk)","Var(e)"),
                                  Estimatives=base::c(base::exp(logs_sd_re)[[2]]^2,
                                                      base::exp(logs_sd_re)[[1]]^2,
                                                      base::exp(logs_sd_re)[[3]]^2),
                                  Std_Err=base::c(stderr_tau_u000,
                                                  stderr_tau_r000,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    } 
    else{
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_r100 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u100 <- msm::deltamethod(~exp(x5)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x7)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE_Components`=base::c("Var(t00k)","Var(t10k)",
                                                          "Var(v0jk)","Var(v1jk)",
                                                          "Var(e)"),
                                  `Variance Estimatives`=base::c(base::exp(logs_sd_re)[[4]]^2,
                                                                 base::exp(logs_sd_re)[[5]]^2,
                                                                 base::exp(logs_sd_re)[[1]]^2,
                                                                 base::exp(logs_sd_re)[[2]]^2,
                                                                 base::exp(logs_sd_re)[[7]]^2),
                                  `Std Err.`=base::c(stderr_tau_u000,
                                                     stderr_tau_u100,
                                                     stderr_tau_r000,
                                                     stderr_tau_r100,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                            base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                                                               base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
}

################################################################################
#
# 1. Desempenho de 120 estudantes em competição de ciências de acordo com o país
# de origem (24 países), a renda média da família do estudante e o investimento
# do país de origem com Pesquisa e Desenvolvimento (em % do PIB)
# Regressão Multinível em 2 níveis (aluno -> país)

ciencia <- read.dta("Competição de Ciências.dta")
ciencia %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

# Análise Exploratória
summary(ciencia)

ciencia %>% 
  group_by(país) %>% 
  summarise(quantidade = n(),
            `nota média` = mean(nota, na.rm = T)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

ciencia %>%
  group_by(país) %>%
  mutate(nota_media = mean(nota, na.rm = T)) %>% 
  ggplot() +
  geom_point(aes(x = país, y = nota),color = "orange", alpha = 0.5, size = 2) +
  geom_line(aes(x = país, y = nota_media, 
                group = 1, color = "Nota Média"), size = 1.5) +
  scale_colour_viridis_d() +
  labs(x = "País",
       y = "Nota") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))

ggplotly(
  ciencia %>%
    ggplot(aes(x = renda, y = nota)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    scale_colour_viridis_d() +
    labs(x = "Renda média da família do estudante (US$)",
         y = "Nota de Ciências do estudante") +
    theme_bw()
)

# verifica-se nos gráficos, que a nota do aluno aumenta à medida em que a renda
# média da sua família também aumenta, porém este comportamento é diferente em 
# cada país, denotando-se que os investimentos em P&D de cada país possa ter
# influência na nota do aluno.

# Vamos estimar o modelo multinível nulo
# com aluno no nível 1 e o país de origem no nível 2
ciencia_nulo_hlm2 <- lme(fixed = nota ~ 1, 
                         random = ~ 1 | país,
                         data = ciencia,
                         method = "REML")
summary(ciencia_nulo_hlm2)
stderr_nlme(ciencia_nulo_hlm2)

# Fazendo o mesmo, agora com o modelo OLS
ciencia_ols_nulo <- lm(formula = nota ~ 1, 
                      data = ciencia)
summary(ciencia_ols_nulo)
lrtest(ciencia_ols_nulo, ciencia_nulo_hlm2)

# O modelo nulo HLM2 possui loglik maior que o modelo OLS
# além de passar em todos os testes de variância

# Correlação intraclasse
icc <- stderr_nlme(ciencia_nulo_hlm2)[,2]
icc[1] / sum(icc)
# O país de origem representa 97.4% da variância

# Gráfico de ajuste linear da nota em função da renda, para cada país
ggplotly(
  ciencia %>%
    ggplot(aes(x = renda, y = nota, color = país)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    guides(color = F) +
    scale_colour_viridis_d() +
    labs(x = "Renda média da família do estudante (US$)",
         y = "Nota de Ciências do estudante") +
    theme_bw()
)

# Vamos estimar o modelo multinível com interceptos aleatórios
# com aluno no nível 1 e o país de origem no nível 2
ciencia_intercept_hlm2 <- lme(fixed = nota ~ renda,
                             random = ~ 1 | país,
                             data = ciencia,
                             method = "REML")
summary(ciencia_intercept_hlm2)
stderr_nlme(ciencia_intercept_hlm2)
lrtest(ciencia_nulo_hlm2, ciencia_intercept_hlm2)

# Termos dos interceptos aleatórios por país
v_intercept <- data.frame(ciencia_intercept_hlm2[["coefficients"]][["random"]][["país"]]) %>%
  rename(v00 = 1)
v_intercept %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

random.effects(ciencia_intercept_hlm2) %>% 
  rename(v0j = 1) %>% 
  rownames_to_column("País") %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(País) %>% 
  ggplot(aes(label = format(v0j, digits = 2), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(País), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = País, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "País",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

# Verifica-se que as variânicas dos termos aleatórios são diferentes de zero

# Vamos estimar o modelo multinível com interceptos e inclinações aleatórios
# com aluno no nível 1 e o país de origem no nível 2
ciencia_intercept_inclin_hlm2 <- lme(fixed = nota ~ renda,
                                     random = ~ renda | país,
                                     data = ciencia,
                                     method = "REML")
summary(ciencia_intercept_inclin_hlm2)
stderr_nlme(ciencia_intercept_inclin_hlm2)
lrtest(ciencia_intercept_hlm2, ciencia_intercept_inclin_hlm2)
# Modelo HLM2 de interceptos e inclinações aleatórias possuem
# loglik maior que o modelo HLM2 de interceptos aleatórios

# Vamos estimar o modelo multinível final
# com aluno no nível 1 e o país de origem no nível 2
ciencia_final_hlm2 <- lme(fixed = nota ~ renda + renda:pesqdes,
                          random = ~ renda | país,
                          data = ciencia,
                          method = "REML")
summary(ciencia_final_hlm2)
stderr_nlme(ciencia_final_hlm2)
lrtest(ciencia_final_hlm2)

# O modelo completo multinível de dois níveis é:
#nota[ij] = 20.7517 + 0.000971*renda[ij] + 0.001136*renda[ij]*pesqdes[ij] + u[0j] + r[ij]
#onde u[0j] são os termos dos interceptos aleatórios para cada país j=1..24
#e r[ij] são os resíduos do modelo

# Análise dos valores previstos no modelo HLM2 Final:
ciencia$hlm2_fitted <- predict(ciencia_final_hlm2, ciencia)
predict(ciencia_final_hlm2, level = 0:1) %>% 
  mutate(país = gsub("^.*?\\/","",país),
         país = as.factor(país),
         nota = ciencia$nota,
         etjk = resid(ciencia_final_hlm2)) %>% #função resid gera os termos etjk
  select(país, nota, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

ciencia %>%
  ggplot() +
  geom_smooth(aes(x = nota, y = nota, color = "Valores Reais"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = nota, y= hlm2_fitted, color = "HLM2 Final"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_point(aes(x = nota, y = nota,
                 color = "Valores Reais")) +
  geom_point(aes(x = nota, y = hlm2_fitted,
                 color = "HLM2 Final"))  +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1","darkorchid")) +
  labs(x = "Notas", y = "Fitted Values") +
  theme_bw()

################################################################################
#
# 2. Preço dos aluguéis de imóveis comerciais (log(preço)) ao longo dos 6 anos,
# dividos em 15 distritos, levando-se em conta a presença de vagas de estacionamento,
# presença de parque de alimentação ou restaurante, presença de valet park no
# prédio, presença de estação de metrô no distrito onde localiza-se o imóvel,
# e índice de violência no distrito (taxa homicídios por 100000hab)
# Regressão Multinível em 3 níveis (tempo -> imóvel -> distrito)

imoveis <- read.dta("Imóveis Comerciais.dta")
imoveis %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

# Análise Exploratória
summary(imoveis)

imoveis %>% 
  rename(Distrito = 1,
         `Quantidade de Imóveis` = 2) %>% 
  group_by(Distrito) %>% 
  summarise(`Quantidade de Imóveis` = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

imoveis %>%
  rename(Ano = 4,
         `Quantidade de Imóveis` = 2) %>% 
  group_by(Ano) %>% 
  summarise(`Quantidade de Imóveis` = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

# verifica-se nas tabelas os distritos possuem quantidades distintas de imóveis
# entre si, além de nem todos os imóveis foram analisados ao longo dos 6 anos,
# visto também que a quantidade de imóveis é distinta para cada ano, e a quantidade
# total de imóveis analisados são 277.

# Gráfico do lnp ao longo dos 6 anos
ggplotly(
  imoveis %>%
    ggplot(aes(x = ano, y = lnp, group = 1, label = distrito)) +
    geom_point(color = "gold", size = 2, alpha = 0.2) +
    geom_smooth(color = "#440154FF", method = "lm", se = F, size = 2) +
    labs(x = "Ano",
         y = "Log Preço") +
    theme_bw()
)

# Gráfico do lnp com reta de regressão linear por distrito
ggplotly(
  imoveis %>%
    ggplot(aes(x = ano, y = lnp, color = distrito)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point(size = 3, alpha = 0.2) +
    guides(color = F) +
    scale_colour_viridis_d() +
    labs(x = "Ano",
         y = "Log Preço") +
    theme_bw()
)

# Gráficos agrupados por distrito
imoveis %>% 
  group_by(distrito) %>% 
  ggplot(aes(x = ano, y = lnp, group = distrito, label = distrito)) +
  geom_point(color = "gold", size = 2, alpha = 0.2) +
  geom_smooth(color = "#440154FF", method = "lm", se = F, size = 2) +
  facet_wrap(~ distrito) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw()

# Vamos estimar o modelo multinível nulo -> log(preço)
# com tempo no nível 1, imóvel no nível 2 e o distrito no nível 3
imoveis_nulo_hlm3 <- lme(fixed = lnp ~ 1,
                         random = list(distrito = ~1, imóvel = ~1),
                         data = imoveis,
                         method = "REML")
summary(imoveis_nulo_hlm3)
stderr_nlme(imoveis_nulo_hlm3)

icc <- stderr_nlme(imoveis_nulo_hlm3)[,2]
icc[1] + icc[2] / sum(icc)
icc[1] / sum(icc)

# Comparando com o modelo OLS nulo
imoveis_ols_nulo <- lm(formula = lnp ~ 1,
                      data = imoveis)
summary(imoveis_ols_nulo)
lrtest(imoveis_ols_nulo, imoveis_nulo_hlm3)
# Modelo multinível com Loglik muito maior que o modelo OLS

# Vamos estimar o modelo multinível com interceptos aleatórios
imoveis_intercept_hlm3 <- lme(fixed = lnp ~ ano,
                              random = list(distrito = ~1, imóvel = ~1),
                              data = imoveis,
                              method = "REML")
summary(imoveis_intercept_hlm3)
stderr_nlme(imoveis_intercept_hlm3)

# Termos dos interceptos aleatórios por distrito e por imóvel
random.effects(imoveis_intercept_hlm3)[["distrito"]] %>% 
  rename(v0jk = 1) %>% 
  rownames_to_column("Distrito") %>% 
  mutate(color_v0jk = ifelse(v0jk < 0, "A", "B"),
         hjust_v0jk = ifelse(v0jk > 0, 1.15, -0.15)) %>% 
  arrange(Distrito) %>% 
  ggplot(aes(label = format(v0jk, digits = 2), 
             hjust = hjust_v0jk)) +
  geom_bar(aes(x = fct_rev(Distrito), y = v0jk, fill = color_v0jk),
           stat = "identity", color = "black") +
  geom_text(aes(x = Distrito, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Distrito",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

ggplotly(
  random.effects(imoveis_intercept_hlm3)[["imóvel"]] %>% 
    rename(v0jk = 1) %>% 
    rownames_to_column("Imóvel") %>% 
    mutate(Imóvel = gsub("^.*?\\/","",Imóvel)) %>% 
    group_by(Imóvel) %>% 
    summarise(v0jk = mean(v0jk)) %>% 
    ggplot(aes(x = fct_rev(Imóvel), y = v0jk, label = Imóvel)) +
    geom_bar(stat = "identity", color = "gray50") +
    coord_flip() +
    labs(x = "Imóvel",
         y = "v0jk") +
    theme(legend.title = element_blank(), 
          panel.background = element_rect("white"),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
)

# Vamos estimar o modelo multinível com interceptos e inclinações aleatórios
imoveis_intercept_inclin_hlm3 <- lme(fixed = lnp ~ ano,
                              random = list(distrito = ~ano, imóvel = ~ano),
                              data = imoveis,
                              method = "REML")
summary(imoveis_intercept_inclin_hlm3)
stderr_nlme(imoveis_intercept_inclin_hlm3)
lrtest(imoveis_intercept_hlm3, imoveis_intercept_inclin_hlm3)
# Comparando com o modelo de interceptos aleatórios,
# o modelo de interceptos e inclinações aleatórios possui um Loglik maior

icc <- stderr_nlme(imoveis_intercept_inclin_hlm3)[,2]
icc[1] + icc[2] / sum(icc)
icc[1] / sum(icc)

# Vamos estimar o modelo multinível com interceptos e inclinações aleatórios
# de nível 2 (variáveis alim, vaga4 e valet para os imóveis)
imoveis_nivel2_hlm3 <- lme(fixed = lnp ~ ano + alim + vaga4 + valet:ano,
                           random = list(distrito = ~ano, imóvel = ~ano),
                           data = imoveis,
                           method = "REML")
summary(imoveis_nivel2_hlm3)
stderr_nlme(imoveis_nivel2_hlm3)
lrtest(imoveis_intercept_inclin_hlm3, imoveis_nivel2_hlm3)

# O modelo HLM3 de Nível 2 é:
#lnp[tjk] = 4.168459 + 0.014813*ano[jk] + 0.231367*alim[jk] + 0.146295*vaga4[jk] - 0.003168*valet[jk]*ano[jk] +
#           u[00k] + u[10k]*ano[jk] + r[0jk] + r[1jk]*ano[jk] + e[tjk]
# onde u[00k] e u[10k] são os termos dos interceptos aleatórios para cada distrito j=1..15
# r[0jk] e r[1jk] são os termos das inclinações aleatórias para cada distrito j=1..15
# e imóvel k=1..277, e[tjk] são os resíduos do modelo

# O modelo HLM3 de Nível 2 possui loglik maior que o modelo estimado anterior
# Porém, com nível de significência de 5%, a variável dummy vaga4 não passa
# no teste t, pois seu p-valor > 0.05

icc <- stderr_nlme(imoveis_nivel2_hlm3)[,2]
icc[1] + icc[2] / sum(icc)
icc[1] / sum(icc)

# Vamos estimar o modelo multinível com interceptos e inclinações aleatórios
# de nível 2 e 3 (variáveis alim, vaga4 e valet para os imóveis,
# e variáveis metrô e violência para os distritos)
imoveis_nivel2_3_hlm3 <- lme(fixed = lnp ~ ano + alim + vaga4 + metrô +
                             valet:ano + metrô:ano + violência:ano,
                           random = list(distrito = ~ano, imóvel = ~ano),
                           data = imoveis,
                           method = "REML")
summary(imoveis_nivel2_3_hlm3)
stderr_nlme(imoveis_nivel2_3_hlm3)
lrtest(imoveis_nivel2_hlm3, imoveis_nivel2_3_hlm3)

# O modelo HLM3 de Nível 2 e 3 é:
#lnp[tjk] = 3.780677 + 0.014459*ano[jk] + 0.231405*alim[jk] + 0.207074*vaga4[jk] + 0.511133*metrô[jk] -
#           0.003059*valet[jk]*ano[jk] - 0.00717*metrô[jk]*ano[jk] + 0.000091*violência[jk]*ano[jk] +
#           u[00k] + u[10k]*ano[jk] + r[0jk] + r[1jk]*ano[jk] + e[tjk]
# onde u[00k] e u[10k] são os termos dos interceptos aleatórios para cada distrito j=1..15
# r[0jk] e r[1jk] são os termos das inclinações aleatórias para cada distrito j=1..15
# e imóvel k=1..277, e[tjk] são os resíduos do modelo

# O modelo passa no teste Qui-quadrado e todas as variáveis passam nos testes t,
# porém, o seu loglik apresenta-se menor que o do modelo de nível 2

# Tabela dos valores previstos do Modelo de Nível 2 e 3
predict(imoveis_nivel2_3_hlm3, level = 0:2) %>% 
  mutate(imóvel = gsub("^.*?\\/","",imóvel),
         imóvel = as.factor(as.numeric(imóvel)),
         distrito = as.factor(as.numeric(distrito)),
         ano = imoveis$ano,
         lnp = imoveis$lnp,
         etjk = resid(imoveis_nivel2_3_hlm3)) %>% #função resid gera os termos etjk
  rename("fitted fixed" = 3,
         "fitted distrito" = 4,
         "fitted imóvel" = 5) %>%
  select(distrito, imóvel, ano, lnp, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 18)

# E se for feito um modelo OLS com dummies (variável distrito)?
base_dummizada <- dummy_cols(.data = imoveis,
                             select_columns = "distrito",
                             remove_most_frequent_dummy = T,
                             remove_selected_columns = TRUE)
imoveis_nivel2_3_olsdumm <- lm(formula = lnp ~ . - imóvel - valet +
                                valet:ano + metrô:ano + violência:ano,
                                data = base_dummizada)
summary(imoveis_nivel2_3_olsdumm)
imoveis_step_olsdumm <- step(object = imoveis_nivel2_3_olsdumm,
                             k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
summary(imoveis_step_olsdumm)
lrtest(imoveis_nivel2_3_hlm3, imoveis_step_olsdumm)

# Gráfico dos valores previstos por modelo
predict(imoveis_nivel2_3_hlm3, level = 0:2) %>% 
  mutate(imóvel = gsub("^.*?\\/","",imóvel),
         imóvel = as.factor(as.numeric(imóvel)),
         ano = imoveis$ano) %>% 
  rename(fitted_fixed = 3,
         fitted_distrito = 4,
         fitted_imóvel = 5) %>% 
  right_join(imoveis, 
             by = c("distrito","imóvel","ano"))  %>% 
  mutate(fitted_ols = imoveis_step_olsdumm$fitted.values) %>% 
  ggplot() +
  geom_line(aes(x = lnp, y = lnp)) +
  geom_smooth(aes(x = lnp, y = fitted_ols,
                  color = "OLS"), se = F, size = 1)  +
  geom_smooth(aes(x = lnp, y = fitted_imóvel,
                  color = "HLM3"), se = F, size = 1) +
  geom_point(aes(x = lnp, y = fitted_ols,
                 color = "OLS"), shape = 1, size = 2, alpha = 4)  +
  geom_point(aes(x = lnp, y = fitted_imóvel,
                 color = "HLM3"), shape = 0, size = 2, alpha = 0.4) +
  labs(x = NULL,
       y = NULL) +
  scale_color_manual("Modelos:", values = c("#440154FF","orange")) +
  theme(legend.title = element_blank(), 
        panel.border = element_rect(NA),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey80"),
        legend.position = "bottom")
