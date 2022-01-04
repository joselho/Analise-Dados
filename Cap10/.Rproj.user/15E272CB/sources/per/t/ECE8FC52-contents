################################################################################
#           EXERCÍCIOS DO LIVRO DO FÁVERO - CAP 10 - ANÁLISE FATORIAL          #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","knitr","kableExtra","car","rgl","gridExtra",
             "PerformanceAnalytics","reshape2","rayshader","psych","ggrepel",
             "factoextra","sp","tmap","magick","gridExtra","foreign")

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
# 1. Análise Fatorial para dados do CRM de um Banco

vars <- c("idade", "rfixa", "rvariavel", "pessoas")
cargas <- c(0.917, 0.874, -0.844, 0.031, 0.047, 0.077, 0.197, 0.979)
data.frame(matrix(cargas, ncol = 2), row.names = vars) %>% 
  rename(F1 = X1, F2 = X2) -> CRM
CRM %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

#Eigenvalues e variância compartilhada
data.frame(c(sum(CRM[,1]^2), sum(CRM[,2]^2)), 
           row.names = colnames(CRM)) %>% 
              `colnames<-`("eigen") -> relat1
relat1
data.frame(relat1, var_comp = relat1[,1] * 100 / 4) -> relat1
data.frame(relat1) %>% 
  mutate(var_acum = sum(relat1[, 2])) -> relat1

relat1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

#Comunalidades
data.frame(CRM) %>% 
  mutate(Comunalidades = rowSums(CRM^2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Loading Plot
data.frame(CRM) %>%
  ggplot(aes(x = F1, y = F2)) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(CRM)) +
  labs(x = paste("F1", paste0("(",
                              round(relat1[1,2], digits = 2),
                              "%)")),
       y = paste("F2", paste0("(",
                              round(relat1[2,2], digits = 2),
                              "%)"))) +
  theme_bw()


################################################################################
#
# 2. Indicadores de 50 países por 2 anos - Comportamento

df2 <- as.data.frame(read.dta(file = "IndicadorPaíses.dta"))
df2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 18)

#correlações ano 1
chart.Correlation(df2[, 2:5], histogram = TRUE, pch = "+")
#correlações ano 2
chart.Correlation(df2[, 6:9], histogram = TRUE, pch = "+")

ano1 <- df2[, 2:5]
ano2 <- df2[, 6:9]
rho_ano1 <- cor(ano1)
rho_ano2 <- cor(ano2)
#Bartlett
cortest.bartlett(R = rho_ano1)
cortest.bartlett(R = rho_ano2)
#graus de liberdade (idem para os 2 anos)
(ncol(rho_ano1) * (ncol(rho_ano1) - 1)) / 2
#KMO
KMO(r = rho_ano1)
KMO(r = rho_ano2)
#PCA
afpc_ano1 <- prcomp(scale(ano1))
summary(afpc_ano1)
afpc_ano2 <- prcomp(scale(ano2))
summary(afpc_ano2)

#Relatório ano1
data.frame(eigenvalue = afpc_ano1$sdev ^ 2,
           var_compartilhada = summary(afpc_ano1)$importance[2,],
           var_cumulativa = summary(afpc_ano1)$importance[3,]) -> relat_ano1
relat_ano1 %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)
#Relatório ano2
data.frame(eigenvalue = afpc_ano2$sdev ^ 2,
           var_compartilhada = summary(afpc_ano2)$importance[2,],
           var_cumulativa = summary(afpc_ano2)$importance[3,]) -> relat_ano2
relat_ano2 %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Cargas fatoriais e Comunalidades ano1
k1 <- sum((afpc_ano1$sdev ^ 2) > 1) 
cargas_ano1 <- as.data.frame(
                afpc_ano1$rotation[, 1:k1] %*% diag(afpc_ano1$sdev[1:k1]))
data.frame(cargas_ano1,
           row.names = colnames(afpc_ano1$x))%>%
  rename('Cargas Fatoriais' = V1) %>%
  mutate(Comunalidades = rowSums(cargas_ano1 ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Cargas fatoriais e Comunalidades ano2
k2 <- sum((afpc_ano2$sdev ^ 2) > 1) 
cargas_ano2 <- as.data.frame(
                afpc_ano2$rotation[, 1:k2] %*% diag(afpc_ano2$sdev[1:k2]))
data.frame(cargas_ano2,
           row.names = colnames(afpc_ano2$x))%>%
  rename('Cargas Fatoriais' = V1) %>%
  mutate(Comunalidades = rowSums(cargas_ano2 ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Scores Fatoriais
scores_ano1 <- t(afpc_ano1$rotation)/afpc_ano1$sdev
scores_ano2 <- t(afpc_ano2$rotation)/afpc_ano2$sdev 

sa1 = scores_ano1['PC1',]
sa2 = scores_ano2['PC1',]
data.frame(sa1, sa2) %>%
  rename('Ano 1' = sa1, 'Ano 2' = sa2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Rankings para os Fatores 1 de cada ano
F.ano1 <- t(apply(scale(ano1), 1, function(x) x * sa1))
F.ano2 <- t(apply(scale(ano2), 1, function(x) x * sa2))

F.ano1 <- data.frame(F.ano1) %>%
  mutate(fator1 = rowSums(.) * -1)
F.ano1 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

F.ano2 <- data.frame(F.ano2) %>%
  mutate(fator1 = rowSums(.) * -1)
F.ano2 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

df2["FatorAno1"] <- F.ano1$fator1
df2["FatorAno2"] <- F.ano2$fator1
data.frame(df2) %>%
  mutate(PontuacaoAno1 = FatorAno1 * 
           relat_ano1$var_compartilhada[1]) -> df2
data.frame(df2) %>%
  mutate(PontuacaoAno2 = FatorAno2 * 
           relat_ano2$var_compartilhada[1]) -> df2

df2 %>% 
  arrange(desc(PontuacaoAno1)) %>%
  select(país) -> Rank.Ano1
df2 %>% 
  arrange(desc(PontuacaoAno2)) %>%
  select(país) -> Rank.Ano2

data.frame(Rank.Ano1, Rank.Ano2, row.names = NULL) %>%
  rename("Rank Ano1" = país, "Rank Ano2" = país.1) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

############################################################


df3 <- as.data.frame(read.dta(file = "PercepçãoDrogaria.dta"))
df3 %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 18)

#matriz de correlação
chart.Correlation(df3, histogram = TRUE, pch = "+")
rho_drog <- cor(df3)
rho_drog %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))

#Análise de componentes
KMO(r = rho_drog)
cortest.bartlett(R = rho_drog)
afpc_drog <- prcomp(scale(df3))
summary(afpc_drog)
#Relatório análise
data.frame(eigenvalue = afpc_drog$sdev ^ 2,
           var_compartilhada = summary(afpc_drog)$importance[2,],
           var_cumulativa = summary(afpc_drog)$importance[3,]) -> relat_drog
relat_drog %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

data.frame(afpc_drog$rotation) %>%
  mutate(var = names(df3)) %>% 
  melt(id.vars = "var") %>%
  mutate(var = factor(var)) %>%
  ggplot(aes(x = var, y = value, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~variable) +
  labs(x = NULL, y = NULL, fill = "Legenda:") +
  scale_fill_viridis_d() +
  theme_bw()

ggplotly(
  fviz_eig(X = afpc_drog,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod4")
)

#Quais os fatores e as cargas fatoriais?
k <- sum((afpc_drog$sdev ^ 2) > 1) 
cargas_fatoriais <- afpc_drog$rotation[, 1:k] %*% diag(afpc_drog$sdev[1:k])
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Quais os scores fatoriais?
scores_fatoriais <- t(afpc_drog$rotation)/afpc_drog$sdev 
colnames(scores_fatoriais) <- colnames(scale(df3))
scores_fatoriais %>%
  t() %>%
  data.frame() %>%
  rename(PC1 = 1,
         PC2 = 2) %>%
  select(PC1, PC2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
data.frame(cargas_fatoriais) %>%
  ggplot(aes(x = X1, y = X2)) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = paste("F1", paste0("(",
                              round(summary(afpc_drog)$importance[2,1] * 100,
                                    digits = 2),
                              "%)")),
       y = paste("F2", paste0("(",
                              round(summary(afpc_drog)$importance[2,2] * 100,
                                    digits = 2),
                              "%)"))) +
  theme_bw()

#E se tivessemos 3 fatores?
k2 <- sum((afpc_drog$sdev ^ 2) > 0.8) 
cargas_fatoriais2 <- afpc_drog$rotation[, 1:k2] %*% diag(afpc_drog$sdev[1:k2])
data.frame(cargas_fatoriais2) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
#cargas fatoriais e comunalidades
data.frame(cargas_fatoriais2) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais2 ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
#scores fatoriais
scores_fatoriais2 <- t(afpc_drog$rotation)/afpc_drog$sdev 
colnames(scores_fatoriais2) <- colnames(scale(df3))
scores_fatoriais2 %>%
  t() %>%
  data.frame() %>%
  rename(PC1 = 1,
         PC2 = 2,
         PC3 = 3) %>%
  select(PC1, PC2, PC3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
#cargas fatoriais 3D
data.frame(cargas_fatoriais2) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) -> cargas_fatoriais2
cargas_fatoriais2 %>% 
  ggplot(aes(x = -F2, y = F1)) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, color = "darkorchid") +
  geom_vline(xintercept = 0, color = "darkorchid") +
  geom_text_repel(label = row.names(cargas_fatoriais2)) +
  theme_bw() 

afpc_drog_3D <- plot_ly()
afpc_drog_3D <- add_trace(p = afpc_drog_3D, 
                           x = cargas_fatoriais2$F2, 
                           y = cargas_fatoriais2$F3, 
                           z = cargas_fatoriais2$F1,
                           mode = 'text', 
                           text = rownames(cargas_fatoriais2),
                           textfont = list(color = "orange"), 
                           showlegend = FALSE)
afpc_drog_3D <- layout(p = afpc_drog_3D,
                        scene = list(xaxis = list(title = colnames(cargas_fatoriais2)[2]),
                                     yaxis = list(title = colnames(cargas_fatoriais2)[3]),
                                     zaxis = list(title = colnames(cargas_fatoriais2)[1]),
                                     aspectmode = "data"))
afpc_drog_3D
