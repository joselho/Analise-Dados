################################################################################
#             EXERCÍCIOS DO LIVRO DO FÁVERO - CAP 11 - ANACOR e ACM            #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","sjPlot","knitr","kableExtra",
             "FactoMineR","cabootcrs","foreign")

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
# 1. Pesquisa com 3000 clientes em uma loja, sobre o nível de percepção do 
# atendimento e do nível de preço (duas variáveis qualitativas)

clientes <- read.dta("Atendimento x Preço.dta")

clientes %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 14)

summary(clientes)

#Tabela de Contingência
tabela <- table(clientes$atendimento, clientes$preço)
tabela
sjt.xtab(var.row = clientes$atendimento,
         var.col = clientes$preço)

#Tabela de Frequências Absolutas Esperadas 
sjt.xtab(var.row = clientes$atendimento,
         var.col = clientes$preço,
         show.exp = TRUE)

#Teste de Qui-Quadrado
quisq <- chisq.test(tabela)
quisq
# Pela estatística Qui-quadrado, há associação entre as categorias da tabela

#Tabela de Resíduos Padronizados Ajustados
data.frame(quisq$stdres) %>%
  rename(atendimento = 1,
         preço = 2) %>% 
  ggplot(aes(x = atendimento, 
             y = preço, 
             fill = Freq, 
             label = round(Freq,3))) +
  geom_tile() +
  geom_label(size = 3, fill = "white") +
  scale_fill_gradient2(low = "#440154FF", 
                       mid = "white", 
                       high = "#FDE725FF",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text())

#ANACOR
anacor.loja <- CA(tabela)
#inércias
anacor.loja$eig

#Mapa Perceptual
ca_coordenadas <- rbind(anacor.loja$row$coord, anacor.loja$col$coord)[,1:2]
ca_coordenadas
data.frame(t(c(nrow(tabela), ncol(tabela))), row.names = "") %>% 
  rename(atendimento = X1, preço = X2) -> id_var
id_var
ca_coordenadas_final <- data.frame(ca_coordenadas, 
                                   Variable = rep(names(id_var), id_var))
ca_coordenadas_final

ca_coordenadas_final %>% 
  rownames_to_column() %>% 
  rename(Category = 1) %>% 
  ggplot(aes(x = Dim.1, 
             y = Dim.2, 
             label = Category, 
             fill = Variable,
             color = Variable,
             shape = Variable)) +
  geom_point(size = 2) +
  geom_label_repel(max.overlaps = 100,
                   size = 3,
                   color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = paste("Dimension 1:", paste0(round(anacor.loja$eig[1,2], digits = 2), "%")),
       y = paste("Dimension 2:", paste0(round(anacor.loja$eig[2,2], digits = 2), "%"))) +
  scale_fill_viridis_d(option = "cividis") +
  scale_color_viridis_d(option = "cividis") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

################################################################################
#
# 2. Pesquisa com 2304 indivíduos sobre a classificação do índice colesterol 
# e quantidade de exercícios por semana (duas variáveis qualitativas)

colesterol <- c("Muito elevado", "Elevado", "Limitrofe", "Subotimo", "Otimo")
atividades <- c("0", "1", "2", "3", "4", "5")
dados <- c(32, 158, 264, 140, 40, 0,
           22, 108, 178, 108, 58, 0,
           0, 26, 98, 180, 86, 36,
           0, 16, 114, 166, 104, 54,
           0, 0, 82, 118, 76, 30)
tabsaude <- matrix(dados, ncol = 6, byrow = T)
colnames(tabsaude) <- atividades
rownames(tabsaude) <- colesterol
tabsaude <- as.table(tabsaude)

#Frequências Esperadas
saude.esperado <- (rowSums(tabsaude) %o% colSums(tabsaude)) / sum(tabsaude)
saude.esperado %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 14)

#Resíduos 
saude.residuos <- tabsaude - saude.esperado
saude.residuos %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 14)

#Qui-quadrado da tabela
quisaude <- sum((saude.residuos ^ 2) / saude.esperado)
quisaude
quisaude > qchisq(0.05, (ncol(tabsaude)-1)*(nrow(tabsaude)-1), lower.tail = F)
# Pela estatística Qui-quadrado, há associação entre as categorias da tabela

#Correspondência
anacor.saude <- CA(tabsaude)
anacor.saude$eig

#Mapa de calor (resíduos)
quisaude <- chisq.test(tabsaude)
data.frame(quisaude$stdres) %>%
  rename(colesterol = 1,
         atividades = 2) %>% 
  ggplot(aes(x = colesterol, 
             y = atividades, 
             fill = Freq, 
             label = round(Freq,3))) +
  geom_tile() +
  geom_label(size = 3, fill = "white") +
  scale_fill_gradient2(low = "#440154FF", 
                       mid = "white", 
                       high = "#FDE725FF",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text())

# Há uma correspondência muito grande entre os níveis de colesterol elevado e 
# muito elevado, e o tempo de atividades físicas menor ou igual a 2 v/p semana
# Da mesma forma, também há uma correspondência muito grande entre os níveis de
# colesterol limítrofe, subótimo e ótimo, e tempo de atividade maior ou igual
# a 3 vezes por semana (ficaram acima de 1.96).

#Mapa Perceptual
ca_coordenadas <- rbind(anacor.saude$row$coord, anacor.saude$col$coord)[,1:2]
ca_coordenadas
data.frame(t(c(nrow(tabsaude), ncol(tabsaude))), row.names = "") %>% 
  rename(colesterol = X1, atividade = X2) -> id_var
id_var
ca_coordenadas_final <- data.frame(ca_coordenadas, 
                                   Variable = rep(names(id_var), id_var))
ca_coordenadas_final

ca_coordenadas_final %>% 
  rownames_to_column() %>% 
  rename(Category = 1) %>% 
  ggplot(aes(x = Dim.1, 
             y = Dim.2, 
             label = Category, 
             fill = Variable,
             color = Variable,
             shape = Variable)) +
  geom_point(size = 2) +
  geom_label_repel(max.overlaps = 100,
                   size = 3,
                   color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = paste("Dimension 1:", paste0(round(anacor.saude$eig[1,2], digits = 2), "%")),
       y = paste("Dimension 2:", paste0(round(anacor.saude$eig[2,2], digits = 2), "%"))) +
  scale_fill_viridis_d(option = "viridis") +
  scale_color_viridis_d(option = "viridis") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")


################################################################################
#
# 3. Pesquisa de popularidade de um prefeito por 3 anos de mandato 

prefeito <- read.dta("Gestão do Prefeito.dta")

prefeito %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 14)

summary(prefeito)

#Tabela de Contingência
tabela.prefeito <- table(prefeito$avaliação, prefeito$ano)
tabela.prefeito
sjt.xtab(var.row = prefeito$avaliação,
         var.col = prefeito$ano,
         show.exp = T)

#Resíduos Padronizados
#Teste de Qui-Quadrado
qui.prefeito <- chisq.test(tabela.prefeito)
qui.prefeito

data.frame(qui.prefeito$stdres) %>%
  rename(avaliação = 1,
         ano = 2) %>% 
  ggplot(aes(x = avaliação, 
             y = ano, 
             fill = Freq, 
             label = round(Freq,3))) +
  geom_tile() +
  geom_label(size = 3, fill = "white") +
  scale_fill_gradient2(low = "#440154FF", 
                       mid = "white", 
                       high = "#FDE725FF",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text())

#Anacor e Mapa Perceptual
anacor.prefeito <- CA(tabela.prefeito)
anacor.prefeito$eig

# Pelo mapa dos resíduos, é evidente que a associação de 'concordo totalmente'
# diminui pelos 3 anos da pesquisa, o que ocorre inversamente na associação 
# de 'discordo totalmente' no mesmo período. Conclui-se que a popularidade do
# prefeito diminui ao longo dos três anos.

################################################################################
#
# 4. Análise de Perfil do Investidor: Aplicações e se possui filhos 

investimento <- read.dta("Perfil_Investidor x Aplicação x Filhos.dta")

investimento %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 14)

summary(investimento)

#Tabelas de Contingências
tabela.perfil.aplicacao <- table(investimento$perfil, investimento$aplicação)
tabela.perfil.aplicacao
sjt.xtab(var.row = investimento$perfil,
         var.col = investimento$aplicação,
         show.exp = T)

tabela.perfil.filhos <- table(investimento$perfil, investimento$filhos)
tabela.perfil.filhos
sjt.xtab(var.row = investimento$perfil,
         var.col = investimento$filhos,
         show.exp = T)

tabela.aplicacao.filhos <- table(investimento$aplicação, investimento$filhos)
tabela.aplicacao.filhos
sjt.xtab(var.row = investimento$aplicação,
         var.col = investimento$filhos,
         show.exp = T)

#Análise de Correspondência
ACM.investimento <- MCA(investimento[,2:4], method = "Indicador")
ACM.investimento$var$coord
ACM.investimento$eig

#Categorias e Mapa Perceptual
categorias <- apply(investimento[,2:4], 
                    MARGIN = 2, 
                    FUN = function(x) nlevels(as.factor(x)))
categorias

categ.MP <- data.frame(ACM.investimento$var$coord, 
                       Variável = rep(names(categorias), categorias))
categ.MP %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Dim.1, 
             y = Dim.2, 
             label = Categoria, 
             color = Variável, 
             fill = Variável,
             shape = Variável)) +
  geom_point() +
  geom_label_repel(color = "red") +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey") +
  labs(x = paste("Dimensão 1:", paste0(round(ACM.investimento$eig[1,2], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(ACM.investimento$eig[2,2], 2), "%"))) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

# A presença de filhos aumenta a sua associação com o perfil conservador
# De forma contrária, pessoas sem filhos se associam fortemente ao perfil agressivo

################################################################################
#
# 5. Pesquisa de 500 executivos sobre a qualidade de 3 empresas de consultoria 

consultoria <- read.dta("Consultoria.dta")

consultoria %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 14)

summary(consultoria)

#Tabelas de Contingências
tabela.qualidade.pontualidade <- table(consultoria$qualidade, 
                                       consultoria$pontualidade)
tabela.qualidade.pontualidade
sjt.xtab(var.row = consultoria$qualidade,
         var.col = consultoria$pontualidade,
         show.exp = T)

tabela.qualidade.empresa <- table(consultoria$qualidade, 
                                  consultoria$empresa)
tabela.qualidade.empresa
sjt.xtab(var.row = consultoria$qualidade,
         var.col = consultoria$empresa,
         show.exp = T)

tabela.pontualidade.empresa <- table(consultoria$pontualidade, 
                                     consultoria$empresa)
tabela.pontualidade.empresa
sjt.xtab(var.row = consultoria$pontualidade,
         var.col = consultoria$empresa,
         show.exp = T)

#Análise de Correspondência
ACM.consultoria <- MCA(consultoria, method = "Indicador")
ACM.consultoria$var$coord
ACM.consultoria$eig

#Categorias e Mapa Perceptual
categorias <- apply(consultoria, 
                    MARGIN = 2, 
                    FUN = function(x) nlevels(as.factor(x)))
categ.MP <- data.frame(ACM.consultoria$var$coord, 
                       Variável = rep(names(categorias), categorias))
categ.MP %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Dim.1, 
             y = Dim.2, 
             label = Categoria, 
             color = Variável, 
             fill = Variável,
             shape = Variável)) +
  geom_point() +
  geom_label_repel(color = "red") +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "grey") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey") +
  labs(x = paste("Dimensão 1:", paste0(round(ACM.consultoria$eig[1,2], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(ACM.consultoria$eig[2,2], 2), "%"))) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

# A empresa 'Montvero' (coincidentemente a mesma empresa do Fávero!), se associa
# fortemente à qualidade ótima do serviço. Além de se associar levemente ao 
# 'Sim' de pçercepção ao respeito aos prazos.

#Apenas 'qualidade' e 'empresa' na análise de correspondência
consultoria2 <- consultoria[,-2]
summary(consultoria2)

tabela.consultoria2 <- table(consultoria2$qualidade, 
                             consultoria2$empresa)
tabela.consultoria2
sjt.xtab(var.row = consultoria2$qualidade,
         var.col = consultoria2$empresa,
         show.exp = T)

quiconsultoria <- chisq.test(tabela.consultoria2)
data.frame(quiconsultoria$stdres) %>%
  rename(qualidade = 1,
         empresa = 2) %>% 
  ggplot(aes(x = qualidade, 
             y = empresa, 
             fill = Freq, 
             label = round(Freq,3))) +
  geom_tile() +
  geom_label(size = 3, fill = "white") +
  scale_fill_gradient2(low = "#440154FF", 
                       mid = "white", 
                       high = "#FDE725FF",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.text.x = element_text())

anacor.consultoria <- CA(tabela.consultoria2)
anacor.consultoria$eig

ca_coordenadas <- rbind(anacor.consultoria$row$coord, anacor.consultoria$col$coord)
data.frame(t(c(nrow(tabela.consultoria2), ncol(tabela.consultoria2))), row.names = "") %>% 
  rename(qualidade = X1, empresa = X2) -> id_var
ca_coordenadas_final <- data.frame(ca_coordenadas, 
                                   Variable = rep(names(id_var), id_var))
ca_coordenadas_final %>% 
  rownames_to_column() %>% 
  rename(Category = 1) %>% 
  ggplot(aes(x = Dim.1, 
             y = Dim.2, 
             label = Category, 
             fill = Variable,
             color = Variable,
             shape = Variable)) +
  geom_point(size = 2) +
  geom_label_repel(max.overlaps = 100,
                   size = 3,
                   color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = paste("Dimension 1:", paste0(round(anacor.consultoria$eig[1,2], digits = 2), "%")),
       y = paste("Dimension 2:", paste0(round(anacor.consultoria$eig[2,2], digits = 2), "%"))) +
  scale_fill_viridis_d(option = "cividis") +
  scale_color_viridis_d(option = "cividis") +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")

# A associação verificada na ANACOR segue a mesma ao que foi considerado na ACM,
# quando apenas verifca as associações entre empresa e qualidade do serviço
