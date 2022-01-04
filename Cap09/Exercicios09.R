################################################################################
#               EXERCÍCIOS DO LIVRO DO FÁVERO - CAP 9 - CLUSTERS               #
################################################################################
#Pacotes utilizados
pacotes <- c("tidyverse","knitr","kableExtra","cluster","gplots",
             "dendextend","factoextra","fpc","gridExtra","foreign")

# tidyverse: pacote para manipulacao de dados
# knitr: relatórios dinâmicos
# kableExtra: tabelas elegantes do Kable
# cluster: algoritmo de cluster
# gplots: pacote gráfico similar ao ggplot2
# dendextend: compara dendogramas
# factoextra: algoritmo de cluster e visualizacao
# fpc: algoritmo de cluster e visualizacao
# gridExtra: para a funcao grid arrange
# foreign: lê arquivos de pacotes estatísticos como SPSS, Stata, SAS, etc.

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
# 1. Clusters em uma base de dados de 100 alunos, com base em duas variáveis
# métricas (idade e renda), para a determinação de Bolsa de Estudos.

bolsa <- as.data.frame(read.dta(file = "Bolsa de Estudo.dta"))
bolsa %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 18)

# Análise de Clusters

rownames(bolsa) <- bolsa[,1]
bolsa <- bolsa[,-1]         #transforma a coluna dos nomes dos alunos
bolsa_pad <- scale(bolsa) #padroniza os dados numéricos

distancia1 <- dist(bolsa_pad, method = "euclidean")
cluster1 <- hclust(distancia1, method = "complete" )
plot(cluster1, cex = 0.6, hang = -1)

grupos1 <- cutree(cluster1, k = 3)
table(grupos1)

cluster_bolsas <- cbind(bolsa, as.data.frame(grupos1))
cluster_bolsas %>% ggplot() +
  geom_point(aes(x = idade,
                 y = renda,
                 color = as.factor(grupos1)),
             size = 3)

# O Antonio é um 'ponto fora da curva' nesta análise de clusters
# Tem mais de 80 anos e renda maior que $25000
# Vamos analisar o banco de dados sem o Antonio

bolsa["Antonio",]
bolsa_sem_Antonio <- bolsa[-9,]

dados_pad.novo <- scale(bolsa_sem_Antonio)
distancia.novo <- dist(dados_pad.novo, method = "euclidean")
cluster.novo <- hclust(distancia.novo, method = "complete" )
plot(cluster.novo, cex = 0.6, hang = -1)
rect.hclust(cluster.novo, k = 3)
grupos.novo <- cutree(cluster.novo, k = 3)
table(grupos.novo)

# Vemos que existem 3 clusters de alunos nesta nova análise

cluster_bolsas.novo <- cbind(bolsa_sem_Antonio, as.data.frame(grupos.novo))
cluster_bolsas.novo %>% ggplot() +
  geom_point(aes(x = idade,
                 y = renda,
                 color = as.factor(grupos.novo)),
             size = 3)

################################################################################
#
# 2. Pesquisa realizado em 18 lojas distribuidas em 3 regionais

varejo <- as.data.frame(read.dta(file = "Regional Varejista.dta"))
varejo %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 18)

rownames(varejo) <- varejo[,1]
varejo <- varejo[,-1] 
varejo_sonotas <- varejo[,-1]

distancia2 <- dist(varejo_sonotas, method = "euclidean")
cluster2 <- hclust(distancia2, method = "single" )
plot(cluster2, cex = 0.6, hang = -1)

# Análise k-means de 2, 3 e 4 clusters

notas.k2 <- kmeans(varejo_sonotas, centers = 2)
fviz_cluster(notas.k2, data = varejo_sonotas, main = "Cluster K2")
notas.k3 <- kmeans(varejo_sonotas, centers = 3)
fviz_cluster(notas.k3, data = varejo_sonotas, main = "Cluster K3")
notas.k4 <- kmeans(varejo_sonotas, centers = 4)
fviz_cluster(notas.k4, data = varejo_sonotas, main = "Cluster K4")

# Vamos fazer a análise WSS (cotovelo)
fviz_nbclust(varejo_sonotas, kmeans, method = "wss")

# Serão 2 clusters para nossa análise
grupos2 <- cutree(cluster2, k = 2)
table(grupos2)

cluster_notas <- cbind(varejo, as.data.frame(grupos2))

# Vamos analisar graficamente as notas das 18 lojas nos 3 quesitos
# Tanto pelas suas Regionais quanto pelos clusters criados

G1 <- cluster_notas %>% ggplot() +
      geom_point(aes(x = atendimento, y = sortimento,
                 color = as.factor(regional)), size = 3)
G2 <- cluster_notas %>% ggplot() +
      geom_point(aes(x = atendimento, y = organização,
                 color = as.factor(regional)), size = 3)
G3 <- cluster_notas %>% ggplot() +
      geom_point(aes(x = sortimento, y = organização,
                 color = as.factor(regional)), size = 3)
G4 <- cluster_notas %>% ggplot() +
      geom_point(aes(x = atendimento, y = sortimento,
                 color = as.factor(grupos2)), size = 3)
G5 <- cluster_notas %>% ggplot() +
      geom_point(aes(x = atendimento, y = organização,
                 color = as.factor(grupos2)), size = 3)
G6 <- cluster_notas %>% ggplot() +
      geom_point(aes(x = sortimento, y = organização,
                 color = as.factor(grupos2)), size = 3)

grid.arrange(G1, G2, G3, G4, G5, G6, nrow = 2)
# As lojas da Regional 3 estão no cluster 1, e tem as piores notas
# As demais lojas estão no cluster 2, e concentram as notas médias e altas
# Estas lojas das Regionais 1 e 2 poderiam ser divididas em mais 2 clusters
# Mas as lojas da Regional 1 possuem notas médias no quesito 'organização'
# e notas altas nos demais quesitos, e as lojas da Regional 2 possuem notas
# médias no quesito 'sortimento' e notas altas nos demais quesitos

################################################################################
#
# 3. Pesquisa em 35 empresas de setores diversos

pesquisa <- as.data.frame(read.dta(file = "Pesquisa Binária.dta"))
pesquisa %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 18)

pesquisa_dummy <- pesquisa[,-ncol(pesquisa)]
distancia3 <- dist(pesquisa_dummy, method = "binary")
cluster3 <- hclust(distancia3, method = "average")
plot(cluster3, cex = 0.6, hang = -1)
rect.hclust(cluster3, k = 3)

fviz_nbclust(pesquisa_dummy, kmeans, method = "wss")
grupos3 <- cutree(cluster3, k = 3)
table(grupos3)

setores <- cbind(pesquisa, grupos3)
table(setores[,-c(1:50)])
# Empresas do setores de educação e transporte ficaram no cluster 2
# enquanto as empresas do setor de saúde ficaram no cluster 1
# Existe uma única empresa de saúde que ficou em um cluster isolado

################################################################################
#
# 4. Compras de produtos hortifruti por 16 semanas em 4 meses

horti <- as.data.frame(read.dta(file = "Hortifrúti.dta"))
horti %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 18)

vendas <- data.frame(scale(horti[,3:5]))
rownames(vendas) <- paste(horti[,1], horti[,2])

# Análise de cllusters das semanas de vendas
dist.lin <- cor(vendas, method = "pearson")
dist.col <- cor(t(vendas), method = "pearson")
clust.lin <- hclust(as.dist(1-dist.lin), method = "single")
clust.col <- hclust(as.dist(1-dist.col), method = "single")

plot(clust.lin, cex = 0.6, hang = -1)
plot(clust.col, cex = 0.6, hang = -1)
rect.hclust(clust.col, k = 2)

mat.vendas <- as.matrix(vendas)
heatmap.2(t(mat.vendas), Rowv = as.dendrogram(clust.lin), 
          Colv = as.dendrogram(clust.col), scale = "row",
          col = bluered(100), trace = "none", density.info = "none")
# As vendas de maçã e laranja são mais frequentes nas primeiras semanas
# e as vendas de banana são mais frequentes nas segundas e terceiras semanas
