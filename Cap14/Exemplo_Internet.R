pacotes <- c("plotly","tidyverse","knitr","kableExtra","reshape2","ggrepel",
             "fastDummies","lmtest","splines","jtools","questionr","MASS",
             "pscl","overdisp","magick","foreign","stargazer","rio")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

########################################################################
# Exemplo tirado do site:
# https://francish.netlify.app/post/poisson-and-negative-binomial-regression-using-r/
########################################################################

articles <- rio::import("http://faculty.missouri.edu/huangf/data/poisson/articles.csv")
articles %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

freq(articles$art, cum = T, valid = F, total = T) %>%
  kable()%>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

fr <- table(articles$art) %>% data.frame
names(fr) <- c('articles', 'freq')
fr$articles <- as.numeric(as.character(fr$articles)) #convert factor to numeric
ggplot(fr, aes(x = articles, y = freq)) +
  geom_col() +
  theme_bw() +
  lims(y = c(0, 300)) + 
  geom_line() + 
  labs(x = "Number of articles", y = "Frequency") +
  geom_text(aes(x = articles, y = freq, label = freq, vjust = -1)) +
  theme(axis.title.y = element_text(angle = 0)) 

### Run the models
linear <- glm(art ~ fem + ment + phd + mar + kid5, 
              data = articles) #identity link, OLS
pois <- glm(art ~ fem + ment + phd + mar + kid5, 
            family = "poisson", data = articles) #Poisson
negb <- glm.nb(art ~ fem + ment + phd + mar + kid5, 
               data = articles) #negative binomial
#in the MASS package
zinb <- zeroinfl(art ~ fem + ment + phd + mar + kid5 | fem + ment + phd + mar + kid5,
                 dist = "negbin", data = articles) #zero inflated nb, in the pscl package

summ(pois, exp = T) #could exp the coefficients but this is
summ(negb, exp = T) #difference in marital status
summary(zinb) #more complex, has two parts to it. 

tmp <- data.frame(OLS = AIC(linear), pois = AIC(pois), 
                  negb = AIC(negb), zinb = AIC(zinb))
tmp %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)

#Plotting Probabilities
po.p <- predprob(pois) %>% colMeans
po.nb <- predprob(negb) %>% colMeans
po.zinb <- predprob(zinb) %>% colMeans
df <- data.frame(x = 0:max(articles$art), Poisson = po.p, 
                 NegBin = po.nb, Zinb = po.zinb)

obs <- table(articles$art) %>% prop.table() %>% data.frame #Observed
names(obs) <- c("x", 'Observed')

p1 <- predict(linear) %>% round() %>% table %>% prop.table %>% data.frame #for OLS
names(p1) <- c('x', 'OLS')

tmp <- merge(p1, obs, by = 'x', all = T)
tmp$x <- as.numeric(as.character(tmp$x))

comb <- merge(tmp, df, by = 'x', all = T)
comb[is.na(comb)] <- 0

comb2 <- comb[1:11, ] #just for the first 11 results, including zero

mm <- melt(comb2, id.vars = 'x', value.name = 'prob', variable.name = 'Model')
mm <- filter(mm, Model != "OLS") #can include the linear model too if you want
#the SAS note does not, so I am not including it

ggplot(mm, aes(x = x, y = prob, group = Model, col = Model)) +
  geom_line(aes(lty = Model), lwd = 1) +
  theme_bw() +
  labs(x = "Number of articles", y = 'Probability',
       title = "Models for number of published articles") +
  scale_color_manual(values = c('black', 'blue', 'red', 'green')) +
  scale_linetype_manual(values = c('solid', 'dotted', 'dotted', 'dotted')) +
  theme(legend.position=c(.75, .65), axis.title.y = element_text(angle = 0))
