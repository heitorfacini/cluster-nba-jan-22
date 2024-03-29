#instalando pacotes
library(tibble)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(tidyverse) 
library(cluster) 
library(dendextend)
library(factoextra) 
library(fpc) 
library(gridExtra)
library(readxl)

#Carrega os dados Avan�ados dos times da NBA do dia 31 de janeiro de 2022
nba_stats <- read.csv(file = 'advanced_31-01.csv')


#Para realizar a divis�o em clusters, vou utilizar apenas os dados de Defensive Rating e
manter <- c('Time', 'OFFRTG', 'DEFRTG')
nba_stats_rtg <-  nba_stats[manter]

#Transformando os dados de strings para dados num�ricos
nba_stats_rtg <- nba_stats_rtg %>% 
  mutate_at(vars(OFFRTG, DEFRTG), 
            ~parse_number(.x,locale = locale(decimal_mark = ",")))


#Plotando em um gr�fico de dispers�o os dados e saber em que quadrante os times est�o
graf_def_ofrt <- ggplot(nba_stats_rtg, aes(x=OFFRTG, y=DEFRTG)) +
  geom_point(color = "blue", size = 3) +
  labs(subtitle = "Times da NBA",
       y = "Defensive Rating", x = "Offensive Rating",
       caption = "Fonte: NBA") +
  geom_label_repel(aes(label = Time),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic()


#Para analisar os dados do plot: quanto menor o defensive rating melhor. Quanto maior o offensive rating melhor
plot(graf_def_ofrt)


#Tirando o �ndice e adicionando a coluna time no nome
nba_stats_rtg <- data.frame(column_to_rownames(nba_stats_rtg, var = "Time"))


#Apesar de estarem na mesma unidade (pontos por 100 posses de bola), � importante padronizar para
#ver a rela��o deles em comparativo com a m�dia
nba_stats_rtg.padronizado <- scale(nba_stats_rtg)


#antes, vamos testar o m�todo de dendograma, calculando as dist�ncias e ver o comportamento
distancia <- dist(nba_stats_rtg.padronizado, method = "euclidean")
cluster.hierarquico <- hclust(distancia, method = "single" )
plot(cluster.hierarquico, cex = 0.6, hang = -1)
rect.hclust(cluster.hierarquico, k = 4)
fviz_nbclust(nba_stats_rtg.padronizado, FUN = hcut, method = "wss")
grupo_NBA <- cutree(cluster.hierarquico, k = 4)


#A partir da an�lise do m�todo elbow, o n�mero ideal de clusters parece o de 4
table(grupo_NBA)
nba_grupos <- data.frame(grupo_NBA)
Base_nba_fim <- cbind(nba_stats_rtg, nba_grupos)
view (Base_nba_fim)

#Existem algumas caracter�sticas em comum, por exemplo, o Orlando Magic e o Detroit Pistons est�o mal na temporada
#Por�m, a divis�o criou um grupo de 26 times, 1 de 2 times e 2 de 1 time. A an�lise fica prejudicada
#Vamos tentar pelo K-Means

#Inicialmente, vamos testar pelo m�todo Elbow como ficaria o n�mero ideal de clusters

fviz_nbclust(nba_stats_rtg.padronizado, FUN = kmeans, method = "wss")

#Seguindo o resultado, o n�mero 4 ou 6 parecem os mais ideais. Vamos testando com todos at� 4
nba_stats_rtg.k2 <- kmeans(nba_stats_rtg.padronizado, centers = 2)
nba_stats_rtg.k3 <- kmeans(nba_stats_rtg.padronizado, centers = 3)
nba_stats_rtg.k4 <- kmeans(nba_stats_rtg.padronizado, centers = 4)



G1 <- fviz_cluster(nba_stats_rtg.k2, geom = "point", data = nba_stats_rtg.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(nba_stats_rtg.k3, geom = "point",  data = nba_stats_rtg.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(nba_stats_rtg.k4, geom = "point",  data = nba_stats_rtg.padronizado) + ggtitle("k = 4")
grid.arrange(G1, G2, G3, nrow = 2)

#A princ�pio, o m�todo KMeans se mostra mais interessante, j� que divide em grupos mais populosos e
#com n�mero parecidos de membros

#vamos analisar o cluster K4
fviz_cluster(nba_stats_rtg.k4, data = nba_stats_rtg.padronizado, main = "Cluster K4")




#Existe similaridade nos Clusters, a princ�pio. Cluster 1 � de times tankando. Cluster 2 � de times
#De meio de tabela para baixo. Cluster 3 � de times bons. O Cluster 4 tamb�m tem times bons.
#Ainda est� meio fraco, vou testar a op��o de 6 clusters e ver se faz mais sentido
nba_stats_rtg.k5 <- kmeans(nba_stats_rtg.padronizado, centers = 5)

nba_stats_rtg.k6 <- kmeans(nba_stats_rtg.padronizado, centers = 6)
fviz_cluster(nba_stats_rtg.k6, data = nba_stats_rtg.padronizado, main = "Cluster K6")


#Agora o grupo de 6 parece fazer bem mais sentido. Vamos separar os grupos

nbafit <- data.frame(nba_stats_rtg.k6$cluster)
table(nbafit)
nba_stats_rtg_kmeans <- cbind(nba_stats_rtg, nbafit)
view(nba_stats_rtg_kmeans)

#Agora, vamos entender a partir do Defensive Rating e Offensive Rating o que cada grupo representa

mediagrupo <- nba_stats_rtg_kmeans %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Def_RTG = mean(DEFRTG), 
            Off_Rtg = mean(OFFRTG))
view(mediagrupo)


#Segundo o c�digo rodado na �poca, chegamos a
#Grupo 1:Atlanta Hawks, Charlotte Hornets, Utah Jazz
#Grupo 2: Houston Rockets, Portland Trail Blazers, Sacramento Kings, New Orleans Pelicans, Washington Wizards
#Grupo 3: San Antonio Spurs, Chicago Bulls, Toronto Raptors, Brooklyn Nets, Minnesota Timberwolves, Denver Nuggets
#Toronto Raptors, Philadelphia 76ers, Memphis Grizzlies, Milwaukee Bucks, Miami Heat
#Grupo 4: Cleveland Cavaliers, Phoenix Suns e Golden State Warriors
#Grupo 5: Los Angeles Lakers, New York Knicks, LA Clippers, Dallas Mavericks, Boston Celtics
#Grupo 6: Detroit Pistons, Orlando Magic, Oklahoma City Thunder

#O grupo 1 traz times muito bons no ataque e mal na defesa, mas que compensam pelo ataque excelente
#O grupo 2 traz times ruins no defensive rating e medianos no offensive rating. Brigam l� embaixo
#O grupo 3 � composto por times bons no ataque e n�o t�o bons na defesa, mas que s�o compensados pelo
#ataque
#O grupo 4 � composto pelos melhores times da NBA, com um excelente defesnvie rating e um bom offensive rating. 
#O Grupo 5 � composto por times medianos, com um um bom offensive Rating e um bom defensive rating
#estando com um saldo quase 0
#O grupo 6 � composto por times p�ssimos no ataque e na defesa


#Vamos analisar a amplitude do grupo
amplitude_grupo<- nba_stats_rtg_kmeans %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Def_RTG = range(DEFRTG), 
            Off_Rtg = range(OFFRTG))
amplitude_grupo
# n�o demonstra uma amplitude t�o alta


#vamos analisar a vari�ncia
var_grupo <- nba_stats_rtg_kmeans %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Def_RTG = var(DEFRTG), 
            Off_Rtg = var(OFFRTG))
var_grupo
# a vari�ncia � maior no grupo 3

#vamos analisar o desvio padr�o
sd_grupo <-  nba_stats_rtg_kmeans %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Def_RTG = sd(DEFRTG), 
            Off_Rtg = sd(OFFRTG))
sd_grupo
#No geral, s�o desvios baixos, mostrando grupos bem homog�neos quanto �s vari�veis. 

#Vamos incorporar outras stats para analisar como eles se comportam e entender se o rating ofensivo e defensivo 
#influenciam em n�meros de vit�rias. Al�m disso, vamos ver o n�mero de 3pts, assist�ncias e plus minus

nba_trad_stats <- read.csv(file = 'traditional_31-01.csv')

manter_trad <- c('Time','Pct_vitorias', 'PLUS_MINUS','X3P_PCT', 'X3PTM', 'AST')
nba_trad_stats <- nba_trad_stats[manter_trad]
nba_trad_stats <- data.frame(column_to_rownames(nba_trad_stats, var = "Time"))
nba_trad_stats <- nba_trad_stats %>% 
  mutate_at(vars(Pct_vitorias, PLUS_MINUS, X3P_PCT, X3PTM, AST), 
            ~parse_number(.x,locale = locale(decimal_mark = ",")))

#vamos adicionar mais uns valores do advanced, como TS e PACE.
#PACE = n�mero de posses para o jogo
#TS = estat�sticas que contabiliza quantos pontos os times fazem por posse, normalizando 100% como 2 pontos


nba_adv_stats <- read.csv(file = 'advanced_31-01.csv')
manter_advanced <- c('Time', 'TS_PCT', 'PACE')
nba_adv_stats <- nba_adv_stats[manter_advanced]
nba_adv_stats <- data.frame(column_to_rownames(nba_adv_stats, var = "Time"))
nba_adv_stats <- nba_adv_stats %>% 
  mutate_at(vars(TS_PCT, PACE), 
            ~parse_number(.x,locale = locale(decimal_mark = ",")))

#agora, junto os valores



grupo_nba_final <- cbind (nba_stats_rtg_kmeans, nba_adv_stats)
grupo_nba_final <- cbind (grupo_nba_final, nba_trad_stats)
view(grupo_nba_final)


#analisando a m�dia de vit�rias de cada grupo, o plus minus e o pace

mediagrupo_fim <- grupo_nba_final %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Pct_vitorias = mean(Pct_vitorias), 
            PLUS_MINUS = mean(PLUS_MINUS), 
            PACE = mean(PACE))
view(mediagrupo_fim)

#grupo 1: maior pace da NBA (jogo super r�pido e � o grupo mais ofensivo), plus minus positivo e terceira maior
#m�dia de vit�rias
#grupo 2: porcentagem de vit�rias baixa, plus minus negativo e pace intermedi�rio
#grupo 3: segundo melhor grupo da nba, com uma boa porcentagem de vit�rias, pace intermedi�rio e um plus minus 
#positivo. Parecido com o segundo grupo, mas mais equilibrado
#grupo 4: porcentagem de vit�rias alta, com um pace intermedi�rio e o maior plus minus
#grupo 5: porcentagem de vit�rias m�dias com o pace mais baixo e um plus minus que chega pr�ximo do 0
#grupo 6: pior grupo da nba, com o segundo maior pace, mas com muitos erros e poucas vit�rias e menor
# plus minus

#analisando a vari�ncia de vit�rias de cada grupo, o plus minus e o pace

var_grupo_fim <- grupo_nba_final %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Pct_vitorias = var(Pct_vitorias), 
            PLUS_MINUS = var(PLUS_MINUS), 
            PACE = var(PACE))

view(var_grupo_fim)
#os grupos demonstraram pouca vari�ncia no n�mero de vit�rias, uma vari�ncia elevada no 
#plus minus e m�dia no pace

#analisando a vari�ncia de vit�rias de cada grupo, o plus minus e o pace

sd_grupo_fim <- grupo_nba_final %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Pct_vitorias = sd(Pct_vitorias), 
            PLUS_MINUS = sd(PLUS_MINUS), 
            PACE = sd(PACE))

view(sd_grupo_fim)
# o grupo 4 e o grupo 3 tem um desvio padr�o elevado na porcentagem de vit�rias, o grupo 2 tem um desvio padr�o
# elevado no plus minus e o grupo 1 tem um desvio padr�o elevado no pace

#analisando a m�dia de TS, 3PTS e AST

mediagrupo_fim_ast_arremesso <- grupo_nba_final %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            TS = mean(TS_PCT), 
            X3P_PCT = mean(X3P_PCT), 
            X3PTM = mean(X3PTM),
            AST = mean(AST))
view(mediagrupo_fim_ast_arremesso)

#os 3 melhores grupos passam mais a bola, os 3 melhores grupos fazem mais bolas de tr�s, os 4 melhores
#grupos convertem mais cestas, no TS, o terceiro

#analisando a vari�ncia de ts de cada grupo, 3pts e ast

var_grupo_fim_ast_arremesso <- grupo_nba_final %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            TS = var(TS_PCT), 
            X3P_PCT = var(X3P_PCT), 
            X3PTM = var(X3PTM),
            AST = var(AST))
view(var_grupo_fim_ast_arremesso)

#os grupos demonstraram pouca vari�ncia no TS, muito pouca em porcentagem de 3 pontos, ligeiramente elevada em assist�ncias
# e em bolas de 3 convertidas


#analisando o desvio padr�o de ts de cada grupo, 3pts e ast

sd_grupo_fim_ast_arremesso <- grupo_nba_final %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            TS = sd(TS_PCT), 
            X3P_PCT = sd(X3P_PCT), 
            X3PTM = sd(X3PTM),
            AST = sd(AST))
view(sd_grupo_fim_ast_arremesso)

#os grupos demonstraram pouco desvio padr�o no TS, muito pouco em porcentagem de 3 pontos, ligeiramente elevada em assist�ncias
# e em bolas de 3 convertidas


