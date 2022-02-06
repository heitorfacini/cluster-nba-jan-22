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

#Carrega os dados Avançados dos times da NBA do dia 31 de janeiro de 2022
nba_stats <- read.csv(file = 'advanced_31-01.csv')


#Para realizar a divisão em clusters, vou utilizar apenas os dados de Defensive Rating e
manter <- c('Time', 'OFFRTG', 'DEFRTG')
nba_stats_rtg <-  nba_stats[manter]

#Transformando os dados de strings para dados numéricos
nba_stats_rtg <- nba_stats_rtg %>% 
  mutate_at(vars(OFFRTG, DEFRTG), 
            ~parse_number(.x,locale = locale(decimal_mark = ",")))


#Plotando em um gráfico de dispersão os dados e saber em que quadrante os times estão
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


#Tirando o índice e adicionando a coluna time no nome
nba_stats_rtg <- data.frame(column_to_rownames(nba_stats_rtg, var = "Time"))


#Apesar de estarem na mesma unidade (pontos por 100 posses de bola), é importante padronizar para
#ver a relação deles em comparativo com a média
nba_stats_rtg.padronizado <- scale(nba_stats_rtg)


#antes, vamos testar o método de dendograma, calculando as distâncias e ver o comportamento
distancia <- dist(nba_stats_rtg.padronizado, method = "euclidean")
cluster.hierarquico <- hclust(distancia, method = "single" )
plot(cluster.hierarquico, cex = 0.6, hang = -1)
rect.hclust(cluster.hierarquico, k = 4)
fviz_nbclust(nba_stats_rtg.padronizado, FUN = hcut, method = "wss")
grupo_NBA <- cutree(cluster.hierarquico, k = 4)


#A partir da análise do método elbow, o número ideal de clusters parece o de 4
table(grupo_NBA)
nba_grupos <- data.frame(grupo_NBA)
Base_nba_fim <- cbind(nba_stats_rtg, nba_grupos)
view (Base_nba_fim)

#Existem algumas características em comum, por exemplo, o Orlando Magic e o Detroit Pistons estão mal na temporada
#Porém, a divisão criou um grupo de 26 times, 1 de 2 times e 2 de 1 time. A análise fica prejudicada
#Vamos tentar pelo K-Means

#Inicialmente, vamos testar pelo método Elbow como ficaria o número ideal de clusters

fviz_nbclust(nba_stats_rtg.padronizado, FUN = kmeans, method = "wss")

#Seguindo o resultado, o número 4 ou 6 parecem os mais ideais. Vamos testando com todos até 4
nba_stats_rtg.k2 <- kmeans(nba_stats_rtg.padronizado, centers = 2)
nba_stats_rtg.k3 <- kmeans(nba_stats_rtg.padronizado, centers = 3)
nba_stats_rtg.k4 <- kmeans(nba_stats_rtg.padronizado, centers = 4)



G1 <- fviz_cluster(nba_stats_rtg.k2, geom = "point", data = nba_stats_rtg.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(nba_stats_rtg.k3, geom = "point",  data = nba_stats_rtg.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(nba_stats_rtg.k4, geom = "point",  data = nba_stats_rtg.padronizado) + ggtitle("k = 4")
grid.arrange(G1, G2, G3, nrow = 2)

#A princípio, o método KMeans se mostra mais interessante, já que divide em grupos mais populosos e
#com número parecidos de membros

#vamos analisar o cluster K4
fviz_cluster(nba_stats_rtg.k4, data = nba_stats_rtg.padronizado, main = "Cluster K4")




#Existe similaridade nos Clusters, a princípio. Cluster 1 é de times tankando. Cluster 2 é de times
#De meio de tabela para baixo. Cluster 3 é de times bons. O Cluster 4 também tem times bons.
#Ainda está meio fraco, vou testar a opção de 6 clusters e ver se faz mais sentido
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


#Segundo o código rodado na época, chegamos a
#Grupo 1:Atlanta Hawks, Charlotte Hornets, Utah Jazz
#Grupo 2: Houston Rockets, Portland Trail Blazers, Sacramento Kings, New Orleans Pelicans, Washington Wizards
#Grupo 3: San Antonio Spurs, Chicago Bulls, Toronto Raptors, Brooklyn Nets, Minnesota Timberwolves, Denver Nuggets
#Toronto Raptors, Philadelphia 76ers, Memphis Grizzlies, Milwaukee Bucks, Miami Heat
#Grupo 4: Cleveland Cavaliers, Phoenix Suns e Golden State Warriors
#Grupo 5: Los Angeles Lakers, New York Knicks, LA Clippers, Dallas Mavericks, Boston Celtics
#Grupo 6: Detroit Pistons, Orlando Magic, Oklahoma City Thunder

#O grupo 1 traz times muito bons no ataque e mal na defesa, mas que compensam pelo ataque excelente
#O grupo 2 traz times ruins no defensive rating e medianos no offensive rating. Brigam lá embaixo
#O grupo 3 é composto por times bons no ataque e não tão bons na defesa, mas que são compensados pelo
#ataque
#O grupo 4 é composto pelos melhores times da NBA, com um excelente defesnvie rating e um bom offensive rating. 
#O Grupo 5 é composto por times medianos, com um um bom offensive Rating e um bom defensive rating
#estando com um saldo quase 0
#O grupo 6 é composto por times péssimos no ataque e na defesa


#Vamos analisar a amplitude do grupo
amplitude_grupo<- nba_stats_rtg_kmeans %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Def_RTG = range(DEFRTG), 
            Off_Rtg = range(OFFRTG))
amplitude_grupo
# não demonstra uma amplitude tão alta


#vamos analisar a variância
var_grupo <- nba_stats_rtg_kmeans %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Def_RTG = var(DEFRTG), 
            Off_Rtg = var(OFFRTG))
var_grupo
# a variância é maior no grupo 3

#vamos analisar o desvio padrão
sd_grupo <-  nba_stats_rtg_kmeans %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Def_RTG = sd(DEFRTG), 
            Off_Rtg = sd(OFFRTG))
sd_grupo
#No geral, são desvios baixos, mostrando grupos bem homogêneos quanto às variáveis. 

#Vamos incorporar outras stats para analisar como eles se comportam e entender se o rating ofensivo e defensivo 
#influenciam em números de vitórias. Além disso, vamos ver o número de 3pts, assistências e plus minus

nba_trad_stats <- read.csv(file = 'traditional_31-01.csv')

manter_trad <- c('Time','Pct_vitorias', 'PLUS_MINUS','X3P_PCT', 'X3PTM', 'AST')
nba_trad_stats <- nba_trad_stats[manter_trad]
nba_trad_stats <- data.frame(column_to_rownames(nba_trad_stats, var = "Time"))
nba_trad_stats <- nba_trad_stats %>% 
  mutate_at(vars(Pct_vitorias, PLUS_MINUS, X3P_PCT, X3PTM, AST), 
            ~parse_number(.x,locale = locale(decimal_mark = ",")))

#vamos adicionar mais uns valores do advanced, como TS e PACE.
#PACE = número de posses para o jogo
#TS = estatísticas que contabiliza quantos pontos os times fazem por posse, normalizando 100% como 2 pontos


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


#analisando a média de vitórias de cada grupo, o plus minus e o pace

mediagrupo_fim <- grupo_nba_final %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Pct_vitorias = mean(Pct_vitorias), 
            PLUS_MINUS = mean(PLUS_MINUS), 
            PACE = mean(PACE))
view(mediagrupo_fim)

#grupo 1: maior pace da NBA (jogo super rápido e é o grupo mais ofensivo), plus minus positivo e terceira maior
#média de vitórias
#grupo 2: porcentagem de vitórias baixa, plus minus negativo e pace intermediário
#grupo 3: segundo melhor grupo da nba, com uma boa porcentagem de vitórias, pace intermediário e um plus minus 
#positivo. Parecido com o segundo grupo, mas mais equilibrado
#grupo 4: porcentagem de vitórias alta, com um pace intermediário e o maior plus minus
#grupo 5: porcentagem de vitórias médias com o pace mais baixo e um plus minus que chega próximo do 0
#grupo 6: pior grupo da nba, com o segundo maior pace, mas com muitos erros e poucas vitórias e menor
# plus minus

#analisando a variância de vitórias de cada grupo, o plus minus e o pace

var_grupo_fim <- grupo_nba_final %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Pct_vitorias = var(Pct_vitorias), 
            PLUS_MINUS = var(PLUS_MINUS), 
            PACE = var(PACE))

view(var_grupo_fim)
#os grupos demonstraram pouca variância no número de vitórias, uma variância elevada no 
#plus minus e média no pace

#analisando a variância de vitórias de cada grupo, o plus minus e o pace

sd_grupo_fim <- grupo_nba_final %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            Pct_vitorias = sd(Pct_vitorias), 
            PLUS_MINUS = sd(PLUS_MINUS), 
            PACE = sd(PACE))

view(sd_grupo_fim)
# o grupo 4 e o grupo 3 tem um desvio padrão elevado na porcentagem de vitórias, o grupo 2 tem um desvio padrão
# elevado no plus minus e o grupo 1 tem um desvio padrão elevado no pace

#analisando a média de TS, 3PTS e AST

mediagrupo_fim_ast_arremesso <- grupo_nba_final %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            TS = mean(TS_PCT), 
            X3P_PCT = mean(X3P_PCT), 
            X3PTM = mean(X3PTM),
            AST = mean(AST))
view(mediagrupo_fim_ast_arremesso)

#os 3 melhores grupos passam mais a bola, os 3 melhores grupos fazem mais bolas de três, os 4 melhores
#grupos convertem mais cestas, no TS, o terceiro

#analisando a variância de ts de cada grupo, 3pts e ast

var_grupo_fim_ast_arremesso <- grupo_nba_final %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            TS = var(TS_PCT), 
            X3P_PCT = var(X3P_PCT), 
            X3PTM = var(X3PTM),
            AST = var(AST))
view(var_grupo_fim_ast_arremesso)

#os grupos demonstraram pouca variância no TS, muito pouca em porcentagem de 3 pontos, ligeiramente elevada em assistências
# e em bolas de 3 convertidas


#analisando o desvio padrão de ts de cada grupo, 3pts e ast

sd_grupo_fim_ast_arremesso <- grupo_nba_final %>% 
  group_by(nba_stats_rtg.k6$cluster) %>% 
  summarise(n = n(),
            TS = sd(TS_PCT), 
            X3P_PCT = sd(X3P_PCT), 
            X3PTM = sd(X3PTM),
            AST = sd(AST))
view(sd_grupo_fim_ast_arremesso)

#os grupos demonstraram pouco desvio padrão no TS, muito pouco em porcentagem de 3 pontos, ligeiramente elevada em assistências
# e em bolas de 3 convertidas


