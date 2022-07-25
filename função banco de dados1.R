library(readxl)
dados <- read_excel("C:/Users/bruno/Desktop/TCC/Dados/Dados_filtros_final.xlsx",
                    col_types = c("text","text","text","text","text","text","text","text","text","numeric","numeric","numeric",
                                  "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                  "numeric","numeric","numeric","numeric","numeric","numeric","numeric","text","text","text","text",
                                  "text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                  "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                  "numeric","numeric"))

dados$GAME_DATE = as.Date(dados$GAME_DATE, format = "%Y-%m-%d")

oeste <- c("Dallas Mavericks","Los Angeles Lakers","San Antonio Spurs","Memphis Grizzlies","Utah Jazz","Phoenix Suns","Golden State Warriors","Los Angeles Clippers",
           "Minnesota Timberwolves","Denver Nuggets","Portland Trail Blazers","Houston Rockets","Sacramento Kings",
           "Oklahoma City Thunder","New Orleans Pelicans","New Orleans Hornets","Seattle SuperSonics","New Orleans/Oklahoma City Hornets")
leste <- c("Detroit Pistons","Toronto Raptors","Cleveland Cavaliers","Orlando Magic","Boston Celtics","Miami Heat","Philadelphia 76ers","Chicago Bulls",
           "Milwaukee Bucks","Washington Wizards","New York Knicks","Indiana Pacers","Atlanta Hawks","Brooklyn Nets","Charlotte Hornets","Charlotte Bobcats",
           "New Jersey Nets")

temporadas <- c("2021-2022","2020-2021","2019-2020","2018-2019","2017-2018","2016-2017","2015-2016","2014-2015","2013-2014","2012-2013",
                "2011-2012","2010-2011","2009-2010","2008-2009","2007-2008","2006-2007","2005-2006","2004-2005")

temporadas_16<- c("2019-2020","2018-2019","2017-2018","2016-2017","2015-2016","2014-2015","2013-2014","2012-2013",
               "2011-2012","2010-2011","2009-2010","2008-2009","2007-2008","2006-2007","2005-2006","2004-2005")

temporadas_2<- c("2021-2022","2020-2021")

playoffs_ida <- list("2004-2005WEST" = c("Phoenix Suns","San Antonio Spurs","Dallas Mavericks","Seattle SuperSonics","Houston Rockets","Sacramento Kings","Denver Nuggets","Memphis Grizzlies"),
                     "2004-2005EAST" = c("Miami Heat","Detroit Pistons","Chicago Bulls","Boston Celtics","Washington Wizards","Indiana Pacers","Philadelphia 76ers","New Jersey Nets"),
                     "2005-2006WEST" = c("San Antonio Spurs","Phoenix Suns","Denver Nuggets","Dallas Mavericks","Memphis Grizzlies","Los Angeles Clippers","Los Angeles Lakers","Sacramento Kings"),
                     "2005-2006EAST" = c("Detroit Pistons","Miami Heat","New Jersey Nets","Cleveland Cavaliers","Washington Wizards","Indiana Pacers","Chicago Bulls","Milwaukee Bucks"),
                     "2006-2007WEST" = c("Dallas Mavericks","Phoenix Suns","San Antonio Spurs","Houston Rockets","Utah Jazz","Denver Nuggets","Golden State Warriors","Los Angeles Lakers"),
                     "2006-2007EAST" = c("Detroit Pistons","Cleveland Cavaliers","Toronto Raptors","Miami Heat","Chicago Bulls","New Jersey Nets","Washington Wizards","Orlando Magic"),
                     "2007-2008WEST" = c("Los Angeles Lakers","New Orleans Hornets","San Antonio Spurs","Houston Rockets","Phoenix Suns","Utah Jazz","Dallas Mavericks","Denver Nuggets"),
                     "2007-2008EAST" = c("Boston Celtics","Detroit Pistons","Orlando Magic","Cleveland Cavaliers","Washington Wizards","Toronto Raptors","Philadelphia 76ers","Atlanta Hawks"),
                     "2008-2009WEST" = c("Los Angeles Lakers","Denver Nuggets","San Antonio Spurs","Portland Trail Blazers","Houston Rockets","Dallas Mavericks","New Orleans Hornets","Utah Jazz"),
                     "2008-2009EAST" = c("Cleveland Cavaliers","Boston Celtics","Orlando Magic","Atlanta Hawks","Miami Heat","Philadelphia 76ers","Chicago Bulls","Detroit Pistons"),
                     "2009-2010WEST" = c("Los Angeles Lakers","Dallas Mavericks","Phoenix Suns","Denver Nuggets","Utah Jazz","Portland Trail Blazers","San Antonio Spurs","Oklahoma City Thunder"),
                     "2009-2010EAST" = c("Cleveland Cavaliers","Orlando Magic","Atlanta Hawks","Boston Celtics","Miami Heat","Milwaukee Bucks","Charlotte Bobcats","Chicago Bulls"),
                     "2010-2011WEST" = c("San Antonio Spurs","Los Angeles Lakers","Dallas Mavericks","Oklahoma City Thunder","Denver Nuggets","Portland Trail Blazers","New Orleans Hornets","Memphis Grizzlies"),
                     "2010-2011EAST" = c("Chicago Bulls","Miami Heat","Boston Celtics","Orlando Magic","Atlanta Hawks","New York Knicks","Portland Trail Blazers","Indiana Pacers"),
                     "2011-2012WEST" = c("San Antonio Spurs","Oklahoma City Thunder","Los Angeles Lakers","Memphis Grizzlies","Los Angeles Clippers","Denver Nuggets","Dallas Mavericks","Utah Jazz"),
                     "2011-2012EAST" = c("Chicago Bulls","Miami Heat","Indiana Pacers","Boston Celtics","Atlanta Hawks","Orlando Magic","New York Knicks","Philadelphia 76ers"),
                     "2012-2013WEST" = c("Oklahoma City Thunder","San Antonio Spurs","Denver Nuggets","Los Angeles Clippers","Memphis Grizzlies","Golden State Warriors","Los Angeles Lakers","Houston Rockets"),
                     "2012-2013EAST" = c("Miami Heat","New York Knicks","Indiana Pacers","Brooklyn Nets","Chicago Bulls","Atlanta Hawks","Boston Celtics","Milwaukee Bucks"),
                     "2013-2014WEST" = c("San Antonio Spurs","Oklahoma City Thunder","Los Angeles Clippers","Houston Rockets","Portland Trail Blazers","Golden State Warriors","Memphis Grizzlies","Dallas Mavericks"),
                     "2013-2014EAST" = c("Indiana Pacers","Miami Heat","Toronto Raptors","Chicago Bulls","Washington Wizards","Brooklyn Nets","Charlotte Bobcats","Atlanta Hawks"),
                     "2014-2015WEST" = c("Golden State Warriors","Houston Rockets","Los Angeles Clippers","Portland Trail Blazers","Memphis Grizzlies","San Antonio Spurs","Dallas Mavericks","New Orleans Pelicans"),
                     "2014-2015EAST" = c("Atlanta Hawks","Cleveland Cavaliers","Chicago Bulls","Toronto Raptors","Washington Wizards","Milwaukee Bucks","Boston Celtics","Brooklyn Nets"),
                     "2015-2016WEST" = c("Golden State Warriors","San Antonio Spurs","Oklahoma City Thunder","Los Angeles Clippers","Portland Trail Blazers","Dallas Mavericks","Memphis Grizzlies","Houston Rockets"),
                     "2015-2016EAST" = c("Cleveland Cavaliers","Toronto Raptors","Miami Heat","Atlanta Hawks","Boston Celtics","Charlotte Hornets","Indiana Pacers","Detroit Pistons"),
                     "2016-2017WEST" = c("Golden State Warriors","San Antonio Spurs","Houston Rockets","Los Angeles Clippers","Utah Jazz","Oklahoma City Thunder","Memphis Grizzlies","Portland Trail Blazers"),
                     "2016-2017EAST" = c("Boston Celtics","Cleveland Cavaliers","Toronto Raptors","Washington Wizards","Atlanta Hawks","Milwaukee Bucks","Indiana Pacers","Chicago Bulls"),
                     "2017-2018WEST" = c("Houston Rockets","Golden State Warriors","Portland Trail Blazers","Oklahoma City Thunder","Utah Jazz","New Orleans Pelicans","San Antonio Spurs","Minnesota Timberwolves"),
                     "2017-2018EAST" = c("Toronto Raptors","Boston Celtics","Philadelphia 76ers","Cleveland Cavaliers","Indiana Pacers","Miami Heat","Milwaukee Bucks","Washington Wizards"),
                     "2018-2019WEST" = c("Golden State Warriors","Denver Nuggets","Houston Rockets","Portland Trail Blazers","Utah Jazz","Oklahoma City Thunder","San Antonio Spurs","Los Angeles Clippers"),
                     "2018-2019EAST" = c("Milwaukee Bucks","Toronto Raptors","Philadelphia 76ers","Boston Celtics","Indiana Pacers","Orlando Magic","Brooklyn Nets","Detroit Pistons"),
                     "2019-2020WEST" = c("Los Angeles Lakers","Los Angeles Clippers","Denver Nuggets","Houston Rockets","Oklahoma City Thunder","Utah Jazz","Dallas Mavericks","Portland Trail Blazers"),
                     "2019-2020EAST" = c("Milwaukee Bucks","Toronto Raptors","Boston Celtics","Indiana Pacers","Miami Heat","Philadelphia 76ers","Brooklyn Nets","Orlando Magic"),
                     "2020-2021WEST" = c("Utah Jazz","Phoenix Suns","Denver Nuggets","Los Angeles Clippers","Dallas Mavericks","Portland Trail Blazers","Los Angeles Lakers","Memphis Grizzlies"),
                     "2020-2021EAST" = c("Philadelphia 76ers","Brooklyn Nets","Milwaukee Bucks","New York Knicks","Atlanta Hawks","Miami Heat","Boston Celtics","Washington Wizards"),
                     "2021-2022WEST" = c("Phoenix Suns","Memphis Grizzlies","Golden State Warriors","Dallas Mavericks","Utah Jazz","Denver Nuggets","Minnesota Timberwolves","New Orleans Pelicans"),
                     "2021-2022EAST" = c("Miami Heat","Boston Celtics","Milwaukee Bucks","Philadelphia 76ers","Toronto Raptors","Chicago Bulls","Brooklyn Nets","Atlanta Hawks"))


banco = function(jogos,temp,mediamovel,times){
  require(dplyr)
  
  mat = matrix(ncol = 0, nrow = 0) # Criando uma matriz com 0 linhas e 0 colunas
  df=data.frame(mat) # Convertendo a matriz em um data frame
  
  
  for (j in 1:temp){ # Uusário escolhe quantas temporadas ele quer utilizar
    dados2 <- dados # Copiando os dados a um novo banco de dados
    dados2 <- dados2[order(dados2$GAME_DATE, decreasing = FALSE),] # Ordenando para pegar os primeiros jogos de cada temporada
    dados2 <- dados2[dados2$SEASON==temporadas[j], ] # Selecionando somente os dados da temporada "j"
    vetor_contagem <- numeric(length(unique(dados2$TEAM_NAME_HOME))) # Criando um vetor vazio para realizar a contagem da quantidade de jogos de cada time
    names(vetor_contagem) <- unique(dados2$TEAM_NAME_HOME) # Nomeando os campos do vetor vazio
    names(dados2) <- gsub("_HOME","",names(dados2)) # Excluindo a String "_HOME" das variáveis
    names(dados2) <- gsub("_AWAY","",names(dados2)) # Excluindo a String "_AWAY" das variáveis
    for (i in 1:nrow(dados2)){
      if (vetor_contagem[match(dados2[i,5], names(vetor_contagem))] < jogos){ # Ser selecionado quando o time aparecer na ala "HOME", usuário escolhe a quantidade de jogos em cada temporada que ele quer utilizar
        df <- rbind(df,dados2[i,c(1:29,32:33,36:55)]) # Adicionar essas linhas ao data frame e depois fazer adicionar um valor à contagem
        vetor_contagem[match(dados2[i,5], names(vetor_contagem))] <-  vetor_contagem[match(dados2[i,5], names(vetor_contagem))] + 1
      }
      if (vetor_contagem[match(dados2[i,31], names(vetor_contagem))] < jogos){ # Ser selecionado quando o time aparecer na ala "AWAY", usuário escolhe a quantidade de jogos em cada temporada que ele quer utilizar
        df <- rbind(df,dados2[i,c(1:3,30:55,6:7,10:29)]) # Adicionar essas linhas ao data frame e depois fazer adicionar um valor à contagem
        vetor_contagem[match(dados2[i,31], names(vetor_contagem))] <-  vetor_contagem[match(dados2[i,31], names(vetor_contagem))] + 1
      }
      
    }
    
  }
  
  df[,"TEAM_NAME_OPP"] <- NULL # Criando uma varivel vazia 
  
  
  for (r in 1:nrow(df)){
    conct <- NA
    conct <- paste(df[r,30], df[r,31])
    df[r, "TEAM_NAME_OPP"] <-  conct
  } # Fazendo concatenação da coluna 30 e 31 do df para formar o nome do time na coluna TEAM_NAME_OPP
  
  df <- df[,-c(30,31)] # Excluindo as colunas 30 e 31
  
  
  
  # Renomeando as variáveis dos oponentes
  colnames(df)[30] <- "MIN_OPP"
  colnames(df)[31] <- "FGM_OPP"
  colnames(df)[32] <- "FGA_OPP"
  colnames(df)[33] <- "FG_PCT_OPP"
  colnames(df)[34] <- "FG3M_OPP"
  colnames(df)[35] <- "FG3A_OPP"
  colnames(df)[36] <- "FG3_PCT_OPP"
  colnames(df)[37] <- "FTM_OPP"
  colnames(df)[38] <- "FTA_OPP"
  colnames(df)[39] <- "FT_PCT_OPP"
  colnames(df)[40] <- "OREB_OPP"
  colnames(df)[41] <- "DREB_OPP"
  colnames(df)[42] <- "REB_OPP"
  colnames(df)[43] <- "AST_OPP"
  colnames(df)[44] <- "STL_OPP"
  colnames(df)[45] <- "BLK_OPP"
  colnames(df)[46] <- "TOV_OPP"
  colnames(df)[47] <- "PF_OPP"
  colnames(df)[48] <- "PTS_OPP"
  colnames(df)[49] <- "PLUS_MINUS_OPP" 

  
  
  df <- na.omit(df) # Excluir as linhas que ficaram com NA
  
  df <- unique(df) # Excluindo as linhas repetidas
  
  
  WINS <- c() # Vetor vazio para adicionar dados
  LOSES <- c() # Vetor vazio para adicionar dados
  HOME <- c() # Vetor vazio para adicionar dados
  AWAY <- c() # Vetor vazio para adicionar dados
  WINS_HOME <- c() # Vetor vazio para adicionar dados
  LOSES_HOME <- c() # Vetor vazio para adicionar dados
  WINS_AWAY <- c() # Vetor vazio para adicionar dados
  LOSES_AWAY <- c() # Vetor vazio para adicionar dados

  
  for (w in 1:nrow(df)){ #Verificando a quantidade de vitórias e derrotas de cada time na temporada
    if (df$WL[w] == "W"){
      WINS <- append(WINS,1)
      LOSES <- append(LOSES,0)
    } else{
      WINS <- append(WINS,0)
      LOSES <- append(LOSES,1)
    }
  }
  
  
  for (h in 1:nrow(df)){ #Verificando a quantidade de jogos em casa e fora de cada time na temporada
    if (df$HA[h] == "H"){
      HOME <- append(HOME,1)
      AWAY <- append(AWAY,0)
    } else{
      HOME <- append(HOME,0) 
      AWAY <- append(AWAY,1)
    }
  }
  
  for (t in 1:nrow(df)){ # Verificando a quantidade de vitórias e derrotas dentro e fora de casa de cada time na temporada
    if(df$HA[t] == "H" && df$WL[t] == "W"){
      WINS_HOME <- append(WINS_HOME,1)
      LOSES_HOME <- append(LOSES_HOME,0)
      WINS_AWAY <- append(WINS_AWAY,0)
      LOSES_AWAY <- append(LOSES_AWAY,0)
      
    } else if(df$HA[t] == "H" && df$WL[t] == "L"){
      WINS_HOME <- append(WINS_HOME,0)
      LOSES_HOME <- append(LOSES_HOME,1)
      WINS_AWAY <- append(WINS_AWAY,0)
      LOSES_AWAY <- append(LOSES_AWAY,0)
      
    } else if(df$HA[t] == "A" && df$WL[t] == "W"){
      WINS_HOME <- append(WINS_HOME,0)
      LOSES_HOME <- append(LOSES_HOME,0)
      WINS_AWAY <- append(WINS_AWAY,1)
      LOSES_AWAY <- append(LOSES_AWAY,0)
      
    } else if(df$HA[t] == "A" && df$WL[t] == "L"){
      WINS_HOME <- append(WINS_HOME,0)
      LOSES_HOME <- append(LOSES_HOME,0)
      WINS_AWAY <- append(WINS_AWAY,0)
      LOSES_AWAY <- append(LOSES_AWAY,1)
    } 
  }
  
  df <- cbind(df, WINS) #Adicionando variável ao data frame
  df <- cbind(df, LOSES) #Adicionando variável ao data frame
  df <- cbind(df, HOME) #Adicionando variável ao data frame
  df <- cbind(df, AWAY) #Adicionando variável ao data frame
  df <- cbind(df, WINS_HOME) #Adicionando variável ao data frame
  df <- cbind(df, LOSES_HOME) #Adicionando variável ao data frame
  df <- cbind(df, WINS_AWAY) #Adicionando variável ao data frame
  df <- cbind(df, LOSES_AWAY) #Adicionando variável ao data frame
  
  df <- df%>%relocate(TEAM_NAME_OPP,.after = PLUS_MINUS ) # Realocando variável
  df4 <- df # Salvando o df para saber quais times jogou contra
  
  df2 <- df # Salvando df para poder manipular depois a média móvel
  df2 <- df2[,-30] # Excluindo o nome do time adversário
  df2 <- df2 %>% arrange(desc(GAME_DATE)) # Ordenando os jogos do mais recente ao mais antigo para fazer a média móvel
  
  df = df %>% group_by(SEASON, TEAM_NAME) %>%
    summarise(SEASON = SEASON,TEAM_NAME=TEAM_NAME,WINS = sum(WINS), LOSES = sum(LOSES),HOME = sum(HOME),AWAY = sum(AWAY),
              WINS_HOME = sum(WINS_HOME),WINS_AWAY = sum(WINS_AWAY),LOSES_HOME = sum(LOSES_HOME),
              LOSES_AWAY = sum(LOSES_AWAY),MIN=mean(MIN),FGM=mean(FGM),FGA=mean(FGA),FG_PCT=mean(FG_PCT),
              FG3M=mean(FG3M),FG3A=mean(FG3A),FG3_PCT=mean(FG3_PCT),FTM=mean(FTM),FTA=mean(FTA),FT_PCT=mean(FT_PCT),OREB=mean(OREB),
              DREB=mean(DREB),REB=mean(REB),AST=mean(AST),STL=mean(STL),BLK=mean(BLK), TOV =mean(TOV), PF=mean(PF),PTS=mean(PTS),
              PLUS_MINUS=mean(PLUS_MINUS),MIN_OPP=mean(MIN_OPP),FGM_OPP=mean(FGM_OPP),FGA_OPP=mean(FGA_OPP),FG_PCT_OPP=mean(FG_PCT_OPP),
              FG3M_OPP=mean(FG3M_OPP),FG3A_OPP=mean(FG3A_OPP),FG3_PCT_OPP=mean(FG3_PCT_OPP),FTM_OPP=mean(FTM_OPP),FTA_OPP=mean(FTA_OPP),
              FT_PCT_OPP=mean(FT_PCT_OPP),OREB_OPP=mean(OREB_OPP),DREB_OPP=mean(DREB_OPP),REB_OPP=mean(REB_OPP),AST_OPP=mean(AST_OPP),
              STL_OPP=mean(STL_OPP),BLK_OPP=mean(BLK_OPP), TOV_OPP =mean(TOV_OPP), PF_OPP=mean(PF_OPP),PTS_OPP=mean(PTS_OPP),
              PLUS_MINUS_OPP=mean(PLUS_MINUS_OPP),.groups = 'drop') 
  # Agrupando as linhas por SEASON e TEAM_NAME e gerando as estatísticas corretas   
  
  df = df %>% mutate(across(6:length(df), round, 2)) # Arredondando os valores para 2 casas decimais
  
  df <- unique(df) # Excluindo as linhas repetidas
  
  
  CONF <- c() # Criando um vetor vazio que irá dizer qual é a conferência de cada time
  for (a in 1:nrow(df)){
    if (df$TEAM_NAME[a] %in% oeste){ # Caso o time 'a' estiver no vetor oeste:
      CONF <- append(CONF,"WEST") # Ele será do oeste
    } else{
      CONF <- append(CONF,"EAST")# Se não, ele será do leste
    }
    
  }
  
  
  df <- cbind(df, CONF)# Adicionando o vetor da conferência no banco de dados
  
  
  CONF_TEAM <- c() # Criando um vetor vazio que irá dizer qual é a conferência de cada time
  for (a in 1:nrow(df)){
    if (df$TEAM_NAME[a] %in% oeste){ # Caso o time 'a' estiver no vetor oeste:
      CONF_TEAM <- append(CONF_TEAM,1) # Ele será do oeste
    } else{
      CONF_TEAM <- append(CONF_TEAM,0)# Se não, ele será do leste
    }
    
  }
  
  
  df <- cbind(df, CONF_TEAM)# Adicionando o vetor da conferência no banco de dados
  
  df$CONF_TEAM <- as.factor(df$CONF_TEAM)

  
  PLAYOFFS <- c() # Criando um vetor vazio que irá dizer se um time foi aos playoffs ou não
  for (a in 1:nrow(df)){
    conc = paste(df$SEASON[a],df$CONF[a],sep="") # Concatenando para melhorar a identificação
    for(b in c(1:36)){ # número de grupos na lista de quem foi aos playoffs
      playoffs_name = names(playoffs_ida)[b] # Pegando o nome de cada vetor da lista playoffs_ida
      if (conc == playoffs_name){ #Caso eles forem iguais:
        if (df$TEAM_NAME[a] %in% playoffs_ida[[b]][1:times]){ # Verificar se o time se encontra na lista indicada
          PLAYOFFS <- append(PLAYOFFS,"1")} # Se sim, ele participou dos playoffs
        else{
          PLAYOFFS <- append(PLAYOFFS,"0") # Se não, ele não participou
        } 
      }
    }
    
  }
  df <- cbind(df, PLAYOFFS) # Adicionando o vetor playoffs no banco de dados
  
  
  df <- df%>%relocate(CONF,.after = TEAM_NAME ) # Realocando variável
  df <- df%>%relocate(PLAYOFFS,.after = CONF ) # Realocando variável
  df <- df%>%relocate(CONF_TEAM,.after = PLAYOFFS ) # Realocando variável
  df <- df%>% relocate(PTS,.after = MIN ) # Realocando variável
  df <- df%>% relocate(PTS_OPP,.after = MIN_OPP ) # Realocando variável
 
  
  
  mat = matrix(ncol = 0, nrow = 0) # Criando uma matriz com 0 linhas e 0 colunas
  df3=data.frame(mat) # Convertendo a matriz em um data frame
  
  
  for (m in 1:temp){ # Uusário escolhe quantas temporadas ele quer utilizar
    df1 <-df2
    df1 <- df1[df1$SEASON==temporadas[m], ] # Selecionando somente os dados da temporada "m"
    vetor_contagem <- numeric(length(unique(df1$TEAM_NAME))) # Criando um vetor vazio para realizar a contagem da quantidade de jogos de cada time
    names(vetor_contagem) <- unique(df1$TEAM_NAME) # Nomeando os campos do vetor vazio
    for (n in 1:nrow(df1)){
      if (vetor_contagem[match(df1[n,5], names(vetor_contagem))] < mediamovel){ # Ser selecionado quando o time aparecer na ala "HOME", usuário escolhe a quantidade de jogos em cada temporada que ele quer utilizar
        df3 <- rbind(df3,df1[n,]) # Adicionar essas linhas ao data frame e depois fazer adicionar um valor à contagem
        vetor_contagem[match(df1[n,5], names(vetor_contagem))] <-  vetor_contagem[match(df1[n,5], names(vetor_contagem))] + 1
      }
      
    }
    
  }
  
  # Renomeando as variáveis de média móvel
  colnames(df3)[8] <- "WL_MM"
  colnames(df3)[9] <- "HA_MM"
  colnames(df3)[10] <- "MIN_MM"
  colnames(df3)[11] <- "FGM_MM"
  colnames(df3)[12] <- "FGA_MM"
  colnames(df3)[13] <- "FG_PCT_MM"
  colnames(df3)[14] <- "FG3M_MM"
  colnames(df3)[15] <- "FG3A_MM"
  colnames(df3)[16] <- "FG3_PCT_MM"
  colnames(df3)[17] <- "FTM_MM"
  colnames(df3)[18] <- "FTA_MM"
  colnames(df3)[19] <- "FT_PCT_MM"
  colnames(df3)[20] <- "OREB_MM"
  colnames(df3)[21] <- "DREB_MM"
  colnames(df3)[22] <- "REB_MM"
  colnames(df3)[23] <- "AST_MM"
  colnames(df3)[24] <- "STL_MM"
  colnames(df3)[25] <- "BLK_MM"
  colnames(df3)[26] <- "TOV_MM"
  colnames(df3)[27] <- "PF_MM"
  colnames(df3)[28] <- "PTS_MM"
  colnames(df3)[29] <- "PLUS_MINUS_MM" 
  colnames(df3)[30] <- "MIN_OPP_MM"
  colnames(df3)[31] <- "FGM_OPP_MM"
  colnames(df3)[32] <- "FGA_OPP_MM"
  colnames(df3)[33] <- "FG_PCT_OPP_MM"
  colnames(df3)[34] <- "FG3M_OPP_MM"
  colnames(df3)[35] <- "FG3A_OPP_MM"
  colnames(df3)[36] <- "FG3_PCT_OPP_MM"
  colnames(df3)[37] <- "FTM_OPP_MM"
  colnames(df3)[38] <- "FTA_OPP_MM"
  colnames(df3)[39] <- "FT_PCT_OPP_MM"
  colnames(df3)[40] <- "OREB_OPP_MM"
  colnames(df3)[41] <- "DREB_OPP_MM"
  colnames(df3)[42] <- "REB_OPP_MM"
  colnames(df3)[43] <- "AST_OPP_MM"
  colnames(df3)[44] <- "STL_OPP_MM"
  colnames(df3)[45] <- "BLK_OPP_MM"
  colnames(df3)[46] <- "TOV_OPP_MM"
  colnames(df3)[47] <- "PF_OPP_MM"
  colnames(df3)[48] <- "PTS_OPP_MM"
  colnames(df3)[49] <- "PLUS_MINUS_OPP_MM" 
  
  
  
  df3 <- na.omit(df3) # Excluir as linhas que ficaram com NA
  
  df3 <- unique(df3) # Excluindo as linhas repetida
  
  
  
  WINS_MM <- c() # Vetor vazio para adicionar dados
  LOSES_MM <- c() # Vetor vazio para adicionar dados
  HOME_MM <- c() # Vetor vazio para adicionar dados
  AWAY_MM <- c() # Vetor vazio para adicionar dados
  WINS_HOME_MM <- c() # Vetor vazio para adicionar dados
  LOSES_HOME_MM <- c() # Vetor vazio para adicionar dados
  WINS_AWAY_MM <- c() # Vetor vazio para adicionar dados
  LOSES_AWAY_MM <- c() # Vetor vazio para adicionar dados
  
  for (c in 1:nrow(df3)){ #Verificando a quantidade de vitórias e derrotas de cada time na temporada
    if (df3$WL_MM[c] == "W"){
      WINS_MM <- append(WINS_MM,1)
      LOSES_MM <- append(LOSES_MM,0)
    } else{
      WINS_MM <- append(WINS_MM,0)
      LOSES_MM <- append(LOSES_MM,1)
    }
  }
  
  for (d in 1:nrow(df3)){ #Verificando a quantidade de jogos em casa e fora de cada time na temporada
    if (df3$HA_MM[d] == "H"){
      HOME_MM <- append(HOME_MM,1)
      AWAY_MM <- append(AWAY_MM,0)
    } else{
      HOME_MM <- append(HOME_MM,0) 
      AWAY_MM <- append(AWAY_MM,1)
    }
  }
  
  for (e in 1:nrow(df3)){ # Verificando a quantidade de vitórias e derrotas dentro e fora de casa de cada time na temporada
    if(df3$HA_MM[e] == "H" && df3$WL_MM[e] == "W"){
      WINS_HOME_MM <- append(WINS_HOME_MM,1)
      LOSES_HOME_MM <- append(LOSES_HOME_MM,0)
      WINS_AWAY_MM <- append(WINS_AWAY_MM,0)
      LOSES_AWAY_MM <- append(LOSES_AWAY_MM,0)
      
    } else if(df3$HA_MM[e] == "H" && df3$WL_MM[e] == "L"){
      WINS_HOME_MM <- append(WINS_HOME_MM,0)
      LOSES_HOME_MM <- append(LOSES_HOME_MM,1)
      WINS_AWAY_MM <- append(WINS_AWAY_MM,0)
      LOSES_AWAY_MM <- append(LOSES_AWAY_MM,0)
      
    } else if(df3$HA_MM[e] == "A" && df3$WL_MM[e] == "W"){
      WINS_HOME_MM <- append(WINS_HOME_MM,0)
      LOSES_HOME_MM <- append(LOSES_HOME_MM,0)
      WINS_AWAY_MM <- append(WINS_AWAY_MM,1)
      LOSES_AWAY_MM <- append(LOSES_AWAY_MM,0)
      
    } else if(df3$HA_MM[e] == "A" && df3$WL_MM[e] == "L"){
      WINS_HOME_MM <- append(WINS_HOME_MM,0)
      LOSES_HOME_MM <- append(LOSES_HOME_MM,0)
      WINS_AWAY_MM <- append(WINS_AWAY_MM,0)
      LOSES_AWAY_MM <- append(LOSES_AWAY_MM,1)
    } 
  }
  
  df3 <- cbind(df3, WINS_MM) #Adicionando variável ao data frame
  df3 <- cbind(df3, LOSES_MM) #Adicionando variável ao data frame
  df3 <- cbind(df3, HOME_MM) #Adicionando variável ao data frame
  df3 <- cbind(df3, AWAY_MM) #Adicionando variável ao data frame
  df3 <- cbind(df3, WINS_HOME_MM) #Adicionando variável ao data frame
  df3 <- cbind(df3, LOSES_HOME_MM) #Adicionando variável ao data frame
  df3 <- cbind(df3, WINS_AWAY_MM) #Adicionando variável ao data frame
  df3 <- cbind(df3, LOSES_AWAY_MM) #Adicionando variável ao data frame
  
  
  df3$MIN_OPP_MM <- as.numeric(df3$MIN_OPP_MM)
  
  
  df3 = df3 %>% group_by(SEASON, TEAM_NAME) %>%
    summarise(SEASON = SEASON,TEAM_NAME=TEAM_NAME,WINS_MM = sum(WINS_MM), LOSES_MM = sum(LOSES_MM),HOME_MM = sum(HOME_MM),AWAY_MM = sum(AWAY_MM),
              WINS_HOME_MM = sum(WINS_HOME_MM),WINS_AWAY_MM = sum(WINS_AWAY_MM),LOSES_HOME_MM = sum(LOSES_HOME_MM),
              LOSES_AWAY_MM = sum(LOSES_AWAY_MM),MIN_MM=mean(MIN_MM),FGM_MM=mean(FGM_MM),FGA_MM=mean(FGA_MM),FG_PCT_MM=mean(FG_PCT_MM),
              FG3M_MM=mean(FG3M_MM),FG3A_MM=mean(FG3A_MM),FG3_PCT_MM=mean(FG3_PCT_MM),FTM_MM=mean(FTM_MM),FTA_MM=mean(FTA_MM),FT_PCT_MM=mean(FT_PCT_MM),OREB_MM=mean(OREB_MM),
              DREB_MM=mean(DREB_MM),REB_MM=mean(REB_MM),AST_MM=mean(AST_MM),STL_MM=mean(STL_MM),BLK_MM=mean(BLK_MM), TOV_MM =mean(TOV_MM), PF_MM=mean(PF_MM),PTS_MM=mean(PTS_MM),
              PLUS_MINUS_MM=mean(PLUS_MINUS_MM),MIN_OPP_MM=mean(MIN_OPP_MM),FGM_OPP_MM=mean(FGM_OPP_MM),FGA_OPP_MM=mean(FGA_OPP_MM),FG_PCT_OPP_MM=mean(FG_PCT_OPP_MM),
              FG3M_OPP_MM=mean(FG3M_OPP_MM),FG3A_OPP_MM=mean(FG3A_OPP_MM),FG3_PCT_OPP_MM=mean(FG3_PCT_OPP_MM),FTM_OPP_MM=mean(FTM_OPP_MM),FTA_OPP_MM=mean(FTA_OPP_MM),
              FT_PCT_OPP_MM=mean(FT_PCT_OPP_MM),OREB_OPP_MM=mean(OREB_OPP_MM),DREB_OPP_MM=mean(DREB_OPP_MM),REB_OPP_MM=mean(REB_OPP_MM),AST_OPP_MM=mean(AST_OPP_MM),
              STL_OPP_MM=mean(STL_OPP_MM),BLK_OPP_MM=mean(BLK_OPP_MM), TOV_OPP_MM =mean(TOV_OPP_MM), PF_OPP_MM=mean(PF_OPP_MM),PTS_OPP_MM=mean(PTS_OPP_MM),
              PLUS_MINUS_OPP_MM=mean(PLUS_MINUS_OPP_MM),.groups = 'drop')
  # Agrupando as linhas por SEASON e TEAM_NAME e gerando as estatísticas corretas  
  
  df3 = df3 %>% mutate(across(6:length(df3), round, 2)) # Arredondando os valores para 2 casas decimais
  
  df3 <- unique(df3) # Excluindo as linhas repetida
  
  
  # Criar variável Força de calendário
  df5 <- df[,c(1,2,6)] # Criando df5 com Temporada, nome do time e quantas vitórias ele teve até aquela quantidade x de jogos
  
  
  # Concatenando Temporada e nome de time para facilitar buscar informações de outro banco de dados
  for (r in 1:nrow(df5)){
    conct <- NA
    conct <- paste(df5[r,1], df5[r,2], sep ='')
    df5[r, "concatena"] <-  conct
  }
  
  df5 <- df5[,-c(1,2)] # Excluindo temporada e nome do time, ficando vitórias e a concatenação
  
  
  teste_team <-  df4[, c(1,5,30)] # Criando teste_team com temporada, o nome do time e o nome do time que ele jogou contra
  
  for (r in 1:nrow(teste_team)){ # Concatenando a temporada e o nome do time adversário, para facilitar buscar informações de outro banco de dados
    conct <- NA
    conct <- paste(teste_team[r,1], teste_team[r,3], sep ='')
    teste_team[r, "concatena"] <-  conct
  }
  
  # Juntando os 2 df criados, ele resulta em um df que apresenta o time, nome do time adversário e quantas vitórias o time adversário teve
  vitorias_opp <- left_join(teste_team,df5,by = "concatena")
  
  vitorias_opp$FC <- (vitorias_opp$WINS)/jogos
  
  # Agrupando por temporada e time, será feita a média da proporção de vitórias dos adversários. vitorias_opp_final apresenta então o time, a temporada e a força de calendário
  vitorias_opp_final = vitorias_opp %>% group_by(SEASON, TEAM_NAME) %>%
    summarise(TEAM_NAME,SEASON, FC = mean(FC), .groups = 'drop')
  
  
  vitorias_opp_final <- unique(vitorias_opp_final) # Excluindo as linhas duplicadas
  colnames(vitorias_opp_final)[3] <- "WINS_OPP" # Renomeando a variável força de calendário
  
  vitorias_opp_final <- unique(vitorias_opp_final) # Excluindo as linhas repetida
  
  
  df_final1 <- left_join(df,df3,by = c("TEAM_NAME","SEASON")) # Juntando banco de dados princial com banco de média móvel
  df_final <- left_join(df_final1,vitorias_opp_final,by = c("TEAM_NAME","SEASON")) # Juntando banco de dados já unificado com o de força de calendário
  
  df_final <- unique(df_final) # Excluindo as linhas repetidas
  
  
  
  return(df_final) # Bando de dados final
  
  
  
}




banco_dados <- banco(25,18,5,8)


banco_dados_teste <- banco_dados[481:nrow(banco_dados),]

banco_dados_trei <- banco_dados[-(481:nrow(banco_dados)),]

## Ridge e Lasso

x.t <- as.matrix(banco_dados_trei[, -(1:5)])
y.t <- as.numeric(banco_dados_trei[, 4])

x.teste <- as.matrix(banco_dados_teste[, -(1:5)])
y.teste <- as.numeric(banco_dados_teste[, 4])

