library(readr) #ler dados
library(magrittr)
library(dplyr)
library(nycflights13) #dados do aeroporto
library(tidyr)
library(stringr)
library(tidyverse) #tudo
library(openxlsx) #escrever em xlsx
library(jsonlite) # obter e usar dados de API
library(rvest) #facilita o consumo de dados em html
library(ggplot2) #constru??o de gr?ficos
library(RColorBrewer) #disco de cores para graficos
library(ggThemeAssist) #modificar temas dos graficos manualmente
library(hrbrthemes) #tema interessante usado no cap 9
library(ggrepel) #extensão do ggplot
library(plotly) #gráficos interativos
library(xts) #uma opção ao ts() para declarar série de tempo
library(dygraphs) #biblioteca para a visualização de dados temporais (interativo)


#
#### cap 3 ####

senado <- read_csv("senado.csv")

head(senado)
tail(senado)
class(senado)
str(senado)
summary(senado)

#execicios 3.4
#1
read_delim("TA_PRECOS_MEDICAMENTOS.csv", delim = "|")
#2
read_fwf("fwf-sample.txt", col_positions = fwf_widths(c(20,10,12),c('nomes', 'estado','c?digo')))

#### cap 4 ####
dim(senado)

vector.chr <- c('type1', 'type2', 'type3', 'type4')
vector.num <- c(1, 2, 5, 8, 1001)
vector.num.rep <- c(rep(2,5))
vector.num.seq <- c(seq(from =  0, to = 100, by = 5))
vector.log <- c(TRUE, TRUE, TRUE, FALSE, FALSE)

nome <- c('jose', 'joao', 'maria', 'janaina')
idade <- c(45, 12, 28, 31)
adulto <- c(TRUE, FALSE, TRUE, TRUE)
uf <- c('df', 'sp', 'rj', 'mg')
clientes <- data.frame(nome, idade, adulto, uf)
clientes

data('airquality')
summary(airquality)
is.na(airquality)

a <- 936
c <- ifelse(a >= 10000, 'valor alto', ifelse(a < 10000 & a > 1000, 'valor medio', 'valor baixo' ))
c

lista.de.arquivo <- list.files('dados_loop')
is.vector(lista.de.arquivo)
for(i in lista.de.arquivo){
  print(paste('leia o arquivo:', i))
}

for (i in 1:1000) {
  if((i %% 29 == 0) && (i %% 3 == 0))
    print(i)
}

automatico <- list.files('automatico')
length(automatico) == 0

#programa verifica se chegou um arquivo numa pasta
while(length(automatico) == 0){
  automatico <- list.files('automatico')
  if(length(automatico) > 0){
    print('o arquivo chegou!!')
    print('inicia a leitura dos dados')
    print('faz a manipula??o')
    print('envia o email informando a comclus?o ')
  } else{
      print('aguardando arquivo...')
      Sys.sleep(5) #codigo espera 5s antes de iniciar o loop novamente
    
    }
}

montanha_russa = function(palavra){
  retorno <- NULL
  for(i in 1:nchar(palavra)){
    if(i %% 2 == 0){
      retorno <- paste0(retorno, tolower(substr(palavra, i, i)))
    }else{
      retorno <- paste0(retorno, toupper(substr(palavra, i, i)))
      
      }
  };return(retorno)
}
montanha_russa('paralelepipado')

head(airquality$Ozone) #selecionar uma coluna do data frame
tail(airquality$Solar.R)
class(airquality$Ozone)
is.vector(airquality$Ozone) #saber se a coluna ? um vetor
unique(senado$Party) # Fun??o que retorna apenas os valores ?nicos, sem repeti??o, de um vetor

senado[2,]
senado[2,3]

head(senado[senado$Party == "PDT",]) #seleciona todos dos partido 

library(dplyr)
senadores.partidos <- senado %>% select(SenatorUpper, Party) #selecionar uma coluna
head(senadores.partidos)
senadores.partidos <- senado %>% select(-SenatorUpper, -Party) #retirar uma coluna
head(senadores.partidos)

senadoreres.pdt.df <- senado %>%
  select(SenatorUpper, Party, State) %>%
  filter(State == 'RJ', Party == 'PMDB') %>%
  distinct() #semelhante ao unique(),traz registros ?nicos semrepeti??o
head(senadoreres.pdt.df)

senadoreres.pdt.df <- senado %>%
  select(SenatorUpper, Party,State) %>%
  filter(Party == 'PMDB') %>%
  distinct()
head(senadoreres.pdt.df)

library(nycflights13)
data("flights")
str(flights)
media <- flights %>%
  group_by(month)%>%
  summarise(arr_delay_media = mean(arr_delay, na.rm = TRUE),
            dep_delay_media = mean(dep_delay, na.rm = TRUE))
media

media <- flights %>%
  group_by(month) %>%
  summarise(arr_delay_media = mean(arr_delay, na.rm = TRUE),
            dep_delay_media = mean(dep_delay, na.rm = TRUE))%>%
  arrange(dep_delay_media)
media

#Exercicios
#1
is.na(senado$State)
summary(senado)
senado2 <- na.omit(senado)
#2
head(senado2)
senadores.col.T <- senado2 %>%
  select(SenatorUpper, GovCoalition, Party) %>%
  filter(GovCoalition == 'TRUE') %>%
  distinct()
head(senadores.col.T)
senadores.col.F <- senado2 %>%
  select(SenatorUpper, GovCoalition, Party) %>%
  filter(GovCoalition == 'FALSE') %>%
  distinct()
head(senadores.col.F)
#3
quantidade <- senado2%>%
  group_by(Party)%>%
  summarise(tam = n()) %>%
  arrange(-tam)
quantidade
#table(senado2$Party)
#4
tipo.voto.S <- senado2 %>%
  group_by(Party)%>%
  summarise(quant = sum(Vote == 'S'))%>%
  arrange(-quant)
tipo.voto.S

tipo.voto.N <- senado2 %>%
  group_by(Party) %>%
  summarise(quant = sum(Vote == 'N'))%>%
  arrange(-quant)
tipo.voto.N
#5
voto.regiao <- senado2 %>%
  mutate(Regiao=ifelse(State%in%c('AM','AC','TO','PA','RO','RR'),
                       'Norte',ifelse(State %in%c('SP','MG','RJ','ES'),
                                      'Sudeste',ifelse(State%in%c('MT','MS','GO', 'DF'),
                                                       "Centro-Oeste",ifelse(State%in%c('PR','SC','RS'),
                                                                             'Sul','Nordeste'))))) %>%
  group_by(Regiao) %>%
  summarise(quant = sum(Vote == "S")) %>%
  arrange(-quant)
voto.regiao

voto_coligaçães <- senado %>%
  mutate( tipo.coalisao = ifelse(c(GovCoalition, "Coalisão", 'Não_Coalisão')))
#### cap 5 ####
library(tidyverse)
table1
table2
table3
table4a
#...
install.packages('tidyr')
library(tidyr)

data('USArrests')
str(USArrests)
head(USArrests)
#transformando o nome das linhas em colunas
USArrests$State <- row.names(USArrests)
USArrests
usa.long <- USArrests %>%
  gather(key = 'tipo_crime', value = 'valor', -State)

head(usa.long)

head(table2)
tabela2.wide <- table2 %>%
  spread(key = type, value = count)
tabela2.wide

tabela3.wide <- table3 %>%
  separate(rate, into = c('case', 'population'), sep = '/')
tabela3.wide

table_relatorio <- table2 %>%
  unite(type_year, type, year) %>%
  spread(key = type_year, value = count, sep = '_')
table_relatorio

install.packages('stringr')
library(stringr)

cnae.texto <- c('10 Fabrica??odeprodutosaliment?cios', '11 Fabrica??odebebidas',
                '12 Fabrica??odeprodutosdofumo', '13 Fabrica??odeprodutost?xteis',
                '14 Confec??o deartigosdovestu?rioeacess?rios',
                '15 Prepara??odecourosefabrica??odeartefatosdecouro,artigosparaviagemecal?ados',
                '16 Fabrica??odeprodutosdemadeira',
                '17 Fabrica??odecelulose,papeleprodutosdepapel')
cnae <- str_sub(cnae.texto, 0, 2)
texto <- str_sub(cnae.texto,4)

telefones <- c('9931-9512', '8591-5892', '8562-1923')
str_replace(telefones,'-','')
cnpj <- c('19.702.231/9999-98', '19.498.482/9999-05', '19.499.583/9999-50', '19.500.999/9999-46')
str_replace_all(cnpj, '\\.|/|-', '')

#exercicios
#01
voto_coligacoes <- senado %>%
  select( Vote, GovCoalition) %>%
  mutate( tipo.coalisao = ifelse(GovCoalition%in%c('TRUE'), "Coalis?o", 'N?o_Coalis?o')) %>%
  mutate(Voto = ifelse(Vote%in%c('S'),'Votos_SIM', ifelse(Vote%in%c('N'),'Votos_N?O', 'Outros'))) %>%
  group_by(tipo.coalisao, Voto)%>%
  summarise(GovCoalition = n())%>%
  spread(key = tipo.coalisao, value = GovCoalition)
voto_coligacoes

#2
cadastros <- data.frame(email = c('joaodasilva@gmail.com', 'rafael@hotmail.com','maria@uol.com.br', 'juliana.morais@outlook.com'),
                        telefone = c('(61)99831-9482', '32 89762913', '62-9661-1234', '15-40192.5812'))
cadastros  %>%
  separate(email, "@")

#### cap 6 ####

dados2016 <- data.frame(ano = c(2016, 2016, 2016),
                   valor = c(938, 113, 1748),
                   produto = c('A', 'B', 'C'))
dados2017 <- data.frame(valor = c(8400, 837, 10983),
                        produto = c('H', 'Z', 'X'),
                        ano = c(2017, 2017, 2017))
dados.finais <- rbind(dados2016, dados2017)

band_members
band_instruments
str(band_members)
str(band_instruments)
#juntas os dois conjuntos com um join
band_members %>% inner_join(band_instruments)

band_instruments2
str(band_instruments2)
band_members %>% inner_join(band_instruments2, by= c('name' = 'artist') ) #? preciso especificar qual ? a chave

setwd('dados')

empregados <- read.csv('Employees.csv')
departamentos <- read.csv('Departments.csv')
str(empregados)
str(departamentos)
final <- empregados %>% inner_join(departamentos, by = c('Employee' = 'Manager'))
final

band_members %>% left_joi(band_instruments2, by= c('name' = 'artist') ) #? preciso especificar qual ? a chave
final2 <- empregados %>% left_join(departamentos, by = c('Employee' = 'Manager'))
final2

final3 <- departamentos %>% right_join(empregados, by = c('Manager' = 'Employee'))
final3
empregados %>% right_join(departamentos, by = c('Employee' = 'Manager'))

band_members %>% full_join(band_instruments2, by = c('name' = 'artist'))

final4 <- departamentos %>% full_join(empregados, by = c('Manager' = 'Employee'))
final4

#exerc?cios
#1
aeroporto <- nycflights13::airports %>% select(name, faa)
voos <- nycflights13::flights %>% select(origin,dest)

tabela <- aeroporto %>%
  mutate(origin = faa) %>% semi_join(voos)%>%
  select(name, faa)
tabela2 <- aeroporto %>% 
  mutate(dest = faa) %>% semi_join(voos) %>%
  select(name, faa)

finish = voos %>% mutate(faa = dest) %>% inner_join(tabela2) %>% mutate(destino = name) %>%
  select(destino, origin, origin)
finish = finish %>% mutate(faa = origin) %>% inner_join(tabela)%>% mutate(origem = name) %>%
  select(origem, destino,origin)%>%
  group_by(origem, destino) %>% summarise(origin = n())

  table(finish)
#2
  participantes <- data.frame(
    Nome = c('Carlos', 'Maur?cio', 'Ana Maria', 'Rebeca', 'Patr?cia'),
    Estado = c('Bras?lia', 'Minas Gerais', 'Goi?s', 'S?o Paulo', 'Cear?'),
    Idade = c(23, 24, 22, 29, 28)
  )
  aprovados <- data.frame(
    Nome = c('Carlos', 'Patr?cia'),
    Pontuacao = c(61, 62)
  )
  eliminados <- data.frame(
    Nome = c('Maur?cio', 'Ana Maria', 'Rebeca'),
    Pontuacao = c(49, 48, 48)
  )

resultado <-rbind(aprovados,eliminados) %>% 
  mutate(Resultado = ifelse(Pontuacao%in%c(61,62), "aprovado", 'reprovado')) %>%
  right_join(participantes) %>% select(Nome,Estado,Idade,Pontuacao,Resultado) %>% arrange(-Pontuacao)

aprovados %>% right_join(participantes)

#### cap 7 ####

participantes <- data.frame(
  Nome = c('Carlos', 'Maur?cio', 'Ana Maria', 'Rebeca', 'Patr?cia'),
  Estado = c('Bras?lia', 'Minas Gerais', 'Goi?s', 'S?o Paulo', 'Cear?'),
  Idade = c(23, 24, 22, 29, 28)
)
save(participantes,file = "participantes.Rdata")
rm(participantes)

load("participantes.Rdata")
str(participantes)

write.csv(participantes, file = 'teste1.CSV') #escrevendo em csv
library(openxlsx)
write.xlsx(participantes, file = 'teste2.xlsx') #escrevendo em xlsx

#1
write.csv(finish, file = 'aeroporto.CSV')
#2
save(finish, file = "Aeroporto.Rdata")
rm(finish)

load("Aeroporto.Rdata")
str(finish)

#### cap 8 ####
library(jsonlite)
library(curl)
hadley.rep <- jsonlite::fromJSON("https://api.github.com/users/hadley/repos")
dim(hadley.rep)
head(hadley.rep[c('name', 'description')], 15)

#o link do livro nao deu certo "https://dadosabertos.camara.leg.br/api/v2/proposicoes", nem oque seria o certo
proposicoes <- jsonlite::fromJSON("https://dadosabertos.camara.leg.br/swagger/api.html/proposicoes/legislaturas")
head(proposicoes$dados %>% select(siglaTipo, numero,ementa ,ementa))

# Quandl: pacote que fornece diversos dados econ?micos de diversos pa?ses;
# Rfacebook:pacote que facilita o uso da API do facebook(requercadastropr?vio);
# twitterR:pacoteque facilita o uso da API do twitter(requercadastropr?vio);
# ggmap: pacote que facilita o uso da API do googlemaps  

library(rvest) #facilita o consumo de dados em html

html <- read_html("https://pt.wikipedia.org/wiki/Lista_de_redes_de_televis%C3%A3o_do_Brasil")
html.table <- html %>% html_node('table')
dados <- html.tabl %>% html_table()
dados<- dados %>% select(-'Lista de emissoras')

#exerc?cios
#1
fut <- read_html("http://globoesporte.globo.com/futebol/brasileirao-serie-a/")
fut.table <- fut %>% html_node('table')
fut.table.final <- fut.table %>%html_table()
fut.table.final <- fut.table.final[,c(1,2)]
#2
idh <- read_html('https://pt.wikipedia.org/wiki/%C3%8Dndice_de_Desenvolvimento_Humano')
idh.table <- idh %>% html_node('table')
idh.table.final <- idh.table %>% html_table()

#### cap 9 ####

library(ggplot2)
data('mtcars')

g <- ggplot(mtcars) # inicia o plot
# Adicionar pontos (geom_point) e
# vamos mapear vari?veis a elementos est?ticos dos pontos
# Size=3 define o tamanho de todos os pontos
g <- g + geom_point(aes(x = hp, y = mpg, color = factor(am)), size = 3)

#altera a escala de cores
g <- g + scale_color_manual("Automatic", values = c('red', 'blue'), labels = c('Yes', 'No'))

#rotulos/titulos
g <- g + labs(title = 'Rela??o entre pot?cia, consumo e tipo de c?mbio', y = 'Consumo', x = 'Pot?ncia')
g

#fazendo em um ?nico bloco
ggplot(mtcars) +
  geom_point(aes(x = hp, y = mpg, color = factor(am)), size = 3) +
  scale_color_manual("Automatic", values = c('red', 'blue'), labels = c('Yes', 'No')) +
  labs(title = 'Rela??o entre pot?cia, consumo e tipo de c?mbio', y = 'Consumo', x = 'Pot?ncia')

####### 9.1
g1 <- ggplot(mtcars, aes(y = mpg, x = disp)) + geom_point(); g1

###### 9.2
#library(dplyr)
mtcars <- mtcars %>%mutate(names = row.names(mtcars))
ggplot(mtcars, aes(x = disp, y = mpg)) + geom_point() + geom_smooth() 

###### 9.3
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + geom_point() +
  scale_color_manual(values = c('orange', 'purple', 'sky blue')) # mudar a cor

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + geom_point() +
  scale_color_manual(values = c('green', 'dark blue', 'red'))+
  scale_x_continuous(name = 'Petal.Length', breaks = 1:7) + #break diz o tamanho da reta
  scale_y_continuous(name = 'Petal.Width', breaks = 0:3, limits = c(0,3))


#scale_x_continuous(name = waiver(), breaks = waiver(), minor_breaks = waiver(), labels = waiver(),
#                   limits = NULL, expand = waiver(), oob = censor, na.value = NA_real_,
#                   trans = 'identity')
#scale_y_continuous(name = waiver(), breaks = waiver(), minor_breaks = waiver(), labels = waiver(),
#                   limits = NULL, expand = waiver(), oob = censor, na.value = NA_real_,
#                   trans = 'identity')

library(ISLR)
ggplot(Wage, aes(x = age, y = wage, color = education)) + geom_point() +
  scale_x_continuous('idade', breaks = seq(0,80, 5), expand = c(0, 5)) + # break diz os pulos do vetor,5 em 5 nesse caso
  scale_y_continuous('salario', labels = function(x) paste0("US$", x), limits = c(0, 400))

#scale_x_discrete(..., expand = waiver(), position = "bottom")
#scale_y_discrete(..., expand = waiver(), position = "left")

ggplot(Default, aes(x = default, y = balance )) + geom_boxplot() +
  scale_x_discrete('calote', labels = c('n?o', 'sim')) + 
  labs(y = 'Valor devido depois do pagamento')
#mudando a ordem
ggplot(Default, aes(x = default, y = balance )) + geom_boxplot() +
  scale_x_discrete('calote',limits =  c('Yes', 'No'),labels = c('sim', 'n?o')) + 
  labs(y = 'Valor devido depois do pagamento')

#aceita apenas em dia, mes, ano // date
#scale_y/x_date(name = waiver(), breaks = waiver(), date_breaks = waiver(),
 #            labels = waiver(), date_labels = waiver(), minor_breaks = waiver(),
  #           date_minor_breaks = waiver(), limits = NULL, expand = waiver())
#aceita em tempi/horario // POSIXct
#scale_x/y_datetime(name = waiver(), breaks = waiver(), date_breaks = waiver(),
 #                labels = waiver(), date_labels = waiver(), minor_breaks = waiver(),
  #               date_minor_breaks = waiver(), limits = NULL, expand = waiver())

# o argumento date_labes possibilidades de alterar o modo como as datas s?o apresentadas

ggplot(economics, aes(x = date, y = unemploy)) + geom_line()

#alterando as datas
ggplot(economics, aes(x = date, y = unemploy)) + geom_line() + scale_x_date(date_labels = "%b%Y")
#"%b%Y" definir o formato de data
help("strptime") #diz os formatos de data

ggplot(economics, aes(x = date, y = unemploy)) + geom_line() +
  scale_x_date(date_breaks = '2 year', date_labels = '%Y') #date_breaks mostra os intervalos do eixo x

#outra forma de fazer
seq_datas <- seq.Date(as.Date('1970-01-01'), as.Date('2015-04-01'), by = '5 years')
ggplot(economics, aes(x = date, y = unemploy)) + geom_line() +
  scale_x_date(breaks = seq_datas, date_labels = '%Y')

library(RColorBrewer) #
display.brewer.all(n= NULL, type = 'all', select = NULL, exact.n = TRUE, colorblindFriendly = FALSE)

paleta.gradient <- brewer.pal(n = 9, name = 'Reds')

Credit %>% group_by(Cards, Student) %>% summarise(Balance = mean(Balance), n = n()) %>%
  ggplot(aes(x = Cards, y = Student, fill = Balance)) + geom_tile() +
  scale_fill_gradientn(colours = rev(paleta.gradient)) + scale_x_continuous(breaks = 1:9)

library(viridisLite)
Credit %>% group_by(Cards, Student) %>% summarise(Balance = mean(Balance), n = n()) %>%
  ggplot(aes(x = Cards, y = Student, fill = Balance)) + geom_tile() +
  viridis::scale_fill_viridis() + scale_x_continuous(breaks = 1:9)

#mudando as cores manualmente
ggplot(Wage, aes(y = wage, x = age, color = education)) + geom_point() +
  scale_color_manual(values = c(c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854")))

#9.4
#para formato grid
#facet_grid(facets, margins = FALSE, scales = "fixed", space = "fixed", shrink = TRUE,
#          labeller = "label_value", as.table = TRUE, switch = NULL, drop = TRUE)
#para formato wrap; converte paieneis de uma dimens?o para duas dimens?es 
#facet_wrap(facets, nrow = NULL, ncol = NULL, scales = "fixed", shrink = TRUE,
#          labeller = "label_value", as.table = TRUE, switch = NULL, drop = TRUE,dir = "h")

#exemplo
ggplot(diamonds, aes(x = carat, y = price )) + geom_point()

# com facet_wrap; comparar com as rela??es com diferentes grupos
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + facet_wrap(~ cut)
#usamos o cut para mostras os graficos dessa variavel

#fixando o eixo y
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + facet_wrap(~cut, scales = 'free_y')
#fixando o eixo x
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + facet_wrap(~cut, scales = 'free_x')

#com facet_grid
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + facet_grid(clarity ~ cut)
#usado para o cruzamento de variaveis, nesse caso de clarity e cut

#alterando os rotulos
nomes_cut <- c(Fair = "FAIR", Good = "GOOD", `Very Good` = "VERY GOOD",
               Premium = "PREMIUM", Ideal = "IDEAL")
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() +
  facet_wrap(~cut, scales = 'free_y', labeller = labeller(cut = nomes_cut))

#9.5
library(ggThemeAssist)
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() +
  labs(title = 'carat vs price') + theme(text = element_text(face = 'bold'),
                                         panel.grid.major = element_line(colour = 'gray80'),
                                         axis.title = element_text(size = 14),
                                         panel.background = element_rect('gray100'))

p <- ggplot(diamonds, aes(x = carat, y = price)) + geom_point()

p + theme_gray() + labs(title = "theme_gray()")
p + theme_bw() + labs(title = "theme_bw()")
p + theme_linedraw() + labs(title = "theme_linedraw()")
p + theme_light() + labs(title = "theme_light()")
p + theme_minimal() + labs(title = "theme_minimal()")
p + theme_classic() + labs(title = "theme_classic()")
p + theme_dark() + labs(title = "theme_dark()")
p + theme_void() + labs(title = 'theme_void')

library(hrbrthemes)
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() +
  labs(title = "theme_ipsum()") + theme_ipsum(plot_title_size = 12, axis_title_size = 10)

#como esse comando os graficos do scrip ter?o o mesmo tema
theme_set(theme_ipsum(plot_title_size = 12, axis_title_size = 10) + theme(text = element_text(angle = 0)))

#9.6
#guide_legend(title = waiver(), title.position = NULL, title.theme = NULL,
#             title.hjust = NULL, title.vjust = NULL, label = TRUE,
#             label.position = NULL, label.theme = NULL, label.hjust = NULL,
#             label.vjust = NULL, keywidth = NULL, keyheight = NULL,
#             direction = NULL, default.unit = "line", override.aes = list(),
#             nrow = NULL, ncol = NULL, byrow = FALSE, reverse = FALSE, order = 0, ...)

#guide_colourbar(title = waiver(), title.position = NULL, title.theme = NULL,
#                title.hjust = NULL, title.vjust = NULL, label = TRUE,
#                label.position = NULL, label.theme = NULL, label.hjust = NULL,
#                label.vjust = NULL, barwidth = NULL, barheight = NULL, nbin = 20,
#                raster = TRUE, ticks = TRUE, draw.ulim = TRUE, draw.llim = TRUE,
#                direction = NULL, default.unit = "line", reverse = FALSE, order = 0, ...)

ggplot(diamonds, aes(x = carat, y = price, color = cut, shape = cut)) + geom_point() +
         guides(color = guide_legend(title = "cor", title.position = "left", keywidth = 5),
                shape = guide_legend(title =  "Forma", title.position = "right", override.aes = aes(size = 5)))

ggplot(diamonds, aes(x = carat, y = price, color = cut, shape = cut)) + geom_point() +
  guides(color = guide_legend(title = "cor", title.position = "left", keywidth = 5),
         shape = "none")
#9.7
library(treemapify)
ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country)) + geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = 'white', place = 'center', grow = TRUE) +
  theme(legend.position = 'bottom')

#9.8
library(hrbrthemes)
library(gapminder) #base de dados

gapminder %>% filter(year == max(year)) %>% ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + labs(title = "rela??o entre renda per capita e expectativa de vida",
                          x = 'renda per capita', y = 'expectativa de vida') + 
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)

gapminder %>% filter(year == max(year)) %>% ggplot(aes(x = gdpPercap, y = lifeExp)) +
  geom_point() + scale_x_log10() + 
  labs(title = "rela??o entre renda per capita e expectativa de vida",
                      x = 'renda per capita (escala  log10)', y = 'expectativa de vida') + 
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)

gapminder %>% filter(year == max(year)) %>% ggplot(aes(x = gdpPercap, y = lifeExp, color = continent, 
                                                       shape = continent)) +
  geom_point() + scale_x_log10() + scale_color_discrete('Continente') +
  scale_shape_discrete('Continente') + 
  labs(title = 'rela??o entre renda per capita e expectativa de vida 2007',
       x = 'renda per capita (escala log10', y = 'expectativa de vida') +
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)

#9.9
gapminder %>% filter(year == max(year)) %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, color= continent, shape = continent)) +
  geom_point(fill = "black", size= 3, stroke = 1) + scale_x_log10() + scale_color_discrete("Continente") +
  scale_shape_manual("Continente", values = c(19, 21, 22, 23, 24)) +
  labs(title = "Relação entre Renda per Capita e Expectativa de Vida - 2007",
       x = "Renda per Capita (escala log 10)",
       y = "Expectativa de Vida")

gapminder %>% filter( year == max(year)) %>% ggplot(aes(x = gdpPercap, y = lifeExp, size = pop)) + geom_point()+
scale_size_continuous("População (milhões)", labels = function(x) round(x/1e6)) + scale_x_log10() +
  labs(title = "Relação entre Renda per Capita e Expectativa de Vida - 2007",
       x = "Renda per Capita (escala log 10)", y = "Expectativa de Vida") + 
  theme_ipsum(plot_title_size = 12, axis_text_size = 10)

#9.10
#geom_bar(mapping = NULL, data = NULL, stat = "count", position = "stack", ...,
#         width = NULL, binwidth = NULL, na.rm = FALSE, show.legend = NA,
#         inherit.aes = TRUE)
#geom_col(mapping = NULL, data = NULL, position = "stack", ...,
#         width = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

#usando o geom_bar
ggplot(diamonds,aes( x = cut))+ geom_bar() + theme_ipsum(plot_title_size = 12, axis_title_size = 10) 

#definindo o y com stat = identity
gapminder %>% filter(year == max(year), continent == "Americas") %>% ggplot(aes(x=country, y= lifeExp))+
  geom_bar(stat = 'identity', fill= 'dodgerblue')+ 
  labs(title = "Expectativa de vida por país", subtitle = "2007",  x = "País",  y = "Expectativa de Vida") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

#usando geom_col
gapminder %>% filter(year == max(year), continent == "Americas") %>% ggplot(aes(x=country, y= lifeExp)) +
  geom_col(fill = "dodgerblue") + 
  labs(title = "Expectativa de vida por país", subtitle = "2007", x = "País",  y = "Anos") + 
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#ordenando o eixo x #usando o reorder
gapminder %>% filter(year == max(year), continent == "Americas") %>% ggplot(aes(x= reorder(country, -lifeExp), y= lifeExp)) +
  geom_col(fill = "dodgerblue") + 
  labs(title = "Expectativa de vida por país (com o -X)", subtitle = "2007", x = "País",  y = "Anos") + 
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

#comparando as médias
gapminder %>% filter( year %in% c(1957, 2007)) %>% 
  # Converte o ano para factor - será categoria no gráfico
  mutate(year = factor(year)) %>% group_by(continent, year) %>% summarise(lifeExp = mean(lifeExp)) %>%
  ggplot(aes(x= continent, y= lifeExp, fill=year)) + geom_col() +
  theme_ipsum(plot_title_size = 12, axis_text_size = 10)

#ordenado
gapminder %>% filter( year %in% c(1957, 2007)) %>% 
  # Converte o ano para factor - será categoria no gráfico
  mutate(year = factor(year)) %>% group_by(continent, year) %>% summarise(lifeExp = mean(lifeExp)) %>%
  ggplot(aes(x= reorder(continent, lifeExp), y= lifeExp, fill=year)) + geom_col() +
  theme_ipsum(plot_title_size = 12, axis_text_size = 10)

#colocando as barras lado a lado #position = "dodge"
gapminder %>% filter(year %in% c(1957, 2007)) %>% 
  # Converte o ano para factor - será categoria no gráfico
  mutate(year = factor(year)) %>% group_by(continent, year) %>% summarise(lifeExp = mean(lifeExp)) %>%
  ggplot(aes(x= continent, y = lifeExp, fill = year)) +
  geom_col(position = "dodge") + 
  labs(title = "Expectativa de vida por continente", x = "Continente", y = "Anos", fill = "Ano") +
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)

#colocando as barras na horizontal #coord_flip()
gapminder %>% filter(year %in% c(1957, 2007)) %>% 
  # Converte o ano para factor - será categoria no gráfico
  mutate(year = factor(year)) %>% group_by(continent, year) %>% summarise(lifeExp = mean(lifeExp)) %>%
  ggplot(aes(x= continent, y = lifeExp, fill = year)) +
  geom_col(position = "dodge") + coord_flip()+
  labs(title = "Expectativa de vida por continente", x = "Continente", y = "Anos", fill = "Ano") +
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)

#9.11
#geom_line(mapping = NULL, data = NULL, stat = "identity", position = "identity",
#          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...)

#por linhas #geom_line()
gapminder %>% group_by(continent, year) %>% summarise(lifeExp = mean(lifeExp)) %>%
  ggplot(aes(x= year, y= lifeExp, color= continent)) + geom_line() +
           labs(title = "Evolução da expectativa de vida por continente", x = "Ano", y = "Anos de vida",
                color = "Continente") + theme_ipsum(plot_title_size = 12, axis_title_size = 10)
#por linhas com marcação dos períodos #geom_line()
gapminder %>% group_by(continent, year) %>% summarise(lifeExp = mean(lifeExp)) %>%
  ggplot(aes(x= year, y= lifeExp, color= continent)) + geom_line() + geom_point(aes(shape = continent))+
  labs(title = "Evolução da expectativa de vida por continente", x = "Ano", y = "Anos de vida",
       color = "Continente") + theme_ipsum(plot_title_size = 12, axis_title_size = 10)

#9.12
#geom_freqpoly(mapping = NULL, data = NULL, stat = "bin",
# position = "identity", ..., na.rm = FALSE, show.legend = NA,
# inherit.aes = TRUE)
#geom_histogram(mapping = NULL, data = NULL, stat = "bin",
#               position = "stack", ..., binwidth = NULL, bins = NULL, na.rm = FALSE,
#               show.legend = NA, inherit.aes = TRUE)

#usando o geom_histogram
gapminder %>% filter(year == 2007) %>% ggplot(aes(x= lifeExp)) + 
  geom_histogram(binwidth = 5, fill = 'dodgerblue', color= "black") +
  labs(title = "Distribuição da expectativa vida", x = "Anos", y = "Contagem") +
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)

#usando o geom_freqpoly
gapminder %>% filter(year == 2007) %>% ggplot(aes(x= lifeExp)) + 
  geom_freqpoly(binwidth = 5) +
  labs(title = "Distribuição da expectativa vida", x = "Anos", y = "Contagem") +
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)

#tranformando em proporções
gapminder %>% filter(year == 2007) %>%
  ggplot(aes(x = lifeExp)) +
  geom_histogram(aes(y = ..count../sum(..count..)),binwidth = 5, fill = 'dodgerblue', color = 'black') +
  labs(title = "Distribuição da expectativa vida", x = "Anos", y = "Proporção") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)

#9.13
#usando o geom_boxplot()
ggplot(gapminder, aes(x= factor(year), y= lifeExp)) + geom_boxplot(fill= "dodgerblue") +
  labs(x= "Ano", y= "Anos de vida", title = "Distribuição da expectativa de vida por ano") +
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)

#usando o geom_violin()
ggplot(gapminder, aes(x= factor(year), y= lifeExp)) + geom_violin(fill= "orange") +
  labs(x= "Ano", y= "Anos de vida", title = "Distribuição da expectativa de vida por ano") +
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)

#fazendo por pontos sem "ruido"
ggplot(gapminder, aes(x= factor(year), y= lifeExp)) + geom_point(fill= "orange") +
  labs(x= "Ano", y= "Anos de vida", title = "Distribuição da expectativa de vida por ano") +
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)
#fazendo por pontos com "ruido" com geom_jitter()
ggplot(gapminder, aes(x= factor(year), y= lifeExp)) + geom_jitter(fill= "orange") +
  labs(x= "Ano", y= "Anos de vida", title = "Distribuição da expectativa de vida por ano") +
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)

#9.14
#descobrir qual é o outliner
gapminder %>% filter(year == 1992, lifeExp == min(lifeExp))
#fazendo a anotação
ggplot(gapminder, aes(x= factor(year), y= lifeExp)) + geom_boxplot(fill= "dodgerblue") +
    annotate('text', x= '1992', y= 27, label= "Ruanda")+
  labs(x= "Ano", y= "Anos de vida", title = "Distribuição da expectativa de vida por ano") +
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)
#fazendo anotação com segmentos
ggplot(gapminder, aes(x= factor(year), y= lifeExp)) + geom_boxplot(fill= "dodgerblue") +
  annotate('text', x= '1992', y= 27, label= "Ruanda")+
  annotate('rect', xmin = '1982',xmax= '2002', ymin= 20, ymax= 95, alpha= 0.2) +
  labs(x= "Ano", y= "Anos de vida", title = "Distribuição da expectativa de vida por ano") +
  theme_ipsum(plot_title_size = 12, axis_title_size = 10)

#9.15
# cleveland dot point
gapminder %>% filter(year== 2007, continent == "Americas") %>% 
  ggplot(aes(x= lifeExp, y= reorder(country ,lifeExp)))+
  #muda o tipo e cor do ponto
  geom_point(size= 3, color= "darkgoldenrod3") +
  labs(title = "Expectativa de vida por país - 2007", y= "País", x= "Ano")+
  theme_ipsum(plot_title_size = 12, axis_title_size = 10) +
  #muda o background do grafico
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

#agora com mais de uma variavel
gapminder %>% filter(year %in% c( 1997, 2007), continent == "Americas") %>% 
  ggplot(aes(x= lifeExp, y= reorder(country ,lifeExp)))+
  #muda o tipo e cor do ponto
  geom_point(aes(color= factor(year))) +
  labs(title = "Expectativa de vida por país - 2007", y= "País", x= "Ano")+
  theme_ipsum(plot_title_size = 12, axis_title_size = 10) +
  #muda o background do grafico
  theme(panel.grid.major.y = element_line(linetype = "dashed"))
#
gapminder %>% filter(year %in% c( unique( gapminder$year)), continent == "Americas") %>% 
  ggplot(aes(x= lifeExp, y= reorder(country ,lifeExp)))+
  #muda o tipo e cor do ponto
  geom_point(aes(color= factor(year))) +
  labs(title = "Expectativa de vida por país - 2007", y= "País", x= "Ano")+
  theme_ipsum(plot_title_size = 12, axis_title_size = 10) +
  #muda o background do grafico
  theme(panel.grid.major.y = element_line(linetype = "dashed"))
#ligando os pontos
gapminder %>% filter(year %in% c( 1952, 2007), continent == "Americas") %>% 
  ggplot(aes(x= lifeExp, y= reorder(country ,lifeExp)))+
  #ligando os pontos
  geom_line(aes(group= country), color= "gray")+
  #muda o tipo e cor do ponto
  geom_point(aes(color = factor(year))) +
  labs(title = "Expectativa de vida por país - 2007", y= "País", x= "Ano")+
  theme_ipsum(plot_title_size = 12, axis_title_size = 10) +
  #muda o background do grafico
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

#9.16
#geom_label(mapping = NULL, data = NULL, stat = "identity", position = "identity",
#           ..., parse = FALSE, nudge_x = 0, nudge_y = 0,
#           label.padding = unit(0.25, "lines"), label.r = unit(0.15, "lines"),
#           label.size = 0.25, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
#geom_text(mapping = NULL, data = NULL, stat = "identity", position = "identity",
#          ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE,
#          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)

# com geom_text() sem formatação
gapminder %>% filter(year== 2007, continent == "Americas") %>% 
  ggplot(aes(x= lifeExp, y= reorder(country ,lifeExp)))+
  #segmento que vai da origem até o ponto
  geom_segment(x = 0, aes(xend = lifeExp, yend = country), color = "grey50") +
  geom_point(size= 3, color= "darkgoldenrod3") +
  #colocando o nome pobremente
  geom_text(aes(label = round(lifeExp)))+
  labs(title = "Expectativa de vida por país - 2007", y= "País", x= "Ano")+
  theme_ipsum(plot_title_size = 12, axis_title_size = 10) +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

# com geom_text() com formatação geom_poit()
gapminder %>% filter(year== 2007, continent == "Americas") %>% 
  ggplot(aes(x= lifeExp, y= reorder(country ,lifeExp)))+
  #geom_segment(x = 0, aes(xend = lifeExp, yend = country), color = "grey50") +
  geom_point(size= 3, color= "darkgoldenrod3") +
  #colocando o nome com formatação
  geom_text(aes(label = round(lifeExp)), nudge_x = 1.5)+
  labs(title = "Expectativa de vida por país - 2007", y= "País", x= "Ano")+
  theme_ipsum(plot_title_size = 12, axis_title_size = 10) +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

# com geom_text() com formatação geom_label()
gapminder %>% filter(year== 2007, continent == "Americas") %>% 
  ggplot(aes(x= lifeExp, y= reorder(country ,lifeExp)))+
  #geom_segment(x = 0, aes(xend = lifeExp, yend = country), color = "grey50") +
  geom_point(size= 3, color= "darkgoldenrod3") +
  #colocando o nome com formatação
  geom_label(aes(label = round(lifeExp)), nudge_x = 1.5, size= 2.8)+
  labs(title = "Expectativa de vida por país - 2007", y= "País", x= "Ano")+
  theme_ipsum(plot_title_size = 12, axis_title_size = 10) +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

#9.17 plotando funcao
reta <- function(a, b, x) a+b*x
data <- data.frame(x=seq(0,10, by=0.1)) 
ggplot(data, aes(x=x)) + stat_function(fun = reta, args = list(a=1, b=2)) +
  stat_function(fun = reta, args = list(a=1, b=3), col= "red")

sigmoid <- function(a= 1, z)  1/(1+exp(-a*z))
data <- data.frame(x= -6:6)
ggplot(data, aes(x=x)) + 
  stat_function(fun = sigmoid, args = (a=1))+
  stat_function(fun = sigmoid, args = (a=2), color= "green")+
  stat_function(fun = sigmoid, args = (a=3), color= "blue")+
  stat_function(fun = sigmoid, args = (a=4), color= "coral")

logit <- function(a, z)  log(sigmoid(a, z)/(1 - sigmoid(a, z)))
data <- data.frame(x = -6:6)
ggplot(data, aes(x = x)) +
  stat_function(fun = logit, args = list(a = 1), aes(color = "a = 1")) +
  stat_function(fun = logit, args = list(a = 0.5), aes(color = "a = 0.5")) +
  stat_function(fun = logit, args = list(a = 2), aes(color = "a = 2"))  

# 9.18 # fazer no windows 10 
library(readr)
worlmap <- read_delim('world_map.csv', delim = ";",
                      locale = locale(encoding = "ISO-8859-1", decimal_mark = ","))
head(worlmap)
#removendo a antartica
world_map<- worlmap %>% filter(id != "Antarctica")
rm(worlmap)
#exportando os dados de 2015
exp.2015 <- read_delim('EXP_2015_PAIS.csv', delim = ";",
                       locale = locale(encoding = "ISO-8859-1"),
                       col_types = 'ccd')
#pareando os dados e juntando
world_map <- left_join(world_map, exp.2015, by = "NO_PAIS_POR") %>%
  mutate(class = cut(VL_FOB, breaks = c(0, 1e6, 10e6, 100e6, 1e9, 10e9, Inf)))
#plotando o grafico mapa
ggplot(world_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = class), col = 'black', size = 0.1) +
  scale_fill_brewer(palette = "Reds", breaks = levels(world_map$class),
                    labels = c("(0, 1 Mi]", "(1 Mi, 10 Mi]", "(10 Mi, 100 Mi]",
                               "(100 Mi, 1 Bi]", "(1 Bi, 10 Bi]", "> 10 Bi"),
                    na.value = 'grey70') +
  labs(title = "Exportações Brasileiras - 2015", subtitle = 'Valor FOB') +
  coord_quickmap() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#gerando um de fora
library(rgdax)
library(maptools)
library(rgeos)#
library(broom)
####

# 9.20 usando extensões
#9.20.1
#sem formatação
library(ggrepel) #extensão do ggplot
data("mtcars")
ggplot(mtcars, aes(wt, mpg)) + geom_point(color= "coral") +
  geom_text(aes(label = row.names(mtcars))) + theme_ipsum(plot_title_size = 12, axis_title_size = 10)
#com formatação / organizando os rótulos geom_text_repel()
ggplot(mtcars, aes(wt, mpg)) + geom_point(color= "coral") + geom_text_repel(aes(label = row.names(mtcars)))+
   theme_ipsum(plot_title_size = 12, axis_title_size = 10)
#9.20.2 /fazendo gifs
library(devtools)
devtools::install_github("dgrtwo/gganimate")
library(gganimate) #fazer gifs
p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, color = continent,frame = year)) +
  geom_point() +  scale_x_log10()
animation::ani.options(interval = 1)
x <- gganimate(p, filename = 'images/gapminder1.gif',ani.width = 750,ani.height = 450)


## exercício
#1
library(gapminder)
library(ggrepel)
caralho2 <- gapminder %>% filter(year == "2007", continent == "Africa") %>% select(country, lifeExp, pop)
  ggplot(caralho2, aes(y=lifeExp, x= (pop)) ) +
  geom_point(color = "red") + 
  #geom_label(aes(label = round(lifeExp)), nudge_x = 1.5, size= 2.8)+
  labs(title = "Expectativa de vida por tamanho da população dos paises Africanos - 2007", y= "Expectativa de vida", x= "Tamanho da população")+
  geom_text_repel(aes(label = rownames(caralho2)))+
  theme_ipsum(plot_title_size = 12, axis_title_size = 10) +
  theme(panel.grid.major.y = element_line(linetype = "dashed"))

#2  

#3
library(ISLR)
  head(Wage)
Wage %>% filter() %>% ggplot(aes(x= education, y= wage, fill= education)) + 
  labs(title = "Salário Vs Nivel de escolaridade", x= "Salário", y= "Nível de escolaridade")+
  theme_ipsum(plot_title_size = 12, axis_title_size = 10) +
  geom_point() + theme(axis.text.x = element_text(angle = 65, hjust = 1))

#4
Wage2 <- Wage %>% select(year, maritl, race, region, jobclass, health, health_ins, logwage, wage) #%>%
Wage2%>% ggplot( aes(x= wage))  +
geom_histogram(data = Wage2, fill = "grey50", alpha = 0.5)
 
#5
library(WDI)
library(dplyr)
gdp_growth <- WDI(indicator = "NY.GDP.MKTP.KD.ZG", start = 2016, end = 2016, extra = TRUE)
# Remove regiões - ISO's com números
gdp_growth <- gdp_growth %>% filter(!is.na(region) & region != "Aggregates" & !is.na(NY.GDP.MKTP.KD.ZG))

gdp_growth %>% filter(year == 2016) 

#### cap 10 ####
#10.2
library(plotly) #gráficos interativos
library(tidyverse)
p <- ggplot(mtcars, aes(x= hp, y= mpg)) + geom_point()
ggplotly(p)

#10.3
#ts(data = NA, start = 1, end = numeric(), frequency = 1,
#deltat = 1, ts.eps = getOption("ts.eps"), class = , names = )
x <- rnorm(24, mean = 100, sd= 10)
x<- ts(x, frequency = 12, start = c(2010,1))
plot(x)

#install.packages('xts')
library(xts) #uma opção ao ts() para declarar série de tempo
xts_df <- data.frame(y = rnorm(365, 100, 10))
xts_df$data <- seq.Date(as.Date('2011-01-01'),
                        length.out = 365, by= '1 day')
xts_df <- xts(x= xts_df[,'y'], order.by = xts_df[,'data'])
head(xts_df)

install.packages('dygraphs')
library(dygraphs) #biblioteca para a visualização de dados temporais (interativo)
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths, main = "Mortes por doenças pulmonares - Reino Unido - 1874-1979",
        ylab = 'Númenos de mortes')%>%
  dySeries('mdeaths', color = 'magenta', label = 'Homens')%>%
  dySeries("fdeaths", color = 'blue', label = "mulheres")%>%
  dyRangeSelector()

#funções para mudar os padrões dos números e datas
# Alterar rótulos do eixo x e a legenda
axlabform <- "function(date, granularity, opts, dygraph) {
var months = ['Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio',
'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro'];
return months[date.getMonth()] + \" \" + date.getFullYear()}"

valueform <- "function(ms) {
var months = ['Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio',
'Junho', 'Julho', 'Agosto', 'Setembro',
'Outubro', 'Novembro', 'Dezembro'];10.3. DYGRAPHS
var ms = new Date(ms);
return months[ms.getMonth()] + '/' + ms.getFullYear()}"

valueformy <- "function(value) {
return (Math.round(value * 100)/100).toString()
.replace('.', ',')
.replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.')}"

#modificando os padrões de datas
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths, main = "Mortes por doenças pulmonares - Reino Unido - 1874-1979",
        ylab = 'Númenos de mortes')%>%
  dySeries('mdeaths', color = 'magenta', label = 'Homens')%>%
  dySeries("fdeaths", color = 'blue', label = "mulheres")%>%
  dyAxis('y', valueFormatter = valueformy) %>% 
  dyAxis('x', axisLabelFormatter = axlabform, valueFormatter = valueformy)
  dyRangeSelector()

#10.4
install.packages('leaflat') #não é compativel com a versão 3.4.4
library(dplyr)
library(leaflat)


#exercícios
#1

#2
library(plotly) #gráficos interativos
library(gapminder)
dois <- gapminder %>% filter(country == "Iraq") %>% ggplot(aes(x= year, lifeExp)) +geom_point()
ggplotly(dois)

#3
head(economics)
library(dygraphs) #biblioteca para a visualização de dados temporais (interativo)
economics <- economics %>% select(date, unemploy, pop)
dygraph(economics, main = "Desempregados comparado com a população", ylab = "numero", periodicity = 'month') %>%
  dySeries('pop', color =  'blue', label = 'Populção') %>%
  dySeries('unemploy', color = 'red', label = 'Desempregados') %>%
  dyRangeSelector()

