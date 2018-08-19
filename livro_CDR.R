library(readr) #ler dados
library(magrittr)
library(dplyr)
library(nycflights13) #dados do aeroporto
library(tidyr)
library(stringr)
library(tidyverse)
library(openxlsx) #escrever em xlsx


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
read_fwf("fwf-sample.txt", col_positions = fwf_widths(c(20,10,12),c('nomes', 'estado','código')))

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
    print('faz a manipulação')
    print('envia o email informando a comclusão ')
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
is.vector(airquality$Ozone) #saber se a coluna é um vetor
unique(senado$Party) # Funçãoqueretornaapenasosvaloresúnicos,semrepetição,deumvetor

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
  distinct() #semelhante aounique(),trazregistrosúnicossemrepetição
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

voto_coligações <- senado %>%
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

cnae.texto <- c('10 Fabricaçãodeprodutosalimentícios', '11 Fabricaçãodebebidas',
                '12 Fabricaçãodeprodutosdofumo', '13 Fabricaçãodeprodutostêxteis',
                '14 Confecção deartigosdovestuárioeacessórios',
                '15 Preparaçãodecourosefabricaçãodeartefatosdecouro,artigosparaviagemecalçados',
                '16 Fabricaçãodeprodutosdemadeira',
                '17 Fabricaçãodecelulose,papeleprodutosdepapel')
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
  mutate( tipo.coalisao = ifelse(GovCoalition%in%c('TRUE'), "Coalisão", 'Não_Coalisão')) %>%
  mutate(Voto = ifelse(Vote%in%c('S'),'Votos_SIM', ifelse(Vote%in%c('N'),'Votos_NÃO', 'Outros'))) %>%
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
band_members %>% inner_join(band_instruments2, by= c('name' = 'artist') ) #é preciso especificar qual é a chave

setwd('dados')

empregados <- read.csv('Employees.csv')
departamentos <- read.csv('Departments.csv')
str(empregados)
str(departamentos)
final <- empregados %>% inner_join(departamentos, by = c('Employee' = 'Manager'))
final

band_members %>% left_joi(band_instruments2, by= c('name' = 'artist') ) #é preciso especificar qual é a chave
final2 <- empregados %>% left_join(departamentos, by = c('Employee' = 'Manager'))
final2

final3 <- departamentos %>% right_join(empregados, by = c('Manager' = 'Employee'))
final3
empregados %>% right_join(departamentos, by = c('Employee' = 'Manager'))

band_members %>% full_join(band_instruments2, by = c('name' = 'artist'))

final4 <- departamentos %>% full_join(empregados, by = c('Manager' = 'Employee'))
final4

#exercícios
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
    Nome = c('Carlos', 'Maurício', 'Ana Maria', 'Rebeca', 'Patrícia'),
    Estado = c('Brasília', 'Minas Gerais', 'Goiás', 'São Paulo', 'Ceará'),
    Idade = c(23, 24, 22, 29, 28)
  )
  aprovados <- data.frame(
    Nome = c('Carlos', 'Patrícia'),
    Pontuacao = c(61, 62)
  )
  eliminados <- data.frame(
    Nome = c('Maurício', 'Ana Maria', 'Rebeca'),
    Pontuacao = c(49, 48, 48)
  )

resultado <-rbind(aprovados,eliminados) %>% 
  mutate(Resultado = ifelse(Pontuacao%in%c(61,62), "aprovado", 'reprovado')) %>%
  right_join(participantes) %>% select(Nome,Estado,Idade,Pontuacao,Resultado) %>% arrange(-Pontuacao)

aprovados %>% right_join(participantes)

#### cap 7 ####

participantes <- data.frame(
  Nome = c('Carlos', 'Maurício', 'Ana Maria', 'Rebeca', 'Patrícia'),
  Estado = c('Brasília', 'Minas Gerais', 'Goiás', 'São Paulo', 'Ceará'),
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
