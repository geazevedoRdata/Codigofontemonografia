
# pacotes -------------------------------------------------------------------------------------------------------
install.packages("ipeadatar")
install.packages("sidrar")
install.packages("installr")
install.packages("GetBCBData")
install.packages("scales")
install.packages("Metrics")
install.packages("summarytools")
install.packages("openxlsx")
library(openxlsx)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(lubridate)
library(sidrar)
library(GetBCBData)
library(scales)
library(Metrics)
library(summarytools)

options(scipen = 999)
# Dados IPEA ----------------------------------------------------------------------------------------------------
library(ipeadatar)

# Extrair: tabela com todas as series e códigos disponíveis
series_ipeadata <- ipeadatar::available_series()
territorios <- ipeadatar::available_territories()

# Filtrar séries por termo
## (podemos substituir essa etapa por pesquisa ao código diretamento na tabela)
dplyr::filter(
  series_ipeadata,
  stringr::str_detect(name, stringr::regex("renda", ignore_case = TRUE))
)

# Coletar série de interesse usando o código
## Renda domiciliar percapita méd- INATIVA - 1976 até 2014 - Em R$ Outubro 2014
renda_ipeadata <- ipeadatar::ipeadata("MRDPC")

## obs: Mato grosso do sul a partir de 1981; Tocantis a partir de 1992
## Renda percapita NÃO disponibilizada pela fonte nos anos: 1980, 1991, 1994, 2000, 2010

## Renda percapita (Atlas DH - Pnad Contínua/A) - 2012 até 2021 - Em R$, a preços do ano 2010 ( 1. Os valores correntes de rendimento fornecidos pelos microdados da PNADc, que tem como referência o trimestre, foram convertidos em reais de 01 de agosto de 2010 por meio da variação do IPCA (índice de preços ao consumidor amplo) médio do trimestre e o IPCA de 01 de agosto de 2010 (média geométrica do IPCA de julho e de agosto de 2010). Para converter os valores de 01 de agosto de 2010 para valores a preços médios de 2021, multiplicar por 1,872878. )
renda2_ipeadata <- ipeadatar::ipeadata("ADH12_RDPC")%>%
                   filter(date > as.Date("2014-01-01"))



# Deflacionar bases de renda per capita para o mesmo tempo ----------------------------------------------------------------------------------------
## Importar IPCA e IPC - IPCA inicia a partir de dezembro de 1979. 
ipca = get_sidra(api='/t/1737/n1/all/v/2266/p/all/d/v2266%2013')

ipca %<>% mutate(date = parse_date_time(`Mês (Código)`, 'ym'))

ipca_indice <- ipca %>%
  mutate(indice = Valor) %>%
  select(date, indice)


## Ajustar os preços da primeira série (renda_ipeadata, 1976 até 2014 - Em R$ Outubro 2014)
# não vou mais ajustar as duas séries. Vou ficar com a de outubro de 2014 como sendo a base. alterarei só a outra.
ipca_out2014 <- ipca_indice %>%
                filter(date == as.Date("2014-10-01"))%>%
                pull(indice)

ipcaindice_novasbases <- ipca_indice %>%
                        mutate(base2014 = ((indice/ipca_out2014)*100)) %>%
                        mutate(base2021 = ((indice/ipca_jan2021)*100)) %>%
                        mutate(base2010 = ((indice/ipca_jan2010)*100))


## Ajustar os preços da segunda série ( renda2_ipeadata, 2012 até 2021 - Em R$, a preços do ano 2010 (informações em notas meodologicas do Atlas)
ipca_ago2010 <- ipca_indice %>%
                filter(date == as.Date("2010-08-01"))%>% 
                pull(indice)


renda2_ajustada <- left_join(renda2_ipeadata, ipca_indice, by = "date") %>%
                   mutate(value_nominal = (value/(ipca_ago2010/indice))) %>% # Valor nominal obtido atráves de algebrimos na função de deflacionamento.
                   mutate(fator = (ipca_out2014/indice)) %>%
                   mutate(renda_real = if_else(!is.na(value_nominal), (fator*value_nominal), NA_real_)) %>%
                   select(date, value, value_nominal, indice,fator,renda_real, tcode)


## Juntar as bases de renda percapita 1976 até 2021, apenas para estados
renda_percapita <- bind_rows(renda_ipeadata, renda2_ajustada) %>%
                   arrange(date, tcode) %>%
                   mutate(value2 = if_else(date <= as.Date("2014-01-01"), value, renda_real)) %>% #junta o que está em value somente até 2014 (primeira base de dados) e pois pega da renda real, pq fizemos o ajusta
                   select(date, value, renda_real, value2, tcode)%>% 
                   mutate(temp_value = value,     # Cria uma coluna temporária para armazenar o conteúdo original de value
                              value = value2,         
                              value2 = temp_value) %>%
                   select(-temp_value) %>%
                   rename(regiao_cod = tcode)  # chamei de regiao, mesmo incluindo os estados para facilitar comparação posterior
                   ## obs: Mato grosso do sul a partir de 1981; Tocantis a partir de 1992
                   ## Renda percapita NÃO disponibilizada pela fonte nos anos: 1980, 1991, 1994, 2000, 2010
  
rpc_brasil <- renda_percapita %>%
  filter(regiao_cod == 0) 

rpc_estados <- renda_percapita %>%
filter(nchar(as.character(regiao_cod)) == 2)
  
rpc_regiao <- renda_percapita %>% # disponível até 2014
  filter(regiao_cod %in% c(1, 2, 3, 4, 5))


# Extrair a renda percapita média anual (dados faltantes de 2014 a 2021) das regiões brasileiras (norte, sul, nordeste, sudeste, centro-oeste) -----------------------------
## datas originais xxxx-01-01

rpcmed <- renda_percapita %>% # usada apenas pra efeito de comparação entre as rendas percapitas já calculadas pelo IBGE e as calculadas por mim
  mutate(regiao = substr(regiao_cod, 1, 1)) %>%
  mutate(date = lubridate::year(date)) %>%
  group_by(date, regiao) %>%
  mutate(rpcmed = if (any(is.na(value))) NA else mean(value, na.rm = TRUE)) %>%
  ungroup()

rpcmed_regiao <- rpc_estados %>%
  mutate(regiao = substr(regiao_cod, 1, 1)) %>%
  mutate(date = lubridate::year(date)) %>%
  group_by(date, regiao) %>%
  mutate(rpcmed = if (any(is.na(value))) NA else mean(value, na.rm = TRUE)) %>%
  ungroup()

         ## obs: Mato grosso do sul a partir de 1981; Tocantis a partir de 1992
          ## Renda percapita NÃO disponibilizada pela fonte nos anos: 1980, 1991, 1994, 2000, 2010

## conferindo as maiores renda percaptas para cada ano de acordo com os dados do IPEA 1976 a 2014
rmax_ano <- renda_ipeadata %>%
                     group_by(date) %>%
                     filter(uname == "Regions") %>%
                     slice(which.max(value))

# Gráfico evoluç;ao da renda percapita média por regiao --------

graph_rpcmed <- function(df, regiao) {
  df_filtrado <- df %>% 
    filter(regiao == !!regiao) %>%
    mutate(date = as.numeric(as.character(date)))
  ggplot(df_filtrado, aes(x = date, y = rpcmed)) +
    geom_point(size = 0.75) +
    geom_line() +
    labs(x = "",
         y = "Renda percapita média") +
    scale_x_continuous(breaks = seq(min(df_filtrado$date), max(df_filtrado$date), by = 3)) +
    scale_y_continuous(
      breaks = seq(min(df_filtrado$rpcmed), max(df_filtrado$rpcmed), by = (max(df_filtrado$rpcmed) - min(df_filtrado$rpcmed)) / 5),
      labels = scales::label_number(accuracy = 1) 
    ) +   
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_text(size = 10, margin = margin(t = 10)),  
      axis.title.y = element_text(size = 10, margin = margin(r = 10)),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  
      axis.text = element_text(size = 10),
      axis.ticks.length = unit(0.3, "cm"),
      axis.ticks = element_line(size = 0.5),
      panel.border = element_rect(colour = "black", fill = NA, size = 0,5)
    ) 
}

graph_rpcmed(rpcmed_regiao,5)

# Gráfico todas as regiões num único plot
graph_unico_rpcmed <- function(df) {
  
  region_labels <- c(
    "1" = "Região Norte",
    "2" = "Região Nordeste",
    "3" = "Região Sudeste",
    "4" = "Região Sul",
    "5" = "Região Centro Oeste"
  )
  
  
  df <- df %>%
    mutate(date = as.numeric(as.character(date)))
  
  ggplot(df, aes(x = date, y = rpcmed)) +
    geom_point(size = 0.3) +
    geom_line() +
    labs(x = "",
         y = "Renda percapita média (R$ de 2014)") +
    scale_x_continuous(breaks = seq(min(df$date), max(df$date), by = 3)) +
    scale_y_continuous(
      breaks = c(200, 500, 800, 1100, 1400),
      labels = scales::label_number(accuracy = 1)
    ) +   
    theme_minimal() +
    theme(
      panel.grid.major.x = element_line(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(),
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_text(size = 10, margin = margin(t = 10)),  
      axis.title.y = element_text(size = 10, margin = margin(r = 10)),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  
      axis.text = element_text(size = 10),
      axis.ticks.length = unit(0.3, "cm"),
      axis.ticks = element_line(size = 0.5),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
    ) +
    facet_wrap(~ regiao, nrow = 3, as.table = TRUE, labeller = labeller(regiao = region_labels))  # Ajusta para 2 linhas (2 em cima e 3 embaixo)
}

graph_unico_rpcmed(rpcmed_regiao)





# Dados SIDRA_IBGE Valor de Transformação Industrial -----------------------------------------------------------------------
library(sidrar)
library(curl)

# Códigos de Consulta (obtidos no site) com filtros (Sidra/IBGE)
cod_tab2221 <- "/t/2221/n1/all/n3/all/u/y/v/811,1000811/p/all/c12123/all/d/v1000811%202"
cod_tab1987 <- "/t/1987/n1/all/n3/all/v/811,1000811/p/all/c11939/all/d/v1000811%202"
cod_tab1988 <- "/t/1988/n3/all/v/811,1000811/p/all/c11939/all/d/v1000811%202"
cod_tab1849 <- "/t/1849/n3/all/v/706,811,1000811/p/all/c12762/all/d/v1000811%202"

# Extrair dados a partir do Cod
tab2221 <- sidrar::get_sidra(api = cod_tab2221)
tab1987 <- sidrar::get_sidra(api = cod_tab1987)
tab1988 <- sidrar::get_sidra(api = cod_tab1988)
tab1849 <- sidrar::get_sidra(api = cod_tab1849)

# Filtrar somente informações de interesse
## obs: nessa etapa todas as tabelas foram renomeadas igualmente, para facilitar etapas posteriores,
##     ignorando as distintas nomemclaturas de classificações de atividades econômicas empregadas em cada periódo.
vti_tab2221 <- tab2221 %>%
               filter(`Variável (Código)` == 811) %>%
               select("date" = "Ano", "cnae_cod" = "Classes e gêneros de indústria (Código)", "cnae" = "Classes e gêneros de indústria", 
                      "uf_cod" = "Brasil e Unidade da Federação (Código)", "uf" = "Brasil e Unidade da Federação", "vti" = Valor) %>%
               filter(uf_cod > 1) # retirar os totais para o BR
                                    
vti_tab1987 <- tab1987 %>%
               filter(`Variável (Código)` == 811) %>%
               select("date" = "Ano", "cnae_cod" = "Classificação Nacional de Atividades Econômicas (CNAE) (Código)", "cnae" = "Classificação Nacional de Atividades Econômicas (CNAE)" , 
               "uf_cod" = "Brasil e Unidade da Federação (Código)", "uf" = "Brasil e Unidade da Federação", "vti" = Valor) %>%
               filter(uf_cod > 1)

vti_tab1988 <- tab1988 %>%
               filter(`Variável (Código)` == 811) %>%
               select("date" = "Ano", "cnae_cod" = "Classificação Nacional de Atividades Econômicas (CNAE) (Código)",  "cnae" = "Classificação Nacional de Atividades Econômicas (CNAE)" , 
               "uf_cod" = "Unidade da Federação (Código)", "uf" = "Unidade da Federação", "vti"= "Valor") %>%
               filter(uf_cod > 1)

vti_tab1849 <- tab1849 %>%
               filter(`Variável (Código)` == 811) %>%
               select("date" = "Ano", "cnae_cod" = "Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)",
               "cnae" = "Classificação Nacional de Atividades Econômicas (CNAE 2.0)",
               "uf_cod" = "Unidade da Federação (Código)", "uf" = "Unidade da Federação", "vti" = "Valor") %>%
               filter(uf_cod > 1, date >= 2008)

# df para o BRASIL
vti_brasil <- bind_rows(
  tab2221 %>%
    filter(`Variável (Código)` == 811) %>%
    select(
      "date" = "Ano",
      "cnae_cod" = "Classes e gêneros de indústria (Código)",
      "cnae" = "Classes e gêneros de indústria",
      "uf_cod" = "Brasil e Unidade da Federação (Código)",
      "uf" = "Brasil e Unidade da Federação",
      "vti" = "Valor")%>%
      filter(uf_cod == 1)
    ,
  tab1987 %>%
    filter(`Variável (Código)` == 811) %>%
    select(
      "date" = "Ano",
      "cnae_cod" = "Classificação Nacional de Atividades Econômicas (CNAE) (Código)",
      "cnae" = "Classificação Nacional de Atividades Econômicas (CNAE)",
      "uf_cod" = "Brasil e Unidade da Federação (Código)",
      "uf" = "Brasil e Unidade da Federação",
      "vti" = "Valor") %>%
      filter(uf_cod == 1) 
    ,
  tab1988 %>%
    filter(`Variável (Código)` == 811) %>%
    select(
      "date" = "Ano",
      "cnae_cod" = "Classificação Nacional de Atividades Econômicas (CNAE) (Código)",
      "cnae" = "Classificação Nacional de Atividades Econômicas (CNAE)",
      "uf_cod" = "Unidade da Federação (Código)",
      "uf" = "Unidade da Federação",
      "vti" = "Valor")%>%
      filter(uf_cod == 1)
    ,
  tab1849 %>%
    filter(`Variável (Código)` == 811) %>%
    select(
      "date" = "Ano",
      "cnae_cod" = "Classificação Nacional de Atividades Econômicas (CNAE 2.0) (Código)",
      "cnae" = "Classificação Nacional de Atividades Econômicas (CNAE 2.0)",
      "uf_cod" = "Unidade da Federação (Código)",
      "uf" = "Unidade da Federação",
      "vti" = "Valor") %>%
    filter(uf_cod == 1, date >= 2008)   ## não puxa os dados para o brasil pq a tabela já foi baixada sem ele
    
)

 # Substitui "1" por "0"
      
    
          
## obs: Os dados de VTI não estão disponíveis para todos os anos da série, já que a PIA deixou de ser realizada algumas vezes 
### no período em questão. Faltam dados para os anos de 1970, 1971, 1972, 1975, 1980, 1985, 1986, 1987 e 1991.

# Problema 1: Compatibilizar as unidades de medida para todos os anos NÃO PADRONIZAR--------------------------------------------
 
  ## tab 2221 - Mil Cruzeiros Novos [1966 a 1968], 
  ##          - Mil Cruzeiros [1969, 1973 a 1974, 1976 a 1979, 1981 a 1984, 1990],
  ##          - Milhões de Cruzados [1988], 
  ##          - Mil Cruzados Novos [1989], 
  ##          - Milhões de Cruzeiros [1992], 
  ##          - Milhões de Cruzeiros Reais [1993], 
  ##          - Mil Reais [1994 a 1995]
  ## tab 1987 - Mil Reais
  ## tab 1988 - Mil Reais
  ## tab 1849 - Mil Reais

# Função para padronizar unidades de medida - porque não padronizar - não faz sentido, dado que estaria beneficiando os dados das unidades monetárias em reais.()

tab2221_temp <- tab2221 %>%
  mutate (fator_conversao = case_when(
     tab2221_temp$`Unidade de Medida (Código)`== 1576 ~ 1 / (1000^3 * 2750), # Mil Cruzeiros Novos para Mil Reais
     tab2221_temp$`Unidade de Medida (Código)` == 1012 ~ 1 / (1000^3 * 2750), # Mil Cruzeiros para Mil Reais
     tab2221_temp$`Unidade de Medida (Código)` == 51 ~ 1 / (1000^2 * 2750),   # Milhões de Cruzados para Mil Reais
     tab2221_temp$`Unidade de Medida (Código)` == 42 ~ 1 / (1000 * 2750),     # Mil Cruzados Novos para Mil Reais
     tab2221_temp$`Unidade de Medida (Código)` == 52 ~ 1 / 2750,             # Milhões de Cruzeiros para Mil Reais
     tab2221_temp$`Unidade de Medida (Código)` == 53 ~ 1 / 2750,             # Milhões de Cruzeiros Reais para Mil Reais
     tab2221_temp$`Unidade de Medida (Código)` == 40 ~ tab2221_temp$Valor,                   # Mil Reais (sem conversão)
    TRUE ~ NA_real_
  )) %>%
  mutate(Valor2 = fator_conversao * Valor) %>%
  mutate(Valor2 = if_else(
    `Unidade de Medida (Código)` %in% c(51, 52, 53), 
    Valor2 * 1000, 
    Valor2
  ))



# Problema 2: Compatibilizar as classes e genêros industriais e as CNAIs 1.0 e 2.0-------------------------------
# Anos 1966 a 1995
## - classes e gêneros industriais (e não CNAI) 
## - 28 unidades da federação (1 a mais) - tab 2221

# Anos 1966 a 2007 
## - CNAI 1.0
## - CNAI 2 dígitos para 21 estados - tab 1988
## - CNAI 2 e 3 dígitos para 6 estados -->
## (Minas Gerais Rio de Janeiro, São Paulo, Paraná, Santa Catarina e Rio Grande do Sul) - tab 1987
## Somando as duas tabelas, tab 1987 e 1988, teremos as 27 unidades da federação ao todo

# Anos 2007 a 2021 - teremos CNAI 2.0
## - teremos CNAI 2 digitos para os 27 estados - tab 1849

# Pendente: 
cat("Retirar da atividade industrial os valores referentes à produção dos setores de extração e de refino de petróleo. Em razão de que as atividades de extração e refino de petróleo sempre se mostram problemáticas no encadeamento de séries de dados apurados por gênero e por CNAE, devido tanto a dificuldades na separação dos valores gerados em cada uma individualmente quanto ao enorme crescimento experimentado pela atividade de extração de petróleo no período recente. Para os anos anteriores a 1995, não há dados desagregados do VTI dos gêneros industriais que permitem descontar as parcelas referentes à extração e ao refino de petróleo das indústrias extrativa mineral e química, respectivamente, utilizou-se como primeira aproximação as proporções apuradas em 1996, primeiro ano para o qual o dado era disponível.\n")


# Agrupar os dados das tabelas 1987 (6 estados, cnae 2 e 3 digítos) e 1988 (21 estados) 
# Filtrar os dados para manter apenas os códigos CNAE de 2 dígitos

vti_tab1987_2 <- vti_tab1987 %>%
  filter(str_detect(cnae, "^\\d{2} ") & !str_detect(cnae, "^\\d{2}\\.\\d "))

vti_tabs8788 <- bind_rows(vti_tab1987_2, vti_tab1988)

# Compatibilizar as classes e genêros industriais e as CNAIs 1.0 e 2.0 -----------------------------------------
## Criar coluna de identificação da região:

colum_regiao <- function(df) {
  df %>%
    mutate(
      regiao_cod = substr(uf_cod, 1, 1),
      regiao = case_when(
        regiao_cod == "1" ~ "Norte",
        regiao_cod == "2" ~ "Nordeste",
        regiao_cod == "3" ~ "Sudeste",
        regiao_cod == "4" ~ "Sul",
        regiao_cod == "5" ~ "Centro-Oeste",
        TRUE ~ "Outra"
      )
    ) %>%
    select(date, cnae_cod, cnae, vti, uf_cod, uf, regiao_cod, regiao)
}

vti_tab2221 <- colum_regiao(vti_tab2221)
vti_tabs8788 <- colum_regiao(vti_tab1988)
vti_tab1849 <- colum_regiao(vti_tab1849)

vti_brasil <- colum_regiao(vti_brasil)
vti_brasil <- vti_brasil %>% 
  select(-regiao) %>%
  mutate(
    regiao_cod = if_else(uf_cod == "1", "0", as.character(uf_cod))
  ) 

## vti por região = somatório do vti de todas as UFs de cada região e para cada cod cnae

vtiregiao_tab2221 <- vti_tab2221 %>% 
  group_by(date, cnae_cod,cnae, regiao_cod, regiao) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = 'drop') %>%
  select(date, cnae_cod, cnae, vti, regiao_cod, regiao)

             #teste modleo por estados
            vti_tab2221_estados <- vti_tab2221 %>%
            select(date, cnae_cod, cnae, vti, uf_cod,uf, regiao_cod, regiao)

          
vtiregiao_tabs8788 <- vti_tabs8788 %>%
  filter(!cnae %in% c("D Indústrias de transformação", "C Indústrias extrativas")) %>% # excluir as linhas de seção CNAE
  group_by(date, cnae_cod,cnae, regiao_cod, regiao) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = 'drop') %>%
  select(date, cnae_cod, cnae, vti,regiao_cod, regiao)
                  
              #teste modelo por estados
              vti_tabs8788_estados <- vti_tabs8788 %>%
              filter(!cnae %in% c("D Indústrias de transformação", "C Indústrias extrativas")) %>% # excluir as linhas de seção CNAE
                select(date, cnae_cod, cnae, vti, uf_cod,uf, regiao_cod, regiao)

vtiregiao_tab1849 <- vti_tab1849 %>%
  filter(!cnae %in% c("B Indústrias extrativas", "C Indústrias de transformação")) %>%
  group_by(date, cnae_cod,cnae, regiao_cod, regiao) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = 'drop') %>%
  select(date, cnae_cod, cnae, vti, regiao_cod, regiao)

            #teste modelo por estados
              vti_tab1849_estados <- vti_tab1849 %>%
              filter(!cnae %in% c("B Indústrias extrativas", "C Indústrias de transformação")) %>%
              select(date, cnae_cod, cnae, vti, uf_cod,uf, regiao_cod, regiao)
              

vti_brasil <- vti_brasil %>%
  filter(!cnae %in% c("B Indústrias extrativas", "C Indústrias de transformação",
                      "D Indústrias de transformação", "C Indústrias extrativas", 
                      "Total", "Indústria de transformação", "Indústrias extrativas e de produtos minerais",
                      "Atividades de apoio e de serviços de caráter industrial", "Atividades administrativas")) %>%
  select(date, cnae_cod, cnae, vti, regiao_cod)

# Compatibilizar as classes e genêros industriais e as CNAEs 1.0 e 2.0

# dfs de converção Gênero Industrial para Agregação/ CNAE 1.0 para Agregação/ CNAE 2.0 para Agregação ---------
agregacao_tab2221 <- data.frame(
  cnae = c("Bebidas", "Borracha", "Couros e peles e produtos similares", 
            "Diversas", "Editorial e gráfica", "Fumo", "Madeira", 
            "Material de transporte", "Material elétrico e material de comunicações", 
            "Mecânica", "Metalúrgica", "Minerais não-metálicos", 
            "Mobiliário", "Papel e papelão", "Produtos alimentares", 
            "Produtos de matérias plásticas", "Produtos de perfumaria, sabões e velas", 
            "Produtos farmacêuticos e medicinais", "Produtos minerais", "Química", 
            "Têxtil", "Vestuário, calçados e artefatos de tecidos"),
  agregacao = c("Alimentos e bebidas", "Borracha", "Vestuários couros e calçados", 
                "Diversas", "Editorial e gráfica", "Fumo", "Madeira", 
                "Material de transporte", "Material elétrico e comunicações", 
                "Mecânica", "Metalúrgica", "Minerais não-metálicos", 
                "Mobiliário", "Papel e celulose", "Alimentos e bebidas", 
                "Borracha e plásticos", "Química", 
                "Química", "Extrativa", "Química", 
                "Têxtil", "Vestuários couros e calçados")
)

agregacao_tabs8788 <- data.frame(
  cnae = c("10", "11", "13", "14", "15", "16", "17", "18", "19", 
           "20", "21", "22", "23", "24", "25", "26", "27", "28", 
           "29", "30", "31", "32", "33", "34", "35", "36", "36.1", "36.9", "37"),
  agregacao = c("Extrativa", "Extrativa", "Extrativa", "Extrativa", "Alimentos e bebidas", 
                "Fumo", "Têxtil", "Vestuários couros e calçados", "Vestuários couros e calçados", 
                "Madeira", "Papel e celulose", "Editorial e gráfica", "Química", 
                "Química", "Borracha e plásticos", "Minerais não-metálicos", 
                "Metalúrgica", "Metalúrgica", "Mecânica", "Material elétrico e comunicações", 
                "Material elétrico e comunicações", "Material elétrico e comunicações", 
                "Material elétrico e comunicações", "Material de transporte", "Material de transporte","Mobiliário",
                "Mobiliário", "Diversas", "Diversas")
)

agregacao_tab1849 <- data.frame(
  cnae = c("05", "06", "09", "07", "08", "10", "11", "12", "13", "14", "15", 
           "16", "17", "18", "19", "20", "22","21", "23", "24", "25", 
           "28", "26", "27", "29", "30", "31", "32", "38", "33"),
  agregacao = c("Extrativa", "Extrativa", "Extrativa", "Extrativa", "Extrativa", "Alimentos e bebidas", 
                "Alimentos e bebidas", "Fumo", "Têxtil", "Vestuários couros e calçados", 
                "Vestuários couros e calçados", "Madeira", "Papel e celulose", "Editorial e gráfica",
               "Química", "Química", "Borracha e plásticos", "Química","Minerais não-metálicos", 
                "Metalúrgica", "Metalúrgica", "Mecânica", "Material elétrico e comunicações", 
                "Material elétrico e comunicações", "Material de transporte","Material de transporte","Mobiliário",
                "Diversas", "Diversas", "Mecânica")
  )


## Convertendo a coluna cnae original para a agregação

agregado_vtiregiao_tab2221 <- vtiregiao_tab2221 %>%
  left_join(agregacao_tab2221, by = c("cnae")) %>%
  mutate(cnae = agregacao) %>% # não inclui "Total", "Indústrias extrativas e de produtos minerais", "Indústria de transformação"
  filter (!is.na(cnae))

agregado_vtiregiao_tabs8788 <- vtiregiao_tabs8788 %>%
  mutate(cnae2dig = substr(cnae, 1, 2)) %>%  # Extrai os 2 primeiros dígitos da coluna cnae
  left_join(agregacao_tabs8788, by = c("cnae2dig" = "cnae")) %>%
  mutate(cnae = agregacao) %>% # Substitui a descrição da cnae pela agregação # não inclui "Total" e "outros", pendente: verificar cód 11 - petróleo
  select(date, cnae_cod, cnae2dig, cnae,vti, regiao_cod, regiao) %>%
  filter(!is.na(cnae))

agregado_vtiregiao_tab1849 <- vtiregiao_tab1849 %>%
  mutate(cnae2dig = substr(cnae, 1, 2)) %>%  
  left_join(agregacao_tab1849, by = c("cnae2dig" = "cnae")) %>%
  mutate(cnae = agregacao) %>% 
  select(date, cnae_cod, cnae2dig, cnae,vti, regiao_cod, regiao) %>%
  filter(!is.na(cnae))


ok_vtiregiao_tab2221 <- agregado_vtiregiao_tab2221 %>%
  group_by(date, cnae, regiao_cod, regiao) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = "drop")

ok_vtiregiao_tabs8788 <- agregado_vtiregiao_tabs8788 %>%
  group_by(date, cnae, regiao_cod, regiao) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = "drop")

ok_vtiregiao_tab1849 <- agregado_vtiregiao_tab1849 %>%
  group_by(date, cnae, regiao_cod, regiao) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = "drop")
 
     # teste brasil

agregado_vti_brasil <- vti_brasil %>%
  mutate(cnae2dig = substr(cnae, 1, 2)) %>% # Extrai os dois primeiros dígitos para os períodos posteriores
  mutate(
    agregacao = case_when(
      date >= 1966 & date <= 1995 ~ {
        left_join(., agregacao_tab2221, by = c("cnae" = "cnae"))$agregacao
      },
      date >= 1996 & date <= 2007 ~ {
        left_join(., agregacao_tabs8788, by = c("cnae2dig" = "cnae"))$agregacao
      },
      date >= 2008 & date <= 2022 ~ { #não puxa dessa tabela pq não existe dados para o brasil nela.
        left_join(., agregacao_tab1849, by = c("cnae2dig" = "cnae"))$agregacao
      },
      TRUE ~ cnae
    )
  ) %>%
  mutate(cnae = ifelse(is.na(agregacao), cnae, agregacao))%>%
  filter(!is.na(cnae))

ok_vti_brasil <- agregado_vti_brasil %>%
  group_by(date, cnae, regiao_cod) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = "drop")


        # teste para rodar o modelo por estados

vti_tab2221_estados <- vti_tab2221_estados %>%
  left_join(agregacao_tab2221, by = c("cnae")) %>%
  mutate(cnae = agregacao) %>%
  group_by(date, cnae, uf_cod, uf) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = "drop") %>%
filter(!cnae == "Total")

ok_vti_tab2221_estados <- vti_tab2221_estados %>%
  group_by(date, cnae, uf_cod, uf) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = "drop")


vti_tabs8788_estados <- vti_tabs8788_estados %>%
  mutate(cnae2dig = substr(cnae, 1, 2)) %>%  # Extrai os 2 primeiros dígitos da coluna cnae
  left_join(agregacao_tabs8788, by = c("cnae2dig" = "cnae")) %>%
  mutate(cnae = agregacao) %>% # Substitui a descrição da cnae pela agregação # não inclui "Total" e "outros", pendente: verificar cód 11 - petróleo
  select(date, cnae,vti, uf_cod, uf)%>%
  group_by(date, cnae, uf_cod, uf) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(cnae))

ok_vti_tabs8788_estados <- vti_tabs8788_estados %>%
  group_by(date, cnae, uf_cod, uf) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = "drop")


vti_tab1849_estados <- vti_tab1849_estados %>%
  mutate(cnae2dig = substr(cnae, 1, 2)) %>%  
  left_join(agregacao_tab1849, by = c("cnae2dig" = "cnae")) %>%
  mutate(cnae = agregacao) %>% 
  select(date,cnae,vti, uf_cod, uf) %>%
  group_by(date, cnae, uf_cod, uf) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(cnae))

ok_vti_tab1849_estados <- vti_tab1849_estados %>%
  group_by(date, cnae, uf_cod, uf) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = "drop")


# criar df combinando todos os agregados por regiao

ok_vtiregiao_comb <- bind_rows (
  ok_vtiregiao_tab2221,
  ok_vtiregiao_tabs8788,
  ok_vtiregiao_tab1849
)


ok_vtiestados_comb <- bind_rows (
  ok_vti_tab2221_estados,
  ok_vti_tabs8788_estados,
  ok_vti_tab1849_estados
)

# tabelas para criar e salvas dfs para extrair o percentual de cada setor por ano ---------------------------------------
norte_vti <- ok_vtiregiao_comb %>%
  filter(regiao_cod == 1) %>%
group_by(date) %>%  # Agrupa por ano
  mutate(total_vti = sum(vti, na.rm = TRUE),  # Soma o vti total do ano
         percentual_vti = (vti / total_vti) * 100) %>%  # Calcula o percentual de cada setor
  ungroup() %>%
  select(date, cnae, percentual_vti) %>%
  pivot_wider(names_from = date, values_from = percentual_vti, values_fill = 0)

nordeste_vti <- ok_vtiregiao_comb %>%
  filter(regiao_cod == 2) %>%
group_by(date) %>%  # Agrupa por ano
  mutate(total_vti = sum(vti, na.rm = TRUE),  # Soma o vti total do ano
         percentual_vti = (vti / total_vti) * 100) %>%  # Calcula o percentual de cada setor
  ungroup()%>%
  select(date, cnae, percentual_vti) %>%
  pivot_wider(names_from = date, values_from = percentual_vti, values_fill = 0)

sudeste_vti <- ok_vtiregiao_comb %>%
  filter(regiao_cod == 3) %>%
  group_by(date) %>%  # Agrupa por ano
  mutate(total_vti = sum(vti, na.rm = TRUE),  # Soma o vti total do ano
         percentual_vti = (vti / total_vti) * 100) %>%  # Calcula o percentual de cada setor
  ungroup()%>%
  select(date, cnae, percentual_vti) %>%
  pivot_wider(names_from = date, values_from = percentual_vti, values_fill = 0)

sul_vti <- ok_vtiregiao_comb %>%
  filter(regiao_cod == 4) %>%
  group_by(date) %>%  # Agrupa por ano
  mutate(total_vti = sum(vti, na.rm = TRUE),  # Soma o vti total do ano
         percentual_vti = (vti / total_vti) * 100) %>%  # Calcula o percentual de cada setor
  ungroup()%>%
  select(date, cnae, percentual_vti) %>%
  pivot_wider(names_from = date, values_from = percentual_vti, values_fill = 0)

centro_oeste_vti <- ok_vtiregiao_comb %>%
  filter(regiao_cod == 5) %>%
  group_by(date) %>%  # Agrupa por ano
  mutate(total_vti = sum(vti, na.rm = TRUE),  # Soma o vti total do ano
         percentual_vti = (vti / total_vti) * 100) %>%  # Calcula o percentual de cada setor
  ungroup()%>%
  select(date, cnae, percentual_vti) %>%
  pivot_wider(names_from = date, values_from = percentual_vti, values_fill = 0)


    # teste brasil
brasil_vti_percent <- ok_vti_brasil %>%
  group_by(date) %>%  # Agrupa por ano
  mutate(total_vti = sum(vti, na.rm = TRUE),  # Soma o vti total do ano
         percentual_vti = (vti / total_vti) * 100) %>%  # Calcula o percentual de cada setor
  ungroup()%>%
  select(date, cnae, percentual_vti) %>%
  pivot_wider(names_from = date, values_from = percentual_vti, values_fill = 0)

# inverter para as tabelas de visualização do percentual
write.xlsx(norte_vti, "norte_vti_2.xlsx")
write.xlsx(nordeste_vti, "nordeste_vti_2.xlsx")
write.xlsx(sudeste_vti, "sudeste_vti_2.xlsx")
write.xlsx(sul_vti, "sul_vti_2.xlsx")
write.xlsx(centro_oeste_vti, "centro_oeste_vti_2.xlsx")
write.xlsx(sul_vti, "brasil_vti_percent.xlsx")

# Medida do grau de especialização setorial - coeficiente de Gini-Hirschmann (GH) ------------------------------
## Função calc_gh, realiza o cálculo do GH de acordo com as especificações da tabela de dados, ano e cod da região(1 a 5).
###obs: A PIA deixou de ser realizada nos anos de 1970, 1971, 1972, 1975, 1980, 1985, 1986, 1987 e 1991.

calc_gh <- function(df, ano, regiao) {
  df_filtrado <- df %>%
    filter(date == !!ano, regiao_cod == !!regiao, cnae != "Total")
  
  print("df")
  print(head(df_filtrado))
    xij <- df_filtrado$vti
    
    print("xij:")
    print(head(xij))
    
    xj <- sum(xij, na.rm = TRUE)
    
    print("Xj:")
    print(xj)
    
    ihh <- sum((xij / xj)^2, na.rm = TRUE)
    
    print("ihh:")
    print(ihh)
    
    gh <- 100 * sqrt(ihh)
    
    return(gh)
}
calc_gh(ok_vtiregiao_tab2221,1966,1)
calc_gh(ok_vtiregiao_tab2221, 1966,1)
calc_gh(ok_vti_brasil,1976,0)


            # teste calculo gh por estados
calc_gh_estados <- function(df, ano, regiao) {
df_filtrado <- df %>%
  filter(date == !!ano, uf_cod == !!regiao, cnae != "Total")

print("df")
print(head(df_filtrado))
xij <- df_filtrado$vti

print("xij:")
print(head(xij))

xj <- sum(xij, na.rm = TRUE)

print("Xj:")
print(xj)

ihh <- sum((xij / xj)^2, na.rm = TRUE)

print("ihh:")
print(ihh)

gh <- 100 * sqrt(ihh)

return(gh)
}

calc_gh_estados(vti_tab2221_estados, 1976, 11)


# Obter o GH para todos os anos e criar um data frame

df_gh21 <- ok_vtiregiao_tab2221 %>%
  distinct(date, regiao_cod, regiao) %>%
  rowwise() %>%
  mutate(gh = calc_gh(ok_vtiregiao_tab2221, date, regiao_cod)) %>%
  ungroup()


df_gh8788 <- ok_vtiregiao_tabs8788 %>%
  distinct(date, regiao_cod, regiao) %>%  
  rowwise() %>%
  mutate(gh = calc_gh(ok_vtiregiao_tabs8788, date, regiao_cod)) %>%
  ungroup()


df_gh49 <- ok_vtiregiao_tab1849 %>%
  distinct(date, regiao_cod, regiao) %>%
  rowwise() %>%
  mutate(gh = calc_gh(ok_vtiregiao_tab1849, date, regiao_cod)) %>%
  ungroup()

# Combinar os data frames
df_ghcomb <- bind_rows(
  df_gh21 %>% mutate(fonte = "tab2221"),
  df_gh8788 %>% mutate(fonte = "tabs8788"),
  df_gh49 %>% mutate(fonte = "tab1849")
)

        # teste para o brasil
df_brasil <- ok_vti_brasil %>%
  distinct(date, regiao_cod) %>%
  rowwise() %>%
  mutate(
    gh = calc_gh(ok_vti_brasil, date, regiao_cod)
  ) %>%
  ungroup()

       # teste para todos os estados

df_gh21_estados <- ok_vti_tab2221_estados %>%
  distinct(date, uf_cod, uf) %>%
  rowwise() %>%
  mutate(gh = calc_gh_estados(ok_vti_tab2221_estados, date, uf_cod)) %>%
  ungroup()

df_gh8788_estados <- ok_vti_tabs8788_estados %>%
  distinct(date, uf_cod, uf) %>%
  rowwise() %>%
  mutate(gh = calc_gh_estados(ok_vti_tabs8788_estados, date, uf_cod)) %>%
  ungroup()

df_gh1849_estados <- ok_vti_tab1849_estados %>%
  distinct(date, uf_cod, uf) %>%
  rowwise() %>%
  mutate(gh = calc_gh_estados(ok_vti_tab1849_estados, date, uf_cod)) %>%
  ungroup()


        # Combinar os data frames dos estados
df_ghcomb_estados <- bind_rows(
  df_gh21_estados %>% mutate(fonte = "tab2221"),
  df_gh8788_estados %>% mutate(fonte = "tabs8788"),
  df_gh1849_estados %>% mutate(fonte = "tab1849")
)

# Gráfico da evolução do ìndice de especialização GH no tempo ------------------------------------------
df_ghcomb <- df_ghcomb %>%
  mutate(date = as.numeric(as.character(date)))


plot_function <- function(df, regiao) {
  df_filtrado <- df %>% 
    filter(regiao_cod == !!regiao) %>% #maior igual a 1976, dado que a renda percapita só está disponível a partir dessa data
    mutate(date = as.numeric(as.character(date)))
  
  df_completo <- df_filtrado %>%
    complete(date = seq(min(date), max(date))) %>% 
    fill(gh, .direction = "down")
  
  ggplot(df_filtrado, aes(x = date, y = gh)) +
    geom_line() +
    labs(x = "",
         y = "Gini-Hirschmann (VTI)") +
    scale_x_continuous(breaks = seq(min(df_completo$date, na.rm = TRUE), max(df_completo$date, na.rm = TRUE))) +
    scale_y_continuous(
      breaks = seq(min(df_filtrado$gh), max(df_filtrado$gh), length.out = 7),
      labels = label_number(accuracy = 1), 
    )+
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(colour = "black", size = 0.2),
      axis.title.x = element_text(size = 10, margin = margin(t = 10)),  
      axis.title.y = element_text(size = 10, margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Rotação vertical do texto no eixo X
      axis.ticks.length = unit(0.3, "cm"),
      axis.ticks = element_line(size = 0.5),
      panel.border = element_rect(colour = "black", fill = NA, size = 0,5)
      ) 
}

plot_function(df_gh21,3)
plot_function(df_gh8788,3)
plot_function(df_gh49,3)
plot_function(df_ghcomb,5)

plot_function(df_brasil,0) # teste Brasil

plot_function_estados <- function(df, regiao) {
  df_filtrado <- df %>% 
    filter(uf_cod == !!regiao) %>%
    mutate(date = as.numeric(as.character(date)))
  ggplot(df_filtrado, aes(x = date, y = gh)) +
    geom_point() +
    geom_line() +
    labs(x = "",
         y = "Gini-Hirschmann (VTI)") +
    scale_x_continuous(breaks = seq(min(df_filtrado$date), max(df_filtrado$date), by = 3)) +
    scale_y_continuous(
      breaks = seq(min(df_filtrado$gh), max(df_filtrado$gh), by = (max(df_filtrado$gh) - min(df_filtrado$gh)) / 3),
      labels = label_number(accuracy = 1) 
    ) +   
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.x = element_text(size = 10, margin = margin(t = 10)),  
      axis.title.y = element_text(size = 10, margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      axis.ticks.length = unit(0.3, "cm"),
      axis.ticks = element_line(size = 0.5),
      panel.border = element_rect(colour = "black", fill = NA, size = 0,5)
    ) 
} # teste para estados

plot_function_estados(df_ghcomb_estados, 41)

# Grafico com todas as regiões no mesmo plot

plot_regions_gh <- function(df) {
  df <- df %>% 
    mutate(date = as.numeric(as.character(date))) %>%
    filter(date <= 2021)
  
  df_completo <- df %>%
    complete(date = seq(min(date), 2021)) %>% 
    fill(gh, .direction = "down")

  region_labels <- c(
    "1" = "Região Norte",
    "2" = "Região Nordeste",
    "3" = "Região Sudeste",
    "4" = "Região Sul",
    "5" = "Região Centro-Oeste"
  )
  
  ggplot(df, aes(x = date, y = gh)) +
    geom_line() +
    labs(x = "",
         y = "Gini-Hirschmann (VTI)") +
    scale_x_continuous(
        breaks = union(seq(min(df_completo$date, na.rm = TRUE), 2017, by = 3), 2021)  
      ) +
    scale_y_continuous(
      breaks = seq(min(df$gh), max(df$gh), by = (max(df$gh) - min(df$gh)) / 10),
      labels = label_number(accuracy = 1) 
    ) + 
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(colour = "black", size = 0.2),
      axis.title.x = element_text(size = 10, margin = margin(t = 10)),  
      axis.title.y = element_text(size = 10, margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      axis.ticks.length = unit(0.3, "cm"),
      axis.ticks = element_line(size = 0.5),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
    ) +
    facet_wrap(~ regiao_cod, nrow = 3, scales = "free", labeller = labeller(regiao_cod = region_labels))
}

plot_regions_gh(df_ghcomb)



# Regressões locais não-paramétricas (LOWESS) ----------------------------------------------------------- 

#agrupar renda percapita media de cada regiao com o gh de cada regiao, por ano


gh_rpcmed <- df_ghcomb %>%
  inner_join(
    rpcmed_r %>% 
      mutate(date = as.character(date)) %>%
      select(date, rpcmed, regiao_cod), 
    by = c("date", "regiao_cod")
  )


# teste agrupando a renda percapita do brasil com o gh de cada ano

gh_rpcbrasil <- df_brasil %>%
  mutate(date = as.character(date)) %>%  # Converte 'date' de df_brasil para character
  inner_join(
    rpc_brasil %>%
      mutate(date = as.character(year(date))) %>%   # Converte o ano para character
      mutate(regiao_cod = as.character(regiao_cod)) %>%  # Converte 'regiao_cod' para character
      select(date, value, regiao_cod), 
    by = c("date", "regiao_cod")     # Faz o join pelas colunas 'date' e 'regiao_cod'
  )


        # teste agrupando a renda percapita de cada estado com o gh de cada estado, por ano
rpc_estados <- rpc_estados %>% 
  mutate(date = as.character(year(date))) %>%
  rename(uf_cod = regiao_cod)


gh_rpc_estados <- df_ghcomb_estados %>%
  mutate(uf_cod = as.character(uf_cod)) %>% # Converte para `character`
  inner_join(
    rpc_estados %>%
      mutate(
        date = as.character(date),
        uf_cod = as.character(uf_cod) # Converte para `character`
      ) %>%
      select(date, value, uf_cod),
    by = c("date", "uf_cod")
  )


# Um df por região, na analise agrupada dos estados para formar uma regiao

gh_rpcmed_norte <- gh_rpcmed %>% filter(regiao_cod == "1")
gh_rpcmed_nordeste <- gh_rpcmed %>% filter(regiao_cod == "2")
gh_rpcmed_sudeste <- gh_rpcmed %>% filter(regiao_cod == "3")
gh_rpcmed_sul <- gh_rpcmed %>% filter(regiao_cod == "4")
gh_rpcmed_centro_oeste <- gh_rpcmed %>% filter(regiao_cod == "5")


    # teste para os estados
    gh_rpc_e1 <- gh_rpc_estados %>% filter(str_starts(uf_cod, "1"))
    gh_rpc_e2 <- gh_rpc_estados %>% filter(str_starts(uf_cod, "2"))
    gh_rpc_e3 <- gh_rpc_estados %>% filter(str_starts(uf_cod, "3"))
    gh_rpc_e4 <- gh_rpc_estados %>% filter(str_starts(uf_cod, "4"))
    gh_rpc_e5 <- gh_rpc_estados %>% filter(str_starts(uf_cod, "5"))

# rodando o modelo lowess por região

modelo_loess <- loess(gh ~ rpcmed, data = gh_rpcmed) # Padrão do pacote estátistico, largura da banda utilizada J =0,75, para alterar modificar ,span = 0.__)

modelo_loess_norte <- loess(gh ~ rpcmed, data = gh_rpcmed_norte)
modelo_loess_nordeste <- loess(gh ~ rpcmed, data = gh_rpcmed_nordeste)
modelo_loess_sudeste <- loess(gh ~ rpcmed, data = gh_rpcmed_sudeste)
modelo_loess_sul <- loess(gh ~ rpcmed, data = gh_rpcmed_sul)
modelo_loess_centro_oeste <- loess(gh ~ rpcmed, data = gh_rpcmed_centro_oeste)

modelo_loess_brasil <- loess(gh ~ value, data = gh_rpcbrasil)

summary(modelo_loess_sudeste)

      # teste para os estados
      modelo_loess1 <- loess(gh ~ value, data = gh_rpc_e1)
      modelo_loess2 <- loess(gh ~ value, data = gh_rpc_e2)
      modelo_loess3 <- loess(gh ~ value, data = gh_rpc_e3)
      modelo_loess4 <- loess(gh ~ value, data = gh_rpc_e4)
      modelo_loess5 <- loess(gh ~ value, data = gh_rpc_e5)

# Adicionar nova coluna no df que prevê os valores suavizados usando o modelo 

gh_rpcmed$loess_result <- predict(modelo_loess)

gh_rpcmed_norte$loess_result <- predict(modelo_loess_norte)
gh_rpcmed_nordeste$loess_result <- predict(modelo_loess_nordeste)
gh_rpcmed_sudeste$loess_result <- predict(modelo_loess_sudeste)
gh_rpcmed_sul$loess_result <- predict(modelo_loess_sul)
gh_rpcmed_centro_oeste$loess_result <- predict(modelo_loess_centro_oeste)

gh_rpcbrasil$loess_result <- predict(modelo_loess_brasil)


      # teste para os estados
      gh_rpc_e1$loess_result <- predict(modelo_loess1)
      gh_rpc_e2$loess_result <- predict(modelo_loess2)
      gh_rpc_e3$loess_result <- predict(modelo_loess3)
      gh_rpc_e4$loess_result <- predict(modelo_loess4)
      gh_rpc_e5$loess_result <- predict(modelo_loess5)

# Gráficos de Visualizaçãodo modelo

      # modelo por regiao
plot_lowess <- function(df, regiao) {
  df_filtrado <- df %>% 
    filter(regiao_cod == !!regiao) %>%
    mutate(rpcmed = as.numeric(as.character(rpcmed)))  # Assegura que `rpcmed` está no formato numérico
  
  ggplot(df_filtrado, aes(x = rpcmed, y = gh)) +
    geom_point(color = "gray") +  # Pontos originais
    geom_line(aes(y = loess_result), color = "blue", size = 0.5) +  # Curva ajustada
    labs(x = "Renda Per Capita em R$ de 2014",
         y = "Gini-Hirschmann (VTI)") +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 5),  # Define aproximadamente 5 intervalos com valores cheios
      labels = scales::label_number(accuracy = 1)  # Formata os rótulos como números inteiros
    ) +  
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_text(size = 10, margin = margin(t = 10)),  
      axis.title.y = element_text(size = 10, margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      axis.ticks.length = unit(0.3, "cm"),
      axis.ticks = element_line(size = 0.5),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
    )
}

plot_lowess(gh_rpcmed,5) # registro do erro

plot_lowess(gh_rpcmed_norte, 1)
plot_lowess(gh_rpcmed_nordeste, 2)
plot_lowess(gh_rpcmed_sudeste, 3)
plot_lowess(gh_rpcmed_sul, 4)
plot_lowess(gh_rpcmed_centro_oeste, 5)
      

      # modelo brasil

plot_lowess_brasil <- function(df, regiao) {
  df_filtrado <- df %>% 
    filter(regiao_cod == !!regiao) %>%
    mutate(value = as.numeric(as.character(value)))  # Assegura que `rpcmed` está no formato numérico
  
  ggplot(df_filtrado, aes(x = value, y = gh)) +
    geom_point(color = "gray") +  # Pontos originais
    geom_line(aes(y = loess_result), color = "blue", linewidth = 0.5) +  # Curva ajustada
    labs(x = "Renda Per Capita em R$ de 2014",
         y = "Gini-Hirschmann (VTI)") +
    scale_x_continuous(
      breaks = seq(500, 800, 100),
      minor_breaks = seq(500, 800, 50)  # Ticks intermediários
  ) +
    scale_y_continuous(
      breaks = seq(min(df_filtrado$gh), max(df_filtrado$gh), 1),
      labels = scales::label_number(accuracy = 1)
    ) +   
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_text(size = 10, margin = margin(t = 10)),  
      axis.title.y = element_text(size = 10, margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      axis.ticks.length = unit(0.3, "cm"),
      axis.ticks = element_line(linewidth = 0.5),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
    )
}


plot_lowess_brasil(gh_rpcbrasil, 0)

      # modelo estados
plot_lowess_estados <- function(df) {
  df_filtrado <- df %>% 
    mutate(value = as.numeric(as.character(value)))  # Assegura que `rpcmed` está no formato numérico
  
  ggplot(df_filtrado, aes(x = value, y = gh)) +
    geom_point(color = "gray") +  # Pontos originais
    geom_line(aes(y = loess_result), color = "blue", size = 0.5) +  # Curva ajustada
    labs(x = "Renda Per Capita",
         y = "Gini-Hirschmann (VTI)") +
    scale_x_continuous(breaks = seq(min(df_filtrado$value), max(df_filtrado$value), by = (max(df_filtrado$value) - min(df_filtrado$value)) / 3)) +
    scale_y_continuous(
      breaks = seq(min(df_filtrado$gh), max(df_filtrado$gh), by = (max(df_filtrado$gh) - min(df_filtrado$gh)) / 3),
      labels = scales::label_number(accuracy = 1)
    ) +   
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.title.x = element_text(size = 10, margin = margin(t = 10)),  
      axis.title.y = element_text(size = 10, margin = margin(r = 10)),
      axis.text = element_text(size = 10),
      axis.ticks.length = unit(0.3, "cm"),
      axis.ticks = element_line(size = 0.5),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
    )
}

plot_lowess_estados(gh_rpc_e5)


# dfs com os valores mínimos, renda percapita e ano em cada regiao --------------------------------
