
# pacotes -------------------------------------------------------------------------------------------------------
install.packages("ipeadatar")
install.packages("sidrar")
install.packages("installr")
install.packages("GetBCBData")
install.packages("scales")
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(lubridate)
library(sidrar)
library(GetBCBData)
library(scales)

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


## Renda percapita (Atlas DH - Pnad Contínua/A) - 2012 até 2021 - Em R$, a preços do ano 2010 ( )
renda2_ipeadata <- ipeadatar::ipeadata("ADH12_RDPC")%>%
                   filter(date > as.Date("2014-01-01"))


# Corrigir/deflacionar real para o mesmo período base período base ----------------------------------------------------------------
## Importar IPCA e IPC
## IPCA inicia a partir de dezembro de 1979. 
ipca = get_sidra(api='/t/1737/n1/all/v/2266/p/all/d/v2266%2013')

ipca %<>% mutate(date = parse_date_time(`Mês (Código)`, 'ym'))

ipca_indice <- ipca %>%
  select (date,
          "indice" = Valor)


## Ajustar os preços da primeira série (renda_ipeadata, 1976 até 2014 - Em R$ Outubro 2014)
ipca_out2014 <- ipca_indice %>%
                filter(date == as.Date("2014-10-01"))%>%
                pull(indice)


ipca_jan2021 <- ipca_indice %>%
                filter(date == as.Date("2021-01-01"))%>%
                pull(indice)

renda00_original <- left_join(renda_ipeadata, ipca_indice, by = "date") %>%
                    filter(uname == "States") %>%
                    mutate(valor_nominal = if_else(!is.na(indice), (value/ipca_out2014)*indice, NA_real_))
  
rm(renda00_original)
  
  
renda1_ajustada <- left_join(renda_ipeadata, ipca_indice, by = "date") %>%
                   filter(uname == "States") %>%
                   mutate(fator_original = if_else(!is.na(indice), ipca_jan2021/ indice, NA_real_)) %>%
                   mutate(fator = ipca_jan2021/ipca_out2014) %>%
                   mutate(renda_real = if_else(!is.na(fator), (fator*value), NA_real_)) %>%               
                   select(date, value, indice, fator_original, fator, renda_real, tcode)
                   
                  

## Ajustar os preços da segunda série ( renda2_ipeadata, 2012 até 2021 - Em R$, a preços do ano 2010
ipca_jan2010 <- ipca_indice %>%
                filter(date == as.Date("2010-01-01"))%>%
                pull(indice)

renda2_ajustada <- left_join(renda2_ipeadata, ipca_indice, by = "date") %>%
                   filter(uname == "States") %>%
                   mutate(fator_original = if_else(!is.na(indice), ipca_jan2021/indice, NA_real_))%>%
                   mutate(fator = ipca_jan2021/ipca_out2014) %>%
                   mutate(renda_real = if_else(!is.na(fator), (fator*value), NA_real_)) %>%                 
                   select(date, value, indice, fator_original, fator, renda_real, tcode)


## Juntar as bases de renda percapita 1976 até 2021, apenas para estados
renda_percapita <- bind_rows(renda1_ajustada,renda2_ajustada) %>%
                   arrange(date, tcode)%>%
                   select(date, value, "renda_real", tcode)
                   ## obs: Mato grosso do sul a partir de 1981; Tocantis a partir de 1992
                   ## Renda percapita NÃO disponibilizada pela fonte nos anos: 1980, 1991, 1994, 2000, 2010
  

# Extrair a renda percapita média anual das regiões brasileiras (norte, sul, nordeste, sudeste, centro-oeste)
## datas originais xxxx-01-01

rpcmed_regiao <- renda_percapita %>%
                 mutate(regiao = substr(tcode, 1, 1)) %>%
                 mutate(date = lubridate::year(date)) %>%
                 group_by(date, regiao) %>%
                 arrange(regiao, date) %>%
                 select(date, value, "renda_real", tcode, regiao)%>%
                 summarise(rpcmed = if (any(is.na(`renda_real`))) NA else mean(`renda_real`, na.rm = TRUE), .groups = "drop")

## conferindo as maiores renda percaptas para cada ano de acordo com os dados do IPEA 1976 a 2014
rmax_ano <- renda_ipeadata %>%
                     group_by(date) %>%
                     filter(uname == "Regions") %>%
                     slice(which.max(value))

print(rmax_ano)

# Dados SIDRA_ Valor de Transformação Industrial -----------------------------------------------------------------------
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
##     ignorando as distintas classificações de atividades econômicas empregada em cada periódo.
vti_tab2221 <- tab2221 %>%
               filter(`Variável (Código)` == 811) %>%
               select("date" = "Ano", "cnae_cod" = "Classes e gêneros de indústria (Código)", "cnae" = "Classes e gêneros de indústria", 
                      "uf_cod" = "Brasil e Unidade da Federação (Código)", "uf" = "Brasil e Unidade da Federação", "vti" = Valor) %>%
               filter(uf_cod > 1)
                                    
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
               filter(uf_cod > 1)
## obs: Os dados de VTI não estão disponíveis para todos os anos da série, já que a PIA deixou de ser realizada algumas vezes 
### no período em questão. Faltam dados para os anos de 1970, 1971, 1972, 1975, 1980, 1985, 1986, 1987 e 1991.


# Problema 1: Compatibilizar as unidades de medida para todos os anos --------------------------------------------
 
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
## Agrupar os dados por grandes regiões:

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
vti_tab1988 <- colum_regiao(vti_tab1988)
vti_tab1987 <- colum_regiao(vti_tab1987)
vti_tab1849 <- colum_regiao(vti_tab1849)

## vti por região

vtiregiao_tab2221 <- vti_tab2221 %>% 
  group_by(date, cnae_cod,cnae, regiao_cod, regiao) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = 'drop') %>%
  select(date, cnae_cod, cnae, vti, regiao_cod, regiao)

vtiregiao_tabs8788 <- vti_tabs8788 %>%
  filter(!cnae %in% c("D Indústrias de transformação", "C Indústrias extrativas")) %>% # excluir as linhas de seção CNAE
  group_by(date, cnae_cod,cnae, regiao_cod, regiao) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = 'drop') %>%
  select(date, cnae_cod, cnae, vti, regiao_cod, regiao)

vtiregiao_tab1849 <- vti_tab1849 %>%
  filter(!cnae %in% c("B Indústrias extrativas", "C Indústrias de transformação")) %>%
  group_by(date, cnae_cod,cnae, regiao_cod, regiao) %>%
  summarise(vti = sum(vti, na.rm = TRUE), .groups = 'drop') %>%
  select(date, cnae_cod, cnae, vti, regiao_cod, regiao)

# Compatibilizar as classes e genêros industriais e as CNAEs 1.0 e 2.0

## dfs de converção Gênero Industrial para Agregação/ CNAE 1.0 para Agregação/ CNAE 2.0 para Agregação ---------
agregacao_tab2221 <- data.frame(
  cnae = c("Bebidas", "Borracha", "Couros e peles e produtos similares", 
            "Diversas", "Editorial e gráfica", "Fumo", "Madeira", 
            "Material de transporte", "Material elétrico e material de comunicações", 
            "Mecânica", "Metalúrgica", "Minerais não-metálicos", 
            "Mobiliário", "Papel e papelão", "Produtos alimentares", 
            "Produtos de matérias plásticas", "Produtos de perfumaria, sabões e velas", 
            "Produtos farmacêuticos e medicinais", "Produtos minerais", "Química", 
            "Têxtil", "Vestuário, calçados e artefatos de tecidos"),
  agregacao = c("Alimentos e bebidas", "Borracha e plásticos", "Vestuários couros e calçados", 
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
           "29", "30", "31", "32", "33", "34", "35", "36.1", "36.9", "37"),
  agregacao = c("Extrativa", "Extrativa", "Extrativa", "Extrativa", "Alimentos e bebidas", 
                "Fumo", "Têxtil", "Vestuário couros e calçados", "Vestuário couros e calçados", 
                "Madeira", "Papel e celulose", "Editorial e gráfica", "Química", 
                "Química", "Borracha e plásticos", "Minerais não-metálicos", 
                "Metalúrgica", "Metalúrgica", "Mecânica", "Material elétrico e comunicações", 
                "Material elétrico e comunicações", "Material elétrico e comunicações", 
                "Material elétrico e comunicações", "Material de transporte", "Material de transporte",
                "Mobiliário", "Diversas", "Diversas")
)


## Convertendo a coluna cnae original para a agregação

agregado_vtiregiao_tab2221 <- vtiregiao_tab2221 %>%
  left_join(agregacao_tab2221, by = c("cnae")) %>%
  mutate(cnae = agregacao) %>% # não inclui "Total", "Indústrias extrativas e de produtos minerais", "Indústria de transformação"
  select(-agregacao)  # Remove a coluna de agregação usada para o mapeamento

agregado_vtiregiao_tabs8788 <- vtiregiao_tabs8788 %>%
  mutate(cnae2dig = substr(cnae, 1, 2)) %>%  # Extrai os 2 primeiros dígitos da coluna cnae
  left_join(agregacao_tabs8788, by = c("cnae2dig" = "cnae")) %>%
  mutate(cnae = agregacao) %>% # Substitui a descrição da cnae pela agregação 
                               # não inclui "Total" e "outros", pendente: verificar cód 36
  select(date, cnae_cod, cnae2dig, cnae,vti, regiao_cod, regiao,-agregacao)  # Remove a agregacao, pois não é mais necessárias



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

calc_gh(vtiregiao_tab2221,1966,1)


# Obter o GH para todos os anos e criar um data frame

df_gh21 <- vtiregiao_tab2221 %>%
  distinct(date, regiao_cod, regiao) %>%
  rowwise() %>%
  mutate(gh = calc_gh(vtiregiao_tab2221, date, regiao_cod)) %>%
  ungroup()


df_gh8788 <- vtiregiao_tabs8788 %>%
  distinct(date, regiao_cod, regiao) %>%  
  rowwise() %>%
  mutate(gh = calc_gh(vtiregiao_tabs8788, date, regiao_cod)) %>%
  ungroup()


df_gh49 <- vtiregiao_tab1849 %>%
  distinct(date, regiao_cod, regiao) %>%
  rowwise() %>%
  mutate(gh = calc_gh(vtiregiao_tab1849, date, regiao_cod)) %>%
  ungroup()

# Gráfico:

regions_list <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")


plot_function <- function(df, regiao) {
  df_filtrado <- df %>% 
    filter(regiao_cod == !!regiao) %>%
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
}

plot_function(df_gh21,3)
plot_function(df_gh8788,3)
plot_function(df_gh49,3)




# Combinar os data frames
df_comb <- bind_rows(
  df_gh21 %>% mutate(fonte = "tab2221"),
  df_gh8788 %>% mutate(fonte = "tabs8788"),
  df_gh49 %>% mutate(fonte = "tab1849")
)

# Regressões locais não-paramétricas (LOWESS) ----------------------------------------------------------- 
## Padrão do pacote estátistico, largura da banda utilizada J =0,75

rename(rpcmed_regiao, ano = date)

gh_rpcmed <- df_comb %>%
  mutate(date = as.character(date)) %>%
  filter(!is.na(gh_rpcmed$rpcmed)) %>% # Renda percapita NÃO disponibilizada pela fonte nos anos: 1980, 1991, 1994, 2000, 2010
  left_join(
    rpcmed_regiao %>% mutate(date = as.character(date)), 
    by = c("date", "regiao_cod" = "regiao")
  )


modelo_loess <- loess(gh ~ rpcmed, data = gh_rpcmed)

# Prever os valores suavizados usando o modelo ajustado

gh_rpcmed$loess_result <- predict(modelo_loess)

# Visualização 

plot_lowess <- function(df, regiao) {
  df_filtrado <- df %>% 
    filter(regiao_cod == !!regiao) %>%
    mutate(rpcmed = as.numeric(as.character(rpcmed)))  # Assegura que `rpcmed` está no formato numérico
  
  ggplot(df_filtrado, aes(x = rpcmed, y = gh)) +
    geom_point(color = "gray") +  # Pontos originais
    geom_line(aes(y = loess_result), color = "blue", size = 0.5) +  # Curva ajustada
    labs(x = "Renda Per Capita",
         y = "Gini-Hirschmann (VTI)") +
    scale_x_continuous(breaks = seq(min(df_filtrado$rpcmed), max(df_filtrado$rpcmed), by = (max(df_filtrado$rpcmed) - min(df_filtrado$rpcmed)) / 3)) +
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

plot_lowess(gh_rpcmed,4)
