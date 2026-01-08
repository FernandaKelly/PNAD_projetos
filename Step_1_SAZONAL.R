library(seasonal)
library(zoo)
library(tidyverse)
library(lubridate)
library(readxl)

######################################################################
# EXEMPLO IBGE
######################################################################

pib<-readxl::read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/Dados_completos/PIB/PIB_Sem_Ajuste.xlsx",sheet =1)

pib<-stats::ts(pib[,-1],
               start = c(2012,1),
               freq = 4)

lista<-list()

for(i in 1:ncol(pib)){
  lista[[i]]<-pib[,i]
}


agreg_SA <- lapply(lista, function(x) try(seasonal::seas(ts(x,start=start(pib),freq=4),
                                             transform.function = "auto",
                                             regression.aictest = c("td", "easter"),
                                             pickmdl.method="best",
                                             pickmdl.identify="all",
                                             outlier.types="all",
                                             x11="", 
                                             forecast.maxlead=6,
                                             forecast.maxback=0,
                                             estimate.maxiter = 30000)))

names(agreg_SA)<-colnames(pib)

pib_SA<-lapply(agreg_SA,final)

pib_SA<-do.call(cbind,pib_SA)

write.table(pib_SA,"PIB_Com_Ajuste_Sazonal.csv",sep=";",dec=",",row.names=F)
######################################################################
# REAJUSTE DOS INDICADORES
######################################################################

table_PS_RS <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PS_8.xlsx", 
                         sheet = "Indicador TRI RS") %>% 
  dplyr::filter(complete.cases(.)) %>% 
  dplyr::select(atividade, Ano, Trimestre, indicadorVA_N,  indicadorVA_qtd_HHabituais, indicadorVA_qtd_HEfetivas)

table_PS_BR <- read_excel("C:/Users/fernanda-romeiro/OneDrive - Governo do Estado do Rio Grande do Sul/Projetos/PNAD/PNAD_projetos/Dados/table_PS_8.xlsx", 
                          sheet = "Indicador TRI BR") %>% 
  dplyr::filter(complete.cases(.))


teste <- table_PS_RS %>% 
  tidyr::pivot_wider(names_from = atividade, values_from = c("indicadorVA_N",
                                                             "indicadorVA_qtd_HHabituais",
                                                             "indicadorVA_qtd_HEfetivas")) %>% 
  dplyr::mutate(ano =  base::paste0(Ano, ".", Trimestre)) %>% 
  dplyr::select(-c("Ano", "Trimestre")) %>% 
  dplyr::relocate(ano)


table_PS_RS <- stats::ts(teste[,-1],
               start = c(2012,1),
               freq = 4)

lista<-list()

for(i in 1:ncol(table_PS_RS)){
  lista[[i]]<-table_PS_RS[,i]
}


agreg_SA <- lapply(lista, function(x) try(seasonal::seas(ts(x,start=start(table_PS_RS),freq=4),
                                                         transform.function = "auto",
                                                         regression.aictest = c("td", "easter"),
                                                         pickmdl.method="best",
                                                         pickmdl.identify="all",
                                                         outlier.types="all",
                                                         x11="", 
                                                         forecast.maxlead=6,
                                                         forecast.maxback=0,
                                                         estimate.maxiter = 30000)))

names(agreg_SA)<-colnames(table_PS_RS)
pib_SA<-lapply(agreg_SA,final)
pib_SA<-do.call(cbind,pib_SA)

pib_SA_teste <- base::as.data.frame(pib_SA)



df_long <- pib_SA_teste %>%
  tidyr::pivot_longer(
    cols = matches("^indicadorVA_"),
    names_to = c(".value", "atividade"),
    names_sep = "_(?=[^_]+$)"
  )










