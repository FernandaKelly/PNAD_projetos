# ESTUDOS SOBRE A PNADC

Este repositório reúne estudos e análises com microdados da PNAD Contínua (PNADC), pesquisa domiciliar amostral realizada pelo IBGE, que investiga características do mercado de trabalho, renda, educação e condições de vida da população brasileira. A PNADC possui desenho amostral complexo, com estratificação, conglomerados e pesos amostrais, exigindo métodos estatísticos específicos para a produção de estimativas representativas da população.

No projeto, realizo o levantamento, organização e análise dos microdados da PNAD Contínua, com foco no cálculo de estimativas populacionais, respeitando o plano amostral da pesquisa. As análises são conduzidas a partir da criação de objetos de desenho amostral para cada edição da PNADC, evitando o empilhamento direto dos microdados e adotando abordagens estatisticamente adequadas para comparações e combinações de estimativas ao longo do tempo.

*Metodologia e ferramentas*

As estimativas são produzidas no R, utilizando pacotes especializados em análise de dados amostrais complexos. O pacote PNADcIBGE é empregado para leitura dos microdados e definição do desenho amostral da PNAD Contínua. Em conjunto, utilizo o pacote survey (e sua interface tidy, srvyr) para o cálculo de médias, proporções, totais e medidas de variabilidade, incorporando corretamente pesos, estratos e unidades primárias de amostragem.

Para o tratamento, manipulação e automação dos dados, são utilizados pacotes do tidyverse, como dplyr, purrr e readxl, além do pacote foreign para leitura de formatos externos. A apresentação dos resultados e tabelas interativas é realizada com o reactable, favorecendo a transparência e a reprodutibilidade das análises.

- library(PNADcIBGE)
- library(survey)
- library(srvyr)
- library(foreign)
- library(reactable)
- library(purrr)
- library(readxl)
- library(dplyr)
