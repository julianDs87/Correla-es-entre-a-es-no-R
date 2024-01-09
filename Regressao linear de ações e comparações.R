###modelos Arima

#library


library(forecast)
library(sidrar)
library(tidyverse)
library(tidyquant)
library(scales)
library(ggthemes)
library(tstools)
library(zoo)

1. ###correlações
2. ####coletar dados da bolsa e visualizar a correlacao e beta de empresas
3. # utilizar regressao linear para encontrar os betas dos ativos 

##carregar dados do Ibov e Petr4

tickers <- c('^BVSP', 'PETR4.SA')
BVSP <- tq_get('^BVSP', 
               from= '2015-01-01',
               to= '2024-01-01',
               get= 'stock.prices')

PETR4 <- tq_get('PETR4.SA',
                from= '2015-01-01',
                to= '2024-01-01',
                get= 'stock.prices')

# gerar grafico ggplot

BVSP %>%
  ggplot(aes(y= PETR4$close, x= BVSP$close))+
  geom_point( col = 'darkblue')+
  geom_smooth(method = 'lm', se = F, col= 'red')+
  labs( title = 'Correlação entre Ibovespa e Petrobrás'
  x= 'Ibovespa', y='Petrobrás')+
  theme_calc()
  
#encontrando o beta atraves da regressao que representa o grau de sensibilidade; em termos de elasticidade...

lm(log(PETR4$close)~log(BVSP$close))
###o resultado demonstra uma elasticidade, pois é >1

###vale a pena ter iausa e itub4?????

ITUB3 <- tq_get('ITUB4.SA', 
                from= '2015-01-01',
                to= '2024-01-01',
                get= 'stock.prices')

ITSA4 <- tq_get('ITSA4.SA',
                from='2015-01-01',
                to= '2024-01-01',
                get= 'stock.price')
####correlacao

ITUB3 %>%
  ggplot(aes(x= ITUB3$close, y= ITSA4$close))+
  geom_point(col= 'darkblue')+
  geom_smooth(method = 'lm', se =F, col= 'red')+
  labs(title = 'Vale a pena ter ITUB4 e ITSA4 na mesma carteira?')


####encontrando o beta atraves da regressao linear

lm(log(ITSA4$close)~log(ITUB3$close))


####resultado: ineslático <1, ou seja não é desejável ter na mesma carteira 


  

