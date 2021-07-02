# Script for crypto analysis

library(data.table) #open dataframes
library(dplyr) # manipulate dataframes
library(ggplot2)

data_bitcoin <- fread("/Users/home/Dropbox/Databases/Historical_crypto/coin_Bitcoin.csv")
data_etherum <- fread("/Users/home/Dropbox/Databases/Historical_crypto/coin_Ethereum.csv")
data_binance <- fread("/Users/home/Dropbox/Databases/Historical_crypto/coin_BinanceCoin.csv")
data_litecoin <- fread("/Users/home/Dropbox/Databases/Historical_crypto/coin_Litecoin.csv")

# Crear una base unica

data_cryptos <- bind_rows(data_bitcoin,data_etherum,data_binance,data_litecoin)

Bit_ethe <- c("Bitcoin", "Ethereum")

data_cryptos %>% 
  filter(Name %in% Bit_ethe) %>% 
ggplot(aes(x= Date, y = Open, group = Name)) +
  geom_line(aes(color=Name))+
  geom_point(aes(color=Name))+
  labs(x="", y = "Opening")+
  theme(panel.background = element_blank(), 
        axis.text.x =element_blank())

# Crear base para comparar monedas

# Bitcoin
data_bitcoin_small <- data_bitcoin %>% 
  select(Date,
         Symbol_bitcoin = Symbol,
         High_bitcoin = High,
         Low_bitcoin = Low,
         Open_bitcoin = Open,
         Close_bitcoin = Close,
         Volume_bitcoin = Volume,
         Marketcap_bitcoin = Marketcap)

# Etherum 

data_etherum_small <- data_etherum %>% 
  select(Date,
         Symbol_etherum = Symbol,
         High_etherum = High,
         Low_etherum = Low,
         Open_etherum = Open,
         Close_etherum = Close,
         Volume_etherum = Volume,
         Marketcap_etherum = Marketcap)

# Binance

data_litecoin_small <- data_litecoin %>% 
  select(Date,
         Symbol_litecoin = Symbol,
         High_litecoin = High,
         Low_litecoin = Low,
         Open_litecoin = Open,
         Close_litecoin = Close,
         Volume_litecoin = Volume,
         Marketcap_litecoin = Marketcap)

# Litecoin

data_binance_small <- data_binance %>% 
  select(Date,
         Symbol_binance = Symbol,
         High_binance = High,
         Low_binance = Low,
         Open_binance = Open,
         Close_binance = Close,
         Volume_binance = Volume,
         Marketcap_binance = Marketcap)

# Juntar todo

data_cryptos_wide <- left_join(data_bitcoin_small, data_etherum_small,
                               by = "Date")

data_cryptos_wide <- left_join(data_cryptos_wide, data_binance_small,
                               by = "Date")

data_cryptos_wide <- left_join(data_cryptos_wide, data_litecoin_small,
                               by = "Date")

# Separar variables para crear una matriz de correlaciones
# Todos los picos de los cryptos
data_cryptos_cor_highp <- data_cryptos_wide %>% 
  select(High_bitcoin,
         High_etherum,
         High_litecoin,
         High_binance)

# Resistance
cor(data_cryptos_cor_highp, use = "pairwise.complete.obs")

# Separar variables para crear una matriz de lows
data_cryptos_cor_lowp <- data_cryptos_wide %>% 
  select(Low_bitcoin,
         Low_etherum,
         Low_litecoin,
         Low_binance)

#Support
cor(data_cryptos_cor_lowp, use = "pairwise.complete.obs")

# Separar variables para crear una matriz de opens
data_cryptos_cor_open <- data_cryptos_wide %>% 
  select(Open_bitcoin,
         Open_etherum,
         Open_litecoin,
         Open_binance)

cor(data_cryptos_cor_open, use = "pairwise.complete.obs")

# Separar variables para crear una matriz de close
data_cryptos_cor_close <- data_cryptos_wide %>% 
  select(Close_bitcoin,
         Close_etherum,
         Close_litecoin,
         Close_binance)

cor(data_cryptos_cor_close, use = "pairwise.complete.obs")




