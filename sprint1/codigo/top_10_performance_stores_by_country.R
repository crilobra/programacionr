# Primero indicamos la ruta a la carpeta de de tu computadora
# donde se ubican los datos del E-commerce
# Ejemplo: "C:\Usuarios\[tu nombre]\Descargas"

library(tidyverse)
library(readxl)
library(lubridate)
library(corrplot)

datapath <- "/Users/Christian/Documents/unifranz/nao/R/s1/"

# Establece el directorio de trabajo
setwd(datapath)
salesdata <- "./data/sales.csv"

# Datos adicionales de tiendas
storesinfodata <- "./data/stores_info.xlsx"
sales <- read.csv2(
  salesdata,
  sep = ";",
  header = TRUE
)

sales %>% tail()
# Completa el codigo para leer la
stores <- read_excel(
  storesinfodata,
  sheet = "data" # nombre de la hoja del archivo Excel
)


# Une las tablas
sales_all <- sales %>% left_join(stores, by = c("store"))

sales_all <- sales_all %>% mutate("date_custom" = as.Date(sales_all$date))

#restando 90 dias
max_date <- max(sales_all$date_custom)
m <- as.POSIXlt(max_date)
m$mon <- m$mon - 3
max_date <- as.Date(m)

sales_all_max <- sales_all %>% filter(date_custom >= max_date)

# Une las tablas
country_store <- sales_all  %>%
  group_by(country, store) %>%
  summarise(
    # calculo de ventas en millones
    sales = sum(sales),
    # calculo de customers en millones
    customers = sum(customers)
  )


ggplot(data = country_store) +
  geom_point(mapping = aes(x = customers, y = country))
