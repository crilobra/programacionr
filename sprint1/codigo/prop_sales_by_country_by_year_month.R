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

# Muestra la ultimas 10 filas
sales_all %>% tail(10)


# Une las tablas
sales_all  %>%
  group_by(country) %>%
  summarise(
    # calculo de ventas en millones
    sales = sum(sales) / 1000000,
    # calculo de customers en millones
    customers = sum(customers) / 1000000
  )

# convierte a fecha la columna
sales$date <- ymd(sales$date)

# extrae el año de la fecha de venta
sales_all <- sales_all %>%
  mutate(year = as.numeric(format(as.Date(sales_all$date), "%Y")))
# Extrae al mes de la fecha de venta
sales_all <- sales_all %>%
  mutate(month = as.numeric(format(as.Date(sales_all$date), "%m")))
# Extrae al dia de la fecha de venta
sales_all <- sales_all %>%
  mutate(day = as.numeric(format(as.Date(sales_all$date), "%d")))
# Cre una fecha auxiliar para el analisis (date_custom)
sales_all <- sales_all %>% mutate(day = 1)
sales_all <- sales_all %>%
  mutate("date_custom" = ISOdate(year = year, month = month, day = day))
sales_all <- sales_all %>%
  mutate(yearmonth = as.numeric(format(as.Date(sales_all$date), "%Y%m")))

# Ventas por pais
prop_sales_country <- sales_all  %>%
  group_by(yearmonth, country) %>%
  summarise(
    # calculo de ventas en millones
    sales = sum(sales)
  ) %>%
  ungroup() %>%
  group_by(yearmonth) %>%
  mutate(percentage = sales / sum(sales) * 100)

ggplot(data = prop_sales_country) +
  geom_point(mapping = aes(x = country, y = percentage))

write.csv(
  prop_sales_country,
  "prop_sales_by_country_by_year_month.csv",
  row.names = TRUE
)
