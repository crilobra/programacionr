# Primero indicamos la ruta a la carpeta de de tu computadora 
# donde se ubican los datos del E-commerce
# Ejemplo: "C:\Usuarios\[tu nombre]\Descargas"

library(tidyverse)
library(readxl)
library(lubridate)
library(corrplot)

DATA_PATH="/Users/Christian/Documents/unifranz/nao/R/s1/"

# Establece el directorio de trabajo
setwd(DATA_PATH)
SALES_DATA = './data/sales.csv'

# Datos adicionales de tiendas
STORES_INFO_DATA = './data/stores_info.xlsx'
sales <- read.csv2(
  SALES_DATA,
  sep=";",
  header=T
)

sales %>% tail()
# Completa el codigo para leer la
stores <- read_excel(
  STORES_INFO_DATA,
  sheet = "data" # nombre de la hoja del archivo Excel
)
view(sales)

# Une las tablas
sales_all <- sales %>% left_join(stores, by=c("store"))

# Muestra la ultimas 10 filas
sales_all %>% tail(10)


# Une las tablas
sales_all  %>% 
  group_by(country) %>%
  summarise(
    # calculo de ventas en millones
    sales = sum(sales)/1000000,
    # calculo de customers en millones
    customers = sum(customers)/1000000
  )

# convierte a fecha la columna
sales$date <- ymd(sales$date)

# extrae el año de la fecha de venta
sales_all <- sales_all %>% mutate(year = as.numeric(format(as.Date(sales_all$date), "%Y")))
# Extrae al mes de la fecha de venta 
sales_all <- sales_all %>% mutate(month = as.numeric(format(as.Date(sales_all$date), "%m")))
# Extrae al dia de la fecha de venta 
sales_all <- sales_all %>% mutate(day = as.numeric(format(as.Date(sales_all$date), "%d")))
# Cre una fecha auxiliar para el analisis (date_custom)
sales_all <- sales_all %>% mutate(day = 1)
sales_all <- sales_all %>% mutate('date_custom' = ISOdate(year = year, month = month, day = day))
# Extrae al dia de la fecha de venta 
sales_all <- sales_all %>% mutate(yearmonth = as.numeric(format(as.Date(sales_all$date), "%Y%m")))


prop_changes_sales_by_country_by_year_month <- sales_all  %>% 
  group_by(country, yearmonth) %>%
  summarise(
    # calculo de ventas en customers
    sales = sum(sales)
  ) %>%
  ungroup()%>%
  group_by(yearmonth) %>%
  mutate(sales = sales / sum(sales) * 100)%>%
  # ungroup para obtener un dataframe sin grupos
ungroup() %>% 
  # Crea la estructura de la pivot alargando la tabla
  pivot_wider(
    names_from = yearmonth,
    values_from=sales
  )


write.csv(prop_changes_sales_by_country_by_year_month,'prop_changes_sales_by_country_by_year_month.csv',row.names =TRUE)
