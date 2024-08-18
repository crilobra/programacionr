# Instala paquetes relevantes si no los tenemos
listofpackages <- c("tidyverse", "readxl", "lubridate", "GGally")

new <- listofpackages[!(listofpackages %in% installed.packages()[, "Package"])]

if (length(new)) install.packages(new)

# Carga paquetes de trabajo
library(tidyverse)
library(readxl)
library(lubridate)
library(GGally)

# Establece el directorio de trabajo
setwd("/Users/Christian/Documents/unifranz/nao/R/s1/")

# Datos de ventas
salesdata <- "./data/sales.csv"

# Datos adicionales de tiendas
storesinfodata <- "./data/stores_info.xlsx"

sales <- read.csv2(
  salesdata,
  sep = ";",
  header = TRUE
)

stores <- read_excel(
  storesinfodata,
  sheet = "data" # nombre de la hoja del archivo Excel
)

# Unimos ambas tablas
sales_all <- sales %>% left_join(stores, by = c("store"))

# convierte a fecha la columna
sales$date <- ymd(sales$date)

# extrae el año de la fecha de venta
sales_all <- sales_all %>%
  mutate(year = as.character(format(as.Date(sales_all$date), "%Y")))
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

# extrae el año de la fecha de venta
sales_all <- sales_all %>%
  mutate(yearmonth = as.character(format(as.Date(sales_all$date), "%Y%m")))


sales_all$date <- ymd(sales$date)

#promedio de ventas por clientes
avg_sales <- sales_all  %>%
  group_by(yearmonth, store_type, country) %>%
  summarise(
    # calculo de ventas en customers
    avg_sales_customes = sum(sales) / sum(customers)
  )


avg_sales %>%
  # genera la serie de tiempo
  ggplot(
    aes(x = yearmonth, y = avg_sales_customes,
    group = store_type, color = store_type)
        ) +
  geom_line() +
  # agrega un tema minimalista
  theme_minimal() +
  # agrega un titulo
  labs(title = "Promedio venta por mes")

ggpairs(
  avg_sales,
  columns = c("avg_sales_customes","store_type", "country"), 
  aes(
    color = country,  # Color por store_type
    alpha = 0.5
  )
)
