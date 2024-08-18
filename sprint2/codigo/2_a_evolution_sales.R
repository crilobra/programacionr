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

# contamos cuantos datos hay con ventas positivas
sales_all %>% filter(sales > 0) %>% count()

# Contamos cuantos registros son con ventas igual a cero
sales_all %>% filter(sales == 0) %>% count()


sales_all$date <- ymd(sales$date)

sales_all %>%
  group_by(date_custom) %>%
  summarise(total_sales = sum(sales) / 1000000) %>%
  ungroup()  %>%
  # genera la serie de tiempo
  ggplot(aes(x = date_custom, y = total_sales)) +
  geom_line() +
  # agrega un tema minimalista
  theme_minimal() +
  # agrega un titulo
  labs(title = "ventas mensuales por pais")

sales_all %>%
  group_by(country, date_custom) %>%
  summarise(total_sales = sum(sales) / 1000000) %>%
  ungroup()  %>%
  # genera la serie de tiempo
  ggplot(
    aes(x = date_custom, y = total_sales, group = country, color = country)
  ) +
  geom_line() +
  # agrega un tema minimalista
  theme_minimal() +
  # agrega un titulo
  labs(title = "ventas mensuales por pais")
