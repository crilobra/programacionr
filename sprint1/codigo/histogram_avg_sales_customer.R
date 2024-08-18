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
# Extrae al dia de la fecha de venta
sales_all <- sales_all %>%
  mutate(yearmonth = as.numeric(format(as.Date(sales_all$date), "%Y%m")))

sales_all <- sales_all %>% filter(country != "italy")

avg_sales <- sales_all  %>%
  group_by(country, year) %>%
  summarise(
    # calculo de ventas en customers
    avg_sales_customes = sum(sales) / sum(customers)
  )

# Calcula promedio and  desviation estandar de las ventas
mean_sales <- mean(avg_sales$avg_sales_customes)
std_dev_sales <- sd(avg_sales$avg_sales_customes)

# Creamos el histograma agregando las linea
ggplot(avg_sales, aes(x = avg_sales_customes)) +
  geom_histogram(color = "black", fill = "white") +
  # Agrega una linea en el promedio
  geom_vline(xintercept = mean_sales, linetype = "dashed", color = "blue") +
  # Agrega una linea en el promedio mas 2 desviaciones estandar
  geom_vline(xintercept = mean_sales + 2 * std_dev_sales,
    linetype = "dashed", color = "red"
  ) +
  # Agrega una linea en el promedio menos 2 desviaciones estandar
  geom_vline(xintercept = mean_sales - 2 * std_dev_sales,
    linetype = "dashed", color = "red"
  ) +
  labs(title = "Histograma de Ventas y la regla empírica débil")
