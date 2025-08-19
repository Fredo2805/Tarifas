library(tidyverse)
library(janitor)

library(readxl)
Base_internet_2025 <- read_excel("~/DCU/Otros/R/Base_fijos_internet_2025.xlsx")
View(Base_internet_2025)

##Crear variables
Base_internet_2025 <- Base_internet_2025 %>% 
  mutate(empaquetado=case_when(flag_telefonia==1 &  flag_tv_paga==1 & flag_internet==1 ~ "Triple play", 
                               flag_telefonia==1 &  flag_tv_paga==0 & flag_internet==1 ~"Int+Tel",
                               flag_telefonia==0 &  flag_tv_paga==1 & flag_internet==1 ~ "INT+TV",
                               flag_telefonia==0 &  flag_tv_paga==0 & flag_internet==1 ~"Single_int"))


###Limpiar nombre de columnas
Base_internet_2025 <- clean_names(Base_internet_2025)
View(Base_internet_2025)
remove(Base_fijos_internet_2025)

###Cálculos
Tipo_plan <-  Base_internet_2025 %>% 
  count(modalidad) %>%
  mutate(porcentaje= round(n/sum(n)*100,2))

Pago_operador <- Base_internet_2025 %>% 
  filter(renta_mensual_ci !="NA") %>% 
  mutate(renta_mensual_ci=as.numeric(renta_mensual_ci)) %>% 
  group_by(nombre_comercial) %>% 
  summarise(total=mean(renta_mensual_ci))

Pago_mes <- Base_internet_2025 %>% 
  filter(renta_mensual_ci !="NA") %>% 
  mutate(renta_mensual_ci=as.numeric(renta_mensual_ci)) %>% 
  group_by(mes) %>% 
  summarise(total=round (mean(renta_mensual_ci),2)) %>% 
  ungroup()

graf_pago_mes <- ggplot(Pago_mes,aes(factor(mes,levels = c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio")),total,label=total))+ 
  geom_col()+ geom_text(aes(mes, total+20))+  labs(x="mes", y="Renta mensual")

graf_pago_mes

Pago_mes_op <- Base_internet_2025 %>% 
  filter(renta_mensual_ci !="NA") %>% 
  mutate(renta_mensual_ci=as.numeric(renta_mensual_ci)) %>% 
  group_by(mes, nombre_comercial) %>% 
  summarise(total=round (mean(renta_mensual_ci),2)) %>% 
  ungroup()

Pago_mes_mod <- Base_internet_2025 %>% 
  filter(renta_mensual_ci !="NA") %>% 
  mutate(renta_mensual_ci=as.numeric(renta_mensual_ci)) %>% 
  group_by(mes, empaquetado) %>% 
  summarise(total=round (mean(renta_mensual_ci),2),Mbps=round(mean(velocidad_de_bajada),2)) %>% 
  ungroup()

