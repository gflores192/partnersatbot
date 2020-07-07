#Load packages
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(anytime)
library(hms)
library(broom)
library(ggplot2)
#Run query https://glovoapp.eu.looker.com/sql/skpqzkwgd9rqcs and save as 'partnersatbotdata.csv'
#------------------------------------------------------------------------------------------------
#Read data
satdata <- read.table('partnersatbotdata.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE, allowEscapes = TRUE, quote = "\"") #Query info
hourmin <- read.table('horarios.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE, allowEscapes = TRUE, quote = "\"") #Time info
#------------------------------------------------------------------------------------------------
#All stores
#satdata %>% group_by(store_address_id) %>% summarise(orders=n_distinct(order_id)) %>% arrange(desc(orders)) %>% summarise(n = n(), orders = sum(orders))
##Get stores with orders based on percentile (Min quantile=0.95 (otherwise takes too much memory in join))
topstor <- satdata %>% 
           group_by(store_address_id) %>% 
           summarise(orders = n_distinct(order_id)) %>% 
           arrange(desc(orders)) %>% 
           filter(orders >= quantile(orders, 0.975)) %>% #quantile = 0.975
           select(store_address_id) %>%
           pull()
#Check
#satdata %>% group_by(store_address_id) %>% summarise(orders=n_distinct(order_id)) %>% arrange(desc(orders)) %>% filter(orders >= quantile(orders, 0.975)) %>% summarise(n = n(), orders = sum(orders))
#------------------------------------------------------------------------------------------------
#Transform
satstor <- satdata %>%
           #Filter top stores
           filter(store_address_id %in% topstor) %>%
           #Change from character vectors to datetime
           mutate(activation_time_local = ymd_hms(activation_time_local),
                  partner_dispatch_time_local = ymd_hms(partner_dispatch_time_local),
                  partner_accepted_time_local = ymd_hms(partner_accepted_time_local),
                  pickup_time_local = ymd_hms(pickup_time_local),
                  enters_pickup_local = ymd_hms(enters_pickup_local),
                  ) %>%
           #Extract hour/minute
           mutate(partner_dispatch_time_local_hm = parse_hms(strftime(partner_dispatch_time_local, format = "%H:%M:%S", tz = "UTC")),
                  pickup_time_local_hm = parse_hms(strftime(pickup_time_local, format = "%H:%M:%S", tz = "UTC"))
                  ) %>%
           #Calculations orders
           mutate(delivered = case_when(final_status == "DeliveredStatus" ~ 1),
                  cancels = case_when(final_status == "CanceledStatus" ~ 1)
                  ) %>%
           #Calculations times  
           mutate(min_dispatch = as.numeric(difftime(partner_dispatch_time_local, activation_time_local, units = "mins")),
                  min_accept = as.numeric(difftime(partner_accepted_time_local, partner_dispatch_time_local, units = "mins")),
                  min_wtp = as.numeric(difftime(pickup_time_local, enters_pickup_local, units = "mins")),
                  min_prep_time = as.numeric(difftime(pickup_time_local, partner_accepted_time_local, units = "mins"))
                  ) %>%
           #Auxiliar
           mutate(aux = 1)
hourmin <- hourmin %>%
           #Change from character vector to time
           mutate(hourmin = parse_hms(hourmin)) %>%
           #Working hours
           mutate(hour = hour(hourmin)) %>%
           filter(hour>=7 & hour<21) %>%
           select(hourmin) %>%
           #Auxiliar
           mutate(aux = 1)
#------------------------------------------------------------------------------------------------
#Detail (t ~ 60s) (Optimize!!)
detail <- hourmin %>% 
          inner_join(satstor) %>%
          filter(partner_dispatch_time_local_hm <= hourmin & pickup_time_local_hm > hourmin) %>%
          mutate(date = as.Date(activation_time_local))
#------------------------------------------------------------------------------------------------
#STORE (Summary kpis store)
kpistore <- detail %>% 
            group_by(store_address_id) %>%
            summarise(delivered = length(unique(order_id[final_status == 'DeliveredStatus'])),
                      cancels = length(unique(order_id[final_status == 'CanceledStatus'])),
                      avg_min_dispatch = mean(min_dispatch, na.rm = TRUE),
                      avg_min_accept = mean(min_accept, na.rm = TRUE),
                      avg_min_wtp = mean(min_wtp, na.rm = TRUE),
                      avg_min_prep_time = mean(min_prep_time, na.rm = TRUE)
                      ) %>%
            pivot_longer(cols = delivered:avg_min_prep_time, names_to = "concept", values_to = "value")
#------------------------------------------------------------------------------------------------
##DAILY
###Hour (Delivered and cancels based on date hour)
decahoda <- satstor %>% 
            mutate(date = as.Date(activation_time_local), 
                   hour = hour(activation_time_local)) %>%
            group_by(store_address_id, date, hour) %>% 
            summarise(delivered = sum(delivered, na.rm = TRUE),
                      cancels = sum(cancels, na.rm = TRUE)
                      )
#Check
#decahoda %>% group_by(store_address_id) %>% summarise(delivered = sum(delivered), cancels = sum(cancels)) %>% arrange(desc(delivered))
###Ongoing orders (Delivered and cancels based on date ongoing orders) (Note: Not all orders get to the store that's why there are less cancels)
decaorda <- detail %>%
            group_by(store_address_id, store_name, date, hourmin) %>%
            mutate(ongoing_orders = n())  %>%
            ungroup() %>%
            group_by(order_id) %>%
            mutate(ongoing_orders = round(mean(ongoing_orders, na.rm = TRUE),0)) %>%
            group_by(store_address_id, date, ongoing_orders) %>%
            summarise(delivered = length(unique(order_id[final_status == 'DeliveredStatus'])),
                      cancels = length(unique(order_id[final_status == 'CanceledStatus']))
                      )
#Check
#decaorda %>% group_by(store_address_id) %>% summarise(delivered = sum(delivered), cancels = sum(cancels)) %>% arrange(desc(delivered))
#------------------------------------------------------------------------------------------------
##SUMMARY (DATE)
###Hour (Summary kpis date hour)
tablehoda <- detail %>%
             group_by(store_address_id, store_name, date, hourmin) %>%
             mutate(ongoing_orders = n()) %>%
             ungroup() %>%
             mutate(hour = hour(hourmin)) %>%
             group_by(store_address_id, store_name, date, hour) %>%
             summarise(avg_ongoing_orders = mean(ongoing_orders, na.rm = TRUE),
                       avg_min_dispatch = mean(min_dispatch, na.rm = TRUE),
                       avg_min_accept = mean(min_accept, na.rm = TRUE),
                       avg_min_wtp = mean(min_wtp, na.rm = TRUE),
                       avg_min_prep_time = mean(min_prep_time, na.rm = TRUE)
                       ) %>%
             ungroup() %>%
             left_join(decahoda, by = c("store_address_id" = "store_address_id", "date" = "date", "hour" = "hour")) %>%
             select(store_address_id:hour, delivered:cancels, avg_ongoing_orders:avg_min_prep_time)
#Check
#tablehoda %>% group_by(store_address_id) %>% summarise(delivered = sum(delivered, na.rm = TRUE), cancels = sum(cancels, na.rm = TRUE)) %>% arrange(desc(delivered))
###Ongoing orders (Summary kpis date ongoing orders) (Check summarisation)
tableorda <- detail %>%
             group_by(store_address_id, store_name, date, hourmin) %>%
             mutate(ongoing_orders = n()) %>%
             ungroup() %>%
             group_by(order_id) %>%
             mutate(ongoing_orders = round(mean(ongoing_orders, na.rm = TRUE),0)) %>%
             ungroup() %>%
             group_by(store_address_id, store_name, date, ongoing_orders) %>%
             summarise(avg_min_dispatch = mean(min_dispatch, na.rm = TRUE),
                       avg_min_accept = mean(min_accept, na.rm = TRUE),
                       avg_min_wtp = mean(min_wtp, na.rm = TRUE),
                       avg_min_prep_time = mean(min_prep_time, na.rm = TRUE)
                       ) %>%
             left_join(decaorda, by = c("store_address_id" = "store_address_id", "date" = "date", "ongoing_orders" = "ongoing_orders")) %>%
             select(store_address_id:ongoing_orders, delivered:cancels, avg_min_dispatch:avg_min_prep_time)
#Check
#tableorda %>% group_by(store_address_id) %>% summarise(delivered = sum(delivered, na.rm = TRUE), cancels = sum(cancels, na.rm = TRUE)) %>% arrange(desc(delivered))
#------------------------------------------------------------------------------------------------
##HOUR
###Hour 
decahoho <- satstor %>% 
            mutate(date = as.Date(activation_time_local), 
                   hour = hour(activation_time_local)) %>%
            group_by(store_address_id, hour) %>% 
            summarise(delivered = length(unique(order_id[final_status == 'DeliveredStatus'])),
                      cancels = length(unique(order_id[final_status == 'CanceledStatus']))
                     )
#Check
#decahoho %>% group_by(store_address_id) %>% summarise(delivered = sum(delivered), cancels = sum(cancels)) %>% arrange(desc(delivered))
###Ongoing orders (Note: Not all orders get to the store that's why there are less cancels)
decaorho <- detail %>%
            group_by(store_address_id, store_name, date, hourmin) %>%
            mutate(ongoing_orders = n())  %>%
            ungroup() %>%
            group_by(order_id) %>%
            mutate(ongoing_orders = round(mean(ongoing_orders, na.rm = TRUE),0)) %>%
            group_by(store_address_id, ongoing_orders) %>%
            summarise(delivered = length(unique(order_id[final_status == 'DeliveredStatus'])),
                      cancels = length(unique(order_id[final_status == 'CanceledStatus']))
                      )
#Check
#decaorho %>% group_by(store_address_id) %>% summarise(delivered = sum(delivered), cancels = sum(cancels)) %>% arrange(desc(delivered))
#------------------------------------------------------------------------------------------------
##SUMMARY
###Hour (Summary kpis hour)
tablehour <- detail %>%
             group_by(store_address_id, store_name, date, hourmin) %>%
             mutate(ongoing_orders = n()) %>%
             ungroup() %>%
             mutate(hour = hour(hourmin)) %>% 
             group_by(store_address_id, store_name, date, hour) %>%
             mutate(avg_ongoing_orders = mean(ongoing_orders, na.rm = TRUE)) %>% #892
             ungroup() %>%
             group_by(store_address_id, store_name, hour) %>% 
             summarise(avg_ongoing_orders = mean(avg_ongoing_orders, na.rm = TRUE),
                       avg_min_dispatch = mean(min_dispatch, na.rm = TRUE),
                       avg_min_accept = mean(min_accept, na.rm = TRUE),
                       avg_min_wtp = mean(min_wtp, na.rm = TRUE),
                       avg_min_prep_time = mean(min_prep_time, na.rm = TRUE)
                       ) %>%
             ungroup() %>%
             left_join(decahoho, by = c("store_address_id" = "store_address_id", "hour" = "hour")) %>%
             select(store_address_id:hour, delivered:cancels, avg_ongoing_orders:avg_min_prep_time)
#Check
#tablehour %>% group_by(store_address_id) %>% summarise(delivered = sum(delivered, na.rm = TRUE), cancels = sum(cancels, na.rm = TRUE)) %>% arrange(desc(delivered))
###Ongoing orders (Summary kpis ongoing orders)
tableords <- detail %>%
             group_by(store_address_id, store_name, date, hourmin) %>%
             mutate(ongoing_orders = n()) %>%
             ungroup() %>%
             group_by(order_id) %>%
             mutate(ongoing_orders = round(mean(ongoing_orders, na.rm = TRUE),0)) %>% #892
             ungroup() %>%
             group_by(store_address_id, store_name, ongoing_orders) %>%
             summarise(avg_min_dispatch = mean(min_dispatch, na.rm = TRUE),
                       avg_min_accept = mean(min_accept, na.rm = TRUE),
                       avg_min_wtp = mean(min_wtp, na.rm = TRUE),
                       avg_min_prep_time = mean(min_prep_time, na.rm = TRUE)
                       ) %>%
             ungroup() %>%
             left_join(decaorho, by = c("store_address_id" = "store_address_id", "ongoing_orders" = "ongoing_orders")) %>%
             select(store_address_id:ongoing_orders, delivered:cancels, avg_min_dispatch:avg_min_prep_time)
#Check
#tableords %>% group_by(store_address_id) %>% summarise(delivered = sum(delivered, na.rm = TRUE), cancels = sum(cancels, na.rm = TRUE)) %>% arrange(desc(delivered))
#------------------------------------------------------------------------------------------------
#REGRESSION
##Ongoing orders (Get r2 for every kpi compared to ongoing orders)
by_said <- tableords %>% group_by(store_address_id)
r2_del <- do(by_said, glance(lm(delivered ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("delivered" = "r.squared")
r2_can <- do(by_said, glance(lm(cancels ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("cancels" = "r.squared")
r2_dis <- do(by_said, glance(lm(avg_min_dispatch ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("avg_min_dispatch" = "r.squared")
r2_acc <- do(by_said, glance(lm(avg_min_accept ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("avg_min_accept" = "r.squared")
r2_wtp <- do(by_said, glance(lm(avg_min_wtp ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("avg_min_wtp" = "r.squared")
r2_pre <- do(by_said, glance(lm(avg_min_prep_time ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("avg_min_prep_time" = "r.squared")
r2 <- r2_del %>%
      left_join(r2_can, by = c("store_address_id" = "store_address_id")) %>%
      left_join(r2_dis, by = c("store_address_id" = "store_address_id")) %>%
      left_join(r2_acc, by = c("store_address_id" = "store_address_id")) %>%
      left_join(r2_wtp, by = c("store_address_id" = "store_address_id")) %>%
      left_join(r2_pre, by = c("store_address_id" = "store_address_id")) %>%
      pivot_longer(cols = delivered:avg_min_prep_time, names_to = "concept", values_to = "r2")
#------------------------------------------------------------------------------------------------
#OUTPUTS
#Hour
pivothour <- tablehour %>% 
             group_by(store_address_id, store_name) %>%
             pivot_longer(cols = delivered:avg_min_prep_time, names_to = "concept", values_to = "value") %>%
             pivot_wider(names_from = "hour", values_from = "value", names_sort = TRUE) %>%
             inner_join(kpistore, by = c("store_address_id" = "store_address_id", "concept" = "concept")) %>%
             select(store_address_id:concept, value, everything())
#Ongoing orders
pivotords <- tableords %>% 
             group_by(store_address_id, store_name) %>%
             pivot_longer(cols = delivered:avg_min_prep_time, names_to = "concept", values_to = "value") %>%
             pivot_wider(names_from = "ongoing_orders", values_from = "value", names_sort = TRUE) %>%
             inner_join(kpistore, by = c("store_address_id" = "store_address_id", "concept" = "concept")) %>%
             inner_join(r2, by = c("store_address_id" = "store_address_id", "concept" = "concept")) %>%
             select(store_address_id:concept, r2, value, everything())
#Export pivots
write_csv(pivothour, "satbotpivothour.csv")
write_csv(pivotords, "satbotpivotords.csv")
#------------------------------------------------------------------------------------------------
#OUTPUT ONGOING ORDERS GROUPED
ongordran <- tableords %>% 
             group_by(store_address_id, store_name) %>%
             mutate(ongoing_orders = case_when(ongoing_orders>0 & ongoing_orders<=5 ~ "01--05",
                                               ongoing_orders>5 & ongoing_orders<=10 ~ "05--10",
                                               ongoing_orders>10 & ongoing_orders<=15 ~ "10--15",
                                               ongoing_orders>15 & ongoing_orders<=20 ~ "15--20",
                                               ongoing_orders>20 & ongoing_orders<=25 ~ "20--25",
                                               ongoing_orders>25 & ongoing_orders<=30 ~ "25--30",
                                               ongoing_orders>30 & ongoing_orders<=35 ~ "30--35",
                                               ongoing_orders>35 & ongoing_orders<=40 ~ "35--40",
                                               ongoing_orders>40 & ongoing_orders<=45 ~ "40--45",
                                               ongoing_orders>45 & ongoing_orders<=50 ~ "45--50",
                                               ongoing_orders>50 ~ "50+")
                                               ) %>%
             group_by(store_address_id, store_name, ongoing_orders) %>%
             summarise(delivered = sum(delivered),
                       cancels = sum(cancels),
                       avg_min_dispatch = mean(avg_min_dispatch),
                       avg_min_accept = mean(avg_min_accept),
                       avg_min_wtp = mean(avg_min_wtp),
                       avg_min_prep_time = mean(avg_min_prep_time)
                       ) %>%
             ungroup() %>%
             group_by(store_address_id, store_name) %>%
             mutate(accum = cumsum(delivered),
                    perc = cumsum(proportions(delivered))) %>%
             ungroup() %>%
             select(store_address_id:delivered, accum:perc, avg_min_dispatch:avg_min_prep_time) %>%
             pivot_longer(cols = delivered:avg_min_prep_time, names_to = "concept", values_to = "value") %>%
             pivot_wider(names_from = "ongoing_orders", values_from = "value", names_sort = TRUE) %>%
             left_join(kpistore, by = c("store_address_id" = "store_address_id", "concept" = "concept")) %>%
             left_join(r2, by = c("store_address_id" = "store_address_id", "concept" = "concept")) %>%
             select(store_address_id:concept, r2, value, everything())
#Export range ongoing orders
write_csv(ongordran, "ongoingordersrange.csv")
#------------------------------------------------------------------------------------------------
#OPTIONAL
#------------------------------------------------------------------------------------------------
#Theme
themeplot <- theme(plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),
                   plot.subtitle = element_text(color = "black", size = 10, face = "bold", hjust = 0.5),
                   panel.background = element_rect(fill = "white", color = "black"),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.x = element_text(color = "black", size = 8),
                   axis.text.y = element_text(color = "black", size = 8),
                   strip.text.x = element_text(color = "black", size = 8),
                   legend.title = element_blank(),
                   legend.position = "bottom"
                   )
#------------------------------------------------------------------------------------------------
#GRAPHS
##Graph times - ongoing_orders
onortigg <- tableords %>% 
            select(-c(delivered:cancels)) %>% 
            pivot_longer(cols = c(avg_min_dispatch:avg_min_prep_time), names_to = "kpi") %>%
            ggplot(aes(x = ongoing_orders, y = value, col = kpi, group = kpi)) +
            geom_line(show.legend = TRUE) +
            facet_wrap(~store_address_id, scales = "free") +
            labs(title = "Kpis per ongoing orders", 
                 x = "Ongoing orders", 
                 y = "value") +
            themeplot
##Graph orders - ongoing_orders
onororgg <- tableords %>% 
            select(-c(avg_min_dispatch:avg_min_prep_time)) %>% 
            pivot_longer(cols = c(delivered:cancels), names_to = "kpi") %>%
            ggplot(aes(x = ongoing_orders, y = value, col = kpi, group = kpi)) +
            geom_line(show.legend = TRUE) +
            facet_wrap(~store_address_id, scales = "free") +
            labs(title = "Orders per ongoing orders", 
                 x = "Ongoing orders", 
                 y = "value") +
            themeplot
##Scatterplot
library(car)
scatterplotMatrix(formula = ~ ongoing_orders + 
                              avg_min_dispatch + 
                              avg_min_accept + 
                              avg_min_wtp + 
                              avg_min_prep_time, 
                  data = tableords)
##Corrplot
library(corrplot)
library(gplots)
corrplot(corr = cor(tableords[3:9], use = "complete.obs"), 
         method = "ellipse", 
         tl.pos = "lt", 
         type = "upper",
         col = colorpanel(50, "red", "gray60", "blue4"))
corrplot.mixed(corr = cor(tableords[3:9]),
               upper = "ellipse")