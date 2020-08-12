#Load packages
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(anytime)
library(hms)
library(broom)
library(ggplot2)
library(googlesheets4)
#Run query https://glovoapp.eu.looker.com/sql/bnbmw6jsyhrf5k and save as 'partnersatbotdata.csv'
#------------------------------------------------------------------------------------------------
#Read data
satdata <- read.table('C:/Users/Gerardo Flores/Documents/RSupplyOps/PartnerSatBot/partnersatbotdata.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE, allowEscapes = TRUE, quote = "\"") #Query info
hourmin <- read.table('C:/Users/Gerardo Flores/Documents/RSupplyOps/PartnerSatBot/horarios.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE, allowEscapes = TRUE, quote = "\"") #Time info
#------------------------------------------------------------------------------------------------
#All stores
#satdata %>% filter(final_status=='DeliveredStatus') %>% group_by(store_address_id) %>% summarise(orders=n_distinct(order_id)) %>% arrange(desc(orders)) %>% summarise(n = n(), orders = sum(orders))
##Get stores with orders based on percentile (Min quantile=0.95 (otherwise takes too much memory in join))
topstor <- satdata %>% 
           group_by(store_address_id) %>% 
           summarise(orders = n_distinct(order_id)) %>% 
           arrange(desc(orders)) %>% 
           filter(orders >= quantile(orders, 0.95)) %>% #quantile = 0.975
           select(store_address_id) %>%
           pull()
top30 <- satdata %>% 
         group_by(store_address_id) %>% 
         summarise(orders = n_distinct(order_id)) %>% 
         arrange(desc(orders)) %>% 
         filter(orders >= quantile(orders, 0.95)) %>%
         top_n(30) %>% slice(1:30) %>%
         select(store_address_id) %>%
         pull()
#Check
#satdata %>% filter(final_status=='DeliveredStatus') %>% group_by(store_address_id) %>% summarise(orders=n_distinct(order_id)) %>% arrange(desc(orders)) %>% filter(orders >= quantile(orders, 0.95)) %>% summarise(n = n(), orders = sum(orders))
#Distribution
#satdata %>% filter(final_status=='DeliveredStatus') %>% group_by(store_address_id) %>% summarise(orders=n_distinct(order_id)) %>% ungroup() %>% group_by(orders) %>% summarise(addresses = n_distinct(store_address_id))
#------------------------------------------------------------------------------------------------
#Transform
satstor <- satdata %>%
           #Filter top stores
           filter(store_address_id %in% topstor) %>%
           #filter(store_name == 'McDonalds') %>%
           #Change from character vectors to datetime
           mutate(activation_time_local = ymd_hms(activation_time_local),
                  partner_dispatch_time_local = ymd_hms(partner_dispatch_time_local),
                  partner_accepted_time_local = ymd_hms(partner_accepted_time_local),
                  pickup_time_local = ymd_hms(pickup_time_local),
                  enters_pickup_local = ymd_hms(enters_pickup_local),
                  delivery_time_local = ymd_hms(delivery_time_local)
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
                  min_prep_time = as.numeric(difftime(pickup_time_local, partner_accepted_time_local, units = "mins")),
                  min_dt = as.numeric(difftime(delivery_time_local, activation_time_local, units = "mins"))
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
                      avg_min_prep_time = mean(min_prep_time, na.rm = TRUE),
                      avg_min_dt = mean(min_dt, na.rm = TRUE),
                      avg_cpo = mean(cpo, na.rm = TRUE),
                      avg_cm0 = mean(cm0, na.rm = TRUE)
                      ) %>%
            pivot_longer(cols = delivered:avg_cm0, names_to = "concept", values_to = "value")
#------------------------------------------------------------------------------------------------
##DAILY
###Hour (Delivered and cancels based on date hour)
decahoda <- satstor %>% 
            mutate(date = as.Date(activation_time_local), 
                   hour = hour(activation_time_local)
                   ) %>%
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
                       avg_min_prep_time = mean(min_prep_time, na.rm = TRUE),
                       avg_min_dt = mean(min_dt, na.rm = TRUE),
                       avg_cpo = mean(cpo, na.rm = TRUE),
                       avg_cm0 = mean(cm0, na.rm = TRUE)
                       ) %>%
             ungroup() %>%
             left_join(decahoda, by = c("store_address_id" = "store_address_id", "date" = "date", "hour" = "hour")) %>%
             select(store_address_id:hour, delivered:cancels, avg_ongoing_orders:avg_cm0)
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
                       avg_min_prep_time = mean(min_prep_time, na.rm = TRUE),
                       avg_min_dt = mean(min_dt, na.rm = TRUE),
                       avg_cpo = mean(cpo, na.rm = TRUE),
                       avg_cm0 = mean(cm0, na.rm = TRUE)
                       ) %>%
             left_join(decaorda, by = c("store_address_id" = "store_address_id", "date" = "date", "ongoing_orders" = "ongoing_orders")) %>%
             select(store_address_id:ongoing_orders, delivered:cancels, avg_min_dispatch:avg_cm0)
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
                       avg_min_prep_time = mean(min_prep_time, na.rm = TRUE),
                       avg_min_dt = mean(min_dt, na.rm = TRUE),
                       avg_cpo = mean(cpo, na.rm = TRUE),
                       avg_cm0 = mean(cm0, na.rm = TRUE)
                       ) %>%
             ungroup() %>%
             left_join(decahoho, by = c("store_address_id" = "store_address_id", "hour" = "hour")) %>%
             select(store_address_id:hour, delivered:cancels, avg_ongoing_orders:avg_cm0)
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
                       avg_min_prep_time = mean(min_prep_time, na.rm = TRUE),
                       avg_min_dt = mean(min_dt, na.rm = TRUE),
                       avg_cpo = mean(cpo, na.rm = TRUE),
                       avg_cm0 = mean(cm0, na.rm = TRUE)
                       ) %>%
             ungroup() %>%
             left_join(decaorho, by = c("store_address_id" = "store_address_id", "ongoing_orders" = "ongoing_orders")) %>%
             select(store_address_id:ongoing_orders, delivered:cancels, avg_min_dispatch:avg_cm0)
#Check
#tableords %>% group_by(store_address_id) %>% summarise(delivered = sum(delivered, na.rm = TRUE), cancels = sum(cancels, na.rm = TRUE)) %>% arrange(desc(delivered))
#------------------------------------------------------------------------------------------------
#REGRESSION
##Ongoing orders (Get r2 for every kpi compared to ongoing orders)
by_said <- tableords %>% group_by(store_address_id)
r2_del <- do(by_said, glance(lm(delivered ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("delivered" = "r.squared")
r2_can <- do(by_said, glance(lm(cancels ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("cancels" = "r.squared")
r2_dis <- do(by_said, glance(lm(avg_min_dispatch ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("avg_min_dispatch" = "r.squared")
r2_acc <- do(by_said, glance(lm(avg_min_wtp ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("avg_min_accept" = "r.squared") #QUITAR!!
r2_wtp <- do(by_said, glance(lm(avg_min_wtp ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("avg_min_wtp" = "r.squared")
r2_pre <- do(by_said, glance(lm(avg_min_wtp ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("avg_min_prep_time" = "r.squared") #QUITAR!!
r2_dt <- do(by_said, glance(lm(avg_min_dt ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("avg_min_dt" = "r.squared") #QUITAR!!
r2_cpo <- do(by_said, glance(lm(avg_cpo ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("avg_cpo" = "r.squared") #QUITAR!!
r2_cm0 <- do(by_said, glance(lm(avg_cm0 ~ ongoing_orders, data = .))) %>% select(r.squared) %>% rename("avg_cm0" = "r.squared") #QUITAR!!
r2 <- r2_del %>%
      left_join(r2_can, by = c("store_address_id" = "store_address_id")) %>%
      left_join(r2_dis, by = c("store_address_id" = "store_address_id")) %>%
      left_join(r2_acc, by = c("store_address_id" = "store_address_id")) %>%
      left_join(r2_wtp, by = c("store_address_id" = "store_address_id")) %>%
      left_join(r2_pre, by = c("store_address_id" = "store_address_id")) %>%
      left_join(r2_dt, by = c("store_address_id" = "store_address_id")) %>%
      left_join(r2_cpo, by = c("store_address_id" = "store_address_id")) %>%
      left_join(r2_cm0, by = c("store_address_id" = "store_address_id")) %>%   
      pivot_longer(cols = delivered:avg_cm0, names_to = "concept", values_to = "r2")
#------------------------------------------------------------------------------------------------
#TOTALS
totalsto <- kpistore %>% 
            group_by(store_address_id) %>% 
            pivot_wider(names_from = concept, values_from = "value") %>%
            summarise(total_orders = delivered,
                      total_wtp = delivered*avg_min_wtp,
                      avg_wtp = avg_min_wtp
                      )
#IMPACTS
threords <- tableords %>% 
            group_by(store_address_id, store_name) %>%
            mutate(threshold_dispatch = case_when(as.numeric(avg_min_dispatch) == max(avg_min_dispatch) ~ as.numeric(ongoing_orders)),
                   threshold_accept = case_when(as.numeric(avg_min_accept) == max(avg_min_accept) ~ as.numeric(ongoing_orders)),
                   threshold_wtp = case_when(as.numeric(avg_min_wtp) >= 15 ~ as.numeric(ongoing_orders)),
                   threshold_prep_time = case_when(as.numeric(avg_min_prep_time) >= 30 ~ as.numeric(ongoing_orders))
                   ) %>%
            mutate(threshold_dispatch = max(threshold_dispatch, na.rm = TRUE),
                   threshold_accept = max(threshold_accept, na.rm = TRUE),
                   threshold_wtp = nth(threshold_wtp, 2, order_by = threshold_wtp, default = min(threshold_wtp, na.rm = TRUE)),
                   threshold_prep_time = nth(threshold_prep_time, 2, order_by = threshold_prep_time, default = min(threshold_prep_time, na.rm = TRUE))
                   )
#Potential lost orders and wtp impact #Hasta aquí bien
disimpac <- threords %>%
            group_by(store_address_id, store_name) %>%
            filter(ongoing_orders > max(threshold_dispatch)) %>%
            group_by(store_address_id, store_name, threshold_dispatch) %>%
            summarise(lost_orders_dispatch = sum(delivered, na.rm = TRUE),
                      wtp_impact_dispatch = sum(delivered*avg_min_wtp, na.rm = TRUE)
                      )
accimpac <- threords %>%
            group_by(store_address_id, store_name) %>%
            filter(ongoing_orders > max(threshold_accept)) %>%
            group_by(store_address_id, store_name, threshold_accept) %>%
            summarise(lost_orders_accept = sum(delivered, na.rm = TRUE),
                      wtp_impact_accept = sum(delivered*avg_min_wtp, na.rm = TRUE)
                      )
wtpimpac <- threords %>%
            group_by(store_address_id, store_name) %>%
            filter(ongoing_orders > max(threshold_wtp)) %>%
            group_by(store_address_id, store_name, threshold_wtp) %>%
            summarise(lost_orders_wtp = sum(delivered, na.rm = TRUE),
                      wtp_impact_wtp = sum(delivered*avg_min_wtp, na.rm = TRUE)
                      )
preimpac <- threords %>%
            group_by(store_address_id, store_name) %>%
            filter(ongoing_orders > max(threshold_prep_time)) %>%
            group_by(store_address_id, store_name, threshold_prep_time) %>%         
            summarise(lost_orders_prep_time = sum(delivered, na.rm = TRUE),
                      wtp_impact_prep_time = sum(delivered*avg_min_wtp, na.rm = TRUE)
                      )
#Unify impacts
thresh <- threords %>% 
          select(store_address_id,store_name) %>%
          group_by(store_address_id, store_name) %>%
          left_join(disimpac, by = c("store_address_id" = "store_address_id", "store_name" = "store_name")) %>%
          left_join(accimpac, by = c("store_address_id" = "store_address_id", "store_name" = "store_name")) %>% 
          left_join(wtpimpac, by = c("store_address_id" = "store_address_id", "store_name" = "store_name")) %>%
          left_join(preimpac, by = c("store_address_id" = "store_address_id", "store_name" = "store_name")) %>%
          distinct(.keep_all=TRUE) %>%
          select(threshold_dispatch, threshold_accept, threshold_wtp, threshold_prep_time) %>%
          rename("avg_min_dispatch" = "threshold_dispatch", "avg_min_accept" = "threshold_accept", "avg_min_wtp" = "threshold_wtp", "avg_min_prep_time" = "threshold_prep_time") %>%
          pivot_longer(cols = c(avg_min_dispatch:avg_min_prep_time), names_to = "concept", values_to = "threshold")
ordimp <- threords %>% 
          select(store_address_id,store_name) %>%
          group_by(store_address_id, store_name) %>%
          left_join(disimpac, by = c("store_address_id" = "store_address_id", "store_name" = "store_name")) %>%
          left_join(accimpac, by = c("store_address_id" = "store_address_id", "store_name" = "store_name")) %>% 
          left_join(wtpimpac, by = c("store_address_id" = "store_address_id", "store_name" = "store_name")) %>%
          left_join(preimpac, by = c("store_address_id" = "store_address_id", "store_name" = "store_name")) %>%
          group_by(store_address_id, store_name) %>%
          select(lost_orders_dispatch, lost_orders_accept, lost_orders_wtp, lost_orders_prep_time) %>%
          rename("avg_min_dispatch" = "lost_orders_dispatch", "avg_min_accept" = "lost_orders_accept", "avg_min_wtp" = "lost_orders_wtp", "avg_min_prep_time" = "lost_orders_prep_time") %>%
          pivot_longer(cols = c(avg_min_dispatch:avg_min_prep_time), names_to = "concept", values_to = "lost_orders") %>%
          group_by(store_address_id,store_name,concept)  
wtpimp <- threords %>% 
          select(store_address_id,store_name) %>%
          group_by(store_address_id, store_name) %>%
          left_join(disimpac, by = c("store_address_id" = "store_address_id", "store_name" = "store_name")) %>%
          left_join(accimpac, by = c("store_address_id" = "store_address_id", "store_name" = "store_name")) %>% 
          left_join(wtpimpac, by = c("store_address_id" = "store_address_id", "store_name" = "store_name")) %>%
          left_join(preimpac, by = c("store_address_id" = "store_address_id", "store_name" = "store_name")) %>%
          group_by(store_address_id, store_name) %>%
          select(wtp_impact_dispatch, wtp_impact_accept, wtp_impact_wtp, wtp_impact_prep_time) %>%
          rename("avg_min_dispatch" = "wtp_impact_dispatch", "avg_min_accept" = "wtp_impact_accept", "avg_min_wtp" = "wtp_impact_wtp", "avg_min_prep_time" = "wtp_impact_prep_time") %>%
          pivot_longer(cols = c(avg_min_dispatch:avg_min_prep_time), names_to = "concept", values_to = "wtp_impact") %>%
          group_by(store_address_id,store_name,concept)  
impacts <- thresh %>%
           group_by(store_address_id,store_name,concept) %>%
           inner_join(ordimp, by = c("store_address_id" = "store_address_id", "store_name" = "store_name", "concept" = "concept")) %>%
           inner_join(wtpimp, by = c("store_address_id" = "store_address_id", "store_name" = "store_name", "concept" = "concept")) %>%
           distinct(.keep_all=TRUE)
#------------------------------------------------------------------------------------------------
#OUTPUTS
#Hour
pivothour <- tablehour %>% 
             group_by(store_address_id, store_name) %>%
             pivot_longer(cols = delivered:avg_cm0, names_to = "concept", values_to = "value") %>%
             pivot_wider(names_from = "hour", values_from = "value", names_sort = TRUE) %>%
             inner_join(kpistore, by = c("store_address_id" = "store_address_id", "concept" = "concept")) %>%
             select(store_address_id:concept, value, everything())
#Ongoing orders
pivotords <- tableords %>% 
             group_by(store_address_id, store_name) %>%
             pivot_longer(cols = delivered:avg_cm0, names_to = "concept", values_to = "value") %>%
             pivot_wider(names_from = "ongoing_orders", values_from = "value", names_sort = TRUE) %>%
             inner_join(kpistore, by = c("store_address_id" = "store_address_id", "concept" = "concept")) %>%
             inner_join(r2, by = c("store_address_id" = "store_address_id", "concept" = "concept")) %>%
             select(store_address_id:concept, r2, value, everything())
#Export pivots
#write_sheet(pivothour, ss = "12ohbeHEbU67diXiapFHbowwNrsQ8aAWp1rbKFb4D_Pg", sheet = "InfoHour") #MCD
#write_sheet(pivotords, ss = "12ohbeHEbU67diXiapFHbowwNrsQ8aAWp1rbKFb4D_Pg", sheet = "InfoOO") #MCD
write_sheet(pivothour, ss = "1qSCZlqdvhc2M4vxqrzmZ4pO-pcO2eij0L7f3fdTV4BM", sheet = "InfoHour")
write_sheet(pivotords, ss = "1qSCZlqdvhc2M4vxqrzmZ4pO-pcO2eij0L7f3fdTV4BM", sheet = "InfoOO")
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
                       avg_min_prep_time = mean(avg_min_prep_time),
                       avg_min_dt = mean(avg_min_dt, na.rm = TRUE),
                       avg_cpo = mean(avg_cpo, na.rm = TRUE),
                       avg_cm0 = mean(avg_cm0, na.rm = TRUE)
                       ) %>%
             ungroup() %>%
             group_by(store_address_id, store_name) %>%
             mutate(accum = cumsum(delivered),
                    perc = cumsum(proportions(delivered))) %>%
             ungroup() %>%
             select(store_address_id:delivered, accum:perc, avg_min_dispatch:avg_cm0) %>%
             pivot_longer(cols = delivered:avg_cm0, names_to = "concept", values_to = "value") %>%
             pivot_wider(names_from = "ongoing_orders", values_from = "value", names_sort = TRUE) %>%
             left_join(kpistore, by = c("store_address_id" = "store_address_id", "concept" = "concept")) %>%
             left_join(r2, by = c("store_address_id" = "store_address_id", "concept" = "concept")) %>%
             select(store_address_id:concept, r2, value, everything())
#Export range ongoing orders
#write_sheet(pivotords, ss = "12ohbeHEbU67diXiapFHbowwNrsQ8aAWp1rbKFb4D_Pg", sheet = "InfoGroup") #MCD
write_sheet(ongordran, ss = "1qSCZlqdvhc2M4vxqrzmZ4pO-pcO2eij0L7f3fdTV4BM", sheet = "InfoGroup")
#------------------------------------------------------------------------------------------------
#Impacts
finalimp <- impacts %>%
            ungroup() %>%
            group_by(store_address_id) %>%
            left_join(totalsto, by = c("store_address_id" = "store_address_id")) %>%
            mutate(impact_wtp = (total_wtp-wtp_impact)/(total_orders-lost_orders),
                   perc_lost = lost_orders/total_orders
                   ) %>%
            mutate(impact_wtp = case_when(impact_wtp>avg_wtp ~ avg_wtp,
                                          TRUE ~ impact_wtp)
                   ) %>%
            mutate(suggestion = case_when(perc_lost >= 0.10 ~ "Critical",
                                          perc_lost >= 0.05 & perc_lost < 0.10 ~ "Follow-up",
                                          perc_lost >= 0 & perc_lost < 0.05 ~ "Decrease",
                                          threshold = is.na(threshold) & avg_wtp <= 7.5 ~ "Increase",
                                          avg_wtp/impact_wtp>=0.95 ~ "Keep",
                                          TRUE ~ "Keep"
                                          )
                  ) %>%
            mutate(threshold = case_when(suggestion=='Keep' ~ 0, TRUE ~ threshold),
                   lost_orders = case_when(suggestion=='Keep' ~ 0, TRUE ~ as.numeric(lost_orders)),
                   perc_lost = case_when(suggestion=='Keep' ~ 0, TRUE ~ perc_lost),
                   impact_wtp = case_when(suggestion=='Keep' ~ 0, TRUE ~ impact_wtp)
                   ) %>%
            filter(!concept %in% c('avg_min_dispatch','avg_min_accept','avg_min_prep_time')) %>%
            rename("suggested_threshold" = "threshold", "potential_lost_orders" = "lost_orders", "real_wtp" = "avg_wtp") %>%
            select(store_address_id:store_name, suggestion, suggested_threshold, potential_lost_orders, perc_lost, impact_wtp, real_wtp)
#Export pivots
#write_sheet(finalimp, ss = "12ohbeHEbU67diXiapFHbowwNrsQ8aAWp1rbKFb4D_Pg", sheet = "Impacts") #MCD
write_sheet(finalimp, ss = "1qSCZlqdvhc2M4vxqrzmZ4pO-pcO2eij0L7f3fdTV4BM", sheet = "Impacts")
#------------------------------------------------------------------------------------------------
#GRAPHS
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
##Graph times - ongoing_orders
onortigg <- tableords %>% 
            select(-c(delivered:cancels)) %>% #filter(store_address_id %in% c('124653','21564','28570','21566','205734','87324','85420','201670','203367','60246','17755','14484','66138','16180','193236','176560','206482','60315','17644','25834','41635')) %>%
            filter(store_address_id %in% top30) %>%
            pivot_longer(cols = c(avg_min_dispatch:avg_min_dt), names_to = "kpi") %>%
            ggplot(aes(x = ongoing_orders, y = value, col = kpi, group = kpi)) +
            geom_line(show.legend = TRUE) +
            facet_wrap(~store_address_id, scales = "free") +
            labs(title = "Kpis per ongoing orders", 
                 x = "Ongoing orders", 
                 y = "value") +
            themeplot
##Graph orders - ongoing_orders
onororgg <- tableords %>% 
            select(-c(avg_min_dispatch:avg_cm0)) %>% #filter(store_address_id %in% c('124653','21564','28570','21566','205734','87324','85420','201670','203367','60246','17755','14484','66138','16180','193236','176560','206482','60315','17644','25834','41635')) %>%
            filter(store_address_id %in% top30) %>%
            pivot_longer(cols = c(delivered:cancels), names_to = "kpi") %>%
            ggplot(aes(x = ongoing_orders, y = value, col = kpi, group = kpi)) +
            geom_line(show.legend = TRUE) +
            facet_wrap(~store_address_id, scales = "free") +
            labs(title = "Orders per ongoing orders", 
                 x = "Ongoing orders", 
                 y = "value") +
            themeplot
##Graph cpo
onorcost <- tableords %>% 
            select(-c(delivered:avg_min_dt)) %>% #filter(store_address_id %in% c('124653','21564','28570','21566','205734','87324','85420','201670','203367','60246','17755','14484','66138','16180','193236','176560','206482','60315','17644','25834','41635')) %>%
            filter(store_address_id %in% top30) %>%
            pivot_longer(cols = c(avg_cpo:avg_cm0), names_to = "kpi") %>%
            ggplot(aes(x = ongoing_orders, y = value, col = kpi, group = kpi)) +
            geom_line(show.legend = TRUE) +
            facet_wrap(~store_address_id, scales = "free") +
            labs(title = "CPO/CM0 per ongoing orders", 
                 x = "Ongoing orders", 
                 y = "value") +
            themeplot
#Critical <- filter(store_address_id %in% c('93448','124653','137569','185505','41599','41605','53514','60246'))
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
