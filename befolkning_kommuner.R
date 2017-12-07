library(tidyverse)
library(dplyr)


####Befolknings tall
options(encoding="macintosh")

bef <- read.csv2("bef2.csv", 
                 skip = 3, 
                 col.names = c("k_nummer", "bef_2007", "bef_2009", "bef_2013","bef_2017")
                 )

##### Sandefjord
xsandefjord <- c("0706","0710","0720","0719")
år <- c("bef_2007", "bef_2009", "bef_2013","bef_2017")
col_names = c("k_nummer", "bef_2007", "bef_2009", "bef_2013","bef_2017")

sandefjord <- filter(bef, k_nummer %in% xsandefjord) %>%
  select(c(år)) %>%
  map_dbl(sum)

  sandefjord <- rbind(sandefjord) %>% 
  as.tibble() %>% 
  mutate(k_nummer="0710") %>% 
  select(k_nummer, bef_2007, "bef_2009", "bef_2013","bef_2017" )

  ##### Harstad
  xharstad <- c("1901","1903","1915")
  år <- c("bef_2007", "bef_2009", "bef_2013","bef_2017")
  col_names = c("k_nummer", "bef_2007", "bef_2009", "bef_2013","bef_2017")
  
  harstad <- filter(bef, k_nummer %in% xharstad) %>%
    select(c(år)) %>%
    map_dbl(sum)
  
  harstad <- rbind(harstad) %>% 
    as.tibble() %>% 
    mutate(k_nummer="1903") %>% 
    select(k_nummer, bef_2007, "bef_2009", "bef_2013","bef_2017" )

  
  ##### Inderøy
  xinderøy <- c("1723","1756","1729")
  år <- c("bef_2007", "bef_2009", "bef_2013","bef_2017")
  col_names = c("k_nummer", "bef_2007", "bef_2009", "bef_2013","bef_2017")
  
  inderøy <- filter(bef, k_nummer %in% xinderøy) %>%
    select(c(år)) %>%
    map_dbl(sum)
  
  inderøy <- rbind(inderøy) %>% 
    as.tibble() %>% 
    mutate(k_nummer="1756") %>% 
    select(k_nummer, bef_2007, "bef_2009", "bef_2013","bef_2017" )

  ##### Kristiansund
  xkristiansund <- c("1503","1505","1556")
  år <- c("bef_2007", "bef_2009", "bef_2013","bef_2017")
  col_names = c("k_nummer", "bef_2007", "bef_2009", "bef_2013","bef_2017")
  
  kristiansund <- filter(bef, k_nummer %in% xkristiansund) %>%
    select(c(år)) %>%
    map_dbl(sum)
  
  kristiansund <- rbind(kristiansund) %>% 
    as.tibble() %>% 
    mutate(k_nummer="1756") %>% 
    select(k_nummer, bef_2007, "bef_2009", "bef_2013","bef_2017" )  
  
x_kommuner<-c("1723","1756","1729","1901","1903","1915","0706","0710","0720","0719","1503","1505","1556")

bef2 <- bef %>%
  filter(k_nummer %notin% x_kommuner) %>% 
  rbind(sandefjord) %>% 
  rbind(harstad) %>% 
  rbind(inderøy) %>% 
  rbind(kristiansund)

bef3 <- bef2 %>% 
  mutate(bef_vekst_4 = bef_2017/bef_2013-1,
         bef_vekst_8 = bef_2017/bef_2009-1,
         bef_vekst_10 = bef_2017/bef_2007-1) %>% 
  filter(!is.nan(bef_vekst_4))
  

write_csv(bef3, "bef_utv_07_17.csv")


`%notin%` <- Negate(`%in%`)



