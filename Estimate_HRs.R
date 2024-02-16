# required packages
require(tidyverse)
require(lubridate)
require(ctmm)
library(progress)
require(adehabitatHR)
library(eatTools)


load("wolves_data_new.Rdata") # Load data
df_new <- df_new %>% filter(type == "resident") # Filtering only resident individuals
unique(df_new$ID)

# define progress bar
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(unique(df_new$ID)), # Maximum value of the progress bar
                     style = 3,    # Progress bar style 
                     width = 50,   # Progress bar width
                     char = "=") 

# AKDE --------------------------------------------------------------------
# data conversion to "telemetry" object
tel <- lapply(1:length(unique(df_new$ID)), function(b) as.telemetry(df_new %>% 
                                              filter(ID==levels(as.factor(df_new$ID))[b]
                                                     & type == "resident"),
                                            timeformat =  "%Y-%m-%d %H:%M:%S", # Defining the time format
                                            timezone = "UTC")) # Time zone setting
names(tel) <- levels(as.factor(df_new$ID)) 

# variogram and its display
wolves_vg <- lapply(tel, function (b)  variogram(b))

for (i in 1:length(unique(df_new$ID))) {
  ctmm::plot(wolves_vg[[i]], col.CTMM = "red")
  title(main = levels(as.factor(df_new$ID))[i])
}

for (i in 1:length(unique(df_new$ID))) {
  ctmm::plot(wolves_vg[[i]], fraction=0.010, col.CTMM = "red")
  title(main = levels(as.factor(df_new$ID))[i])
}

# Outalier analysis; deviated trajectory or suspiciously high speed is marked in red
OUT <- lapply(tel, function (t) outlie(t))
BAD <- lapply(OUT, function (o) which.max(o$speed))

for (i in 1:length(names(tel))) {
  tel[[i]] <- tel[[i]][-BAD[[i]],]
  
}
lapply(1:length(unique(df_new$ID)), function (b) outlie(tel[b], plot = T, main =  levels(as.factor(df_new$ID))[b]))


# fitting model
GUESS <- lapply(tel, function (t) {
  ctmm.guess(t, interactive = FALSE)
  })

FITS <- lapply(1:length(unique(df_new$ID)), function (f) {
  setTxtProgressBar(pb, f)
  ctmm.select(tel[[f]], GUESS[[f]])
  })
names(FITS) <- names(tel)

lapply(FITS, function(b) summary (b))

UDS <- lapply(1:length(unique(df_new$ID)), function(u) {
  setTxtProgressBar(pb, u)
  akde(tel[[u]], FITS[[u]])
  })

names(UDS) <- names(tel)
lapply(1:length(unique(df_new$ID)), function(p) plot(tel[[p]], UD = UDS[[p]]),
       main = names(UDS)[p])

lapply(UDS, function(b) summary(b))
lapply(UDS, function(b) summary(b, level.UD=0.5))

# KDE ---------------------------------------------------------------------
IDD <- lapply(tel, function(b) ctmm.fit(b))
KDE <- lapply(1:length(unique(df_new$ID)), function (i) akde(tel[[i]], IDD[[i]]))

names(KDE) <- names(tel)

lapply(KDE, function(b) summary(b))
lapply(KDE, function(b) summary(b, level.UD=0.5))

# MCP ---------------------------------------------------------------------
xysp <- lapply(lapply(1:length(unique(df_new$ID)), function (b) df_new %>% 
                        filter(ID == levels(as.factor(df_new$ID))[b]))
               , function(c) SpatialPoints(c[,2:3]))
names(xysp) <- names(tel)

hr.mcp <- lapply(xysp, function (b) mcp(b, percent = 95, unout = "km2"))
core_hr.mcp <- lapply(xysp, function (b) mcp(b, percent = 50, unout = "km2"))


# Data_frame of all methods --------------------------------------------------------------
df_HR <- lapply(1:length(names(UDS)), function (b) summary(UDS[[b]], level.UD = 0.95)$CI %>% 
  tibble()) %>% do_call_rbind_withName(.,names(UDS), "ID") %>% 
  add_column(HR = "95% HR",
             method = "AKDE") %>% 
  rbind(., lapply(1:length(names(UDS)), function (c) summary(UDS[[c]], level.UD = 0.50)$CI %>% 
                    tibble()) %>% do_call_rbind_withName(.,names(UDS), "ID") %>% 
          add_column(HR = "50% HR",
                     method = "AKDE"),
        lapply(1:length(names(KDE)), function (d) summary(KDE[[d]], level.UD = 0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(KDE), "ID") %>% 
          add_column(HR = "95% HR",
                     method ="KDE"),
        lapply(1:length(names(KDE)), function (e) summary(KDE[[e]], level.UD = 0.50)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(KDE), "ID") %>% 
          add_column(HR = "50% HR",
                     method = "KDE"),
        lapply(1:length(names(hr.mcp)), function (f) hr.mcp[[f]]$area %>% 
                 tibble()) %>% do_call_rbind_withName(., names(hr.mcp), "ID") %>% 
          add_column(HR = "95% HR",
                     method = "MCP"),
        lapply(1:length(names(core_hr.mcp)), function (g) core_hr.mcp[[g]]$area %>% 
                 tibble()) %>% do_call_rbind_withName(., names(core_hr.mcp), "ID") %>% 
          add_column(HR = "50% HR",
                     method = "MCP")
        ) 



### spatial-temporal dynamic of HRs
                      
df_new <- df_new %>% filter(type == "resident")
df_month <- rbind(df_new %>% filter(ID == "f29456_CZE" & year == 2020) %>% 
                   mutate(ID = recode(ID,
                                      "f29456_CZE" = "f29456_CZE_20")),
                 df_new %>% filter(ID == "f29456_CZE" & year == 2021) %>% 
                   mutate(ID = recode(ID,
                                      "f29456_CZE" = "f29456_CZE_21")),
                 df_new %>% filter(ID == "m36932" & year == 2022) %>% 
                   mutate(ID = recode(ID,
                                      "m36932" = "m36932_22")),
                 df_new %>% filter(ID == "m36932" & year == 2023) %>% 
                   mutate(ID = recode(ID,
                                      "m36932" = "m36932_23")),
                 df_new %>% filter(ID == "m30778_POL" & year == 2021) %>% 
                   mutate(ID = recode(ID,
                                      "m30778_POL" = "m30778_POL_21")),
                 df_new %>% filter(ID == "m30778_POL" & year == 2022) %>% 
                   mutate(ID = recode(ID,
                                      "m30778_POL" = "m30778_POL_22"))
) %>% 
  rbind(.,
        df_new %>%  filter(!ID %in% c("f29456_CZE", "m36932", "m30778_POL"))) %>% 
  arrange(., timestamp) %>% 
  arrange(., ID) 

unique(df_month$ID)

## estimated number of tracking days in the month 
list <- vector(mode='list', length=length(unique(df_month$ID)))
for (i in 1:length(unique(df_month$ID))) {
  list[[i]] <- df_month %>%
    filter(ID == levels(as.factor(df_month$ID))[i]) %>%
    arrange(timestamp)
  
  list[[i]]$day <- day(list[[i]]$timestamp)
  
  list[[i]]$time <- paste(list[[i]]$year,"-", 
                          list[[i]]$month,"-", list[[i]]$day, sep="") %>%
    as.factor()
}

list <- lapply(list, function(b) aggregate(.~time, data = b[,c(2,3,12)], mean))
for (i in 1:length(unique(df_month$ID))) {
  list[[i]]$time <- as.POSIXct(list[[i]]$time)
  list[[i]]$ID <- rep(levels(as.factor(df_month$ID))[i], nrow(list[[i]]))
  list[[i]] <- list[[i]] %>%
    rename("timestamp" = "time")
  list[[i]] <- list[[i]] %>%
    arrange(timestamp)
  names(list) <- levels(as.factor(df_month$ID))
  
  data_bind<- bind_rows(list)
  data_bind$time <- as.POSIXct(data_bind$time, format = "%Y-%m-%d")
  
  data_bind$month <- format(data_bind$time, "%m")
  data_bind$year <- format(data_bind$time, "%Y")
  data_bind$day <- format(data_bind$time, "%d")
}



month_ <-lapply(unique(data_bind$ID), function (b) {
  lapply(unique(data_bind %>% 
                  filter(ID ==b) %>% 
                  .$month), function(c)
                    nrow(data_bind %>% filter(ID ==b) %>% 
                           filter(month == c)))
})


for (i in 1:length(unique(data_bind$ID))) {
  names(month_) <- unique(data_bind$ID)
  names(month_[[i]]) <- month.abb[as.numeric(unique(data_bind %>% 
                                                    filter(ID == names(month_)[i]) %>% 
                                                    .$month))]  
}
View(month_)

# removal of months with less than 10 days
df_month <- df_month %>% filter(!ID %in% c("f30776",
                                           "f36930", "m46088_CZE",
                                           "f46974", "f47016", "f36931","m36893")) %>% 
  
  
  rbind(., df_month %>%  filter(ID== "f30776" & (month != 5 & month != 9)), 
        df_month %>%  filter(ID== "f36930" & (month != 11 & month != 7)), 
        df_month %>%  filter(ID== "m46088_CZE" & month != 1), 
        df_month %>%  filter(ID== "f46974" & (month != 10 & month != 12)),
        df_month %>%  filter(ID== "f47016" & (month != 8 & month != 2)), 
        df_month %>%  filter(ID== "m36893" & month != 1), 
        df_month %>%  filter(ID== "f36931" & month != 10) 
  ) 



tel_mth <-lapply(unique(df_month$ID), function (b) {
  lapply(unique(df_month %>% 
                  filter(ID ==b) %>% 
                  .$month), function(c)
                    as.telemetry(df_month %>% filter(ID ==b) %>% 
                                   filter(month == c),
                                 timeformat = "%Y-%m-%d %H:%M:%S", timezone = "UTC"))
})

names(tel_mth) <-unique(df_month$ID)

for (i in 1:length(unique(df_month$ID))) {
  names(tel_mth[[i]]) <- month.abb[as.numeric(unique(df_month %>% 
                                                      filter(ID == names(tel_mth)[i]) %>% 
                                                      .$month))]  
}

tel_mth <- lapply(1:length(unique(df_month$ID)), function (c)
  ctmm::projection(tel_mth[[c]]) <- ctmm::median(tel_mth[[c]])
)  # Apply projection telemetry object (North==up)


GUESS_mth <- lapply(unique(df_month$ID), function (i){
  lapply(tel_mth[[i]], function(b) ctmm.guess(b, interactive = FALSE))
  })

names(GUESS_mth) <- names(tel_mth)

pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = length(unique(df_month$ID)), # Maximum value of the progress bar
                     style = 3,    # Progress bar style 
                     width = 50,   # Progress bar width
                     char = "=") 


FIT_mth <- vector(mode='list', length(unique(df_month$ID)))
for (i in 1:length(unique(df_month$ID))) {
  setTxtProgressBar(pb, i)
  FIT_mth[[i]] <- lapply(1:length(tel_mth[[i]]), function(c) 
    ctmm.select(tel_mth[[i]][[c]], GUESS_mth[[i]][[c]])
    )
  
}


for (i in 1:length(unique(df_month$ID))) {
  names(FIT_mth) <- names(tel_mth)
  names(FIT_mth[[i]]) <- month.abb[as.numeric(unique(df_month %>% 
                                                       filter(ID == names(FIT_mth)[i]) %>% 
                                                       .$month))]  
}

UDS_mth <- vector(mode='list', length(unique(df_month$ID)))
for (i in 1:length(unique(df_month$ID))) {
  setTxtProgressBar(pb, i)
  UDS_mth[[i]] <- lapply(1:length(tel_mth[[i]]), function(c)
    akde(tel_mth[[i]][[c]], FIT_mth[[i]][[c]], level = 0.95))
}

for (i in 1:length(unique(df_month$ID))) {
  names(UDS_mth) <- names(tel_mth)
  names(UDS_mth[[i]]) <- month.abb[as.numeric(unique(df_month %>% 
                                                       filter(ID == names(UDS_mth)[i]) %>% 
                                                       .$month))]  
}


df_mth <-lapply(UDS_mth[[1]], function (a) summary(a, level.UD=0.95)$CI %>% 
         tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
  add_column(ID=names(UDS_mth)[1]) %>% 
  rbind(., lapply(UDS_mth[[2]], function (b) summary(b, level.UD=0.95)$CI %>% 
                    tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[2]),
        lapply(UDS_mth[[3]], function (c) summary(c, level.UD=0.95)$CI %>% 
                tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
        add_column(ID=names(UDS_mth)[3]),
        lapply(UDS_mth[[4]], function (d) summary(d, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[4]),
        lapply(UDS_mth[[5]], function (e) summary(e, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[5]),
        lapply(UDS_mth[[6]], function (f) summary(f, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[6]),
        lapply(UDS_mth[[7]], function (g) summary(g, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[7]),
        lapply(UDS_mth[[8]], function (h) summary(h, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[8]),
        lapply(UDS_mth[[9]], function (i) summary(i, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[9]),
        lapply(UDS_mth[[10]], function (j) summary(j, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[10]),
        lapply(UDS_mth[[11]], function (k) summary(k, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[11]),
        lapply(UDS_mth[[12]], function (l) summary(l, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[12]),
        lapply(UDS_mth[[13]], function (m) summary(m, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[13]),
        lapply(UDS_mth[[14]], function (n) summary(n, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[14]),
        lapply(UDS_mth[[15]], function (o) summary(o, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[15]),
        lapply(UDS_mth[[16]], function (p) summary(p, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[16]),
        lapply(UDS_mth[[17]], function (r) summary(r, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[17]),
        lapply(UDS_mth[[18]], function (s) summary(s, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[18]),
        lapply(UDS_mth[[19]], function (t) summary(t, level.UD=0.95)$CI %>% 
                 tibble()) %>% do_call_rbind_withName(.,names(.),"month") %>% 
          add_column(ID=names(UDS_mth)[19]))
