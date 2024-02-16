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
