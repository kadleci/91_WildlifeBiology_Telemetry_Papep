# required packages
require(ctmm)
require(adehabitatHR)
require(tidyverse)


load("wolves_data.Rdata") # loading edited data


# 1) Analysis of home precincts using the MCP method -------------------------
ID <- c("36927", "36928", "36930", "36931", "36932", "29456_CZE", "47015", "47016", "30776",    
        "30777", "30778", "30779", "46090", "46088", "29456_AUT")

xysp <- lapply(data, function(b) SpatialPoints(b[,2:3]))
names(xysp) <- ID
hr.mcp <- lapply(xysp, function (b) mcp(b, percent = 95, unout = "km2"))
core_hr.mcp <- lapply(xysp, function (b) mcp(b, percent = 50, unout = "km2"))


# 2) Analysis of HRs using AKDE and KDE method --------------------
# 36932 
wolf_36932 <- data[[5]] # selection of the wolf

# data conversion to "telemetry" object
tel_36932 <- as.telemetry(wolf_36932, timeformat = "%Y-%m-%d %H:%M:%S", timezone = "UTC") 

# data conversion to "telemetry" object
wolf_36932.vg <- variogram(tel_36932) # variogram and its display
plot(wolf_36932.vg, col.CTMM = "red")
plot(wolf_36932.vg, fraction=0.010, col.CTMM = "red")

OUT_36932 <- outlie(tel_36932, plot = T) 
plot(OUT_36932)
# Outalier analysis; deviated trajectory or suspiciously high speed is marked in red.

# fitting model
GUESS_36932 <- ctmm.guess(tel_36932, interactive = FALSE) 
FITS_36932 <- ctmm.select(tel_36932, GUESS_36932) 
# the function selects the most appropriate home range estimation method using the delta AIC criterion
summary(FITS_36932) #OUF anisotropic

# HR estimation using AKDE method
UDS_36932 <- akde(tel_36932, FITS_36932) # UD estimate
plot(tel_36932, UD = UDS_36932) # display results (including 95% HR)
summary(UDS_36932)
summary(UDS_36932, level.UD = 0.5) # the core of the home area

# HR estimation using KDE method
IDD_36932 <- ctmm.fit(tel_36932)
KDE_36932 <- akde(tel_36932, IDD_36932)
summary(KDE_36932)
summary(KDE_36932, level.UD = 0.5)


# 3) Analysis of spatiotemporal dynamics of monthly home ranges --------------
## 36932
tel_5_22 <- vector("list", 12)
tel_5_23 <- vector("list", 12)

for (i in 5:12){
  tel_5_22[[i]] <- as.telemetry(wolf_36932 %>%
                                  filter(month == i), timeformat = "%Y.%m.%d %H:%M:%S", timezone = "UTC")
}

for (i in 1:6) {
  tel_5_23[[i]] <- as.telemetry(wolf_36932 %>%
                                  filter(month == i), timeformat = "%Y.%m.%d %H:%M:%S", timezone = "UTC")
}

tel_5 <- c(tel_5_22, tel_5_23)


# preparation of telemetry object
names(tel_5) <- c("Jan_22", "Feb_22", "Mar_22", "Apr_22", "May_22", "Jun_22", "Jul_22",
                  "Aug_22", "Sep_22","Oct_22", "Nov_22", "Dec_22",
                  "Jan_23", "Feb_23", "Mar_23", "Apr_23", "May_23", "Jun_23", "Jul_23",
                  "Aug_23", "Sep_23","Oct_23", "Nov_23", "Dec_23")

ctmm::projection(tel_5) <- ctmm::median(tel_5) # Apply projection telemetry object (North==up)
plot(tel_5)

# fit a movement model for each month
GUESS5 <- lapply(tel_5, function(b) ctmm.guess(b,interactive=FALSE) )
FITS5 <- lapply(1:14, function(i) ctmm.select(tel_5[[i]],GUESS5[[i]]) )
names(FITS5) <- names(tel_5)

# AKDE overleap between the months
UDS_5 <- akde(tel_5,FITS5, level = 0.95)
