library(amt)
library(tidyverse)
library(survival)
load("dfs.RData")

# functions -------------------------------------------------------------------------

# computes variance inflection factors (VIF) for dataframe columns
myvif <- function(df){
  sapply(colnames(df), function(col){
    m <- lm(paste(col, "~", paste(colnames(df)[colnames(df)!=col], collapse = " + ")), data=df)
    1/(1 - summary(m)$r.squared)
  })
}

# reduce the list of predictors based on VIF threshold
redvif <- function(preds, data=df, thres=5, silent=FALSE){
  vifs <- myvif(data[,preds])
  while (max(vifs) > thres){
    if (!silent) print(paste("Excluding", preds[vifs==max(vifs)], "with VIF", vifs[vifs==max(vifs)]))
    preds <- preds[vifs!=max(vifs)]
    vifs <- myvif(data[,preds])
  }
  preds
}

# evaluates AIC for all models derived from the input model by dropping one predictor
dr1 <- function(model){
  preds <- all.vars(formula(model))
  preds <- preds[!preds %in% c("case_", "Forest_start", "sl_", "log_sl_", "cos_ta_", "step_id_")]
  if (length(preds) == 0) NULL else {
    mods <- lapply(preds, function(pred) update(model, paste("~.-", pred), model=T)) %>% set_names(preds)
    aics <- sapply(mods, AIC)
    i <- which(aics == min(aics))[1]
    print(paste("Excluding", names(mods[i])))
    list(model = mods[[i]], aic = aics[i])
  }
}

# backward model selection based on AIC
step_aic <- function(model){
  aics <- c(AIC(model))
  models <- list()
  models[[1]] <- model
  stop <- F
  while (!stop){
    m <- dr1(model)
    if (is.null(m)) stop <- T
    else {
      model <- m$model
      aics <- c(aics, m$aic)
      models[[length(models)+1]] <- model
    }
  }
  plot(1:length(aics), aics, ylab="AIC", xlab="Step", type="l")
  points(which(aics==min(aics)), aics[aics==min(aics)], col="red")
  models[[which(aics==min(aics))[1]]]
}

# preparing the data ----------------------------------------------------------------
load("wolves_data_new.Rdata")

# 
wc_legend <- data.frame(
  value = c(10,20,30,40,50,60,70,80,90,95,100),
  label = c("Forest", "Shrubland", "Grassland", "Cropland", "Builtup", "Bare", "Snow", "Water", "Wetland", "Mangroves", "Moss")
)

# IDs of the analysed wolves
ids <- unique(df_new$ID[df_new$type=="resident"])
lags <- sapply(ids, function(id){
  diffs <- df_new %>% 
    filter(ID==id, type=="resident") %>% 
    pull("timestamp") %>% 
    sort %>% 
    diff 
  if (units(diffs) == "hours") diffs<- diffs*60 
  diffs %>% 
    table %>% 
    sort(decreasing = T) %>%
    "["(1) %>% 
    names %>% 
    as.numeric 
})
lags[lags == 30] <- 180  


# scaling the predictors ------------------------------------------------------------
scale_factor <- 500
dfs <- lapply(dfs, function(df) mutate(
  df,
  elevation_end100 = elevation_end/100,
  dist_water100 = dist_water/scale_factor,
  dist_builtup100 = dist_builtup/scale_factor,
  dist_roads100 = dist_roads/scale_factor,
  slope_end5 = slope_end/5,
  habitat = case_when(landcover_end == 10 ~ "Forest",
                      landcover_end == 30 ~ "Grassland",
                      landcover_end == 40 ~ "Cropland",
                      TRUE ~ "Other")
))
dfs <- lapply(dfs, function(df){
  df$habitat[df$habitat == "Other"] <- NA
  if (sum(df$habitat == "Cropland", na.rm = T) > 100) {
    df$habitat <- factor(df$habitat, levels = c("Forest","Grassland","Cropland"))
  }
  else {
    df$habitat[df$habitat == "Cropland"] <- NA
    df$habitat <- factor(df$habitat, levels = c("Forest","Grassland"))
  }
  df
})

# fit models ------------------------------------------------------------------------

# fit full models
models.full <- lapply(names(dfs), function(id){
  print(paste(id,"--------------------------"))
  df <- dfs[[id]]
  preds <- redvif(preds = c("dist_water100", "dist_builtup100", "dist_roads100", "elevation_end100", "slope_end5"), 
                  data = df, thres = 5)
  f <- paste("case_ ~ habitat + sl_ + log_sl_ + cos_ta_  + strata(step_id_) +", paste(preds, collapse = " + "))
  clogit(as.formula(f), data=df, model=T)
}) %>% set_names(names(dfs))

# AIC-based model selection
models <- lapply(names(dfs), function(id){
  print(paste(id,"--------------------------"))
  df <- dfs[[id]]
  assign("df", df, pos=".GlobalEnv")
  step_aic(models.full[[id]])
}) %>% set_names(names(dfs))

# outputs ---------------------------------------------------------------------------

# model-fits table
sumtab <- sapply(models, function(m) summary(m)$concordance %>% round(3)) %>% t %>% as_tibble(rownames = "ID") %>% 
  left_join(sapply(models, function(m) summary(m)$logtest) %>% t %>% as_tibble(rownames = "ID"))

# tables of coefficients
coefs <- lapply(names(dfs), function(id) models[[id]]) %>% 
  lapply(summary) %>% 
  lapply(coef) %>% 
  lapply(function(tab) tab[,c(1,3,5)]) %>% 
  lapply(as_tibble, rownames="predictor") %>% 
  lapply(set_names, c("predictor","coef","se","p")) %>%
  set_names(names(dfs))
