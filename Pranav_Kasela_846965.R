if (!require("ggplot2")) install.packages('ggplot2')
if (!require("forecast")) install.packages('forecast')
if (!require("tidyverse")) install.packages('tidyverse')
if (!require("KFAS")) install.packages('KFAS')
if (!require("data.table")) install.packages('data.table')
if (!require("timeDate")) install.packages('timeDate')
if (!require("xts")) install.packages('xts')

df <- read.csv2("/home/pranav/Desktop/time_series_project/time_series_dataset.csv", dec = ".")
df$Data <- as.Date(df$Data)

idata <- xts(df$value, start=c(2010,1), order.by=df$Data) 

train <- idata["2010-01-01/2017-06-30"]
validation <- idata["2017-07-01/2018-12-31"]

autoplot(cbind(train, validation), facet=NULL) + ylab("value")

ggplot(data = df, aes(x = Data, y = value)) + 
  geom_line(color='black') +
  geom_smooth(method = "lm", se = FALSE, col="red")


ggtsdisplay_2 <- function(x, lag.max, horizontal=TRUE, ...) {
  if (!is.ts(x)) {
    x <- ts(x)
  }
  if (missing(lag.max)) {
    lag.max <- round(min(max(10 * log10(length(x)), 3 * frequency(x)), length(x) / 3))
  }
  ######      END   CHECKING    ########
  
  # Set up grid for plots
  if (horizontal){
    gridlayout <- matrix(c(2, 3), nrow = 1)
  }
  else{
    gridlayout <- matrix(c(2, 3), nrow = 2)
  }
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(gridlayout), ncol(gridlayout))))
  
  # Prepare Acf plot
  acfplot <- do.call(ggAcf, c(x = quote(x), lag.max = lag.max)) +
    ggplot2::ggtitle("ACF") + ggplot2::ylab(NULL)
  
  # Prepare last plot (variable)
  pacfplot <- ggPacf(x, lag.max = lag.max) + ggplot2::ggtitle("PACF") +
    ggplot2::ylab(NULL)
  # Match y-axis
  acfplotrange <- ggplot2::layer_scales(acfplot)$y$range$range
  pacfplotrange <- ggplot2::layer_scales(pacfplot)$y$range$range
  yrange <- range(c(acfplotrange, pacfplotrange))
  acfplot <- acfplot + ggplot2::ylim(yrange)
  pacfplot <- pacfplot + ggplot2::ylim(yrange)
  
  # Add ACF plot
  matchidx <- as.data.frame(which(gridlayout == 2, arr.ind = TRUE))
  print(
    acfplot,
    vp = grid::viewport(
      layout.pos.row = matchidx$row,
      layout.pos.col = matchidx$col
    )
  )
  
  # Add PACF plot
  matchidx <- as.data.frame(which(gridlayout == 3, arr.ind = TRUE))
  print(
    pacfplot,
    vp = grid::viewport(
      layout.pos.row = matchidx$row,
      layout.pos.col = matchidx$col
    )
  )
  
}

ggtsdisplay_2(train, horizontal = TRUE, lag.max = 60)


mod1 <- Arima(train, c(0,0,0), list(order=c(1,0,1), period=7), lambda = "auto")
ggtsdisplay_2(mod1$residuals, lag.max = 40)
mod1

best_mod <- Arima(train, c(0,0,0), list(order=c(1,1,1), period=7), 
                  lambda = "auto")
best_loglik <- best_mod$loglik

for (i in 0:6){
  for (j in 0:6){
    tryCatch({
      temp_mod <- Arima(train, c(i,0,j), list(order=c(1,1,1), period=7), 
                        lambda = "auto")
      temp_mod_loglik <- temp_mod$loglik
      if (best_loglik < temp_mod$loglik){
        best_mod <- temp_mod
        best_loglik <- temp_mod$loglik
      } 
    }, error=function(e){temp_mod_loglik <- 99999})
    print(paste0("AR-",i," MA-",j,
                 " -- LOG_LIKE:",temp_mod_loglik, " --BEST:",best_loglik), 
          flush=TRUE)
  }
}

xts_to_ts <- function(x, start="2010-01-01"){
  n <- length(x)
  ind <- as.numeric(index(x) - as.Date(start))
  tsx <- ts(x, start=ind[1]+1, end=ind[n]+1, frequency=frequency(x))
  tsx
}

autoplot(xts_to_ts(train), series="Real")+
  autolayer(best_mod$fitted, series="Fitted")+
  ylab("Value")


ggtsdisplay_2(best_mod$residuals, lag.max = 40)

pred <- forecast(best_mod, h=365)

autoplot(xts_to_ts(idata["2016-01-01/2017-06-30"])) +
  autolayer(pred,series="Previsione", alpha=0.7) +
  autolayer(xts_to_ts(validation["2017-07-01/2018-06-30"]), series="Valori veri", alpha=0.6) +
  xlab("Time") +
  ylab("Valore")

autoplot(best_mod)

Mod(1/polyroot(c(1,-best_mod$coef[1:6])))

mod2 <- Arima(train, c(6,1,6), list(order=c(1,1,1), period=7), 
              lambda = "auto")

ggtsdisplay_2(mod2$residuals, lag.max = 40)
mod2

print(tseries::adf.test(mod2$residuals, k=7))

#Another way, it shows the roots
print("Modulus of the AR roots:")
print(Mod(1/polyroot(c(1,-mod2$coef[1:6]))))
autoplot(mod2)

autoplot(xts_to_ts(train), series="Real")+
  autolayer(mod2$fitted, series="Fitted")+
  ylab("Value")

pred <- forecast(mod2, h=365)

autoplot(xts_to_ts(idata["2016-01-01/2017-06-30"])) +
  autolayer(pred,series="Previsione", alpha=0.7) +
  autolayer(xts_to_ts(validation["2017-07-01/2018-06-30"]), series="Valori veri", alpha=0.6) +
  xlab("Time") +
  ylab("Valore")

ggtsdisplay_2(mod2$residuals, lag.max = 365*2)


########################
########################
########################
########XARIMA##########
########################
########################


listHolidays()


#festività natalizie
capodanno <- as.Date(c(unlist(lapply(2010:2019, 
                                     function(a) lapply(c("-01-01","-12-31"), 
                                                        function (b) paste0(a, b))))))
natale <- as.Date(c(unlist(lapply(2010:2019, 
                                  function(a) lapply(paste0("-12-",c(24:26)), 
                                                     function (b) paste0(a, b))))))

#Ferragosto
ferragosto <- as.Date(paste0(2010:2019, "-08-15"))

#Pasqua festività
pasqua <- as.Date(c(Easter(2010:2018), EasterMonday(2010:2018)))

#Altre Festività (quasi tutte le più importanti)
altre <- c(as.Date(c(LaborDay(2010:2018), ITAllSaints(2010:2018), 
                     ITLiberationDay(2010:2018), ITImmaculateConception(2010:2018))),
           as.Date(paste0(2010:2018,"-06-02")), #Festa della Repubblica
           as.Date(paste0(2010:2018,"-01-06"))) #Befana

freq <- outer(1:nrow(df), 1:24)*2*pi/365.25 

cs   <- cos(freq)                   
colnames(cs) <- paste("cos", 1:24)
si   <- sin(freq)                   
colnames(si) <- paste("cos", 1:24)

more_reg <- as.matrix(cbind(cs,si))

data.frame(Data=df$Data) %>%
  mutate(Christmas = as.numeric(Data %in% natale)) %>%
  mutate(NewYear = as.numeric(Data %in% capodanno)) %>%
  mutate(Easter = as.numeric(Data %in% pasqua)) %>%
  mutate(Ferragosto = as.numeric(Data %in% ferragosto)) %>%
  mutate(Other = as.numeric(Data %in% altre)) %>%
  #mutate(WeekDay = data.table::wday(df$Data)) %>%
  #mutate(WeekDay = paste0("W",data.table::wday(df$Data)), ind=1) %>% 
  #spread(WeekDay, ind, fill = 0) %>% #nice way to do one-hot encoding
  select(-starts_with("Data")) %>% 
  cbind(more_reg) %>% 
  as.matrix() -> more_reg

#eliminiamo la prima e l'ultima colonna, per evitare multicollinearità
xreg <- more_reg 

mod1_reg <- Arima(train, c(6,1,6), list(order=c(1,1,1), period=7), 
                  xreg=xreg[1:(length(train)),], include.constant = TRUE, lambda = "auto")

ggtsdisplay_2(mod1_reg$residuals, lag.max = 40)

print(tseries::adf.test(mod1_reg$residuals)) #sono RW


pred_reg <- forecast(mod1_reg, h=365, 
                     xreg=xreg[(length(train)+1):(length(train)+365),])
autoplot(pred_reg)


#i=0
temp_forecast <- forecast(mod1_reg, h=365, xreg=xreg[(length(train)):(length(train)+364),])
tot_score_arima <- mean(abs(temp_forecast$mean - validation[1:365])/validation[1:365])

for (i in 1:(length(validation) - 365)){
  temp_mod <- Arima(c(train[i:length(train)],validation[1:i]), 
                    model=mod1_reg, xreg=xreg[i:(length(train)+i),])
  temp_forecast <- forecast(temp_mod, h=365, xreg=xreg[(length(train)+i+1):(length(train)+i+364+1),])$mean
  score <- mean(abs(temp_forecast - validation[(i+1):(i+365)])/validation[(i+1):(i+365)])
  tot_score_arima <- tot_score_arima + score
}
print(paste0("ARIMA average MAPE:", tot_score_arima/(length(validation) - 365 + 1)))
arima_score <- tot_score_arima/(length(validation) - 365 + 1)*100

i=1
temp_mod <- Arima(c(train[i:length(train)],validation[1:i]), 
                  model=mod1_reg, xreg=xreg[i:(length(train)+i),])
temp_forecast <- forecast(temp_mod, h=365, 
                          xreg=xreg[(length(train)+i+1):(length(train)+i+364+1),])$mean
valid <- validation[(i+1):(i+365)]
mae <- mean(abs(temp_forecast - valid)/valid)

p1 <- ggplot() +
  autolayer(xts_to_ts(valid), series="Real",size=1) +
  autolayer(ts(temp_forecast, start=start(xts_to_ts(valid)),
               frequency=frequency(xts_to_ts(valid))),
            series="ARIMA", size=1, alpha=0.7) +
  xlab("Time") + ylab("Value") + ggtitle(paste0("With ", i, " Lags of validation, MAPE ",round(mae,4)*100,"%"))

i=90

temp_mod <- Arima(c(train[i:length(train)],validation[1:i]), 
                  model=mod1_reg, xreg=xreg[i:(length(train)+i),])
temp_forecast <- forecast(temp_mod, h=365, 
                          xreg=xreg[(length(train)+i+1):(length(train)+i+364+1),])$mean
valid <- validation[(i+1):(i+365)]
mae <- mean(abs(temp_forecast - valid)/valid)

p2 <- ggplot() +
  autolayer(xts_to_ts(valid), series="Real",size=1) +
  autolayer(ts(temp_forecast, start=start(xts_to_ts(valid)),
               frequency=frequency(xts_to_ts(valid))),
            series="ARIMA", size=1, alpha=0.7) +
  xlab("Time") + ylab("Value") + ggtitle(paste0("With ", i, " Lags of validation, MAPE ",round(mae,4)*100,"%"))

i=160

temp_mod <- Arima(c(train[i:length(train)],validation[1:i]), 
                  model=mod1_reg, xreg=xreg[i:(length(train)+i),])
temp_forecast <- forecast(temp_mod, h=365, 
                          xreg=xreg[(length(train)+i+1):(length(train)+i+364+1),])$mean
valid <- validation[(i+1):(i+365)]
mae <- mean(abs(temp_forecast - valid)/valid)

p3 <- ggplot() +
  autolayer(xts_to_ts(valid), series="Real",size=1) +
  autolayer(ts(temp_forecast, start=start(xts_to_ts(valid)),
               frequency=frequency(xts_to_ts(valid))),
            series="ARIMA", size=1, alpha=0.7) +
  xlab("Time") + ylab("Value") + ggtitle(paste0("With ", i, " Lags of validation, MAPE ",round(mae,4)*100,"%"))

i=183
temp_mod <- Arima(c(train[i:length(train)],validation[1:i]), 
                  model=mod1_reg, xreg=xreg[i:(length(train)+i),])
temp_forecast <- forecast(temp_mod, h=365, 
                          xreg=xreg[(length(train)+i+1):(length(train)+i+364+1),])$mean
valid <- validation[(i+1):(i+365)]
mae <- mean(abs(temp_forecast - valid)/valid)

p4 <- ggplot() +
  autolayer(xts_to_ts(valid), series="Real",size=1) +
  autolayer(ts(temp_forecast, start=start(xts_to_ts(valid)),
               frequency=frequency(xts_to_ts(valid))),
            series="ARIMA", size=1, alpha=0.7) +
  xlab("Time") + ylab("Value") + ggtitle(paste0("With ", i, " Lags of validation, MAPE ",round(mae,4)*100,"%"))

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)



########################
########################
########################
##########UCM###########
########################
########################


ytrain <- as.numeric(train)

data.frame(Data=df$Data) %>%
  mutate(Christmas = as.numeric(as.numeric(Data %in% natale))) %>%
  mutate(Ferragosto = as.numeric(as.numeric(Data %in% ferragosto))) %>%
  mutate(Easter = as.numeric(as.numeric(Data %in% pasqua))) %>%
  mutate(Other = as.numeric(Data %in% altre)) %>%
  mutate(WeekDay = data.table::wday(df$Data)) %>%
  mutate(Month = data.table::month(df$Data)) %>%
  select(-starts_with("Data"))  -> more_reg

#Si comporta peggio se si usano i regressori con i quali XARIMA funzionava meglio!?
ucm_mod1 <- SSModel(ytrain ~  Christmas + Ferragosto + Easter +
                      Other + WeekDay +
                      SSMtrend(2, list(NA,NA)) +
                      SSMseasonal(7, NA, "dummy") +
                      SSMseasonal(365, NA, "trig",
                                  harmonics = 1:24),
                    H = NA, data=more_reg[1:length(ytrain),])

vary <- var(ytrain, na.rm = TRUE)
ucm_mod1$P1inf <- ucm_mod1$P1inf * 0
ucm_mod1$a1[1] <- mean(ytrain, na.rm = TRUE)
diag(ucm_mod1$P1) <- vary



# Initial values for the variances we have to estimate
init <- numeric(5)
init[1] <- log(vary/10) # log-var(dist.rw)
init[2] <- log(vary/10)
init[3] <- log(vary/100)# log-var(dist.seas)
init[4] <- log(vary/100)# log-var(trig.seas)
init[5] <- log(vary/10) # log-var(err.oss.)

# Estimate
update_fun <- function(pars, model){
  model$Q[1, 1, 1] <- exp(pars[1])
  model$Q[2, 2, 1] <- exp(pars[2])
  model$Q[3, 3, 1] <- exp(pars[3])
  diag(model$Q[4:51, 4:51, 1]) <- exp(pars[4])
  model$H[1, 1, 1] <- exp(pars[5])
  model
}

fit1 <- fitSSM(ucm_mod1, init, update_fun)
print(fit1$optim.out$convergence)

### i=0
data <- c(rep(NA, 365))
temp_mod <- SSModel(data ~  Christmas + Ferragosto + Easter +
                      Other + WeekDay +
                      SSMtrend(2, list(fit1$model$Q[1,1,1],fit1$model$Q[2,2,1])) +
                      SSMseasonal(7, fit1$model$Q[3,3,1], "dummy") +
                      SSMseasonal(365, fit1$model$Q[4, 4, 1], "trig",
                                  harmonics = 1:24),
                    H = fit1$model$H, 
                    data=more_reg[(length(train)+1):(length(train)+365),])
ucm_pred <- predict(fit1$model, newdata=temp_mod, n.ahead=365)[1:365]
valid <- as.numeric(validation)[1:365]

tot_score_ucm <- mean(abs(ucm_pred - valid)/valid)

for (i in 1:(length(validation) - 365)){
  data <- c(as.numeric(validation[1:i]), rep(NA, 365))
  temp_mod <- SSModel(data ~  Christmas + Ferragosto + Easter +
                        Other + WeekDay +
                        SSMtrend(2, list(fit1$model$Q[1,1,1],fit1$model$Q[2,2,1])) +
                        SSMseasonal(7, fit1$model$Q[3,3,1], "dummy") +
                        SSMseasonal(365, fit1$model$Q[4, 4, 1], "trig",
                                    harmonics = 1:24),
                      H = fit1$model$H, 
                      data=more_reg[(length(train)+1):(length(train)+i+365),])
  #ucm_pred <- predict(fit1$model, newdata=temp_mod, n.ahead=365)[i:(i+365)]
  ucm_pred <- predict(fit1$model, newdata=temp_mod, n.ahead=365)[(i+1):(i+365)]
  valid <- as.numeric(validation)[(i+1):(i+365)]
  score <- mean(abs(ucm_pred - valid)/valid)
  tot_score_ucm <- tot_score_ucm + score
}
print(tot_score_ucm/(length(validation) - 365 + 1))


ucm_mod2 <- SSModel(ytrain ~ SSMtrend(2, list(NA,NA)) +
                      SSMseasonal(7, NA, "dummy") +
                      SSMseasonal(365, NA, "trig",
                                  harmonics = 1:24),
                    H = NA)

vary <- var(ytrain, na.rm = TRUE)
ucm_mod2$P1inf <- ucm_mod2$P1inf * 0
ucm_mod2$a1[1] <- mean(ytrain, na.rm = TRUE)
diag(ucm_mod2$P1) <- vary



# Initial values for the variances we have to estimate
init <- numeric(5)
init[1] <- log(vary/10) # log-var(dist.rw)
init[2] <- log(vary/10)
init[3] <- log(vary/100)# log-var(dist.seas)
init[4] <- log(vary/100)# log-var(trig.seas)
init[5] <- log(vary/10) # log-var(err.oss.)

# Estimate
update_fun <- function(pars, model){
  model$Q[1, 1, 1] <- exp(pars[1])
  model$Q[2, 2, 1] <- exp(pars[2])
  model$Q[3, 3, 1] <- exp(pars[3])
  diag(model$Q[4:51, 4:51, 1]) <- exp(pars[4])
  model$H[1, 1, 1] <- exp(pars[5])
  model
}

fit2 <- fitSSM(ucm_mod2, init, update_fun)
print(fit2$optim.out$convergence)

data <- c(rep(NA, 365))
temp_mod <- SSModel(data ~  SSMtrend(2, list(fit2$model$Q[1,1,1],fit2$model$Q[2,2,1])) +
                      SSMseasonal(7, fit2$model$Q[3,3,1], "dummy") +
                      SSMseasonal(365, fit2$model$Q[4, 4, 1], "trig",
                                  harmonics = 1:24),
                    H = fit2$model$H)
ucm_pred <- predict(fit2$model, newdata=temp_mod)[1:365]
valid <- as.numeric(validation)[1:365]

tot_score_ucm <- mean(abs(ucm_pred - valid)/valid)

for (i in 1:(length(validation) - 365)){
  data <- c(as.numeric(validation[1:i]), rep(NA, 365))
  temp_mod <- SSModel(data ~ SSMtrend(2, list(fit2$model$Q[1,1,1],fit2$model$Q[2,2,1])) +
                        SSMseasonal(7, fit2$model$Q[3,3,1], "dummy") +
                        SSMseasonal(365, fit2$model$Q[4, 4, 1], "trig",
                                    harmonics = 1:24),
                      H = fit2$model$H)
  ucm_pred <- predict(fit2$model, newdata=temp_mod)[(i+1):(i+365)]
  valid <- as.numeric(validation)[(i+1):(i+365)]
  score <- mean(abs(ucm_pred - valid)/valid)
  tot_score_ucm <- tot_score_ucm + score
}
print(paste0("UCM average MAPE:", tot_score_ucm/(length(validation) - 365 + 1)))
ucm_score <- tot_score_ucm/(length(validation) - 365 + 1)*100

i=1
data <- c(as.numeric(validation[1:i]), rep(NA, 365))
temp_mod <- SSModel(data ~ SSMtrend(2, list(fit2$model$Q[1,1,1],fit2$model$Q[2,2,1])) +
                      SSMseasonal(7, fit2$model$Q[3,3,1], "dummy") +
                      SSMseasonal(365, fit2$model$Q[4, 4, 1], "trig",
                                  harmonics = 1:24),
                    H = fit2$model$H)
temp_forecast <- predict(fit2$model, newdata=temp_mod)[(i+1):(i+365)]
valid <- validation[(i+1):(i+365)]
mae <- mean(abs(temp_forecast - valid)/valid)

p1 <- ggplot() +
  autolayer(xts_to_ts(valid), series="Vero",size=1) +
  autolayer(ts(temp_forecast, start=start(xts_to_ts(valid)),
               frequency=frequency(xts_to_ts(valid))),
            series="UCM", size=1, alpha=0.7) +
  xlab("Time") + ylab("Value") + ggtitle(paste0("With ", i, " Lags of validation, MAPE ",round(mae,4)*100,"%"))

i=90

data <- c(as.numeric(validation[1:i]), rep(NA, 365))
temp_mod <- SSModel(data ~ SSMtrend(2, list(fit2$model$Q[1,1,1],fit2$model$Q[2,2,1])) +
                      SSMseasonal(7, fit2$model$Q[3,3,1], "dummy") +
                      SSMseasonal(365, fit2$model$Q[4, 4, 1], "trig",
                                  harmonics = 1:24),
                    H = fit2$model$H)
temp_forecast <- predict(fit2$model, newdata=temp_mod)[(i+1):(i+365)]
valid <- validation[(i+1):(i+365)]
mae <- mean(abs(temp_forecast - valid)/valid)

p2 <- ggplot() +
  autolayer(xts_to_ts(valid), series="Vero",size=1) +
  autolayer(ts(temp_forecast, start=start(xts_to_ts(valid)),
               frequency=frequency(xts_to_ts(valid))),
            series="UCM", size=1, alpha=0.7) +
  xlab("Time") + ylab("Value") + ggtitle(paste0("With ", i, " Lags of validation, MAPE ",round(mae,4)*100,"%"))

i=160

data <- c(as.numeric(validation[1:i]), rep(NA, 365))
temp_mod <- SSModel(data ~ SSMtrend(2, list(fit2$model$Q[1,1,1],fit2$model$Q[2,2,1])) +
                      SSMseasonal(7, fit2$model$Q[3,3,1], "dummy") +
                      SSMseasonal(365, fit2$model$Q[4, 4, 1], "trig",
                                  harmonics = 1:24),
                    H = fit2$model$H)
temp_forecast <- predict(fit2$model, newdata=temp_mod)[(i+1):(i+365)]
valid <- validation[(i+1):(i+365)]
mae <- mean(abs(temp_forecast - valid)/valid)

p3 <- ggplot() +
  autolayer(xts_to_ts(valid), series="Vero",size=1) +
  autolayer(ts(temp_forecast, start=start(xts_to_ts(valid)),
               frequency=frequency(xts_to_ts(valid))),
            series="UCM", size=1, alpha=0.7) +
  xlab("Time") + ylab("Value") + ggtitle(paste0("With ", i, " Lags of validation, MAPE ",round(mae,4)*100,"%"))

i=183
data <- c(as.numeric(validation[1:i]), rep(NA, 365))
temp_mod <- SSModel(data ~ SSMtrend(2, list(fit2$model$Q[1,1,1],fit2$model$Q[2,2,1])) +
                      SSMseasonal(7, fit2$model$Q[3,3,1], "dummy") +
                      SSMseasonal(365, fit2$model$Q[4, 4, 1], "trig",
                                  harmonics = 1:24),
                    H = fit2$model$H)
temp_forecast <- predict(fit2$model, newdata=temp_mod)[(i+1):(i+365)]
valid <- validation[(i+1):(i+365)]
mae <- mean(abs(temp_forecast - valid)/valid)

p4 <- ggplot() +
  autolayer(xts_to_ts(valid), series="Vero",size=1) +
  autolayer(ts(temp_forecast, start=start(xts_to_ts(valid)),
               frequency=frequency(xts_to_ts(valid))),
            series="UCM", size=1, alpha=0.7) +
  xlab("Time") + ylab("Value") + ggtitle(paste0("With ", i, " Lags of validation, MAPE ",round(mae,4)*100,"%"))

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)
