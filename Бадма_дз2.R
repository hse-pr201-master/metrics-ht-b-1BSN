library(tidyverse)
library(lmtest)
library(memisc)
library(car)
library(psych)
library(sjPlot)
library(MASS)
library(caret)
library(mlbench)
library(ggstatsplot)
library(pls)
library(mctest)
library(perturb)
library(sandwich)
library(broom)
library(Hmisc)
library(AER)
library(fabletools)
library(fable)
library(glmnet)
set.seed(5)
setwd("~/Documents")

# Часть 1

# Задание 1 Исследовательский вопрос
# В своём дз я исследую зависимость уровня детской смертности от макроэкономических
# Факторов, я опираюсь на работу A Panel Study of Infant and Child Mortality in Less- Developed Countries, 1990–2005
# Ссылка: https://www.tandfonline.com/doi/pdf/10.1080/10875549.2010.517073?casa_token=0BLL_1_nO3YAAAAA:wWiNfUk4eRv30gRs-20oGEWMalyo3yWvSXAZ6hb-89l_-Ij77kC1daEaDuSicvVzY9FOZ2jLHus9kA
# В этой работе была выполнена регрессия уровня детской смертности на ВВП на душу населения, процентное соотношение жителей городов
# В стране, а также экспорт в процентах от ВВП
# Задачей своего дз я ставлю нахождение иных качественных регрессоров уровня детской смертности
# Дополнительная задача: посмотреть, значимы ли различия во влиянии доли экспорта в ВВП в завиимости от географических регионов

# Задание 2 Загрузка данных и обработка переменных
d <- read.csv('datacountry.csv', header = TRUE, sep = ',')
d <- d[c(1, 2, 5, 6, 8, 9, 10, 11, 12, 16, 17, 19, 23, 24, 25, 28, 32, 35,
           36, 40, 42, 46, 48, 20, 7)] # 23
colnames(d) <- c('country', 'region', 'popdens', 'sexratio',
                  'gdpgrowth', 'gdppc', 'agriculture', 'industry',
                  'services', 'unemp', 'laborpart', 'foodprod',
                  'currentac', 'popgrowth', 'urbanratio',
                  'lifeexp', 'infantmort', 'educspending', 'educ', 'mobilesubs', 'webusers',
                  'energyprod', 'drinkingwater', 'exports', 'gdp')

describe(d)[3:10] # Данные грязные, уберу переменные, которые не представляют интерес
d$lifeexp <- NULL
d$laborpart <- NULL
d$educ <- NULL
d$drinkingwater <- NULL
d[d == '...'] <- NA
d <- impute(d, mean) # Заполняю пропуски средним
d$currentac <- gsub('~', '', d$currentac)
d$exports <- gsub('~', '', d$exports)
d['exports'] <- apply(d['exports'], 1, as.numeric)
d['gdp'] <- apply(d['gdp'], 1, as.numeric)
d['currentac'] <- apply(d['currentac'], 1, as.numeric)
d <- mutate(d, exportsshare = exports / gdp, currentacshare = currentac / gdp)
d$exports <- NULL # Новые переменные -- в процентах от ВВП
d$gdp <- NULL # Удаляю старые перменные
d$currentac <- NULL
d$country <- NULL
AS <- c('SouthernAsia', 'WesternAsia', 'Oceania', 'South-easternAsia',
        'EasternAsia', '	Polynesia', '	Melanesia', 'CentralAsia')
EUR <- c('NorthernEurope', 'EasternEurope', 'WesternEurope', 'SouthernEurope', 'CentralEurope')
NAMR <- c('NorthernAmerica')
LAMR <- c('CentralAmerica', 'SouthernAmerica', 'Caribbean')
AFR <- c('WesternAfrica', 'EasternAfrica', 'MiddleAfrica', 'NorthernAfrica', 'SouthernAfrica')
f <- function(x){
  if (x %in% AS)
    return ("asia")
  else if (x %in% EUR)
    return ("europe")
  else if (x %in% NAMR)
    return ("noramerica")
  else if (x %in% LAMR)
    return ('latamerica')
  else
    return ("africa")
}
d[1] <- apply(d[1], 1, f) # Модифицирую категориальную переменную -- уменьшу кол-во категорий
d[1] <- apply(d[1], 1, as.factor)
d[3] <- impute(d[3], median)
d[4] <- apply(d[4], 1, as.numeric)
d[4] <- impute(d[4], median)
d[6] <- apply(d[6], 1, as.numeric)
d[6] <- impute(d[6], median)
d[9] <- apply(d[9], 1, as.numeric)
d[11] <- apply(d[11], 1, as.numeric)
d[11] <- impute(d[11], median)
d[13] <- apply(d[13], 1, as.numeric) # Перевод переменных в правильный формат
d[14] <- apply(d[14], 1, as.numeric)
d[15] <- apply(d[15], 1, as.numeric)
str(d) # Итого: 1 качественная и 17 непрерывных
d <- filter(d, mobilesubs > 0)
d <- filter(d, gdppc > 0)
d <- filter(d, infantmort > 0)
d <- mutate(d, logpopdens = log(popdens), logsex = log(sexratio), loggdppc = log(gdppc),
                   logindustry = log(industry), logservices = log(services), logurban = log(urbanratio),
                   logweb = log(webusers), logmobile = log(mobilesubs), loginfmort = log(infantmort),
            logenergy = log(energyprod)) # Нелинейные преобразования переменных
d$energyprod <- NULL # Удаляю старые переменные
d$infantmort <- NULL
d$popdens <- NULL
d$sexratio <- NULL
d$gdppc <- NULL
d$industry <- NULL
d$services <- NULL
d$urbanratio <- NULL
d$webusers <- NULL
d$mobilesubs <- NULL
d$agriculture <- NULL
d <- drop_na(d) # Удаляю оставшиеся NA, связанные с плохим форматом данных
# Я завершил преобразования переменных, перехожу к отбору регрессоров
full.model <- lm(loginfmort~. , data = d)
step.model <- stepAIC(full.model, direction = "both", 
                          trace = FALSE)
summary(step.model)
model <- lm(loginfmort ~foodprod + popgrowth + region:exportsshare + 
              loggdppc + logindustry + logservices + logurban -1, data = d)
summary(model) # Я применил небольшой хитрый трюк, а именно провёл процедуру последовательного
# исключения регрессоров по критерию AIC,
# По моему мнению, в данном случае эта процедура по выделению регрессоров оправдана,
# Потому что: 1) выделенные регрессоры имеют экономический смысл (производство пищи, рост населения,
# Доля экспорта, регион, доля промышленности, доля сектора услаг, доля городского населения),
# 2) Все регрессоры высокозначимы, R^2 adj = 0.9
resettest(model) # На 5% УЗ спецификация модели выбрана корректно, нет пропущенных регрессоров

# Задание 3 Визуальный анализ данных
ggplot(d, aes(x=loginfmort)) + 
  geom_histogram(aes(y=..density..), col = 'darkred') + 
  geom_density() + ggtitle('Гистограмма уровней детской смертности') + xlab('Детская смертность') +
  ylab('Частотность') + theme_classic()
boxplot(d$loginfmort, main = 'Ящичковая диаграмма детской смертности', ylab = 'Показатель смертности')
# Как можно видеть, выбросы целевой переменной несущественные, поэтому ничего не буду удалять
ggplot(d, aes(region, loginfmort, fill = region)) + geom_boxplot() + ggtitle('Ящичковая диаграмма детской смертности') +
  xlab("Регионы") + ylab("Смертность") #+ theme_classic()
# Смертность чётко различается по регионам, поэтому эта качественная переменная очень важна
ggplot(d, aes(exportsshare, loginfmort)) + geom_jitter() + stat_smooth() + 
  ggtitle('Диаграмма рассеяния детской смертности и доли экспорта в ВВП') + 
  xlab('Доля экспорта') + ylab('Детская смертность') + theme_classic()
ggplot(d, aes(exportsshare, loginfmort, col = region)) + geom_jitter() +
  ggtitle('Диаграмма рассеяния детской смертности и доли экспорта в ВВП') + 
  xlab('Доля экспорта') + ylab('Детская смертность') + theme_classic()
# Видна хороша дифференциация по регионам, поэтому имеет смысл использовать новый регрессор:
# Доля экспорта * Регион
ggplot(d, aes(logurban, loginfmort)) + geom_jitter() + stat_smooth() +
  ggtitle('Диаграмма рассеяния городского населения и детской смертности') +
  xlab('Доля городского населения (лог)') + ylab('Детская смертность') + theme_classic()
ggplot(d, aes(foodprod, loginfmort)) + geom_jitter() + stat_smooth() +
  ggtitle('Диаграмма рассеяния производства еды и детской смертности') +
  xlab('Объём произв.еды (лог)') + ylab('Детская смертность') + theme_classic()
ggplot(d, aes(popgrowth, loginfmort)) + geom_jitter() + stat_smooth() +
  ggtitle('Диаграмма рассеяния темпов роста населения и детской смертности') +
  xlab('Темпы прироста населения') + ylab('Детская смертность') + theme_classic()

# Задание 4 Тесты на мультиколлинеарность, гетероскедастичность, эндогенность
# Для применения тестов я сперва оценю базовую модель
model_base <- lm(loginfmort ~foodprod + popgrowth + region:exportsshare + 
                   loggdppc + logindustry + logservices + logurban, data = d)
summary(model_base)
plot(model_base$fitted.values, type = 'l', col = 'red')
lines(d$loginfmort, col="blue", type="l") # Как можно видеть, модель обладает высокой объясеяющей силой,
# R^2 adj = 0.8842, красным выделены y_hat, синим y
# Тестирование на мульиколлинеарность
vif(model_base) # Мультиколлинеарность не обнаружена
olsrr::ols_eigen_cindex(model_base) # В соответствии с CI коллинеарность в данных слабо выражена
# Тестирование на гетероскедастичность
bptest(model_base) # Наличие гетероскедастичности не подтверждается в соотвествии с тестом Бройша-Пагана
ncvTest(model_base) # NCV тест также не указывает на гетероскедастичность
# Тестирование на эндогенность
# Темпы прироста численности населения могут быть эндогенной переменной, темп прироста может порождаться
# DGP одновременно с целевой перменной
cov(d$popgrowth, d$logsex) # В качестве инструмента я буду использовать логарифм соотношения полов
model_iv <- ivreg(loginfmort ~ foodprod + popgrowth + region:exportsshare + 
                    loggdppc + logindustry + logservices + logurban | foodprod + logsex + region:exportsshare + 
                    loggdppc + logindustry + logservices + logurban, data = d)
summary(model_iv)
# Тест Дарбина-Ву-Хансмана 
e <- model_base$res
## Step 2. Regress every endogenous regressor (in this case only educ) 
## on z (constant, fatheduc, motheduc, exper, expersq) and obtain residuals. Done!
aux1 <- lm(popgrowth ~ logsex, data = d)
v <- aux1$res
aux2 <- lm(e ~ foodprod + popgrowth + region:exportsshare + 
             loggdppc + logindustry + logservices + logurban + v, data = d)
LM <- length(aux2$res) * summary(aux2)$r.squared
pchisq(q = LM, df = 1, lower.tail = F) # p-value 0.98, H_0: Все регрессоры экзогенны
# Тест Хаусмана (ссылка: https://klein.uk/teaching/quant2/docs/Lec5.html)
haustest <- function(model.iv, model.ols){
  cf_diff <- coef(model.iv) - coef(model.ols)
  vc_diff <- vcovHC(model.iv, "HC0") - vcovHC(model.ols, "HC0")
  x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
  pchisq(q = x2_diff, df = dim(vc_diff)[1], lower.tail = F)
}
# execute dwh.test-function:
haustest(model_iv, model_base) # H_0: Все регрессоры экзогенны
# Таким образом, эндогенности нет
# По всей видимости, я преобразовал исходные данные правильным образом, кроме того, выбрал корректную спецификацию
# OLS предоставит хорошие результаты

# Задание 5 Оценка моделей
model_base <- lm(loginfmort ~foodprod + popgrowth + region:exportsshare + 
                   loggdppc + logindustry + logservices + logurban, data = d)
summary(model_base)
# Значимы все переменные, модель в целом высокозначима, высокий R^2 adj = 0.8842 -- модель справляется со
# Своими задачами хорошо
# Удалось решить исследовательскую задачу: были найдены значимые регрессоры для уровня детской смертности
# Детскую смертность понижают: ВВП на душу населения, доля сектора услуг в экономике, доля городского населения, доля экспорта в Азии
# Доля экспорта в Европе, доля экспорта в Северной Америке
# Детскую смертность повышают: производство еды, темпы прироста численности населения, доля промышленности в экономике,
# Доля экспорта в Африке, доля экспорта в Латинской Америке
# Интересно, что доля экспорта по-разному связана с детской смертностью на разных континетнах

# В качестве дополнительной модификации я оценю линейную регрессию с преобразованием Бокса-Кокса
boxcox_model <- boxcox(loginfmort ~foodprod + popgrowth + region:exportsshare + 
                         loggdppc + logindustry + logservices + logurban -1, data = d)
lambda <- boxcox_model$x
lik <- boxcox_model$y
sorted_bc <- cbind(lambda, lik)[order(-lik),]
head(sorted_bc, n = 10) # Наибольшее значение для лямбды 0.7
model_aux <- lm(loginfmort^(0.86) ~foodprod + popgrowth + region:exportsshare + 
                   loggdppc + logindustry + logservices + logurban -1, data = d)
summary(model_aux) # R^2 adj = 0.98
resettest(model_aux) # Пропущененных регрессоров нет
ks.test(model_aux$residuals, 'pnorm', mean = mean(model_aux$residuals), sd = sd(model_aux$residuals))
# Остатки распределены нормально по тесту Колмогорова-Смирнова

# Задание 6 MSE
X_ols <- d
numberOfTrainingSamples <- round(nrow(X_ols) * .8)
train_data_ols <- as.matrix(X_ols)[1:numberOfTrainingSamples,]
test_data_ols <- as.matrix(X_ols)[-(1:numberOfTrainingSamples),]
aux <- test_data_ols[, 1]
X_ols$region <- NULL
test_data_ols <- as.matrix(X_ols)[-(1:numberOfTrainingSamples),]
test_data_ols <- as.data.frame(test_data_ols)
test_data_ols <- apply(test_data_ols, 0, as.numeric)
str(test_data_ols)
test_data_ols['region'] <- as.factor(aux)
x_train <- model_base$model[c(2:9)]
y_train <- model_base$model[1]
train_df <- cbind(x_train, y_train)
x_test <- as.data.frame(test_data_ols)
x_test <- x_test[c(3, 4, 18, 6, 10, 11, 12, 13)]
y_test <- test_data_ols$loginfmort
test_df <- cbind(x_test, y_test)

ols_main <- lm(loginfmort ~foodprod + popgrowth + region:exportsshare + 
                 loggdppc + logindustry + logservices + logurban, data = train_df)
pred_ols_main <- predict(ols_main, test_df)
ols_score <- MLmetrics::MSE(pred_ols_main, test_data_ols$loginfmort)

wls <- lm(loginfmort ~foodprod + popgrowth + region:exportsshare + 
            loggdppc + logindustry + logservices + logurban, data = train_df, weights = 1/logservices^2)
pred_wls <- predict(wls, test_df)
wls_score <- MLmetrics::MSE(pred_wls, test_data_ols$loginfmort)

ols_boxcox <- lm(loginfmort^(0.86) ~foodprod + popgrowth + region:exportsshare + 
  loggdppc + logindustry + logservices + logurban, data = train_df)
pred_boxcox <- predict(ols_boxcox, test_df)
boxcox_score <- MLmetrics::MSE(pred_boxcox, test_data_ols$loginfmort)

ols_aux <- lm(loginfmort ~foodprod + popgrowth + exportsshare + 
                 region:loggdppc + logindustry + logservices + logurban, data = train_df)
pred_ols_aux <- predict(ols_aux, test_df)
ols_score_aux <- MLmetrics::MSE(pred_ols_aux, test_data_ols$loginfmort)

cbind(ols_score, wls_score, boxcox_score, ols_score_aux)
# Таким образом, я успешно выполнил задание

# Часть 2

library(forecast)
devtools::install_github("KevinKotze/tsm")
library(tsm) # Я пользуюсь этой библиотекой

# Задание 1 Генерирование рядов
ts_1 <- arima.sim(model = list(ar = 0.8), n = 120) # AR(1)
ts_2 <- arima.sim(model = list(ar = c(0.1, 0.2, 0.3)), n = 120) # AR(3)
ts_3 <- arima.sim(model = list(ma = c(1.2, 2)), n = 120) # MA(2)
plot.ts(ts_1, ylab = 'Values', main = 'AR(1)')
plot.ts(ts_2, ylab = 'Values', main = 'AR(3)')
plot.ts(ts_3, ylab = 'Values', main = 'MA(2)')
# Все ряды являются стационарными, это видно по графику, а также из решений
# Соответствующих прямых характеристических уравнений

# Задание 2 Ещё ряды
ts_4 <- arima.sim(model = list(order = c(0, 1, 2), ma = c(1.2, -0.1)), n = 120)
ts_5 <- arima.sim(model = list(order = c(0, 0, 0)), n = 120)
ts_6 <- arima.sim(model = list(order = c(3, 0, 0), ar = c(-0.2, -0.5, -0.6)), n = 120)
# Все сгенерированные мною ряды стационарны

# Задание 3 Случайное блуждание
random_walk <- arima.sim(model = list(order = c(0, 1, 0)), n = 120)
plot.ts(random_walk, ylab = 'Value', main = 'Random Walk')
# Стационарных решений нет

# Задание 4 ACF, PACF для AR(1), RW
tsm::ac(ts_1) # ACF ряда начинает экспоненциально убывать после 1 лага
# PACF имеет только одно отличное от нуля значение, соответствующее AR-коэффициенту
tsm::ac(random_walk) # ACF крайне персистентна, убывает слабо
# PACF указывает на коэффициент AR(1) = 1, что типично для процесса случайного блуждания

# Задание 5 ARIMA(2,0,3)
ts_last <- arima.sim(model = list(order = c(2, 0, 3), ar = c(0.8, 0.1), ma = c(-1.2, -0.8, 0.1)), n = 120)
plot.ts(ts_last, ylab = 'Value', main = 'ARMA(2, 3)')
train <- ts(ts_last[1:100], start = 1, end = 100)
test <- ts(ts_last[101:120], start = 101, end = 120)
model_ts <- forecast::Arima(train, order = c(2, 0, 3))
frcst <- forecast::forecast(model_ts, h = 20)
plot(frcst)
lines(test, type = 'l', col = 'red')
# С визуальной точки зрения качество модели ужасное, первые 3 прогноза ещё куда ни шло,
# Но с течением времени происходит схождение к среднему


