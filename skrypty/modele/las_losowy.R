#### RANDOM FOREST ####

# customizacja random forest
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
   randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
   predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
   predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# pomocniczy zb danych
df <- train_clean %>% select(., default, one_of(zmienne_model_poj))

# modyfikacja funkcji kontrolnej
ctrl_rf <- ctrl
ctrl_rf$sampling <- "down"

floor(sqrt(ncol(df) + 5))

# na tym dobrze działa 2, 200 i down
# parametry <- expand.grid(
#    mtry = c(2),
#    ntree = seq(50,250, by = 50)
# )
# seq(50,250, by = 50)

parametry <- expand.grid(
   mtry = c(2,3),
   ntree = seq(50,250, by = 50)
)

# zrownoleglenie obliczen
cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
las_losowy <- caret::train(default ~ ., 
                           data = df,
                           method = customRF, 
                           metric = "AUPRC",
                           #trControl = ctrl,
                           trControl = ctrl_rf,
                           tuneGrid = parametry)
stopCluster(cl)


las_losowy
las_losowy$results
plot(las_losowy)
las_losowy$results[which.max(las_losowy$results$AUPRC), ]
las_losowy$bestTune


# ocena na zbiorze treningowym
train_eval <- ocena_train(las_losowy)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zmienne_model_poj))

test_eval <- ocena_test(zb_testowy = df, model = las_losowy, nazwa_modelu = "las_losowy")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = las_losowy, proby_boot = bootstrap, 
                              nazwa_modelu = "las_losowy")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "las_losowy.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu las_losowy",
                                dynamicdate = Sys.Date())
)

las_losowy_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
las_losowy_prognoza <- predict(las_losowy, df, type = "prob")
las_losowy_prognoza$obs <- df$default
las_losowy_prognoza$pred <- forcats::as_factor(
   ifelse(las_losowy_prognoza$default >= 0.5, "default", "no_default")
)
las_losowy_prognoza$pred <- relevel(las_losowy_prognoza$pred, ref = "default")

# zapis gotowego modelu
save(las_losowy, file = "./dane/modele/las_losowy.RData")

# zapis prognozy i oceny bootstrapowej
save(las_losowy_eval_boot, las_losowy_prognoza, file = "./dane/modele/las_losowy_ocena.RData")

# czyszczenie srodowiska
rm(las_losowy, las_losowy_eval_boot, las_losowy_prognoza,
   train_eval, test_eval, boot_eval, cl, df)