
#### PELNY MODEL LOGITOWY ZE ZMIENNYMI WOE ####

df <- train_clean %>% select(., default, one_of(zm_numeric), ends_with("woe"))

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 2,
                     summaryFunction = custom_summary,
                     classProbs = T,
                     verboseIter = T,
                     sampling = "up",
                     allowParallel = T)

# zrownoleglenie obliczen
cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
logit_full <- caret::train(default ~ ., 
                           data = df,
                           method = "glm",
                           family = binomial,
                           metric = "AUPRC",  
                           trControl = ctrl)
stopCluster(cl)

logit_full
logit_full$results
plot(logit_full)
logit_full$results[which.max(logit_full$results$AUPRC), ]
logit_full$bestTune

# ocena na zbiorze treningowym
train_eval <- ocena_train(logit_full)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zm_numeric), ends_with("woe"))

test_eval <- ocena_test(zb_testowy = df, model = logit_full, nazwa_modelu = "logit_full")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = logit_full, proby_boot = bootstrap, 
                              nazwa_modelu = "logit_full")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "logit_full.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu logit_full",
                                dynamicdate = Sys.Date())
)

logit_full_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
df <- test %>% select(., default, one_of(zm_numeric), ends_with("woe"))
logit_full_prognoza <- predict(logit_full, df, type = "prob")
logit_full_prognoza$obs <- df$default
logit_full_prognoza$pred <- forcats::as_factor(
   ifelse(logit_full_prognoza$default >= 0.5, "default", "no_default")
)
logit_full_prognoza$pred <- relevel(logit_full_prognoza$pred, ref = "default")

# zapis gotowego modelu
save(logit_full, file = "./dane/modele/logit_full.RData")

# zapis prognozy i oceny bootstrapowej
save(logit_full_eval_boot, logit_full_prognoza, file = "./dane/modele/logit_full_ocena.RData")

# czyszczenie srodowiska
rm(logit_full, logit_full_eval_boot, logit_full_prognoza, 
   train_eval, test_eval, boot_eval, ctrl, cl, df)


##########################################################################

#### ELASTIC NET ####

df <- train_clean %>% select(., default, one_of(zm_numeric), one_of(zm_factor))

parametry <- expand.grid(
   alpha = seq(0, 1, length.out = 20),
   lambda = seq(0, 0.1, length.out = 20)
)

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 2,
                     summaryFunction = custom_summary,
                     search = "grid",
                     classProbs = T,
                     verboseIter = T,
                     sampling = "up",
                     allowParallel = T)

# zrownoleglenie obliczen
cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
enet_full <- caret::train(default ~ ., 
                          data = df,
                          method = "glmnet", 
                          metric = "AUPRC",
                          trControl = ctrl, 
                          tuneGrid = parametry)
stopCluster(cl)

enet_full
enet_full$results
plot(enet_full)
enet_full$results[which.max(enet_full$results$AUPRC), ]
enet_full$bestTune

# ocena na zbiorze treningowym
train_eval <- ocena_train(enet_full)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zm_numeric), one_of(zm_factor))

test_eval <- ocena_test(zb_testowy = df, model = enet_full, nazwa_modelu = "enet_full")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = enet_full, proby_boot = bootstrap, 
                              nazwa_modelu = "enet_full")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "enet_full.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu enet_full",
                                dynamicdate = Sys.Date())
)

enet_full_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
#df <- test %>% select(., default, one_of(zm_numeric), ends_with("woe"))
enet_full_prognoza <- predict(enet_full, df, type = "prob")
enet_full_prognoza$obs <- df$default
enet_full_prognoza$pred <- forcats::as_factor(
   ifelse(enet_full_prognoza$default >= 0.5, "default", "no_default")
)
enet_full_prognoza$pred <- relevel(enet_full_prognoza$pred, ref = "default")

# zapis gotowego modelu
save(enet_full, file = "./dane/modele/enet_full.RData")

# zapis prognozy i oceny bootstrapowej
save(enet_full_eval_boot, enet_full_prognoza, file = "./dane/modele/enet_full_ocena.RData")

# czyszczenie srodowiska
rm(enet_full, enet_full_eval_boot, enet_full_prognoza, 
   train_eval, test_eval, boot_eval, ctrl, cl, df)

################################################################################

#### RANDOM FOREST ####


df <- train_clean %>% select(., default, one_of(zmienne_model_poj))

floor(sqrt(ncol(df) + 10))

parametry <- expand.grid(
   mtry = c(4,5,6)
)

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 2,
                     summaryFunction = custom_summary,
                     search = "grid",
                     classProbs = T,
                     verboseIter = T,
                     sampling = "up",
                     allowParallel = T)

# zrownoleglenie obliczen
cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
las_losowy <- caret::train(default ~ ., 
                           data = df,
                           method = "rf", 
                           metric = "AUPRC",
                           trControl = ctrl, 
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
#df <- test %>% select(., default, one_of(zm_numeric), ends_with("woe"))
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
   train_eval, test_eval, boot_eval, ctrl, cl, df)

#####################################################################################

#### XGBOOST  ####

df <- train_clean %>% select(., default, one_of(zmienne_model_poj))

parametry <- expand.grid(
   nrounds = c(100, 200, 300),
   max_depth = c(6),
   eta = c(0.1, 0.2, 0.3),
   gamma = c(0, 1),
   colsample_bytree = c(0.7),
   min_child_weight = c(1),
   subsample = c(0.7)
)

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 2,
                     summaryFunction = custom_summary,
                     search = "grid",
                     classProbs = T,
                     verboseIter = T,
                     sampling = "up",
                     allowParallel = T)

cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
xgboost <- caret::train(default ~ ., 
                          data = df,
                          method = "xgbTree",
                          metric = "AUPRC",
                          trControl = ctrl,
                          tuneGrid = parametry
)

stopCluster(cl)

xgboost
xgboost$results
plot(xgboost)
xgboost$results[which.max(xgboost$results$AUPRC), ]
xgboost$bestTune

# ocena na zbiorze treningowym
train_eval <- ocena_train(xgboost)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zmienne_model_poj))

test_eval <- ocena_test(zb_testowy = df, model = xgboost, nazwa_modelu = "xgboost")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = xgboost, proby_boot = bootstrap, 
                              nazwa_modelu = "xgboost")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "xgboost.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu xgboost",
                                dynamicdate = Sys.Date())
)

xgboost_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
#df <- test %>% select(., default, one_of(zm_numeric), ends_with("woe"))
xgboost_prognoza <- predict(xgboost, df, type = "prob")
xgboost_prognoza$obs <- df$default
xgboost_prognoza$pred <- forcats::as_factor(
   ifelse(xgboost_prognoza$default >= 0.5, "default", "no_default")
)
xgboost_prognoza$pred <- relevel(xgboost_prognoza$pred, ref = "default")

# zapis gotowego modelu
save(xgboost, file = "./dane/modele/xgboost.RData")

# zapis prognozy i oceny bootstrapowej
save(xgboost_eval_boot, xgboost_prognoza, file = "./dane/modele/xgboost_ocena.RData")

# czyszczenie srodowiska
rm(xgboost, xgboost_eval_boot, xgboost_prognoza, 
   train_eval, test_eval, boot_eval, ctrl, cl, df)