
#### XGBOOST  ####

df <- train_clean %>% select(., default, one_of(zmienne_model_poj))

parametry <- expand.grid(
   nrounds = c(100, 200, 300),
   max_depth = c(3,4,5,6),
   eta = c(0.1, 0.2, 0.3),
   gamma = c(0, 1),
   colsample_bytree = c(0.7),
   min_child_weight = c(1),
   subsample = c(0.7)
)

# parametry <- expand.grid(
#    nrounds = c(100),
#    max_depth = c(6),
#    eta = c( 0.3),
#    gamma = c(0),
#    colsample_bytree = c(0.7),
#    min_child_weight = c(1),
#    subsample = c(0.7)
# )

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
   train_eval, test_eval, boot_eval, cl, df)
