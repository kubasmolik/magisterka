#### ENSEMBLER 1 ####
#### xgb_ens1 ####

df <- train_clean %>% select(., default, one_of(zmienne_ens1))

parametry <- expand.grid(
   nrounds = c(100, 200, 300),
   max_depth = c(2,3,4,5,6),
   eta = c(0.1, 0.2, 0.3),
   gamma = c(0, 1),
   colsample_bytree = c(0.7),
   min_child_weight = c(1),
   subsample = c(0.7)
)

cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
xgb_ens1 <- caret::train(default ~ ., 
                          data = df,
                          method = "xgbTree",
                          metric = "AUPRC",
                          trControl = ctrl,
                          tuneGrid = parametry
)

stopCluster(cl)

xgb_ens1
xgb_ens1$results
plot(xgb_ens1)
xgb_ens1$results[which.max(xgb_ens1$results$AUPRC), ]
xgb_ens1$bestTune

# ocena na zbiorze treningowym
train_eval <- ocena_train(xgb_ens1)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zmienne_ens1))

test_eval <- ocena_test(zb_testowy = df, model = xgb_ens1, nazwa_modelu = "xgb_ens1")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = xgb_ens1, proby_boot = bootstrap, 
                              nazwa_modelu = "xgb_ens1")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "xgb_ens1.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu xgb_ens1",
                                dynamicdate = Sys.Date())
)

xgb_ens1_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
#df <- test %>% select(., default, one_of(zm_numeric), ends_with("woe"))
xgb_ens1_prognoza <- predict(xgb_ens1, df, type = "prob")
xgb_ens1_prognoza$obs <- df$default
xgb_ens1_prognoza$pred <- forcats::as_factor(
   ifelse(xgb_ens1_prognoza$default >= 0.5, "default", "no_default")
)
xgb_ens1_prognoza$pred <- relevel(xgb_ens1_prognoza$pred, ref = "default")

# wyciagniecie prognoz modelu na zbiorze treningowym
xgb_ens1_train <- xgb_ens1$pred %>% dplyr::select(., rowIndex, default) %>% 
   dplyr::rename( xgb_ens1 = default)

# zapis gotowego modelu
save(xgb_ens1, file = "./dane/modele/xgb_ens1.RData")

# zapis prognoz na zbiorze treningowym
save(xgb_ens1_train, file = "./dane/modele/xgb_ens1_train.RData")

# zapis prognozy i oceny bootstrapowej
save(xgb_ens1_eval_boot, xgb_ens1_prognoza, file = "./dane/modele/xgb_ens1_ocena.RData")

# czyszczenie srodowiska
rm(xgb_ens1, xgb_ens1_eval_boot, xgb_ens1_prognoza, 
   train_eval, test_eval, boot_eval, cl, df)