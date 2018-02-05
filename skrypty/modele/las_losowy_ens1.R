#### ENSEMBLER 1 ####
#### las_losowy_ens1 ####

df <- train_clean %>% select(., default, one_of(zmienne_ens1))

# modyfikacja funkcji kontrolnej
ctrl_rf <- ctrl
ctrl_rf$sampling <- "down"

floor(sqrt(ncol(df) + 6))

parametry <- expand.grid(
   mtry = c(2,3, 4),
   ntree = seq(50,250, by = 50)
)

# zrownoleglenie obliczen
cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
las_losowy_ens1 <- caret::train(default ~ ., 
                           data = df,
                           method = customRF, 
                           metric = "AUPRC",
                           #trControl = ctrl,
                           trControl = ctrl_rf,
                           tuneGrid = parametry)
stopCluster(cl)

las_losowy_ens1
las_losowy_ens1$results
plot(las_losowy_ens1)
las_losowy_ens1$results[which.max(las_losowy_ens1$results$AUPRC), ]
las_losowy_ens1$bestTune

# ocena na zbiorze treningowym
train_eval <- ocena_train(las_losowy_ens1)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zmienne_ens1))

test_eval <- ocena_test(zb_testowy = df, model = las_losowy_ens1, nazwa_modelu = "las_losowy_ens1")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = las_losowy_ens1, proby_boot = bootstrap, 
                              nazwa_modelu = "las_losowy_ens1")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "las_losowy_ens1.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu las_losowy_ens1",
                                dynamicdate = Sys.Date())
)

las_losowy_ens1_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
las_losowy_ens1_prognoza <- predict(las_losowy_ens1, df, type = "prob")
las_losowy_ens1_prognoza$obs <- df$default
las_losowy_ens1_prognoza$pred <- forcats::as_factor(
   ifelse(las_losowy_ens1_prognoza$default >= 0.5, "default", "no_default")
)
las_losowy_ens1_prognoza$pred <- relevel(las_losowy_ens1_prognoza$pred, ref = "default")

# wyciagniecie prognoz modelu na zbiorze treningowym
las_losowy_ens1_train <- las_losowy_ens1$pred %>% dplyr::select(., rowIndex, default) %>% 
   dplyr::rename( las_losowy_ens1 = default)

# zapis gotowego modelu
save(las_losowy_ens1, file = "./dane/modele/las_losowy_ens1.RData")

# zapis prognoz na zbiorze treningowym
save(las_losowy_ens1_train, file = "./dane/modele/las_losowy_ens1_train.RData")

# zapis prognozy i oceny bootstrapowej
save(las_losowy_ens1_eval_boot, las_losowy_ens1_prognoza, file = "./dane/modele/las_losowy_ens1_ocena.RData")

# czyszczenie srodowiska
rm(las_losowy_ens1, las_losowy_ens1_eval_boot, las_losowy_ens1_prognoza, 
   train_eval, test_eval, boot_eval, cl, df)