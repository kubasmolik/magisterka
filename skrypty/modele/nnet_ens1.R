
#### ENSEMBLER 1 ####
#### MODEL NNET ZE ZMIENNYMI WOE ####

df <- train_clean %>% select(., default, one_of(zmienne_ens1_woe))

ctrl_nnet <- ctrl
ctrl_nnet$sampling <- "down"

parametry <- expand.grid(
   size = seq(4,8,1),
   decay = c(10^seq(-4, -1, 1)),
   #decay = 10^(-4),
   bag = TRUE
)

# zrownoleglenie obliczen
cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
nnet_ens1 <- caret::train(default ~ ., 
                     data = df,
                     method = "avNNet", 
                     metric = "AUPRC",
                     trControl = ctrl_nnet,
                     tuneGrid = parametry)
stopCluster(cl)

nnet_ens1
nnet_ens1$results
plot(nnet_ens1)
nnet_ens1$results[which.max(nnet_ens1$results$AUPRC), ]
nnet_ens1$bestTune

# ocena na zbiorze treningowym
train_eval <- ocena_train(nnet_ens1)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zmienne_ens1_woe))

test_eval <- ocena_test(zb_testowy = df, model = nnet_ens1, nazwa_modelu = "nnet_ens1")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = nnet_ens1, proby_boot = bootstrap,
                              nazwa_modelu = "nnet_ens1")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "nnet_ens1.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu nnet_ens1",
                                dynamicdate = Sys.Date())
)

nnet_ens1_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
nnet_ens1_prognoza <- predict(nnet_ens1, df, type = "prob")
nnet_ens1_prognoza$obs <- df$default
nnet_ens1_prognoza$pred <- forcats::as_factor(
   ifelse(nnet_ens1_prognoza$default >= 0.5, "default", "no_default")
)
nnet_ens1_prognoza$pred <- relevel(nnet_ens1_prognoza$pred, ref = "default")

# wyciagniecie prognoz modelu na zbiorze treningowym
nnet_ens1_train <- nnet_ens1$pred %>% dplyr::select(., rowIndex, default) %>% 
   dplyr::rename( nnet_ens1 = default)

# zapis gotowego modelu
save(nnet_ens1, file = "./dane/modele/nnet_ens1.RData")

# zapis prognoz na zbiorze treningowym
save(nnet_ens1_train, file = "./dane/modele/nnet_ens1_train.RData")

# zapis prognozy i oceny bootstrapowej
save(nnet_ens1_eval_boot, nnet_ens1_prognoza, file = "./dane/modele/nnet_ens1_ocena.RData")

# czyszczenie srodowiska
rm(nnet_ens1, nnet_ens1_eval_boot, nnet_ens1_prognoza,
   train_eval, test_eval, boot_eval, cl, df)
