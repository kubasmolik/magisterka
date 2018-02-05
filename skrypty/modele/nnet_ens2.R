
#### ENSEMBLER 2 ####
#### MODEL NNET ZE ZMIENNYMI WOE ####

df <- train_clean %>% select(., default, one_of(zmienne_ens2_woe))

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
nnet_ens2 <- caret::train(default ~ ., 
                          data = df,
                          method = "avNNet", 
                          metric = "AUPRC",
                          trControl = ctrl_nnet,
                          tuneGrid = parametry)
stopCluster(cl)

nnet_ens2
nnet_ens2$results
plot(nnet_ens2)
nnet_ens2$results[which.max(nnet_ens2$results$AUPRC), ]
nnet_ens2$bestTune

# ocena na zbiorze treningowym
train_eval <- ocena_train(nnet_ens2)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zmienne_ens2_woe))

test_eval <- ocena_test(zb_testowy = df, model = nnet_ens2, nazwa_modelu = "nnet_ens2")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = nnet_ens2, proby_boot = bootstrap,
                              nazwa_modelu = "nnet_ens2")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "nnet_ens2.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu nnet_ens2",
                                dynamicdate = Sys.Date())
)

nnet_ens2_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
nnet_ens2_prognoza <- predict(nnet_ens2, df, type = "prob")
nnet_ens2_prognoza$obs <- df$default
nnet_ens2_prognoza$pred <- forcats::as_factor(
   ifelse(nnet_ens2_prognoza$default >= 0.5, "default", "no_default")
)
nnet_ens2_prognoza$pred <- relevel(nnet_ens2_prognoza$pred, ref = "default")

# wyciagniecie prognoz modelu na zbiorze treningowym
nnet_ens2_train <- nnet_ens2$pred %>% dplyr::select(., rowIndex, default) %>% 
   dplyr::rename( nnet_ens2 = default)

# zapis gotowego modelu
save(nnet_ens2, file = "./dane/modele/nnet_ens2.RData")

# zapis prognoz na zbiorze treningowym
save(nnet_ens2_train, file = "./dane/modele/nnet_ens2_train.RData")

# zapis prognozy i oceny bootstrapowej
save(nnet_ens2_eval_boot, nnet_ens2_prognoza, file = "./dane/modele/nnet_ens2_ocena.RData")

# czyszczenie srodowiska
rm(nnet_ens2, nnet_ens2_eval_boot, nnet_ens2_prognoza,
   train_eval, test_eval, boot_eval, cl, df)
