
#### ENSEMBLER 1 ####
#### MODEL LOGITOWY ZE ZMIENNYMI WOE ####

df <- train_clean %>% select(., default, one_of(zmienne_ens1_woe))

# zrownoleglenie obliczen
cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
logit_ens1 <- caret::train(default ~ ., 
                           data = df,
                           method = "glm",
                           family = binomial,
                           metric = "AUPRC",  
                           trControl = ctrl)
stopCluster(cl)

logit_ens1
logit_ens1$results

# ocena na zbiorze treningowym
train_eval <- ocena_train(logit_ens1)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zmienne_ens1_woe))

test_eval <- ocena_test(zb_testowy = df, model = logit_ens1, nazwa_modelu = "logit_ens1")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = logit_ens1, proby_boot = bootstrap, 
                              nazwa_modelu = "logit_ens1")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "logit_ens1.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu logit_ens1",
                                dynamicdate = Sys.Date())
)

logit_ens1_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
logit_ens1_prognoza <- predict(logit_ens1, df, type = "prob")
logit_ens1_prognoza$obs <- df$default
logit_ens1_prognoza$pred <- forcats::as_factor(
   ifelse(logit_ens1_prognoza$default >= 0.5, "default", "no_default")
)
logit_ens1_prognoza$pred <- relevel(logit_ens1_prognoza$pred, ref = "default")

# wyciagniecie prognoz modelu na zbiorze treningowym
logit_ens1_train <- logit_ens1$pred %>% dplyr::select(., rowIndex, default) %>% 
   dplyr::rename( logit_ens1 = default)

# zapis gotowego modelu
save(logit_ens1, file = "./dane/modele/logit_ens1.RData")

# zapis prognoz na zbiorze treningowym
save(logit_ens1_train, file = "./dane/modele/logit_ens1_train.RData")

# zapis prognozy i oceny bootstrapowej
save(logit_ens1_eval_boot, logit_ens1_prognoza, file = "./dane/modele/logit_ens1_ocena.RData")

# czyszczenie srodowiska
rm(logit_ens1, logit_ens1_eval_boot, logit_ens1_prognoza, 
   train_eval, test_eval, boot_eval, cl, df)