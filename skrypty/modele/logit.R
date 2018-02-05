#### MODEL LOGITOWY ZE ZMIENNYMI WOE ####

df <- train_clean %>% select(., default, one_of(zmienne_model_poj_woe))

# zrownoleglenie obliczen
cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
logit <- caret::train(default ~ ., 
                           data = df,
                           method = "glm",
                           family = binomial,
                           metric = "AUPRC",  
                           trControl = ctrl)
stopCluster(cl)

logit
logit$results[which.max(logit$results$AUPRC), ]
logit$bestTune

# ocena na zbiorze treningowym
train_eval <- ocena_train(logit)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zmienne_model_poj_woe))

test_eval <- ocena_test(zb_testowy = df, model = logit, nazwa_modelu = "logit")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = logit, proby_boot = bootstrap, 
                              nazwa_modelu = "logit")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "logit.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu logit",
                                dynamicdate = Sys.Date())
)

logit_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
logit_prognoza <- predict(logit, df, type = "prob")
logit_prognoza$obs <- df$default
logit_prognoza$pred <- forcats::as_factor(
   ifelse(logit_prognoza$default >= 0.5, "default", "no_default")
)
logit_prognoza$pred <- relevel(logit_prognoza$pred, ref = "default")

# zapis gotowego modelu
save(logit, file = "./dane/modele/logit.RData")

# zapis prognozy i oceny bootstrapowej
save(logit_eval_boot, logit_prognoza, file = "./dane/modele/logit_ocena.RData")

# czyszczenie srodowiska
rm(logit, logit_eval_boot, logit_prognoza, 
   train_eval, test_eval, boot_eval, cl, df)
