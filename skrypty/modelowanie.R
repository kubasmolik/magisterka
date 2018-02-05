
#### PELNY MODEL LOGITOWY ZE ZMIENNYMI WOE ####

df <- train_bal %>% select(., default, one_of(zm_numeric), ends_with("woe"))

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 2,
                     #summaryFunction = f2,
                     #summaryFunction = defaultSummary,
                     #summaryFunction = auprcSummary,
                     summaryFunction = custom_summary,
                     classProbs = T,
                     verboseIter = T,
                     allowParallel = T)

# zrownoleglenie obliczen
cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
logit_full <- caret::train(default ~ ., 
                             data = df,
                             method = "glm",
                             family = binomial,
                            # metric = "F2",
                           #metric = "Accuracy",
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
