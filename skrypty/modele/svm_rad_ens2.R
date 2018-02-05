#### ENSEMBLER 1 ####
#### svm_rad_ens2 ####

df <- train_clean %>% select(., default, one_of(zmienne_ens2_woe))

sigma_est <- kernlab::sigest(default~., data = df)
sigma_est[[1]]
parametry <- expand.grid(
   sigma = sigma_est[[1]],
   C = c(2^(seq(-1, 4)))
   # C = c(4)
)

cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
svm_rad_ens2 <- caret::train(default ~ ., 
                             data = df,
                             method = "svmRadial",
                             metric = "AUPRC",
                             trControl = ctrl_svm,
                             tuneGrid = parametry
                             #tuneLength = 10
)

stopCluster(cl)

svm_rad_ens2
svm_rad_ens2$results
plot(svm_rad_ens2)
svm_rad_ens2$results[which.max(svm_rad_ens2$results$AUPRC), ]
svm_rad_ens2$bestTune

# ocena na zbiorze treningowym
train_eval <- ocena_train(svm_rad_ens2)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zmienne_ens2_woe))

test_eval <- ocena_test(zb_testowy = df, model = svm_rad_ens2, nazwa_modelu = "svm_rad_ens2")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = svm_rad_ens2, proby_boot = bootstrap, 
                              nazwa_modelu = "svm_rad_ens2")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "svm_rad_ens2.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu svm_rad_ens2",
                                dynamicdate = Sys.Date())
)

svm_rad_ens2_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
#df <- test %>% select(., default, one_of(zm_numeric), ends_with("woe"))
svm_rad_ens2_prognoza <- predict(svm_rad_ens2, df, type = "prob")
svm_rad_ens2_prognoza$obs <- df$default
svm_rad_ens2_prognoza$pred <- forcats::as_factor(
   ifelse(svm_rad_ens2_prognoza$default >= 0.5, "default", "no_default")
)
svm_rad_ens2_prognoza$pred <- relevel(svm_rad_ens2_prognoza$pred, ref = "default")

# wyciagniecie prognoz modelu na zbiorze treningowym
svm_rad_ens2_train <- svm_rad_ens2$pred %>% dplyr::select(., rowIndex, default) %>% 
   dplyr::rename( svm_rad_ens2 = default)

# zapis gotowego modelu
save(svm_rad_ens2, file = "./dane/modele/svm_rad_ens2.RData")

# zapis prognoz na zbiorze treningowym
save(svm_rad_ens2_train, file = "./dane/modele/svm_rad_ens2_train.RData")

# zapis prognozy i oceny bootstrapowej
save(svm_rad_ens2_eval_boot, svm_rad_ens2_prognoza, file = "./dane/modele/svm_rad_ens2_ocena.RData")

# czyszczenie srodowiska
rm(svm_rad_ens2, svm_rad_ens2_eval_boot, svm_rad_ens2_prognoza, 
   train_eval, test_eval, boot_eval, cl, df)
