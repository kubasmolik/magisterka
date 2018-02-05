#### SVM_RADIAL ####

df <- train_clean %>% select(., default, one_of(zmienne_model_poj_woe))

sigma_est <- kernlab::sigest(default~., data = df)
sigma_est[[1]]
parametry <- expand.grid(
   sigma = sigma_est[[1]],
   C = c(2^(seq(0,3)))
)

ctrl_svm <- ctrl 
ctrl_svm$sampling <- "down"

cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
svm_rad <- caret::train(default ~ ., 
                          data = df,
                          method = "svmRadial",
                          metric = "AUPRC",
                          trControl = ctrl_svm,
                          tuneGrid = parametry
)

stopCluster(cl)

svm_rad
svm_rad$results
plot(svm_rad)
svm_rad$results[which.max(svm_rad$results$AUPRC), ]
svm_rad$bestTune

# ocena na zbiorze treningowym
train_eval <- ocena_train(svm_rad)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zmienne_model_poj_woe))

test_eval <- ocena_test(zb_testowy = df, model = svm_rad, nazwa_modelu = "svm_rad")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = svm_rad, proby_boot = bootstrap, 
                              nazwa_modelu = "svm_rad")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "svm_rad.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu svm_rad",
                                dynamicdate = Sys.Date())
)

svm_rad_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
svm_rad_prognoza <- predict(svm_rad, df, type = "prob")
svm_rad_prognoza$obs <- df$default
svm_rad_prognoza$pred <- forcats::as_factor(
   ifelse(svm_rad_prognoza$default >= 0.5, "default", "no_default")
)
svm_rad_prognoza$pred <- relevel(svm_rad_prognoza$pred, ref = "default")

# zapis gotowego modelu
save(svm_rad, file = "./dane/modele/svm_rad.RData")

# zapis prognozy i oceny bootstrapowej
save(svm_rad_eval_boot, svm_rad_prognoza, file = "./dane/modele/svm_rad_ocena.RData")

# czyszczenie srodowiska
rm(svm_rad, svm_rad_eval_boot, svm_rad_prognoza,
   train_eval, test_eval, boot_eval, cl, df)
