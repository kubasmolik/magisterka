#### MODEL nnetOWY ZE ZMIENNYMI WOE ####

df <- train_clean %>% select(., default, one_of(zmienne_model_poj_woe))

ctrl_nnet <- ctrl
ctrl_nnet$sampling <- "down"

parametry <- expand.grid(
   size = seq(6,10,1),
   decay = c(10^seq(-4, -1, 1)),
   #decay = 10^(-4),
   bag = TRUE
)

# zrownoleglenie obliczen
cl <- makePSOCKcluster(detectCores() - 1, outfile = "")
registerDoParallel(cl)

set.seed(1993)
nnet <- caret::train(default ~ ., 
                     data = df,
                     method = "avNNet", 
                     metric = "AUPRC",
                     trControl = ctrl_nnet,
                     tuneGrid = parametry)
stopCluster(cl)

nnet
nnet$results
plot(nnet)
nnet$results[which.max(nnet$results$AUPRC), ]
nnet$bestTune

# ocena na zbiorze treningowym
train_eval <- ocena_train(nnet)
train_eval$metryki
train_eval$parametry

# ocena na zbiorze testowym
df <- test %>% select(., default, one_of(zmienne_model_poj_woe))

test_eval <- ocena_test(zb_testowy = df, model = nnet, nazwa_modelu = "nnet")
test_eval$ocena_1
test_eval$ocena_2
test_eval$gestosc
test_eval$krzywa_roc
test_eval$krzywa_pr

# ocena na bootstrapowych probach
boot_eval <- wyniki_bootstrap(df = df, model = nnet, proby_boot = bootstrap,
                              nazwa_modelu = "nnet")

boot_eval$wyniki_df
boot_eval$wykres_boot

# zapis danych do raportu
save(train_eval, test_eval, boot_eval, file = "./dane/raporty modeli/model.RData")

# generowanie raportu
rmarkdown::render("./skrypty/raport_modelu.Rmd",
                  output_file = "nnet.html",
                  output_dir = "./dane/raporty modeli",
                  params = list(dynamictitle = "Raport dla modelu nnet",
                                dynamicdate = Sys.Date())
)

nnet_eval_boot <-  boot_eval$wyniki_df

# generuje prognoze
nnet_prognoza <- predict(nnet, df, type = "prob")
nnet_prognoza$obs <- df$default
nnet_prognoza$pred <- forcats::as_factor(
   ifelse(nnet_prognoza$default >= 0.5, "default", "no_default")
)
nnet_prognoza$pred <- relevel(nnet_prognoza$pred, ref = "default")

# zapis gotowego modelu
save(nnet, file = "./dane/modele/nnet.RData")

# zapis prognozy i oceny bootstrapowej
save(nnet_eval_boot, nnet_prognoza, file = "./dane/modele/nnet_ocena.RData")

# czyszczenie srodowiska
rm(nnet, nnet_eval_boot, nnet_prognoza,
   train_eval, test_eval, boot_eval, cl, df)
