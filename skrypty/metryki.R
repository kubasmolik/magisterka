#########################################################################################################
####                            PRZYGOTOWANIE SRODOWISKA DO MODELOWANIA                              ####
#########################################################################################################

load("./dane/dane_modelowanie.RData")

# wczytanie bibliotek
library(caret)
library(MLmetrics)
library(parallel)
library(foreach)
library(doParallel)
library(pROC)
library(PRROC)
library(forcats)


# centrowanie i skalowanie zbioru testowego
pre_proc_test <- preProcess(test, method = c("center","scale"))
test <- predict(pre_proc_test, test)

# generowanie bootstrapowych prob testowych
set.seed(1993)
bootstrap <- createDataPartition(test$default, times = 50, p = 0.5)

# funkcja liczaca F2_score
f2 <- function(data, lev = NULL, model = NULL) {
   
   f2_val <- MLmetrics::FBeta_Score(y_pred = data$pred, y_true = data$obs, beta = 2)
   c(F2 = f2_val)
}

# funkcja liczaca AUPRC
auprcSummary <- function(data, lev = NULL, model = NULL){
   
   index_default <- data$obs == "default"
   index_no_default <- data$obs == "no_default"
   
   the_curve <- PRROC::pr.curve(data$default[index_default], data$default[index_no_default], curve = FALSE)
   out <- the_curve$auc.integral
   names(out) <- "AUPRC"
   
   out
   
}

# funkcja licząca wartosci krzywej AUPRC
calc_auprc <- function(model, data){
   
   index_default <- data$default == "default"
   index_no_default <- data$default == "no_default"
   
   predictions <- predict(model, data, type = "prob")
   
   pr.curve(predictions$default[index_default], predictions$default[index_no_default], curve = TRUE)
   
}

#### CUSTOM SUMMARY ####
# funkcja podsumowujaca modelowanie
custom_summary <- function(data, lev = NULL, model = NULL){
   
   lvls <- levels(data$obs)
   
   f2_val <- MLmetrics::FBeta_Score(y_pred = data$pred, y_true = data$obs, beta = 2)
   
   the_curve <- PRROC::pr.curve(data$default[data$obs == "default"], 
                                data$default[data$obs == "no_default"], 
                                curve = FALSE)
   auprc <- the_curve$auc.integral
   
   #rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 1), data[, lvls[1]])
   rocAUC <- ModelMetrics::auc(data$obs, data$pred)
   
   t <- table(data$pred, data$obs)

   #precision <- t[1,1] / (t[1,1] + t[1,2])
   #recall <- t[1,1] / (t[1,1] + t[2,1])
   accuracy <- (t[1,1] + t[2,2]) / sum(t)
   
   wynik <- c(AUPRC = auprc,
              F2 = f2_val,
              ROC = rocAUC,
              Accuracy = accuracy)
   wynik
}

#### OCENA TRAIN ####
# funkcja zapisujaca paramtery modelu i jego performance na zbiorze treningowym
ocena_train <- function(model){
   temp <- model$results[which.max(model$results$AUPRC), ]
   ind <- dim(temp)[2] - 7
   temp <- temp[,ind:dim(temp)[2]]
   wyniki <- t(temp)
   wyniki <- data.frame(Metryka = rownames(wyniki),
                        Wartosc = wyniki,
                        stringsAsFactors = F)
   names(wyniki) <- c("Metryka","Wartosc")
   rownames(wyniki) <- NULL
   
   ocena <- list(metryki = wyniki, 
                 parametry = model$bestTune)
   ocena
}

#### OCENA TEST ####
# funkcja zapisujaca performance modelu na calym zbiorze testowym
ocena_test <- function(zb_testowy, model, nazwa_modelu){
   
   # generuje prognoze
   prognoza <- predict(model, zb_testowy, type = "prob")
   prognoza$obs <- zb_testowy$default
   prognoza$pred <- forcats::as_factor(ifelse(prognoza$default >= 0.5, "default", "no_default"))
   prognoza$pred <- relevel(prognoza$pred, ref = "default")
   
   # opisowa ocena modelu
   ocena_1 <- custom_summary(prognoza)
   ocena_2 <- caret::confusionMatrix(table(prognoza$pred, prognoza$obs), mode = "everything")
   
   # przygotowanie krzywej Precision-Recall Curve
   temp <- pr.curve(prognoza$default[prognoza$obs == "default" ], 
                    prognoza$default[prognoza$obs == "no_default"], curve = TRUE)
   
   df_prc <- data.frame(Precision = temp$curve[,2],
                        Recall = temp$curve[,1])
   
   krzywa_pr <- ggplot(data = df_prc, aes(x = Recall, y = Precision)) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0,1)) +
      geom_line(size = 1.5, color = kolor) +
      geom_abline(intercept = sum(prognoza$obs == "default")/nrow(prognoza), 
                  slope = 0, col = "black", size = 1, linetype = 2) + 
      labs(title = paste("Krzywa Precision-Recall dla modelu", nazwa_modelu, sep = " ")) + 
      annotate(geom = "text", x = 0.9, y = 0.9, color = kolor, size = 5, 
               label = paste("AUPRC", round(x[1], 4), sep = " = ")) + 
      annotate(geom = "text", x = 0.9, y = 0.1, color = "black", size = 5,  
               label = paste("Udział default", round(sum(prognoza$obs == "default")/nrow(prognoza), 3), sep = ": ")) + 
      theme_bw()
   
   #krzywa_pr
   
   # porownanie funkcji gestosci dla prawidlowych i nieprawidlowych
   default_palette <- RColorBrewer::brewer.pal(n = 3, name = "Set1")[1:2]
   gestosc <- ggplot(data = prognoza, aes(x = default, fill = obs)) +
      coord_cartesian(xlim = c(0,1)) +
      geom_density(alpha = 0.4, size = 1) +
      scale_fill_manual(values = default_palette) +
      labs(x = "Prognozowane prawdopodobienstwo default'u",
           y = "Gestosc",
           title = "Porownanie rozkladow prawdopodobienstwa") + 
      scale_y_continuous(labels = scales::comma) + 
      theme_bw() +
      theme(legend.position = "bottom", legend.title = element_blank())
   
   #gestosc
   
   # przygotowanie krzywej ROC
   temp <- roc.curve(prognoza$default[prognoza$obs == "default" ], 
                     prognoza$default[prognoza$obs == "no_default"], curve = TRUE)
   
   df_roc <- data.frame(FPR = temp$curve[,1], TPR = temp$curve[,2])
   
   krzywa_roc <- ggplot(df_roc, aes(x = FPR, y = TPR)) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0,1)) +
      geom_line(size = 1.5, color = kolor) +
      geom_abline(intercept = 0, slope = 1, col = "black", size = 1, linetype = 2) + 
      labs(title = paste("Krzywa ROC dla modelu", nazwa_modelu, sep = " "),
           x = "False Positive Rate",
           y = "True Positive Rate"
      ) + 
      annotate(geom = "text", x = 0.9, y = 0.1, color = kolor, size = 5, 
               label = paste("AUC", round(ocena_1[3], 4), sep = " = ")) + 
      theme_bw()
   
   #krzywa_roc
   
   list(
      ocena_1 = ocena_1,
      ocena_2 = ocena_2,
      gestosc = gestosc,
      krzywa_roc = krzywa_roc,
      krzywa_pr = krzywa_pr
   )
}

#### OCENA BOOTSTRAP ####
# f. pomocnicza przy ocenie modelu na podstawie prob bootstrapowych
ocena_bootstrap <- function(zb_testowy, model){
   
   # generuje prognoze
   prognoza <- predict(model, zb_testowy, type = "prob")
   prognoza$obs <- zb_testowy$default
   prognoza$pred <- forcats::as_factor(ifelse(prognoza$default >= 0.5, "default", "no_default"))
   prognoza$pred <- relevel(prognoza$pred, ref = "default")
   
   # opisowa ocena modelu
   ocena_1 <- custom_summary(prognoza)
   
   ocena_1
}

#### WYNIKI BOOTSTRAP ####
# funkcja oceniajaca performance modelu na probach bootstrapowych.
# zwraca data_frame z wynikami i wykres
wyniki_bootstrap <- function(df, model, proby_boot, nazwa_modelu){
   
   wynik_bootstrap <- data.frame(Model = rep(nazwa_modelu, length.out = length(proby_boot)),
                                 AUPRC = numeric(length = length(proby_boot)), 
                                 F2 = numeric(length = length(proby_boot)), 
                                 ROC = numeric(length = length(proby_boot)), 
                                 Accuracy = numeric(length = length(proby_boot)),
                                 stringsAsFactors = F)
   
   for(i in 1:length(proby_boot)){
      wynik_bootstrap[i,2:5] <- ocena_bootstrap(zb_testowy = df[proby_boot[[i]],], model = model)
   }
   
   
   temp <- tidyr::gather(wynik_bootstrap[,-1], key = "metric")
   srednie <- temp %>% group_by(metric) %>% summarise(srednia = round(mean(value), 4))
   temp <- temp %>% left_join(srednie)
   
   wykres_boot <- ggplot(temp, aes(x = fct_relevel(as_factor(metric), 
                                                   c("AUPRC","F2","ROC","Accuracy")), y = value)) +
      coord_cartesian(ylim = c(0,1)) + 
      stat_summary(fun.y = mean, geom = "bar", fill = kolor) +
      geom_text(aes(label = srednia), y = 0.05, color = "black") +
      labs(title = paste("Srednie wartosci statystyk na podstawie prob bootstrapowych (n = ", 
                         length(proby_boot), ")", sep = ""),
           x = "Metryka",
           y = "Wartosc") +
      theme_bw()
   
   list(wyniki_df = wynik_bootstrap,
        wykres_boot = wykres_boot)
}


