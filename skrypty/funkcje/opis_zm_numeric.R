
# przypisanie zmiennej
#x <- "loan_amnt"
opis_zm_numeric <- function(zb_danych, x){
   # pomocniczy data.frame
   df_wykres <- zb_danych %>% select(., default, default_num, .data[[x]])
   names(df_wykres)[3] <- "zmienna"
   
   # opis zmiennej
   opis <- DescTools::Desc(df_wykres[, 3], plotit = F)
   opis[1] <- NULL
   names(opis) <- x
   opis[[1]]$xname <- x
   opis[[1]]$main <- x
   #print(opis)
   
   # test Kolomogorova - Smirnova na normalnosc rozkladu
   test_ks_norm <- ks.test(df_wykres$zmienna,
                      rnorm(nrow(df_wykres), 
                            mean = mean(df_wykres$zmienna), 
                            sd = sd(df_wykres$zmienna))
                      )
   
   test_ks_norm$data.name <- paste(x, " ~ Rozklad normalny")
   
   # test Kolomogorova - Smirnova zgodnosci z rozkladem lognormalnym
   test_ks_lognorm <- ks.test(df_wykres$zmienna, 
                              exp(rnorm(nrow(df_wykres), 
                                        mean = mean(df_wykres$zmienna), 
                                        sd = sd(df_wykres$zmienna))
                                  )
                              )
   
   test_ks_lognorm$data.name <- paste(x, " ~ Rozklad lognormalny")
   
   # test rownosci srednich wg default
   t_test <- t.test(zmienna ~ default, data = df_wykres)
   t_test$data.name <- stringr::str_replace(t_test$data.name, pattern = "zmienna", replacement = x)
   
   ### histogram
   kolor <- RColorBrewer::brewer.pal(n = 3, name = "Set1")[2]
   linie <- c("Gestosc" = "#000000", "Rozklad normalny" = rgb(1,0,0,1))
   
   df_histogram <- ggplot(data = df_wykres, aes(x = zmienna)) +
      geom_histogram(fill = kolor, color = "black", aes(y = ..density..), bins = 30) + 
      geom_density(alpha = 0.2, size = 1, aes(colour = "Gestosc")) + 
      stat_function(fun = dnorm, args = 
                       list(mean = mean(df_wykres$zmienna), sd = sd(df_wykres$zmienna)),
                    geom = "line", size = 1, aes(colour = "Rozklad normalny")) + 
      labs(x = paste(x),
           y = "Gestosc",
           title = paste("Rozklad zmiennej ", x, sep ="" )) + 
      scale_y_continuous(labels = scales::comma) + 
      scale_color_manual(values = linie) + 
      theme_bw() +
      theme(legend.position = "bottom", legend.title = element_blank())
   #df_histogram
   
   ### porownanie gestosci
   default_palette <- RColorBrewer::brewer.pal(n = 3, name = "Set1")[2:1]
   df_density <- ggplot(data = df_wykres, aes(x = zmienna, fill = default)) +
      geom_density(alpha = 0.5, size = 1) +
      scale_fill_manual(values = default_palette) +
      labs(x = paste(x),
           y = "Gestosc",
           title = paste("Rozklad zmiennej ", x, " wzgledem zmiennej default", sep ="" )
      ) + 
      scale_y_continuous(labels = scales::comma) + 
      theme_bw() +
      theme(legend.position = "bottom", legend.title = element_blank())
   
   #df_density
   
   ### boxplot
   df_boxplot <- ggplot(data = df_wykres, aes(y = zmienna, x = default, fill = default)) +
      geom_boxplot(outlier.color = "black", outlier.size = 1.5, varwidth = T) +
      scale_fill_manual(values = default_palette) +
      labs(x = NULL,
           y = paste(x),
           title = paste("Rozklad zmiennej ", x, " wzgledem zmiennej default", sep ="" )
      ) + 
      theme_bw() +
      theme(legend.position = "right", legend.title = element_blank())
   
   #df_boxplot
   
   #### geomy
   binomial_smooth <- function(...) {
      geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
   }
   
   kolor_geom <- RColorBrewer::brewer.pal(n = 3, name = "Set1")
   skala_geom <- c("glm" = kolor_geom[1], "cubic_spline_2" = kolor_geom[2],
                   "cubic_spline_4" = kolor_geom[3])
   
   df_geom <- ggplot(data = df_wykres, aes(y = default_num, x = zmienna)) +
      binomial_smooth(aes(color = "glm"), se = F) +
      binomial_smooth(formula = y ~ splines::ns(x,2), aes(color = "cubic_spline_2"), se = F) +
      binomial_smooth(formula = y ~ splines::ns(x,4), aes(color = "cubic_spline_4"), se = F) +
      labs(x = paste(x),
           y = "Prawdopodobienstwo default",
           title = paste("Zaleznosc default vs ", x, sep ="" )) + 
      theme_bw() +
      theme(legend.position = "bottom", legend.title = element_blank())
   
   #df_geom
   
   return(list("opis" = opis,
               "test_ks_norm" = test_ks_norm,
               "test_ks_lognorm" = test_ks_lognorm,
               "t_test" = t_test,
               "df_histogram" = df_histogram,
               "df_density" = df_density,
               "df_boxplot" = df_boxplot,
               "df_geom" = df_geom))
}