
#x <- "home_ownership"

opis_zm_factor <- function(zb_danych, x){
   
   # pomocniczy data.frame
   df_wykres <- zb_danych %>% select(., default, default_num, .data[[x]])
   names(df_wykres)[3] <- "zmienna"
   
   #### opis zmiennej ####
   opis <- DescTools::Desc(df_wykres$zmienna, plotit = F)
   opis[[1]]$xname <- x
   opis[[1]]$main <- x
   
   #opis
   
   #### rozklad kategorii ####
   df <- df_wykres %>% group_by(zmienna) %>% count() %>% ungroup() %>% arrange(desc(n))
   df <- df %>% mutate(proc = n / nrow(df_wykres))
   df$proc <- scales::percent(df$proc)
   
   kolor <- RColorBrewer::brewer.pal(n = 3, name = "Set1")[2]
   
   df_bar <- ggplot(df, aes(x = zmienna, y = n)) +
      geom_bar(stat = "identity", fill = kolor, color = "black") +
      theme_bw() +
      aes(x = forcats::fct_reorder(zmienna, desc(n))) +
      geom_text(aes(label = proc), vjust = 1, colour = "green") +
      labs(title = paste("Rozklad kategorii zmiennej ", x, sep = ""),
           x = x,
           y = "liczebnosc")
      
   #df_bar
   
   #### wykres rozkladu prawdop. defaultu wg kategorii ####
   df <- df_wykres %>% group_by(zmienna, default) %>% count() %>% ungroup()
   temp <- df_wykres %>% group_by(zmienna) %>% count() %>% ungroup() %>% rename(total = n)
   df <- df %>% filter(default == "default") %>% left_join(temp) %>% mutate(proc_def = n/total) %>%
      select( -n, - total) %>% right_join(df)
   df <- df %>% filter(default == "no_default") %>% left_join(temp) %>% mutate(proc_no_def = n/total) %>%
      select( -n, - total, - proc_def) %>% right_join(df)
   
   df$proc_no_def[is.na(df$proc_no_def)] <- 0
   df$proc_def[is.na(df$proc_def)] <- 0
   
   default_palette <- RColorBrewer::brewer.pal(n = 3, name = "Set1")[2:1]
   df_bar2 <- ggplot(df, aes(x = zmienna, y = n, fill = default)) + 
      geom_bar(position = "fill", stat = "identity", color = "black", alpha = 0.5) +
      aes(x = forcats::fct_reorder(zmienna, proc_def)) +
      coord_flip() +
      theme_bw() + 
      labs(title = paste(x, " vs default", sep = ""),
           x = x,
           y = "Rozklad") +
      scale_fill_manual(values = default_palette) +
      scale_y_continuous(labels = scales::percent(seq(0,1,0.25)))
   
   #df_bar2
   
   #### tabela rozklad prawdop. defaultu wg kategorii ####
   temp <- left_join(
      df %>% filter(default == "default") %>% select(zmienna, proc_def) %>% 
         rename(default = proc_def), 
      df %>% filter(default == "no_default") %>% select(zmienna, proc_no_def) %>% 
         rename(no_default = proc_no_def)
      )
   tabela <- arrange(temp, desc(default))
   tabela$default <- scales::percent(tabela$default)
   tabela$no_default <- scales::percent(tabela$no_default)
   
   #knitr::kable(tabela)
   
   #### test proporcji ####
   
   test <- with(df_wykres, prop.test(table(zmienna, default)))
   test$data.name <- paste(x, " vs default", sep = "")
   names(test$estimate) <- levels(df_wykres$zmienna)
   #test
   #print(test, digits = 3)
   
   return(list("opis" = opis,
               "df_bar" = df_bar,
               "df_bar2" = df_bar2,
               "tabela" = tabela,
               "test" = test)
          )
}

