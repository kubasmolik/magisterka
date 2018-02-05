
# funkcja pomocnicza do wstepnego opisu zmiennych
spr_zmienna <- function(zmienna){
   zmienna <- dplyr::enquo(zmienna)
   
   print(sum(is.na(dane %>% select(., !!zmienna))))
   print(summary(dane %>% select(., !!zmienna)))
   print(table(dane %>% select(., !!zmienna)))
   df <- dane %>% group_by(issue_d) %>% summarise(suma = sum(!!zmienna))
   plot(ggplot(df, aes(issue_d, suma)) + geom_line())
   #View(df)
   #print(dane %>% group_by(issue_d) %>% summarise(suma = sum(!!zmienna)))
   print(dane %>% group_by(default) %>% summarise(srednia = mean(!!zmienna)))
}

spr_zmienna(delinq_2yrs)
