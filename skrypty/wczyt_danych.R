#### WCZYTANIE SUROWYCH DANYCH I ICH WSTEPNA OBROBKA ####

dane0 <- tbl_df(data.table::fread("./dane/loan.csv", verbose = T))

# ustawienie formatu czasowego
Sys.setlocale("LC_TIME", "C")

# zmiana na zmienn? czasowa
dane0$issue_d <- as.Date(gsub("^", "01-", dane0$issue_d), format = "%d-%b-%Y")

min(dane0$issue_d, na.rm = T)
max(dane0$issue_d, na.rm = T)

# poczatek i koniec okresu analizy
start <- as.Date("2010-01-01", format = "%Y-%m-%d") # pominiecie pozyzek udzielonych przed kryzysem
stop <- as.Date("2013-12-01", format = "%Y-%m-%d") # min dwa lata na realizacje ryzyka

# filtrowanie wg dat
dane <- filter(dane0, issue_d >= start & issue_d <= stop)

# sprawdzenie procenta brakow danych per zmienna
proc_brakow <- c()
for(i in 1:dim(dane)[2]){
   proc_brakow[i] <- sum(is.na(dane[,i])) / dim(dane)[1]
}
table(round(proc_brakow, digits = 2))

# akceptowalny poziom brakow
prog <- 0.7
dane <- dane[,proc_brakow <= prog]

# zmienne ktore trzeba zamienic na typ factor
apply(X = dane[,c(6,12,13,15,18,21,35,51,52)],MARGIN = 2, FUN = DescTools::Desc, plotit = F)

names(dane[,c(6,12,13,15,18,21,35)])

# usuniecie zmiennych z tylko 1 wartoscia
dane <- dane[,-c(18,51:52)]

# zamiana na typ factor
dane$term <- as.factor(dane$term)
dane$emp_length <- as.factor(dane$emp_length)
dane$emp_length <- reorder(dane$emp_length, 
                           new.order = c("10+ years","9 years","8 years","7 years","6 years","5 years",
                                    "4 years","3 years","2 years","1 year","< 1 year", "n/a")
                )
dane$home_ownership <- as.factor(dane$home_ownership)
dane$verification_status <- as.factor(dane$verification_status)
dane$purpose <- as.factor(dane$purpose)
dane$initial_list_status <- as.factor(dane$initial_list_status)

# zamiana na typ data
dane$earliest_cr_line <- as.Date(gsub("^", "01-", dane$earliest_cr_line), format = "%d-%b-%Y")
dane$last_pymnt_d <- as.Date(gsub("^", "01-", dane$earliest_cr_line), format = "%d-%b-%Y")
dane$next_pymnt_d <- as.Date(gsub("^", "01-", dane$next_pymnt_d), format = "%d-%b-%Y")

statusy <- dane %>% group_by(loan_status) %>% count()

do_usuniecia <- unlist(c(statusy[c(4:5,7),1]))

dane <- filter(dane, !(loan_status %in% do_usuniecia))

temp <- !ls() %in% c("dane","lista_pakietow","start","stop","is.installed")
rm(list = ls()[temp])
rm(temp)