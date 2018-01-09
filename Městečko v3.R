###############################################################################
###############################################################################
###############################################################################

## 4ST417 Výpočetní statistika v R
## Seminárni práce 

## autor skriptu: Jiří Novák
## xnovj159

###############################################################################

# Městečko Palermo

###############################################################################

set.seed(159)

# Základní proměnné -----------------------------------------------------------

h <- 5  # počet hráčů

mira_Katany_risk  <- 0.2   # Míra ochoty Katányho riskovat
  # 0.2 znamená, že jakmile odhalí identitu 20 % všech hráčů tak prozradí, že je Katány a řekne ostatním hráčům na co přišel
  # interpretace hodnot: 0.2 je opatrný Katány; 0.6 je riskující Katány

mira_mafie <- 0.2    # Míra mafie v městečku
  # 0.3 znamená, že v městečku je 20 % mafiánů

mira_nastvani <- 0.1   # Míra naštvání, když vás obvniní druhá osoba, za předpokladu, že pouze tuší
  # tento koeficient se přičítá do matice důvěry k počítaným pravděpodobnostem

# -----------------------------------------------------------------------------

## Odvozenné proměnné ##########################################################

m <- ceiling(mira_mafie * h)   # počet mafinánů

role <- c("Katány")   # Katány je vždy ve hře jeden
for (i in 1:(h-1)) {
  if (length(which(role == "mafián")) != m) {   # dokud se počet mafiánů v role nerovná celkovému počtu mafiánů, 
    role <- c(role, "mafián")                   # tak přidávej mafiány
  } 
  else {
    role <- c(role, "občan")
  }
}

hraci <- c(1:h)   # indexy hráčů
names(hraci) <- role   # pojmenování jednotlivých hráčů

pamet_k <- character(h)   # pamět Katányho

p <- h   # počítadlo přeživších

Katany_odhaleni <- FALSE   # indikátor toho jestli se Katány odhalil

### Matice důvěry -----------------------------------------
m_duvery <- matrix(data = round(m/(h-1), digits = 2), 
                   nrow = h, 
                   ncol = h)  
  # matice důvěry mezi hráči, kde je m/h-1 šance ,že druhý hráč je mafián
dimnames(m_duvery) = list(names(hraci), names(hraci))   # pojmenuju si sloupečky podle rolí
diag(m_duvery) <- 0   
  # každý hráč ví o sobě, co je a tak na diagonále jsou nuly

### -------------------------------------------------------

## ----------------------------------------------------------------------------

###############################################################################

sink("mestecko_prubeh.txt")  # Uloží hru do textového souboru v pracovní složce 

# Hra -------------------------------------------------------------------------

repeat {
  
  print(hraci)
  cat("\n")   # odřádkování
  print(m_duvery)
  cat("\n")   # odřádkování
  
  # NOC -----------------------------------------------------------------------
  print("Městečko Palermo usíná ...")
  cat("\n")   # odřádkování
  
  ## Mafie řádí -------------------------------------------
  print("Mafie se probouzí a vybírá si...")
  cat("\n")   # odřádkování
  
  repeat {   
    if (Katany_odhaleni == TRUE & names(hraci)[1] == "Katány") {   # Zabití Katányho je hlavní priorita mafie
      zavrazden <- hraci[1]
    } else {
      zavrazden <- sample(hraci,1) 
    }
    
    if (names(zavrazden) != "mafián" & names(zavrazden) != "mrtev") {
      break
    }
  }   # Mafie zavraždí jednoho náhodného hráče, který není mafián a není mrtev
  
  names(hraci)[as.double(zavrazden)] <- "mrtev"
  dimnames(m_duvery) = list(names(hraci), names(hraci))   # přejmenuji sloupčeky podle aktuálního stavu hry
  
  m_duvery_sance_0 <- round(m/(p-1), digits = 2)   # současný stav pravděpodobnosti mafiána; (h-1) protože nepočítá sám sebe
  m_duvery_sance_1 <- round(m/(p-1-1), digits = 2)   # stav pravděpodobnosti mafiána s dalším zemřelým (-1)
  m_duvery_sance_zmena <- m_duvery_sance_1 - m_duvery_sance_0   # přírůstek pravděpodobnosti
 
  p <- p-1   # jeden hráč zemřel, počet přeživších se zmenšuje
  
  for(j in 1:h){
    for(i in 1:h){
      if (m_duvery[i,j] != 0) {
        m_duvery[i,j] <- m_duvery[i,j] + m_duvery_sance_zmena
      }   # matice důvěry se přepočítá podle aktuálních pravděpodobností, že druhá osoba je mafiánem
    }
  }
  
  m_duvery[as.double(zavrazden),] <- 0   # když zemře tak ostatní se dozví co byl zač
  m_duvery[,as.double(zavrazden)] <- 0   # ten co je mrtev si už taky nic myslet nemůže
  
  print(m_duvery)
  cat("\n")   # odřádkování
  
  print("Mafie usíná.")
  cat("\n")   # odřádkování
  
  ## Katány vyšetřuje -------------------------------------
  if (names(hraci)[1] == "Katány") {   # pokud je Katány naživu tak se probudí
    print("Katány se probouzí a ptá se ...")
    cat("\n")   # odřádkování
    
    repeat {   
      odhali <- sample(hraci,1)
      if (names(odhali) != "Katány" & names(hraci)[odhali] != "mrtev" & pamet_k[odhali] == "") 
        # Neptá se na sám sebe, na mrtvé hráče a na ty co už odhalil
        break
    }   # Katány odhalí identitu jednoho ostatního hráče
    
    pamet_k[as.double(odhali)] <-  names(odhali)   # zapamatuje si koho odhalil
    
    print(pamet_k)
    cat("\n")   # odřádkování
    
    if (names(odhali)  == "občan") {   # přiřadím hodnoty odhalené do matice důvery
      m_duvery[as.double(odhali),1] <- 0 
    } else {
      m_duvery[as.double(odhali),1] <- 1 
    }   # pokud je to občan tak mu věří (0), pokud je to mafián, tak ho odhalil (1)
    
    print(m_duvery)
    cat("\n")   # odřádkování
    
    m_duvery_sance_0 <- round(m/(p-1), digits = 2)   # současný stav pravděpodobnosti mafiána; (h-1) protože nepočítám
    m_duvery_sance_1 <- round(m/(p-1-1), digits = 2)   # stav pravděpodobnosti mafiána s dalším odhaleným (-1)
    m_duvery_sance_zmena <- m_duvery_sance_1 - m_duvery_sance_0   # přírůstek pravděpodobnosti
    
    for(i in 1:h) {
      if (m_duvery[i,1] != 0 & m_duvery[i,1] != 1) {   # pokud daná osoba není mrtvá nebo mafián
        m_duvery[i,1] <- m_duvery[i,1] + m_duvery_sance_zmena
      }   # matice důvěry se přepočítá pro sloupec Katányho a jeho odhalení
      
    }
    
    print(m_duvery)
    cat("\n")   # odřádkování
    
    print("Katány usíná.")
    cat("\n")   # odřádkování
    
  } else {
    print("Katány je mrtev a na nic se neptá.")
    cat("\n")   # odřádkování
    
  }
  
  cat("\n", 
      paste0("Městečko Palermo se probouzí a v noci zemřel hráč číslo ",
             zavrazden,
             " (",
             names(zavrazden),
             ")."),
      "\n"
  )
  cat("\n")   # odřádkování
  
  # DEN -----------------------------------------------------------------------
  print("Městečko Palermo vybírá pachatele ...")
  cat("\n")   # odřádkování
  print(hraci)
  cat("\n")   # odřádkování
  print(m_duvery)
  cat("\n")   # odřádkování
  
  repeat {   # Je povinnost městečka vybrat jednoho pachatele
    
    
    # Když Katáni odhalí dost hráčů, tak si řekne, že nebude dále riskovat a dohalí se
    if (length(which(pamet_k != "")) / h >= mira_Katany_risk & Katany_odhaleni == FALSE) {  
      
      print("Já jsem Katáni, prohlásí jeden z hráčů. Je možné, že dále nepřežiju a tak zde je to, co vím.")
      cat("\n")   # odřádkování
      
      for (i in 2:h) {
        flush.console()
        cat("\r",
            paste0("Hráč číslo "),
            hraci [i],
            " (",
            names(hraci)[i],
            ") ",
            "je mafián s pravděpodobností ",
            m_duvery[i,1] * 100,
            " %.",
            sep = "")
        cat("\r")
        print(i)
      } 
      Katany_odhaleni <- TRUE
      
      for(j in 2:h) {   # Ve většině případů hráči Katánymu věří, takže zde mu budou věřit a přijmou jeho poznatky 
        for(i in 1:h) {
          if (m_duvery[i,j] != 0) {   # Hodnota se přiřadí jen k hráčům kteří jsou naživu
            if (m_duvery[i,j] < m_duvery[i,1] | m_duvery[i,1] == 0 ) {# Pokud Katániho odhalení přinese lepší poznatky než již hráč má
              m_duvery[i,j] <- m_duvery[i,1]
            }   
          }   
        }
      }
      
      print(m_duvery)
      cat("\n")   # odřádkování
    }  
    
    # -----------------------------------------------------    
    
    # Když si je nějaký hráč jistý <1>, že už objevil mafiána, tak bude obviňovat
    jistota <- c() 
    for(j in 1:h) {   # Když je nějaký sloupeček matice důvery poute s jednou hodnotou, pak si je daný hráč jistý
      if (length(which(m_duvery[,j] != 0)) == 1) {
        jistota <- c(jistota, hraci[j])   # vytvoří se "zásobník" hráčů, kteří jsou si jisti, protože jich může být více
      }
    }
    
    if (!is.null(jistota)) {   # Zjistím si, jestli je někdo, kdo si je jistý, jinak podmínka neproběhne
      
      zalobce <- sample(jistota, 1)   # Žalobce bude ten rychlejší, zde nastanveno prvkem náhody
      
      cat("\n",paste0("Jsem si jistý tím, kdo je mafián a chci obvinit jiného hráče, prohlásí hráč číslo ",
                      zalobce,
                      " (",
                      names(zalobce),
                      ")."),
          "\n"
      )
      
      
      
      obvineny <- m_duvery[,zalobce]   # uložím si matici důvěry žalobce
      for (i in 1:h) {                 # uložím si index hráče, kterého bude obviňovat
        if (obvineny[i] > 0 ){
          vina <- i
        }
      }
      
      
      cat("\n", paste0("Obviňuji hráče číslo ",
                       vina,
                       " (",
                       names(hraci)[vina],
                       ")."),
          "\n"
      )
     
      print("Hlasování o vině:")
      cat("\n")   # odřádkování
      
      hlasy <- c()
      for (i in 1:h) {
        if (m_duvery[vina,i] >= 0.5) {   # Když si hráč myslí, že je víc jak poloviční šance, že daný hráč je mafián, tak pro něj bude hlasovat
          hlasy <- c(hlasy, 1)   # Hlasuje o vině
        } else {
          hlasy <- c(hlasy, 0)  # Hlasuje o nevině
        }
      }
      
      print(hlasy)
      cat("\n")   # odřádkování
      
      hlasovani <- sum(hlasy)/p   # Výsedek hlasování, počet hlasů pro vinu, dělím počtem přeživších
      if (hlasovani > 0.5) {   # K odsouzení je třeba nadpoloviční většiny hlasů
        
        if (names(hraci)[vina] == "mafián") {   # pokud to byl mafián tak se sníží počet mafiánů o jedna
          m <- m - 1
        }
        names(hraci)[as.double(vina)] <- "mrtev"
        p <- p-1   # sniží se počet přeživších
        dimnames(m_duvery) = list(names(hraci), names(hraci))   # přejmenuji sloupčeky podle aktuálního stavu hry
        
        m_duvery_sance_0 <- round(m/(p-1), digits = 2)   # současný stav pravděpodobnosti mafiána; (h-1) protože nepočítá sám sebe
        m_duvery_sance_1 <- round(m/(p-1-1), digits = 2)   # stav pravděpodobnosti mafiána s dalším zemřelým (-1)
        m_duvery_sance_zmena <- m_duvery_sance_1 - m_duvery_sance_0   # přírůstek pravděpodobnosti
        
        for(j in 1:h){
          for(i in 1:h){
            if (m_duvery[i,j] != 0) {
              m_duvery[i,j] <- m_duvery[i,j] + m_duvery_sance_zmena
            }   # matice důvěry se přepočítá podle aktuálních pravděpodobností, že druhá osoba je mafiánem
          }
        }
        
        m_duvery[as.double(vina),] <- 0   # když zemře tak ostatní se dozví co byl zač
        m_duvery[,as.double(vina)] <- 0   # ten co je mrtev si už taky nic myslet nemůže
        
        print(m_duvery)
        cat("\n")   # odřádkování
        
        break   # Někoho odsoudili a tak je konec dne   
      }
    }
    
    # -----------------------------------------------------
    
    # Když si je nějaký hráč tuší <0.5,1), že už objevil mafiána, tak bude obviňovat
    tusi <- c() 
    for(j in 1:h){
      if (length(which(m_duvery[,j] > 0.5)) >= 2) {  # Podmínka pro 1 v je výše 
        tusi <- c(tusi, hraci[j])   # vytvoří se "zásobník" hráčů, kteří tuší, protože jich může být více
      }
    }
    
    if (!is.null(tusi)) {   # Zjistím si, jestli je někdo, kdo tuší, jinak podmínka neproběhne
      
      zalobce <- sample(tusi, 1)   # Žalobce bude ten rychlejší, zde nastaveno prvkem náhody
      
      cat("\n",
          paste0("Tuším, kdo by mohl být mafiánem, prohlásí hráč číslo ",
                 zalobce,
                 " (",
                 names(hraci)[zalobce],
                 ")."),
          "\n"
      )
      
      
      
      obvineny <- m_duvery[,zalobce]   # uložím si matici důvěry žalobce
      vina <- c()
      for (i in 1:h) {                 # uložím si indexy hráčů z kterých se bude rozhodovat, že je obviní
        if (obvineny[i] > 0 ){
          vina <- c(vina,i)
        }
      }
      
      vina <- sample(vina, 1)   # Z těch, které může obvinit si náhodně vybere. Nerozhodnost simulována prvkem náhody
      
      cat("\n",paste0("Obviňuji hráče číslo ",
                      vina,
                      " (",
                      names(hraci)[vina],
                      ")."),
          
          "\n"
      )
      
      
      
      
      m_duvery[zalobce,vina] <- m_duvery[zalobce,vina] + mira_nastvani   # koeficient naštvání, že mne daná osoba obvninila jen když tuší
     
      print("Hlasování o vině:")
      cat("\n")   # odřádkování
      
      hlasy <- c()
      for (i in 1:h) {
        if (m_duvery[vina,i] >= 0.5) {   # Když si hráč myslí, že je víc jak poloviční šance, že daný hráč je mafián, tak pro něj bude hlasovat
          hlasy <- c(hlasy, 1)   # Hlasuje o vině
        } else {
          hlasy <- c(hlasy, 0)  # Hlasuje o nevině
        }
        
      }
      
      print(hlasy)
      cat("\n")   # odřádkování
      
      hlasovani <- sum(hlasy)/p   # Výsedek hlasování, počet hlasů pro vinu, dělím počtem přeživších
      if (hlasovani > 0.5) {   # K odsouzení je třeba nadpoloviční většiny hlasů
        
        if (names(hraci)[vina] == "mafián") {   # pokud to byl mafián tak se sníží počet mafiánů o jedna
          m <- m - 1
        }
        
        names(hraci)[as.double(vina)] <- "mrtev"
        dimnames(m_duvery) = list(names(hraci), names(hraci))   # přejmenuji sloupčeky podle aktuálního stavu hry
      
        m_duvery_sance_0 <- round(m/(p-1), digits = 2)   # současný stav pravděpodobnosti mafiána; (h-1) protože nepočítá sám sebe
        m_duvery_sance_1 <- round(m/(p-1-1), digits = 2)   # stav pravděpodobnosti mafiána s dalším zemřelým (-1)
        m_duvery_sance_zmena <- m_duvery_sance_1 - m_duvery_sance_0   # přírůstek pravděpodobnosti
        
        p <- p-1   # sniží se počet přeživších
        
        for(j in 1:h){
          for(i in 1:h){
            if (m_duvery[i,j] != 0) {
              m_duvery[i,j] <- m_duvery[i,j] + m_duvery_sance_zmena
            }   # matice důvěry se přepočítá podle aktuálních pravděpodobností, že druhá osoba je mafiánem
          }
        }
        
        m_duvery[as.double(vina),] <- 0   # když zemře tak ostatní se dozví co byl zač
        m_duvery[,as.double(vina)] <- 0   # ten co je mrtev si už taky nic myslet nemůže
        
        print(m_duvery)
        cat("\n")   # odřádkování
        
        break   # Někoho odsoudili a tak je konec dne
      }
    }
    # -----------------------------------------------------
    
    # Když si nejsou jistím nebo ani netuší, tak někoho vyberou náhodně jako obětního beránka
    zalobce <- c()
    repeat {
      zalobce <- sample(hraci, 1)
      if (names(zalobce) != "mrtev") {
        break
      }
    }
    
    beranek <- c()
    repeat {
      beranek <- sample(hraci, 1)
      if (names(beranek) != "mrtev" & zalobce != beranek) {
        break
      }
    }
    
    cat("\n",
        paste0("Hráč číslo ",
           zalobce,
           " (",
           names(zalobce),
           ")",
           " obviňuje náhodně hráče číslo ",
           beranek,
           " (",
           names(beranek),
           ")."
           ),
        "\n"
        )
    
    m_duvery[zalobce,beranek] <- m_duvery[zalobce,beranek] + mira_nastvani   # koeficient naštvání, že mne daná osoba jako beránka
    
    print("Hlasování o vině:")
    cat("\n")   # odřádkování
    
    hlasy <- c()
    for (i in 1:h) {
      if (m_duvery[beranek,i] >= 0.3) {   # Když si hráč myslí, že je víc jak poloviční šance, že daný hráč je mafián, tak pro něj bude hlasovat
        hlasy <- c(hlasy, 1)   # Hlasuje o vině
      } else {
        hlasy <- c(hlasy, 0)  # Hlasuje o nevině
      }
    }
    
    print(hlasy)
    cat("\n")   # odřádkování
    
    hlasovani <- sum(hlasy)/p   # Výsedek hlasování, počet hlasů pro vinu, dělím počtem přeživších
    if (hlasovani > 0.5) {   # K odsouzení je třeba nadpoloviční většiny hlasů
      
      if (names(hraci)[beranek] == "mafián") {   # pokud to byl mafián tak se sníží počet mafiánů o jedna
        m <- m - 1
      }
      
      names(hraci)[as.double(beranek)] <- "mrtev"
      dimnames(m_duvery) = list(names(hraci), names(hraci))   # přejmenuji sloupčeky podle aktuálního stavu hry
      
      m_duvery_sance_0 <- round(m/(p-1), digits = 2)   # současný stav pravděpodobnosti mafiána; (h-1) protože nepočítá sám sebe
      m_duvery_sance_1 <- round(m/(p-1-1), digits = 2)   # stav pravděpodobnosti mafiána s dalším zemřelým (-1)
      m_duvery_sance_zmena <- m_duvery_sance_1 - m_duvery_sance_0   # přírůstek pravděpodobnosti
      
      p <- p - 1   # sniží se počet přeživších
      
      for(j in 1:h){
        for(i in 1:h){
          if (m_duvery[i,j] != 0) {
            m_duvery[i,j] <- m_duvery[i,j] + m_duvery_sance_zmena
          }   # matice důvěry se přepočítá podle aktuálních pravděpodobností, že druhá osoba je mafiánem
        }
      }
      
      m_duvery[as.double(beranek),] <- 0   # když zemře tak ostatní se dozví co byl zač
      m_duvery[,as.double(beranek)] <- 0   # ten co je mrtev si už taky nic myslet nemůže
      
      print(m_duvery)
      cat("\n")   # odřádkování
      
      
      break   # Někoho odsoudili a tak je konec dne
    }
    
    # -----------------------------------------------------
    
  }   # konec dne 
  
  # 
  if (m/p >= 0.5) {   # Když mafie získá nadpolovitční většinu ve městě  
    print("Zločin ovládl celé město a Vyhrává mafie")
    break
  }
  
  if (m == 0) {   # Když se zabijí všichni mafiáni
    print("Městečko Palermo se zabvilo vlny zločinu a vyhrávají občané")
    break
  }  
  
}   # konec kola

# -----------------------------------------------------------------------------

sink()   # Uloží hru do textového souboru v pracovní složce 

###############################################################################
###############################################################################
###############################################################################