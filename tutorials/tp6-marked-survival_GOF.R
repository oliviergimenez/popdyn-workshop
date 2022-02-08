#' title: "TP 6 analyse de survie avec données sur animaux marqués"
#' output:
#'   pdf_document: default
#'   html_document: default
#' ---
#' 

#' On charge les packages `RMark` et `R2ucare`, ce dernier servant à tester les hypothèses des modèles de capture-recapture en population ouverte. 
## -----------------------------------------------
library(RMark)
library(R2ucare)


#' 
#' # Partie 3 : Hypothèses des modèles de capture-recapture, hétérogénéité et tests d'ajustement
#' 
#' Le but de cet exercice est de se familiariser avec les données de capture-recapture en population ouverte, d’ajuster par maximum de vraisemblance quelques modèles simples, de comparer ces modèles entre eux pour déterminer celui qui fournit la meilleure description des données et de tester la qualité de l’ajustement de ces modèles.
#' 
#' ## Question 1
#' 
#' On simule 2 jeux de donn?es de capture-recapture avec les param?tres de survie ($\phi$) et recapture ($p$) suivants :
#' * jeu de donn?es G1 : $\phi = 0.8$, $p = 0.8$ ;
#' * jeu de donn?es G2 : $\phi = 0.8$, $p = 0.2$.
#' 
## -----------------------------------------------
simul <- function(nind, nocc, phi, p){
  dat <- matrix(0, nrow = nind, ncol = nocc)
  dat[1:nind, 1] <- 1 # a single cohort
  for (i in 1:nind){
    # processus survie
    for (j in 2:nocc){
      alive.or.dead <- rbinom(1, 1, phi)
      # conditional on being alive at t, alive or dead at t+1
      dat[i, j] <- ifelse(dat[i, j - 1] == 0, 0, alive.or.dead) 
    }
    # processus detection
    for (j in 2:nocc){
      detected.or.not <- rbinom(1, 1, p)
      # conditional on being alive at t, detected or not at t
      dat[i, j] <- ifelse(dat[i, j] == 0, 0, detected.or.not) 
    }
  }
  data.frame(y = dat)   
}

#' 
#' 
## -----------------------------------------------
set.seed(2021)
nind <- 500
nocc <- 8
G1 <- simul(nind = nind, nocc = nocc, phi = 0.8, p = 0.8)
G2 <- simul(nind = nind, nocc = nocc, phi = 0.8, p = 0.2)

#' 
#' Ajuster separement sur G1 et G2 le modele ${\Phi(t), p(t)}$ appel? aussi le modele de Cormack-Jolly-Seber (CJS). Que pouvez-vous vous dire sur l’estimation des paramètres ?
#' 
## -----------------------------------------------
G1marked <- data.frame(ch = tidyr::unite(G1, col = "ch", sep = ""), 
                       n = rep(1, nrow(G1)))
G2marked <- data.frame(ch = tidyr::unite(G2, col = "ch", sep = ""), 
                       n = rep(1, nrow(G2)))

#' 
#' On prépare les données. 
## -----------------------------------------------
G1.proc <- process.data(G1marked)
G2.proc <- process.data(G2marked)
G1.ddl <- make.design.data(G1.proc)
G2.ddl <- make.design.data(G2.proc)

#' 
#' On spécifie les paramètres. 
## -----------------------------------------------
phit <- list(formula=~time)  
pt <- list(formula=~time)
phi <- list(formula=~1)  
p <- list(formula=~1)
#' 
#' On ajuste le modèle CJS aux données G1.  
## -----------------------------------------------
cjs.G1 <- mark(G1.proc,
               G1.ddl,
               model.parameters = list(Phi = phit, p = pt))
cjs.G1$results$real

phip.G1 <- mark(G1.proc,
               G1.ddl,
               model.parameters = list(Phi = phi, p = p))
phip.G1$results$real
#' 
#' Puis aux données G2. 
## -----------------------------------------------
cjs.G2 <- mark(G2.proc,
               G2.ddl,
               model.parameters = list(Phi = phit, p = pt))
cjs.G2$results$real

phip.G2 <- mark(G2.proc,
                G2.ddl,
                model.parameters = list(Phi = phi, p = p))
phip.G2$results$real

#' 
#' ## Question 2
#' 
#' a) Grouper les jeux de données G1 et G2 pour obtenir le jeu de données G1+G2.
#' 
## -----------------------------------------------
G1plusG2 <- rbind(G1, G2)

#' 
#' b) Ajuster le modèle CJS à G1+G2. Que remarquez-vous concernant l’estimation des paramètres ?
#' 
## -----------------------------------------------
G1G2marked <- data.frame(ch = tidyr::unite(G1plusG2, col = "ch", sep = ""),
                         n = rep(1, nrow(G1plusG2)))
G1G2.proc <- process.data(G1G2marked)
G1G2.ddl <- make.design.data(G1G2.proc)
cjs.G1G2 <- mark(G1G2.proc,
                 G1G2.ddl,
                 model.parameters = list(Phi = phit, p = pt))
cjs.G1G2$results$real

phip.G1G2 <- mark(G1G2.proc,
                 G1G2.ddl,
                 model.parameters = list(Phi = phi, p = p))
phip.G1G2$results$real

#' 
#' Modèle avec survie qui dépend du temps.
## -----------------------------------------------
phi.time <- list(formula=~time)  
cjs.G1G2 <- mark(G1G2.proc,
                 G1G2.ddl,
                 model.parameters = list(Phi = phi.time, p = p))
cjs.G1G2$results$real

#' 
#' ## Question 3
#' 
#' A l’aide du package `R2ucare`, tester la qualité de l’ajustement du modèle CJS aux données G1, G2 et G1+G2. Quelles sont vos conclusions ?
#' 
#' G1
## -----------------------------------------------
overall_CJS(G1, rep(1,nrow(G1)))

#' 
#' G2
## -----------------------------------------------
overall_CJS(G2, rep(1,nrow(G2)))

#' 
#' G1G2
## -----------------------------------------------
overall_CJS(G1plusG2, rep(1,nrow(G1plusG2)))

#' 
#' ## Question 4
#' 
#' Il peut y avoir des animaux en transit sur la zone d’étude.
#' 
#' a) Pour créer artificiellement une telle situation, rajouter 50 individus en transit (i.e. possédant une histoire avec un seul événement de capture) à chaque date dans G1. 
#' 
## -----------------------------------------------
G1transit <- as.matrix(G1)
ntransients <- 50
for (j in 1:nocc){
  zeros <- matrix(0, nrow = ntransients, ncol = nocc)
  zeros[, j] <- 1
  G1transit <- rbind(G1transit, zeros)
}
G1transit <- data.frame(y = G1transit)

#' 
## -----------------------------------------------
dim(G1transit)

#' 
#' 
## -----------------------------------------------
head(G1transit)

#' 
#' 
## -----------------------------------------------
tail(G1transit)

#' 
#' 
#' b) Faire tourner le modèle CJS à ces nouvelles données avec `RMark`. Quelles sont vos conclusions concernant les estimations ?
#' 
## -----------------------------------------------
G1transitmarked <- data.frame(ch = tidyr::unite(G1transit, col = "ch", sep = ""), 
                              n = rep(1, nrow(G1transit)))

#' 
## -----------------------------------------------
G1transit.proc <- process.data(G1transitmarked)
G1transit.ddl <- make.design.data(G1transit.proc)

#' 
#' Ajuste le modèle. 
## -----------------------------------------------
phip.G1transit <- mark(G1transit.proc,
                      G1transit.ddl,
                      model.parameters = list(Phi = phi, p = p))
phip.G1transit$results$real

#' 
#' Idem avec survie qui dépend du temps.
## -----------------------------------------------
cjs.G1transit <- mark(G1transit.proc,
                      G1transit.ddl,
                      model.parameters = list(Phi = phit, p = pt))
cjs.G1transit$results$real

#' 
#' c) Tester l’ajustement du modèle CJS à ces mêmes données avec `R2ucare`. Interpréter en particulier la composante 3.SR du test.
#' 
## -----------------------------------------------
overall_CJS(G1transit, rep(1,nrow(G1transit)))

test2ct(G1transit, rep(1,nrow(G1transit)))

test3sr(G1transit, rep(1,nrow(G1transit)))

#' 
#' 
#' d) Faire tourner un modèle à 2 classes d’âge sur la survie $\phi(a2*t)$ avec `RMark`. Vos conclusions ?
#' 
## -----------------------------------------------
G1transit.ddl <- make.design.data(G1transit.proc)
# create 0, 1+ age variable
G1transit.ddl <- add.design.data(G1transit.proc,
                                 G1transit.ddl, # add 2 age-class structure to design matrix
                                 "Phi",
                                 type = "age",
                                 bins = c(0, 1, nocc - 1),
                                 name = "ageclass",
                                 right = FALSE)

#' 
#' On spécifie une survie qui dépend de l'âge.
## -----------------------------------------------
phi.age <- list(formula=~ageclass) # age effect on survival

#' 
#' On ajuste le modèle.
## -----------------------------------------------
cjsage.G1transit <- mark(G1transit.proc,
                         G1transit.ddl,
                         model.parameters = list(Phi = phi.age, p = p))
cjsage.G1transit$results$real

#' 
#' D'une autre façon. 
## -----------------------------------------------
G1transit.ddl <- make.design.data(G1transit.proc)
#max age 4
G1transit.ddl$Phi$max.age <- as.factor((G1transit.ddl$Phi$Age < 1) * G1transit.ddl$Phi$Age + (G1transit.ddl$Phi$Age>0) * 1)
phi.max.age <- list(formula=~max.age)
cjsaget.G1transit <- mark(G1transit.proc,
                          G1transit.ddl,
                          model.parameters = list(Phi = phi.max.age, p = p))

PIMS(cjsaget.G1transit,"Phi")
cjsaget.G1transit$results$real



# Donnees cincles
## -----------------------------------------------
cincle <- read_inp("dat/cincle-plongeur.inp")
overall_CJS(cincle$encounter_histories, cincle$sample_size)
test2ct(cincle$encounter_histories, cincle$sample_size)
test3sr(cincle$encounter_histories, cincle$sample_size)

# Donnes martinet
##------------------------------------------------
martinet <- read_inp("dat/martinet-noir.inp",
                        group.df = data.frame(colonie = c("nord", "sud")))

# tests nord
mask <- (martinet$groups == 'nord')
overall_CJS(martinet$encounter_histories[mask,], martinet$sample_size[mask])
test2ct(martinet$encounter_histories[mask,], martinet$sample_size[mask])
test3sr(martinet$encounter_histories[mask,], martinet$sample_size[mask])

# tests sud
mask <- (martinet$groups == 'sud')
overall_CJS(martinet$encounter_histories[mask,], martinet$sample_size[mask])
test2ct(martinet$encounter_histories[mask,], martinet$sample_size[mask])
test3sr(martinet$encounter_histories[mask,], martinet$sample_size[mask])


#' 
#' Supprime fichiers créés en cours de route.
## -----------------------------------------------
cleanup(ask = FALSE)


