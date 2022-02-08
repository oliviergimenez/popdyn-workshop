#' ---
#' title: "TP 5 analyse de survie avec données sur animaux marqués"
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
#' 
#' # Partie 2 : Estimation de la survie, exemple du martinet noir
#' 
#' On remet les compteurs à 0.
## -----------------------------------------------
rm(list = ls())

#' 
#' 
#' Les données.
## -----------------------------------------------
martinet <- convert.inp("dat/martinet-noir.inp",
                        group.df = data.frame(colonie = c("nord", "sud")), 
                        covariates = NULL)
head(martinet)

#' 
#' On prépare les données.
## -----------------------------------------------
martinet.proc <- process.data(martinet, 
                              begin.time = 1, 
                              model = "CJS", 
                              groups = ("colonie"))
martinet.ddl <- make.design.data(martinet.proc)

#' 
#' On spécifie les effets sur les paramètres.
## -----------------------------------------------
phit <- list(formula=~time)
phi <- list(formula=~1)
pt <- list(formula=~time)
p <- list(formula=~1)

#' 
#' Fait tourner modèle CJS, et examine les paramètres estimés. 
## -----------------------------------------------
cjs.martinet <- mark(martinet.proc,
                      martinet.ddl,
                      model.parameters = list(Phi = phit, p = pt))
cjs.martinet$results$real

#' 
#' PIM pour CJS.
## -----------------------------------------------
PIMS(cjs.martinet,"Phi")

#' 
#' Fait tourner modèle avec param constants. 
## -----------------------------------------------
phip.martinet <- mark(martinet.proc,
                      martinet.ddl,
                      model.parameters = list(Phi = phi, p = p))
phip.martinet$results$real
PIMS(phip.martinet,"Phi")
PIMS(phip.martinet,"p")

phitp.martinet <- mark(martinet.proc,
                      martinet.ddl,
                      model.parameters = list(Phi = phit, p = p))
phitp.martinet$results$real
PIMS(phitp.martinet,"Phi")
PIMS(phitp.martinet,"p")

phipt.martinet <- mark(martinet.proc,
                       martinet.ddl,
                       model.parameters = list(Phi = phi, p = pt))
phipt.martinet$results$real
PIMS(phipt.martinet,"Phi")
PIMS(phipt.martinet,"p")

#selection de mod?les
collect.models()

#'2. mod?les avec effet age
#'
#'modele avec toutes les classes d'?ge sur Phi
#' On définit l'effet âge (temps écoulé depuis la première capture). 
## -----------------------------------------------
phi.age <- list(formula =~ age)

#' 
#' On ajuste le modèle.
## -----------------------------------------------
phiagept.martinet <- mark(martinet.proc,
                      martinet.ddl,
                      model.parameters = list(Phi = phi.age, 
                                              p = pt))



#' 
#' Modèle avec 2 classes d'âge sur la survie.
## -----------------------------------------------
# create 0, 1+ age variable
martinet.ddl <- add.design.data(martinet.proc,
                                martinet.ddl, # add 2 age-class structure to design matrix
                             "Phi",
                             type = "age",
                             bins = c(0, 1, 7),
                             name = "ageclass",
                             right = FALSE)

#' 
#' On spécifie une survie qui dépend de l'âge.
## -----------------------------------------------
phi.age2 <- list(formula=~ageclass) # age effect on survival

#' 
#' On ajuste le modèle avec survie âge-dépendante et prob de recapture constante.
## -----------------------------------------------
Phiage2.martinet <- mark(martinet.proc,
                        martinet.ddl,
                        model.parameters = list(Phi = phi.age2, p = pt))
Phiage2.martinet$results$real

#' 
#' PIM pour CJS avec âge.
## -----------------------------------------------
PIMS(Phiage2.martinet,"Phi")
PIMS(Phiage2.martinet,"p")

collect.models()

#' 3. mod?les avec groupes sans effet temps (g)
#' 
phi.g <- list(formula=~colonie) # colonie effect on survival
p.g <- list(formula=~colonie) # colonie effect on recapture

Phigpg <- mark(martinet.proc,
                 martinet.ddl,
                 model.parameters = list(Phi = phi.g, p = p.g))
Phigpg$results$real
PIMS(Phigpg.martinet,"Phi")
PIMS(Phigpg.martinet,"p")

Phigp <- mark(martinet.proc,
               martinet.ddl,
               model.parameters = list(Phi = phi.g, p = p))
Phigp$results$real

Phigpt <- mark(martinet.proc,
              martinet.ddl,
              model.parameters = list(Phi = phi.g, p = pt))
Phigpt$results$real
PIMS(Phigpt,"Phi")
PIMS(Phigpt,"p")

collect.models()

#' 4. mod?les avec groupes et effet temps interactif (g.t)
#' 
phi.g.t <- list(formula=~colonie*time) # colonie and time effect on survival
p.g.t <- list(formula=~colonie*time) # colonie and time effect on recapture

Phigtp <- mark(martinet.proc,
               martinet.ddl,
               model.parameters = list(Phi = phi.g.t, p = p))
Phigtp$results$real
PIMS(Phigtp,"Phi")
PIMS(Phigtp,"p")

Phigtpt <- mark(martinet.proc,
               martinet.ddl,
               model.parameters = list(Phi = phi.g.t, p = pt))
Phigtpt$results$real
PIMS(Phigtpt,"Phi")
PIMS(Phigtpt,"p")

collect.models()

#' 5. mod?les avec groupes et effet temps additif (g+t)
#' 
phi.gaddt <- list(formula=~colonie+time) # colonie + time effect on survival
p.gaddt <- list(formula=~colonie+time) # colonie + time effect on recapture

Phigaddtpt <- mark(martinet.proc,
               martinet.ddl,
               model.parameters = list(Phi = phi.gaddt, p = pt))
Phigaddtpt$results$real

PIMS(Phigaddtpt,"Phi")
PIMS(Phigaddtpt,"p")

Phigpaddt <- mark(martinet.proc,
                   martinet.ddl,
                   model.parameters = list(Phi = phi.g, p = p.gaddt))
Phigpaddt$results$real

collect.models()

Phigpgt <- mark(martinet.proc,
                  martinet.ddl,
                  model.parameters = list(Phi = phi.g, p = p.g.t))
Phigpgt$results$real
PIMS(Phigpgt,"Phi")
PIMS(Phigpgt,"p")

collect.models()

# comparaison visuelle des mod?les Phi(g)p(g*t) et Phi(g)p(g+t)
# recapture

par(mfrow = c(1,2))

# modele additif
plot(x = 1:7,
     y = Phigpaddt$results$real[3:9, 1],
     type = 'l',
     lwd = 3,
     col = "blue",
     ylim = c(0,1),
     xlab = "",
     ylab = "recapture estimée",
     main = "phi(colonie) p(colonie + temps)")
lines(x = 1:7,
     y = Phigpaddt$results$real[10:16, 1],
     type = 'l',
     lwd = 3,
     col = "green")
legend("bottomleft", 
       legend = c("nord",
                  "sud"),
       lty = c(1, 1),
       col = c("blue","green"),
       lwd = 3)


# modele interaction
plot(x = 1:7,
     y = Phigpgt$results$real[3:9, 1],
     type = 'l',
     lwd = 3,
     col = "blue",
     ylim = c(0,1),
     xlab = "",
     ylab = "recapture estimée",
     main = "phi(colonie) p(colonie * temps)")
lines(x = 1:7,
      y = Phigpgt$results$real[10:16, 1],
      type = 'l',
      lwd = 3,
      col = "green")
legend("bottomleft", 
       legend = c("nord",
                  "sud"),
       lty = c(1, 1),
       col = c("blue","green"),
       lwd = 3)


#' 
#' Supprime fichiers créés en cours de route.
## -----------------------------------------------
cleanup(ask = FALSE)

#' 
