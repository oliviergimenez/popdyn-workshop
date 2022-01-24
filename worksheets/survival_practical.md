---
title: "Estimation of survival and productivity using capture-recapture and generalized linear models"
output:
  html_document:
    keep_md: yes
    df_print: paged
    highlight: tango
    number_sections: yes
    theme: united
    toc: yes
    toc_depth: 2
  pdf_document:
    toc: yes
    toc_depth: '2'
date: "January 2022"
---



# Partie 1 : Estimation de la survie, exemple du cincle plongeur

## Premiers modèles avec des effets temps

On charge les packages `RMark` et `R2ucare`, ce dernier servant à tester les hypothèses des modèles de capture-recapture en population ouverte. 

```r
library(RMark)
library(R2ucare)
```

Les données.

```r
cincle <- convert.inp("dat/cincle-plongeur.inp")
```

On jette un coup d'oeil.

```r
head(cincle)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["ch"],"name":[1],"type":["chr"],"align":["left"]},{"label":["freq"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"0000010","2":"23","_rn_":"1"},{"1":"0000011","2":"23","_rn_":"2"},{"1":"0000100","2":"16","_rn_":"3"},{"1":"0000110","2":"9","_rn_":"4"},{"1":"0000111","2":"16","_rn_":"5"},{"1":"0001000","2":"16","_rn_":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

On prépare les données. 

```r
cincle.proc <- process.data(cincle, 
                              begin.time = 1, 
                              model = "CJS")
cincle.ddl <- make.design.data(cincle.proc)
```

On inspecte la structure pour la survie.

```r
head(cincle.ddl$Phi)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["par.index"],"name":[1],"type":["int"],"align":["right"]},{"label":["model.index"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["group"],"name":[3],"type":["fct"],"align":["left"]},{"label":["cohort"],"name":[4],"type":["fct"],"align":["left"]},{"label":["age"],"name":[5],"type":["fct"],"align":["left"]},{"label":["time"],"name":[6],"type":["fct"],"align":["left"]},{"label":["occ.cohort"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["Cohort"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["Age"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["Time"],"name":[10],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1","3":"1","4":"1","5":"0","6":"1","7":"1","8":"0","9":"0","10":"0","_rn_":"1"},{"1":"2","2":"2","3":"1","4":"1","5":"1","6":"2","7":"1","8":"0","9":"1","10":"1","_rn_":"2"},{"1":"3","2":"3","3":"1","4":"1","5":"2","6":"3","7":"1","8":"0","9":"2","10":"2","_rn_":"3"},{"1":"4","2":"4","3":"1","4":"1","5":"3","6":"4","7":"1","8":"0","9":"3","10":"3","_rn_":"4"},{"1":"5","2":"5","3":"1","4":"1","5":"4","6":"5","7":"1","8":"0","9":"4","10":"4","_rn_":"5"},{"1":"6","2":"6","3":"1","4":"1","5":"5","6":"6","7":"1","8":"0","9":"5","10":"5","_rn_":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Et la détection.

```r
head(cincle.ddl$p)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["par.index"],"name":[1],"type":["int"],"align":["right"]},{"label":["model.index"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["group"],"name":[3],"type":["fct"],"align":["left"]},{"label":["cohort"],"name":[4],"type":["fct"],"align":["left"]},{"label":["age"],"name":[5],"type":["fct"],"align":["left"]},{"label":["time"],"name":[6],"type":["fct"],"align":["left"]},{"label":["occ.cohort"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["Cohort"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["Age"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["Time"],"name":[10],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"22","3":"1","4":"1","5":"1","6":"2","7":"1","8":"0","9":"1","10":"0","_rn_":"1"},{"1":"2","2":"23","3":"1","4":"1","5":"2","6":"3","7":"1","8":"0","9":"2","10":"1","_rn_":"2"},{"1":"3","2":"24","3":"1","4":"1","5":"3","6":"4","7":"1","8":"0","9":"3","10":"2","_rn_":"3"},{"1":"4","2":"25","3":"1","4":"1","5":"4","6":"5","7":"1","8":"0","9":"4","10":"3","_rn_":"4"},{"1":"5","2":"26","3":"1","4":"1","5":"5","6":"6","7":"1","8":"0","9":"5","10":"4","_rn_":"5"},{"1":"6","2":"27","3":"1","4":"1","5":"6","6":"7","7":"1","8":"0","9":"6","10":"5","_rn_":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

On spécifie les effets sur les paramètres.

```r
phit <- list(formula=~time)
phi <- list(formula=~1)
pt <- list(formula=~time)
p <- list(formula=~1)
```

On ajuste le modèle Cormack-Jolly-Seber (CJS). 

```r
cjs.cincle <- mark(cincle.proc,
                   cincle.ddl,
                   model.parameters = list(Phi = phit, p = pt))
```

```
## 
## Output summary for CJS model
## Name : Phi(~time)p(~time) 
## 
## Npar :  12  (unadjusted=11)
## -2lnL:  656.9502
## AICc :  681.7057  (unadjusted=679.58789)
## 
## Beta
##                   estimate          se          lcl         ucl
## Phi:(Intercept)  0.9354621   0.7685242   -0.5708454   2.4417697
## Phi:time2       -1.1982821   0.8706719   -2.9047991   0.5082349
## Phi:time3       -1.0228356   0.8049165   -2.6004719   0.5548007
## Phi:time4       -0.4198653   0.8091502   -2.0057997   1.1660691
## Phi:time5       -0.5361042   0.8031452   -2.1102688   1.0380604
## Phi:time6        0.2481360 234.7281100 -459.8189600 460.3152300
## p:(Intercept)    0.8292768   0.7837333   -0.7068406   2.3653941
## p:time3          1.6556307   1.2913781   -0.8754703   4.1867318
## p:time4          1.5220981   1.0729131   -0.5808116   3.6250078
## p:time5          1.3767490   0.9884799   -0.5606717   3.3141697
## p:time6          1.7950968   1.0688749   -0.2998981   3.8900917
## p:time7         -0.0147540 179.2662700 -351.3766400 351.3471400
## 
## 
## Real Parameter Phi
##  
##           1         2         3         4         5         6
## 1 0.7181821 0.4346706 0.4781705 0.6261176 0.5985334 0.7655941
## 2           0.4346706 0.4781705 0.6261176 0.5985334 0.7655941
## 3                     0.4781705 0.6261176 0.5985334 0.7655941
## 4                               0.6261176 0.5985334 0.7655941
## 5                                         0.5985334 0.7655941
## 6                                                   0.7655941
## 
## 
## Real Parameter p
##  
##          2        3         4         5         6         7
## 1 0.696202 0.923077 0.9130434 0.9007893 0.9324138 0.6930724
## 2          0.923077 0.9130434 0.9007893 0.9324138 0.6930724
## 3                   0.9130434 0.9007893 0.9324138 0.6930724
## 4                             0.9007893 0.9324138 0.6930724
## 5                                       0.9324138 0.6930724
## 6                                                 0.6930724
```

Inspectons les résultats. 

```r
cjs.cincle$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.7181821","2":"0.1555467","3":"3.610418e-01","4":"0.9199575","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"0.4346706","2":"0.0688290","3":"3.075046e-01","4":"0.5710586","5":"","6":"","_rn_":"Phi g1 c1 a1 t2"},{"1":"0.4781705","2":"0.0597091","3":"3.643839e-01","4":"0.5942686","5":"","6":"","_rn_":"Phi g1 c1 a2 t3"},{"1":"0.6261176","2":"0.0592655","3":"5.048460e-01","4":"0.7333740","5":"","6":"","_rn_":"Phi g1 c1 a3 t4"},{"1":"0.5985334","2":"0.0560517","3":"4.855434e-01","4":"0.7019411","5":"","6":"","_rn_":"Phi g1 c1 a4 t5"},{"1":"0.7655941","2":"42.1242960","3":"5.119218e-200","4":"1.0000000","5":"","6":"","_rn_":"Phi g1 c1 a5 t6"},{"1":"0.6962020","2":"0.1657633","3":"3.302973e-01","4":"0.9141501","5":"","6":"","_rn_":"p g1 c1 a1 t2"},{"1":"0.9230770","2":"0.0728777","3":"6.161498e-01","4":"0.9889758","5":"","6":"","_rn_":"p g1 c1 a2 t3"},{"1":"0.9130434","2":"0.0581758","3":"7.140649e-01","4":"0.9778505","5":"","6":"","_rn_":"p g1 c1 a3 t4"},{"1":"0.9007893","2":"0.0538329","3":"7.360178e-01","4":"0.9672856","5":"","6":"","_rn_":"p g1 c1 a4 t5"},{"1":"0.9324138","2":"0.0458025","3":"7.684928e-01","4":"0.9828579","5":"","6":"","_rn_":"p g1 c1 a5 t6"},{"1":"0.6930724","2":"38.1340410","3":"5.745209e-153","4":"1.0000000","5":"","6":"","_rn_":"p g1 c1 a6 t7"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Les PIM pour CJS.

```r
PIMS(cjs.cincle,"Phi")
```

```
## group = Group 1 
##    1  2  3  4  5  6
## 1  1  2  3  4  5  6
## 2     2  3  4  5  6
## 3        3  4  5  6
## 4           4  5  6
## 5              5  6
## 6                 6
```

On fait tourner le modèle avec paramètres constants. 

```r
phip.cincle <- mark(cincle.proc,
                    cincle.ddl,
                    model.parameters = list(Phi = phi, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~1)p(~1) 
## 
## Npar :  2
## -2lnL:  666.8377
## AICc :  670.866
## 
## Beta
##                  estimate        se       lcl       ucl
## Phi:(Intercept) 0.2421484 0.1020127 0.0422034 0.4420933
## p:(Intercept)   2.2262659 0.3251094 1.5890516 2.8634803
## 
## 
## Real Parameter Phi
##  
##          1        2        3        4        5        6
## 1 0.560243 0.560243 0.560243 0.560243 0.560243 0.560243
## 2          0.560243 0.560243 0.560243 0.560243 0.560243
## 3                   0.560243 0.560243 0.560243 0.560243
## 4                            0.560243 0.560243 0.560243
## 5                                     0.560243 0.560243
## 6                                              0.560243
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7
## 1 0.9025835 0.9025835 0.9025835 0.9025835 0.9025835 0.9025835
## 2           0.9025835 0.9025835 0.9025835 0.9025835 0.9025835
## 3                     0.9025835 0.9025835 0.9025835 0.9025835
## 4                               0.9025835 0.9025835 0.9025835
## 5                                         0.9025835 0.9025835
## 6                                                   0.9025835
```

Les résultats.

```r
phip.cincle$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.5602430","2":"0.0251330","3":"0.5105493","4":"0.6087577","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"0.9025835","2":"0.0285857","3":"0.8304826","4":"0.9460113","5":"","6":"","_rn_":"p g1 c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Les PIM.

```r
PIMS(phip.cincle,"Phi")
```

```
## group = Group 1 
##    1  2  3  4  5  6
## 1  1  1  1  1  1  1
## 2     1  1  1  1  1
## 3        1  1  1  1
## 4           1  1  1
## 5              1  1
## 6                 1
```

```r
PIMS(phip.cincle,"p")
```

```
## group = Group 1 
##    2  3  4  5  6  7
## 1  2  2  2  2  2  2
## 2     2  2  2  2  2
## 3        2  2  2  2
## 4           2  2  2
## 5              2  2
## 6                 2
```

On considère deux autres modèles, avec l'effet temps sur la survie mais pas sur la détection, et inversément. 

```r
phitp.cincle <- mark(cincle.proc,
                    cincle.ddl,
                    model.parameters = list(Phi = phit, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~time)p(~1) 
## 
## Npar :  7
## -2lnL:  659.7301
## AICc :  673.998
## 
## Beta
##                   estimate        se       lcl       ucl
## Phi:(Intercept)  0.5143901 0.4767827 -0.420104 1.4488842
## Phi:time2       -0.6981400 0.5537221 -1.783435 0.3871553
## Phi:time3       -0.6009354 0.5301019 -1.639935 0.4380644
## Phi:time4       -0.0061054 0.5334638 -1.051694 1.0394837
## Phi:time5       -0.0757105 0.5276525 -1.109910 0.9584885
## Phi:time6       -0.1780623 0.5265674 -1.210134 0.8540098
## p:(Intercept)    2.2203956 0.3288850  1.575781 2.8650103
## 
## 
## Real Parameter Phi
##  
##           1         2         3         4         5         6
## 1 0.6258351 0.4541914 0.4783772 0.6244043 0.6079444 0.5832982
## 2           0.4541914 0.4783772 0.6244043 0.6079444 0.5832982
## 3                     0.4783772 0.6244043 0.6079444 0.5832982
## 4                               0.6244043 0.6079444 0.5832982
## 5                                         0.6079444 0.5832982
## 6                                                   0.5832982
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7
## 1 0.9020662 0.9020662 0.9020662 0.9020662 0.9020662 0.9020662
## 2           0.9020662 0.9020662 0.9020662 0.9020662 0.9020662
## 3                     0.9020662 0.9020662 0.9020662 0.9020662
## 4                               0.9020662 0.9020662 0.9020662
## 5                                         0.9020662 0.9020662
## 6                                                   0.9020662
```

```r
phipt.cincle <- mark(cincle.proc,
                    cincle.ddl,
                    model.parameters = list(Phi = phi, p = pt))
```

```
## 
## Output summary for CJS model
## Name : Phi(~1)p(~time) 
## 
## Npar :  7
## -2lnL:  664.4802
## AICc :  678.7481
## 
## Beta
##                  estimate        se        lcl       ucl
## Phi:(Intercept) 0.2131640 0.1121136 -0.0065786 0.4329066
## p:(Intercept)   1.2955249 0.7437225 -0.1621713 2.7532211
## p:time3         0.8005297 1.1635472 -1.4800230 3.0810823
## p:time4         0.6512761 1.0018542 -1.3123580 2.6149103
## p:time5         0.9977284 0.9454474 -0.8553486 2.8508054
## p:time6         1.4658878 1.0303992 -0.5536945 3.4854702
## p:time7         1.9900754 3.0642218 -4.0157993 7.9959502
## 
## 
## Real Parameter Phi
##  
##           1         2         3         4         5         6
## 1 0.5530901 0.5530901 0.5530901 0.5530901 0.5530901 0.5530901
## 2           0.5530901 0.5530901 0.5530901 0.5530901 0.5530901
## 3                     0.5530901 0.5530901 0.5530901 0.5530901
## 4                               0.5530901 0.5530901 0.5530901
## 5                                         0.5530901 0.5530901
## 6                                                   0.5530901
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7
## 1 0.7850809 0.8905191 0.8750974 0.9083167 0.9405547 0.9639315
## 2           0.8905191 0.8750974 0.9083167 0.9405547 0.9639315
## 3                     0.8750974 0.9083167 0.9405547 0.9639315
## 4                               0.9083167 0.9405547 0.9639315
## 5                                         0.9405547 0.9639315
## 6                                                   0.9639315
```

On affiche la sélection de modèles.

```r
collect.models()
```

```
##                model npar     AICc DeltaAICc      weight Deviance
## 2       Phi(~1)p(~1)    2 670.8660  0.000000 0.811204642 58.15788
## 4    Phi(~time)p(~1)    7 673.9980  3.132004 0.169443317 51.05031
## 3    Phi(~1)p(~time)    7 678.7481  7.882084 0.015760053 55.80039
## 1 Phi(~time)p(~time)   12 681.7057 10.839629 0.003591988 48.27043
```

On a 7 occasions de capture, donc 6 paramètres de survie. La première année de capture dans le jeu de données est 1981, alors on peut estimer la survie entre 1981 et 1982, entre 1982 et 1983 etc. Une inondation eut lieu en 1982 et 1983, avec un effet possible sur la survie. On va comparer les modèles précédents à un modèle incorporant un effet inondation sur les survies sur les intervalles (1982-1983) et (1983-1984). 

Jetons un coup d'oeil à la structure sur la survie.

```r
cincle.ddl$Phi
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["par.index"],"name":[1],"type":["int"],"align":["right"]},{"label":["model.index"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["group"],"name":[3],"type":["fct"],"align":["left"]},{"label":["cohort"],"name":[4],"type":["fct"],"align":["left"]},{"label":["age"],"name":[5],"type":["fct"],"align":["left"]},{"label":["time"],"name":[6],"type":["fct"],"align":["left"]},{"label":["occ.cohort"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["Cohort"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["Age"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["Time"],"name":[10],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1","3":"1","4":"1","5":"0","6":"1","7":"1","8":"0","9":"0","10":"0","_rn_":"1"},{"1":"2","2":"2","3":"1","4":"1","5":"1","6":"2","7":"1","8":"0","9":"1","10":"1","_rn_":"2"},{"1":"3","2":"3","3":"1","4":"1","5":"2","6":"3","7":"1","8":"0","9":"2","10":"2","_rn_":"3"},{"1":"4","2":"4","3":"1","4":"1","5":"3","6":"4","7":"1","8":"0","9":"3","10":"3","_rn_":"4"},{"1":"5","2":"5","3":"1","4":"1","5":"4","6":"5","7":"1","8":"0","9":"4","10":"4","_rn_":"5"},{"1":"6","2":"6","3":"1","4":"1","5":"5","6":"6","7":"1","8":"0","9":"5","10":"5","_rn_":"6"},{"1":"7","2":"7","3":"1","4":"2","5":"0","6":"2","7":"2","8":"1","9":"0","10":"1","_rn_":"7"},{"1":"8","2":"8","3":"1","4":"2","5":"1","6":"3","7":"2","8":"1","9":"1","10":"2","_rn_":"8"},{"1":"9","2":"9","3":"1","4":"2","5":"2","6":"4","7":"2","8":"1","9":"2","10":"3","_rn_":"9"},{"1":"10","2":"10","3":"1","4":"2","5":"3","6":"5","7":"2","8":"1","9":"3","10":"4","_rn_":"10"},{"1":"11","2":"11","3":"1","4":"2","5":"4","6":"6","7":"2","8":"1","9":"4","10":"5","_rn_":"11"},{"1":"12","2":"12","3":"1","4":"3","5":"0","6":"3","7":"3","8":"2","9":"0","10":"2","_rn_":"12"},{"1":"13","2":"13","3":"1","4":"3","5":"1","6":"4","7":"3","8":"2","9":"1","10":"3","_rn_":"13"},{"1":"14","2":"14","3":"1","4":"3","5":"2","6":"5","7":"3","8":"2","9":"2","10":"4","_rn_":"14"},{"1":"15","2":"15","3":"1","4":"3","5":"3","6":"6","7":"3","8":"2","9":"3","10":"5","_rn_":"15"},{"1":"16","2":"16","3":"1","4":"4","5":"0","6":"4","7":"4","8":"3","9":"0","10":"3","_rn_":"16"},{"1":"17","2":"17","3":"1","4":"4","5":"1","6":"5","7":"4","8":"3","9":"1","10":"4","_rn_":"17"},{"1":"18","2":"18","3":"1","4":"4","5":"2","6":"6","7":"4","8":"3","9":"2","10":"5","_rn_":"18"},{"1":"19","2":"19","3":"1","4":"5","5":"0","6":"5","7":"5","8":"4","9":"0","10":"4","_rn_":"19"},{"1":"20","2":"20","3":"1","4":"5","5":"1","6":"6","7":"5","8":"4","9":"1","10":"5","_rn_":"20"},{"1":"21","2":"21","3":"1","4":"6","5":"0","6":"6","7":"6","8":"5","9":"0","10":"5","_rn_":"21"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

On ajoute un effet inondation sur la survie.

```r
cincle.ddl$Phi$Inondation <- 0
cincle.ddl$Phi$Inondation[cincle.ddl$Phi$time==2 | cincle.ddl$Phi$time==3] <- 1
```

On définit l'effet en question, et on fait tourner le modèle correspondant.

```r
phiinondation <- list(formula=~Inondation)
phiinondationp.cincle <- mark(cincle.proc,
                              cincle.ddl,
                              model.parameters = list(Phi = phiinondation, 
                                                      p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~Inondation)p(~1) 
## 
## Npar :  3
## -2lnL:  660.1028
## AICc :  666.1597
## 
## Beta
##                   estimate        se        lcl        ucl
## Phi:(Intercept)  0.4351207 0.1297482  0.1808142  0.6894272
## Phi:Inondation  -0.5599740 0.2163758 -0.9840705 -0.1358775
## p:(Intercept)    2.1948811 0.3246088  1.5586478  2.8311145
## 
## 
## Real Parameter Phi
##  
##           1         2         3         4         5         6
## 1 0.6070958 0.4688272 0.4688272 0.6070958 0.6070958 0.6070958
## 2           0.4688272 0.4688272 0.6070958 0.6070958 0.6070958
## 3                     0.4688272 0.6070958 0.6070958 0.6070958
## 4                               0.6070958 0.6070958 0.6070958
## 5                                         0.6070958 0.6070958
## 6                                                   0.6070958
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7
## 1 0.8997889 0.8997889 0.8997889 0.8997889 0.8997889 0.8997889
## 2           0.8997889 0.8997889 0.8997889 0.8997889 0.8997889
## 3                     0.8997889 0.8997889 0.8997889 0.8997889
## 4                               0.8997889 0.8997889 0.8997889
## 5                                         0.8997889 0.8997889
## 6                                                   0.8997889
```

On compare les modèles avec l'AIC.

```r
collect.models()
```

```
##                   model npar     AICc DeltaAICc       weight Deviance
## 2 Phi(~Inondation)p(~1)    3 666.1596  0.000000 0.8951028662 51.42300
## 3          Phi(~1)p(~1)    2 670.8660  4.706387 0.0850930418 58.15788
## 5       Phi(~time)p(~1)    7 673.9980  7.838391 0.0177741183 51.05031
## 4       Phi(~1)p(~time)    7 678.7481 12.588471 0.0016531844 55.80039
## 1    Phi(~time)p(~time)   12 681.7057 15.546016 0.0003767892 48.27043
```

## Pour aller plus loin avec les effets de l'environnement

On ajoute les covariables environnementales. 

```r
cov.cincle <- readxl::read_xls("dat/covariables-environnementales-cincle-plongeur.xls")
cov.cincle
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["année"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["débit (l/sec)"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["temperature hiver (°C)"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"1981","2":"443","3":"-2.3"},{"1":"1982","2":"1114","3":"-0.4"},{"1":"1983","2":"529","3":"-1.2"},{"1":"1984","2":"434","3":"-4.2"},{"1":"1985","2":"627","3":"-3.0"},{"1":"1986","2":"466","3":"-2.8"},{"1":"1987","2":"730","3":"0.1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

On simplifie le nom des colonnes.

```r
cov.cincle <- janitor::clean_names(cov.cincle)
cov.cincle
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["annee"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["debit_l_sec"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["temperature_hiver_c"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"1981","2":"443","3":"-2.3"},{"1":"1982","2":"1114","3":"-0.4"},{"1":"1983","2":"529","3":"-1.2"},{"1":"1984","2":"434","3":"-4.2"},{"1":"1985","2":"627","3":"-3.0"},{"1":"1986","2":"466","3":"-2.8"},{"1":"1987","2":"730","3":"0.1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Pour rappel, on a 7 occasions de capture, donc 6 paramètres de survie. Si on suppose que la première année de capture dans le jeu de données cincle est 1981, alors on peut estimer la survie entre 1981 et 1982, à laquelle on applique la valeur de covariable en 1981, etc... jusqu'à la survie entre 1986 et 1987 à laquelle s'applique la valeur de covariable de 1986, donc on n'a pas besoin de la dernière ligne dans le jeu de données. 

```r
cov.cincle <- cov.cincle[!(cov.cincle$annee == "1987"),]
```

Jetons un coup d'oeil à la structure sur la survie.

```r
cincle.ddl$Phi
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["par.index"],"name":[1],"type":["int"],"align":["right"]},{"label":["model.index"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["group"],"name":[3],"type":["fct"],"align":["left"]},{"label":["cohort"],"name":[4],"type":["fct"],"align":["left"]},{"label":["age"],"name":[5],"type":["fct"],"align":["left"]},{"label":["time"],"name":[6],"type":["fct"],"align":["left"]},{"label":["occ.cohort"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["Cohort"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["Age"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["Time"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["Inondation"],"name":[11],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1","3":"1","4":"1","5":"0","6":"1","7":"1","8":"0","9":"0","10":"0","11":"0","_rn_":"1"},{"1":"2","2":"2","3":"1","4":"1","5":"1","6":"2","7":"1","8":"0","9":"1","10":"1","11":"1","_rn_":"2"},{"1":"3","2":"3","3":"1","4":"1","5":"2","6":"3","7":"1","8":"0","9":"2","10":"2","11":"1","_rn_":"3"},{"1":"4","2":"4","3":"1","4":"1","5":"3","6":"4","7":"1","8":"0","9":"3","10":"3","11":"0","_rn_":"4"},{"1":"5","2":"5","3":"1","4":"1","5":"4","6":"5","7":"1","8":"0","9":"4","10":"4","11":"0","_rn_":"5"},{"1":"6","2":"6","3":"1","4":"1","5":"5","6":"6","7":"1","8":"0","9":"5","10":"5","11":"0","_rn_":"6"},{"1":"7","2":"7","3":"1","4":"2","5":"0","6":"2","7":"2","8":"1","9":"0","10":"1","11":"1","_rn_":"7"},{"1":"8","2":"8","3":"1","4":"2","5":"1","6":"3","7":"2","8":"1","9":"1","10":"2","11":"1","_rn_":"8"},{"1":"9","2":"9","3":"1","4":"2","5":"2","6":"4","7":"2","8":"1","9":"2","10":"3","11":"0","_rn_":"9"},{"1":"10","2":"10","3":"1","4":"2","5":"3","6":"5","7":"2","8":"1","9":"3","10":"4","11":"0","_rn_":"10"},{"1":"11","2":"11","3":"1","4":"2","5":"4","6":"6","7":"2","8":"1","9":"4","10":"5","11":"0","_rn_":"11"},{"1":"12","2":"12","3":"1","4":"3","5":"0","6":"3","7":"3","8":"2","9":"0","10":"2","11":"1","_rn_":"12"},{"1":"13","2":"13","3":"1","4":"3","5":"1","6":"4","7":"3","8":"2","9":"1","10":"3","11":"0","_rn_":"13"},{"1":"14","2":"14","3":"1","4":"3","5":"2","6":"5","7":"3","8":"2","9":"2","10":"4","11":"0","_rn_":"14"},{"1":"15","2":"15","3":"1","4":"3","5":"3","6":"6","7":"3","8":"2","9":"3","10":"5","11":"0","_rn_":"15"},{"1":"16","2":"16","3":"1","4":"4","5":"0","6":"4","7":"4","8":"3","9":"0","10":"3","11":"0","_rn_":"16"},{"1":"17","2":"17","3":"1","4":"4","5":"1","6":"5","7":"4","8":"3","9":"1","10":"4","11":"0","_rn_":"17"},{"1":"18","2":"18","3":"1","4":"4","5":"2","6":"6","7":"4","8":"3","9":"2","10":"5","11":"0","_rn_":"18"},{"1":"19","2":"19","3":"1","4":"5","5":"0","6":"5","7":"5","8":"4","9":"0","10":"4","11":"0","_rn_":"19"},{"1":"20","2":"20","3":"1","4":"5","5":"1","6":"6","7":"5","8":"4","9":"1","10":"5","11":"0","_rn_":"20"},{"1":"21","2":"21","3":"1","4":"6","5":"0","6":"6","7":"6","8":"5","9":"0","10":"5","11":"0","_rn_":"21"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

On crée une survie qui dépend de la température.

```r
cincle.ddl$Phi$temp <- 0 # nv var mise a 0
for (i in 1:nrow(cov.cincle)){
   cincle.ddl$Phi$temp[cincle.ddl$Phi$time == i] <- as.numeric(cov.cincle[i, "temperature_hiver_c"])
}
```

On vérifie que ça a marché.

```r
cincle.ddl$Phi
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["par.index"],"name":[1],"type":["int"],"align":["right"]},{"label":["model.index"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["group"],"name":[3],"type":["fct"],"align":["left"]},{"label":["cohort"],"name":[4],"type":["fct"],"align":["left"]},{"label":["age"],"name":[5],"type":["fct"],"align":["left"]},{"label":["time"],"name":[6],"type":["fct"],"align":["left"]},{"label":["occ.cohort"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["Cohort"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["Age"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["Time"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["Inondation"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["temp"],"name":[12],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1","3":"1","4":"1","5":"0","6":"1","7":"1","8":"0","9":"0","10":"0","11":"0","12":"-2.3","_rn_":"1"},{"1":"2","2":"2","3":"1","4":"1","5":"1","6":"2","7":"1","8":"0","9":"1","10":"1","11":"1","12":"-0.4","_rn_":"2"},{"1":"3","2":"3","3":"1","4":"1","5":"2","6":"3","7":"1","8":"0","9":"2","10":"2","11":"1","12":"-1.2","_rn_":"3"},{"1":"4","2":"4","3":"1","4":"1","5":"3","6":"4","7":"1","8":"0","9":"3","10":"3","11":"0","12":"-4.2","_rn_":"4"},{"1":"5","2":"5","3":"1","4":"1","5":"4","6":"5","7":"1","8":"0","9":"4","10":"4","11":"0","12":"-3.0","_rn_":"5"},{"1":"6","2":"6","3":"1","4":"1","5":"5","6":"6","7":"1","8":"0","9":"5","10":"5","11":"0","12":"-2.8","_rn_":"6"},{"1":"7","2":"7","3":"1","4":"2","5":"0","6":"2","7":"2","8":"1","9":"0","10":"1","11":"1","12":"-0.4","_rn_":"7"},{"1":"8","2":"8","3":"1","4":"2","5":"1","6":"3","7":"2","8":"1","9":"1","10":"2","11":"1","12":"-1.2","_rn_":"8"},{"1":"9","2":"9","3":"1","4":"2","5":"2","6":"4","7":"2","8":"1","9":"2","10":"3","11":"0","12":"-4.2","_rn_":"9"},{"1":"10","2":"10","3":"1","4":"2","5":"3","6":"5","7":"2","8":"1","9":"3","10":"4","11":"0","12":"-3.0","_rn_":"10"},{"1":"11","2":"11","3":"1","4":"2","5":"4","6":"6","7":"2","8":"1","9":"4","10":"5","11":"0","12":"-2.8","_rn_":"11"},{"1":"12","2":"12","3":"1","4":"3","5":"0","6":"3","7":"3","8":"2","9":"0","10":"2","11":"1","12":"-1.2","_rn_":"12"},{"1":"13","2":"13","3":"1","4":"3","5":"1","6":"4","7":"3","8":"2","9":"1","10":"3","11":"0","12":"-4.2","_rn_":"13"},{"1":"14","2":"14","3":"1","4":"3","5":"2","6":"5","7":"3","8":"2","9":"2","10":"4","11":"0","12":"-3.0","_rn_":"14"},{"1":"15","2":"15","3":"1","4":"3","5":"3","6":"6","7":"3","8":"2","9":"3","10":"5","11":"0","12":"-2.8","_rn_":"15"},{"1":"16","2":"16","3":"1","4":"4","5":"0","6":"4","7":"4","8":"3","9":"0","10":"3","11":"0","12":"-4.2","_rn_":"16"},{"1":"17","2":"17","3":"1","4":"4","5":"1","6":"5","7":"4","8":"3","9":"1","10":"4","11":"0","12":"-3.0","_rn_":"17"},{"1":"18","2":"18","3":"1","4":"4","5":"2","6":"6","7":"4","8":"3","9":"2","10":"5","11":"0","12":"-2.8","_rn_":"18"},{"1":"19","2":"19","3":"1","4":"5","5":"0","6":"5","7":"5","8":"4","9":"0","10":"4","11":"0","12":"-3.0","_rn_":"19"},{"1":"20","2":"20","3":"1","4":"5","5":"1","6":"6","7":"5","8":"4","9":"1","10":"5","11":"0","12":"-2.8","_rn_":"20"},{"1":"21","2":"21","3":"1","4":"6","5":"0","6":"6","7":"6","8":"5","9":"0","10":"5","11":"0","12":"-2.8","_rn_":"21"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

On définit les effets.

```r
phi.temp <- list(formula =~ temp)
```

On ajuste le modèle.

```r
phicov.cincle <- mark(cincle.proc,
                      cincle.ddl,
                      model.parameters = list(Phi = phi.temp, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~temp)p(~1) 
## 
## Npar :  3
## -2lnL:  660.5328
## AICc :  666.5896
## 
## Beta
##                   estimate        se        lcl        ucl
## Phi:(Intercept) -0.2565831 0.2220600 -0.6918207  0.1786545
## Phi:temp        -0.2051771 0.0821952 -0.3662796 -0.0440745
## p:(Intercept)    2.2347028 0.3250019  1.5976991  2.8717065
## 
## 
## Real Parameter Phi
##  
##          1         2         3         4         5         6
## 1 0.553624 0.4564823 0.4974074 0.6468361 0.5887858 0.5788155
## 2          0.4564823 0.4974074 0.6468361 0.5887858 0.5788155
## 3                    0.4974074 0.6468361 0.5887858 0.5788155
## 4                              0.6468361 0.5887858 0.5788155
## 5                                        0.5887858 0.5788155
## 6                                                  0.5788155
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7
## 1 0.9033228 0.9033228 0.9033228 0.9033228 0.9033228 0.9033228
## 2           0.9033228 0.9033228 0.9033228 0.9033228 0.9033228
## 3                     0.9033228 0.9033228 0.9033228 0.9033228
## 4                               0.9033228 0.9033228 0.9033228
## 5                                         0.9033228 0.9033228
## 6                                                   0.9033228
```

La sélection de modèles.

```r
collect.models()
```

```
##                   model npar     AICc DeltaAICc       weight Deviance
## 3 Phi(~Inondation)p(~1)    3 666.1596  0.000000 0.5198230701 51.42300
## 2       Phi(~temp)p(~1)    3 666.5896  0.430000 0.4192588476 51.85299
## 4          Phi(~1)p(~1)    2 670.8660  4.706387 0.0494170312 58.15788
## 6       Phi(~time)p(~1)    7 673.9980  7.838391 0.0103221620 51.05031
## 5       Phi(~1)p(~time)    7 678.7481 12.588471 0.0009600722 55.80039
## 1    Phi(~time)p(~time)   12 681.7057 15.546016 0.0002188170 48.27043
```

Les paramètres estimés.

```r
phicov.cincle$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.5536240","2":"0.0255363","3":"0.5031974","4":"0.6029707","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"0.4564823","2":"0.0480182","3":"0.3649720","4":"0.5510277","5":"","6":"","_rn_":"Phi g1 c1 a1 t2"},{"1":"0.4974074","2":"0.0355628","3":"0.4282021","4":"0.5667121","5":"","6":"","_rn_":"Phi g1 c1 a2 t3"},{"1":"0.6468361","2":"0.0412945","3":"0.5623869","4":"0.7230149","5":"","6":"","_rn_":"Phi g1 c1 a3 t4"},{"1":"0.5887858","2":"0.0277198","3":"0.5335866","4":"0.6418372","5":"","6":"","_rn_":"Phi g1 c1 a4 t5"},{"1":"0.5788155","2":"0.0264124","3":"0.5263662","4":"0.6295444","5":"","6":"","_rn_":"Phi g1 c1 a5 t6"},{"1":"0.9033228","2":"0.0283826","3":"0.8316966","4":"0.9464299","5":"","6":"","_rn_":"p g1 c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Visualisons la relation survie vs. température. On créé d'abord une grille pour la température 

```r
min.temp <- min(cov.cincle$temperature_hiver_c)
max.temp <- max(cov.cincle$temperature_hiver_c)
temp.values <- seq(from = min.temp, to = max.temp, length = 50)
```

On fait la prédiction. Pour cela il nous faut l'ordonnée à l'origine (intercept) et la pente (slope) de l'effet température sur la survie. 

```r
coef(phicov.cincle)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"-0.2565831","2":"0.2220600","3":"-0.6918207","4":"0.1786545","_rn_":"Phi:(Intercept)"},{"1":"-0.2051771","2":"0.0821952","3":"-0.3662796","4":"-0.0440745","_rn_":"Phi:temp"},{"1":"2.2347028","2":"0.3250019","3":"1.5976991","4":"2.8717065","_rn_":"p:(Intercept)"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

On applique la relation aux valeurs de la grille, et on ramène les valeurs obtenues de survie sur l'échelle (0,1).

```r
phi.pred <- plogis(coef(phicov.cincle)[1,1] + coef(phicov.cincle)[2,1] * temp.values)
```

On visualise.

```r
plot(x = temp.values, 
     y = phi.pred, 
     lwd = 3, 
     type = 'l', 
     xlab = "température", 
     ylab = "probabilité de survie estimée")
```

![](survival_practical_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

On représente maintenant les survies estimées pour les 4 meilleurs modèles, le modèle avec effet de l'inondation, de la température, le modèle avec survie constante et le modèle avec effet du temps sur la survie (la prob de recapture est constante dans les 4 modèles).

```r
plot(x = 1981:1986, 
     y = phicov.cincle$results$real[1:6, 1],
     ylim = c(0,1),
     xlab = "intervalle de temps (année, année + 1)",
     ylab = "prob survie estimée",
     pch = 1)
points(x = 1981:1986 + 0.1, 
     y = phitp.cincle$results$real[1:6, 1],
     pch = 2)
points(x = 1981:1986 - 0.1, 
     y = c(phiinondationp.cincle$results$real[1,1], # pas inondation
           phiinondationp.cincle$results$real[2,1], # inondation
           phiinondationp.cincle$results$real[2,1], # inondation
           phiinondationp.cincle$results$real[1,1], # pas inondation
           phiinondationp.cincle$results$real[1,1], # pas inondation
           phiinondationp.cincle$results$real[1,1]), # pas inondation
     pch = 3)
legend("bottomright", 
       legend = c("phi(temperature)",
                  "phi(temps)",
                  "phi(inondation)"),
       pch = c(1,2,3))
```

![](survival_practical_files/figure-html/unnamed-chunk-34-1.png)<!-- -->


## Effet de l'âge

On définit l'effet âge (temps écoulé depuis la première capture). 

```r
phi.age <- list(formula =~ age)
```

On ajuste le modèle.

```r
phiage.cincle <- mark(cincle.proc,
                      cincle.ddl,
                      model.parameters = list(Phi = phi.age, 
                                              p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~age)p(~1) 
## 
## Npar :  7  (unadjusted=6)
## -2lnL:  662.8206
## AICc :  677.0885  (unadjusted=675.02107)
## 
## Beta
##                    estimate           se           lcl          ucl
## Phi:(Intercept)   0.2052262    0.1393354    -0.0678712    0.4783236
## Phi:age1          0.0214798    0.2537615    -0.4758927    0.5188524
## Phi:age2          0.3587543    0.3691000    -0.3646818    1.0821904
## Phi:age3         -0.0576599    0.5381912    -1.1125146    0.9971949
## Phi:age4          0.2395609    0.8866533    -1.4982796    1.9774014
## Phi:age5        -16.5450180 2794.4965000 -5493.7582000 5460.6682000
## p:(Intercept)     2.2552288    0.3288434     1.6106957    2.8997619
## 
## 
## Real Parameter Phi
##  
##           1         2         3         4         5            6
## 1 0.5511272 0.5564350 0.6373731 0.5368248 0.6093991 8.011588e-08
## 2           0.5511272 0.5564350 0.6373731 0.5368248 6.093991e-01
## 3                     0.5511272 0.5564350 0.6373731 5.368248e-01
## 4                               0.5511272 0.5564350 6.373731e-01
## 5                                         0.5511272 5.564350e-01
## 6                                                   5.511272e-01
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7
## 1 0.9051006 0.9051006 0.9051006 0.9051006 0.9051006 0.9051006
## 2           0.9051006 0.9051006 0.9051006 0.9051006 0.9051006
## 3                     0.9051006 0.9051006 0.9051006 0.9051006
## 4                               0.9051006 0.9051006 0.9051006
## 5                                         0.9051006 0.9051006
## 6                                                   0.9051006
```

La sélection de modèles.

```r
collect.models()
```

```
##                   model npar     AICc DeltaAICc       weight Deviance
## 4 Phi(~Inondation)p(~1)    3 666.1596  0.000000 0.5186813025 51.42300
## 3       Phi(~temp)p(~1)    3 666.5896  0.430000 0.4183379647 51.85299
## 5          Phi(~1)p(~1)    2 670.8660  4.706387 0.0493084889 58.15788
## 7       Phi(~time)p(~1)    7 673.9980  7.838391 0.0102994898 51.05031
## 2        Phi(~age)p(~1)    7 677.0885 10.928891 0.0021964543 54.14081
## 6       Phi(~1)p(~time)    7 678.7481 12.588471 0.0009579635 55.80039
## 1    Phi(~time)p(~time)   12 681.7057 15.546016 0.0002183364 48.27043
```

Les paramètres estimés.

```r
phiage.cincle$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"5.511272e-01","2":"0.0344696000","3":"4.830387e-01","4":"0.6173519","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"5.564350e-01","2":"0.0502658000","3":"4.569908e-01","4":"0.6515535","5":"","6":"","_rn_":"Phi g1 c1 a1 t2"},{"1":"6.373731e-01","2":"0.0794809000","3":"4.725209e-01","4":"0.7752136","5":"","6":"","_rn_":"Phi g1 c1 a2 t3"},{"1":"5.368248e-01","2":"0.1293736000","3":"2.947854e-01","4":"0.7626724","5":"","6":"","_rn_":"Phi g1 c1 a3 t4"},{"1":"6.093991e-01","2":"0.2089628000","3":"2.182570e-01","4":"0.8971016","5":"","6":"","_rn_":"Phi g1 c1 a4 t5"},{"1":"8.011588e-08","2":"0.0002238835","3":"4.456594e-316","4":"1.0000000","5":"","6":"","_rn_":"Phi g1 c1 a5 t6"},{"1":"9.051006e-01","2":"0.0282455000","3":"8.335079e-01","4":"0.9478347","5":"","6":"","_rn_":"p g1 c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Ici l'effet d'âge est plein, autrement dit on estime une probabilité de survie pour chaque classe d'âge. On peut contraindre cet effet à un effet jeune (survie qui dure une année de 0 à 1 an) vs. adulte (la même survie pour les individus au-delà d'1 an).

Pour ce faire, on ajoute une variable ageclass. 

```r
# create 0, 1+ age variable
cincle.ddl <- add.design.data(cincle.proc,
                              cincle.ddl, 
                             "Phi",
                             type = "age",
                             bins = c(0, 1, 7), # 2 classes d'âge
                             name = "ageclass",
                             right = FALSE)
```

Jetons un coup d'oeil.

```r
cincle.ddl$Phi
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["par.index"],"name":[1],"type":["int"],"align":["right"]},{"label":["model.index"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["group"],"name":[3],"type":["fct"],"align":["left"]},{"label":["cohort"],"name":[4],"type":["fct"],"align":["left"]},{"label":["age"],"name":[5],"type":["fct"],"align":["left"]},{"label":["time"],"name":[6],"type":["fct"],"align":["left"]},{"label":["occ.cohort"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["Cohort"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["Age"],"name":[9],"type":["dbl"],"align":["right"]},{"label":["Time"],"name":[10],"type":["dbl"],"align":["right"]},{"label":["Inondation"],"name":[11],"type":["dbl"],"align":["right"]},{"label":["temp"],"name":[12],"type":["dbl"],"align":["right"]},{"label":["ageclass"],"name":[13],"type":["fct"],"align":["left"]}],"data":[{"1":"1","2":"1","3":"1","4":"1","5":"0","6":"1","7":"1","8":"0","9":"0","10":"0","11":"0","12":"-2.3","13":"[0,1)","_rn_":"1"},{"1":"2","2":"2","3":"1","4":"1","5":"1","6":"2","7":"1","8":"0","9":"1","10":"1","11":"1","12":"-0.4","13":"[1,7]","_rn_":"2"},{"1":"3","2":"3","3":"1","4":"1","5":"2","6":"3","7":"1","8":"0","9":"2","10":"2","11":"1","12":"-1.2","13":"[1,7]","_rn_":"3"},{"1":"4","2":"4","3":"1","4":"1","5":"3","6":"4","7":"1","8":"0","9":"3","10":"3","11":"0","12":"-4.2","13":"[1,7]","_rn_":"4"},{"1":"5","2":"5","3":"1","4":"1","5":"4","6":"5","7":"1","8":"0","9":"4","10":"4","11":"0","12":"-3.0","13":"[1,7]","_rn_":"5"},{"1":"6","2":"6","3":"1","4":"1","5":"5","6":"6","7":"1","8":"0","9":"5","10":"5","11":"0","12":"-2.8","13":"[1,7]","_rn_":"6"},{"1":"7","2":"7","3":"1","4":"2","5":"0","6":"2","7":"2","8":"1","9":"0","10":"1","11":"1","12":"-0.4","13":"[0,1)","_rn_":"7"},{"1":"8","2":"8","3":"1","4":"2","5":"1","6":"3","7":"2","8":"1","9":"1","10":"2","11":"1","12":"-1.2","13":"[1,7]","_rn_":"8"},{"1":"9","2":"9","3":"1","4":"2","5":"2","6":"4","7":"2","8":"1","9":"2","10":"3","11":"0","12":"-4.2","13":"[1,7]","_rn_":"9"},{"1":"10","2":"10","3":"1","4":"2","5":"3","6":"5","7":"2","8":"1","9":"3","10":"4","11":"0","12":"-3.0","13":"[1,7]","_rn_":"10"},{"1":"11","2":"11","3":"1","4":"2","5":"4","6":"6","7":"2","8":"1","9":"4","10":"5","11":"0","12":"-2.8","13":"[1,7]","_rn_":"11"},{"1":"12","2":"12","3":"1","4":"3","5":"0","6":"3","7":"3","8":"2","9":"0","10":"2","11":"1","12":"-1.2","13":"[0,1)","_rn_":"12"},{"1":"13","2":"13","3":"1","4":"3","5":"1","6":"4","7":"3","8":"2","9":"1","10":"3","11":"0","12":"-4.2","13":"[1,7]","_rn_":"13"},{"1":"14","2":"14","3":"1","4":"3","5":"2","6":"5","7":"3","8":"2","9":"2","10":"4","11":"0","12":"-3.0","13":"[1,7]","_rn_":"14"},{"1":"15","2":"15","3":"1","4":"3","5":"3","6":"6","7":"3","8":"2","9":"3","10":"5","11":"0","12":"-2.8","13":"[1,7]","_rn_":"15"},{"1":"16","2":"16","3":"1","4":"4","5":"0","6":"4","7":"4","8":"3","9":"0","10":"3","11":"0","12":"-4.2","13":"[0,1)","_rn_":"16"},{"1":"17","2":"17","3":"1","4":"4","5":"1","6":"5","7":"4","8":"3","9":"1","10":"4","11":"0","12":"-3.0","13":"[1,7]","_rn_":"17"},{"1":"18","2":"18","3":"1","4":"4","5":"2","6":"6","7":"4","8":"3","9":"2","10":"5","11":"0","12":"-2.8","13":"[1,7]","_rn_":"18"},{"1":"19","2":"19","3":"1","4":"5","5":"0","6":"5","7":"5","8":"4","9":"0","10":"4","11":"0","12":"-3.0","13":"[0,1)","_rn_":"19"},{"1":"20","2":"20","3":"1","4":"5","5":"1","6":"6","7":"5","8":"4","9":"1","10":"5","11":"0","12":"-2.8","13":"[1,7]","_rn_":"20"},{"1":"21","2":"21","3":"1","4":"6","5":"0","6":"6","7":"6","8":"5","9":"0","10":"5","11":"0","12":"-2.8","13":"[0,1)","_rn_":"21"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

On spécifie une survie qui dépend de l'âge.

```r
phi.age2 <- list(formula=~ageclass) # age effect on survival
```

On ajuste le modèle avec survie âge-dépendante et prob de recapture constante.

```r
phiage2.cincle <- mark(cincle.proc,
                        cincle.ddl,
                        model.parameters = list(Phi = phi.age2, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~ageclass)p(~1) 
## 
## Npar :  3
## -2lnL:  666.6804
## AICc :  672.7373
## 
## Beta
##                    estimate        se        lcl       ucl
## Phi:(Intercept)   0.2041822 0.1390053 -0.0682681 0.4766325
## Phi:ageclass[1,7] 0.0841952 0.2119782 -0.3312820 0.4996724
## p:(Intercept)     2.2456921 0.3285355  1.6017624 2.8896217
## 
## 
## Real Parameter Phi
##  
##          1         2         3         4         5         6
## 1 0.550869 0.5715989 0.5715989 0.5715989 0.5715989 0.5715989
## 2          0.5508690 0.5715989 0.5715989 0.5715989 0.5715989
## 3                    0.5508690 0.5715989 0.5715989 0.5715989
## 4                              0.5508690 0.5715989 0.5715989
## 5                                        0.5508690 0.5715989
## 6                                                  0.5508690
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7
## 1 0.9042783 0.9042783 0.9042783 0.9042783 0.9042783 0.9042783
## 2           0.9042783 0.9042783 0.9042783 0.9042783 0.9042783
## 3                     0.9042783 0.9042783 0.9042783 0.9042783
## 4                               0.9042783 0.9042783 0.9042783
## 5                                         0.9042783 0.9042783
## 6                                                   0.9042783
```

```r
phiage2.cincle$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.5508690","2":"0.0343916","3":"0.4829396","4":"0.6169524","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"0.5715989","2":"0.0380167","3":"0.4960218","4":"0.6439771","5":"","6":"","_rn_":"Phi g1 c1 a1 t2"},{"1":"0.9042783","2":"0.0284377","3":"0.8322646","4":"0.9473310","5":"","6":"","_rn_":"p g1 c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Que donne la sélection de modèles?

```r
collect.models()
```

```
##                   model npar     AICc DeltaAICc       weight Deviance
## 5 Phi(~Inondation)p(~1)    3 666.1596  0.000000 0.5088373938 51.42300
## 4       Phi(~temp)p(~1)    3 666.5896  0.430000 0.4103984444 51.85299
## 6          Phi(~1)p(~1)    2 670.8660  4.706387 0.0483726768 58.15788
## 3   Phi(~ageclass)p(~1)    3 672.7373  6.577620 0.0189787229 58.00061
## 8       Phi(~time)p(~1)    7 673.9980  7.838391 0.0101040186 51.05031
## 2        Phi(~age)p(~1)    7 677.0885 10.928891 0.0021547684 54.14081
## 7       Phi(~1)p(~time)    7 678.7481 12.588471 0.0009397825 55.80039
## 1    Phi(~time)p(~time)   12 681.7057 15.546016 0.0002141926 48.27043
```


# Partie 2 : Estimation de la survie, exemple du martinet noir

On remet les compteurs à 0.

```r
rm(list = ls())
```


Les données.

```r
martinet <- convert.inp("dat/martinet-noir.inp",
                        group.df = data.frame(colonie = c("nord", "sud")), 
                        covariates = NULL)
head(martinet)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["ch"],"name":[1],"type":["chr"],"align":["left"]},{"label":["freq"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["colonie"],"name":[3],"type":["chr"],"align":["left"]}],"data":[{"1":"00000001","2":"7","3":"nord","_rn_":"1:1"},{"1":"00000010","2":"6","3":"nord","_rn_":"1:2"},{"1":"00000011","2":"1","3":"nord","_rn_":"1:3"},{"1":"00000100","2":"1","3":"nord","_rn_":"1:4"},{"1":"00001000","2":"1","3":"nord","_rn_":"1:8"},{"1":"00001110","2":"1","3":"nord","_rn_":"1:9"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

On prépare les données.

```r
martinet.proc <- process.data(martinet, 
                              begin.time = 1, 
                              model = "CJS", 
                              groups = ("colonie"))
martinet.ddl <- make.design.data(martinet.proc)
```

On spécifie les effets sur les paramètres.

```r
phit <- list(formula=~time)
phi <- list(formula=~1)
pt <- list(formula=~time)
p <- list(formula=~1)
```

Fait tourner modèle CJS, et examine les paramètres estimés. 

```r
cjs.martinet <- mark(martinet.proc,
                      martinet.ddl,
                      model.parameters = list(Phi = phit, p = pt))
```

```
## 
## Output summary for CJS model
## Name : Phi(~time)p(~time) 
## 
## Npar :  14  (unadjusted=13)
## -2lnL:  354.9445
## AICc :  385.1905  (unadjusted=382.88072)
## 
## Beta
##                   estimate           se           lcl          ucl
## Phi:(Intercept)  1.7439684    0.8654864     0.0476150    3.4403218
## Phi:time2       -0.9669984    1.0306486    -2.9870697    1.0530729
## Phi:time3       -0.5738968    1.1624668    -2.8523317    1.7045382
## Phi:time4       -0.8957156    1.0338558    -2.9220730    1.1306417
## Phi:time5       -0.9809799    0.9802281    -2.9022270    0.9402672
## Phi:time6       -0.6912509    1.0551089    -2.7592644    1.3767627
## Phi:time7       -1.8256839 1067.6869000 -2094.4921000 2090.8407000
## p:(Intercept)    2.0030681    1.0495423    -0.0540349    4.0601711
## p:time3         -0.9689943    1.1967011    -3.3145285    1.3765399
## p:time4         -1.9340754    1.1630690    -4.2136906    0.3455398
## p:time5         -1.2041759    1.1750425    -3.5072593    1.0989075
## p:time6         -0.0882483    1.2916875    -2.6199559    2.4434592
## p:time7         -0.0861434    1.4799884    -2.9869207    2.8146339
## p:time8         -1.1127781 1909.0989000 -3742.9468000 3740.7212000
## 
## 
## Real Parameter Phi
## Group:colonienord 
##           1         2        3         4         5         6         7
## 1 0.8511904 0.6850267 0.763158 0.7002005 0.6820022 0.7412964 0.4795825
## 2           0.6850267 0.763158 0.7002005 0.6820022 0.7412964 0.4795825
## 3                     0.763158 0.7002005 0.6820022 0.7412964 0.4795825
## 4                              0.7002005 0.6820022 0.7412964 0.4795825
## 5                                        0.6820022 0.7412964 0.4795825
## 6                                                  0.7412964 0.4795825
## 7                                                            0.4795825
## 
## Group:coloniesud 
##           1         2        3         4         5         6         7
## 1 0.8511904 0.6850267 0.763158 0.7002005 0.6820022 0.7412964 0.4795825
## 2           0.6850267 0.763158 0.7002005 0.6820022 0.7412964 0.4795825
## 3                     0.763158 0.7002005 0.6820022 0.7412964 0.4795825
## 4                              0.7002005 0.6820022 0.7412964 0.4795825
## 5                                        0.6820022 0.7412964 0.4795825
## 6                                                  0.7412964 0.4795825
## 7                                                            0.4795825
## 
## 
## Real Parameter p
## Group:colonienord 
##           2         3         4         5         6         7       8
## 1 0.8811188 0.7377049 0.5172413 0.6897375 0.8715597 0.8717951 0.70895
## 2           0.7377049 0.5172413 0.6897375 0.8715597 0.8717951 0.70895
## 3                     0.5172413 0.6897375 0.8715597 0.8717951 0.70895
## 4                               0.6897375 0.8715597 0.8717951 0.70895
## 5                                         0.8715597 0.8717951 0.70895
## 6                                                   0.8717951 0.70895
## 7                                                             0.70895
## 
## Group:coloniesud 
##           2         3         4         5         6         7       8
## 1 0.8811188 0.7377049 0.5172413 0.6897375 0.8715597 0.8717951 0.70895
## 2           0.7377049 0.5172413 0.6897375 0.8715597 0.8717951 0.70895
## 3                     0.5172413 0.6897375 0.8715597 0.8717951 0.70895
## 4                               0.6897375 0.8715597 0.8717951 0.70895
## 5                                         0.8715597 0.8717951 0.70895
## 6                                                   0.8717951 0.70895
## 7                                                             0.70895
```

```r
cjs.martinet$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.8511904","2":"0.1096271","3":"5.119015e-01","4":"0.9689412","5":"","6":"","_rn_":"Phi gnord c1 a0 t1"},{"1":"0.6850267","2":"0.1013890","3":"4.640514e-01","4":"0.8452711","5":"","6":"","_rn_":"Phi gnord c1 a1 t2"},{"1":"0.7631580","2":"0.1402705","3":"4.131405e-01","4":"0.9365019","5":"","6":"","_rn_":"Phi gnord c1 a2 t3"},{"1":"0.7002005","2":"0.1187097","3":"4.353322e-01","4":"0.8761682","5":"","6":"","_rn_":"Phi gnord c1 a3 t4"},{"1":"0.6820022","2":"0.0998051","3":"4.653068e-01","4":"0.8409044","5":"","6":"","_rn_":"Phi gnord c1 a4 t5"},{"1":"0.7412964","2":"0.1157330","3":"4.675199e-01","4":"0.9033958","5":"","6":"","_rn_":"Phi gnord c1 a5 t6"},{"1":"0.4795825","2":"266.4767200","3":"5.126204e-309","4":"1.0000000","5":"","6":"","_rn_":"Phi gnord c1 a6 t7"},{"1":"0.8811188","2":"0.1099379","3":"4.864946e-01","4":"0.9830463","5":"","6":"","_rn_":"p gnord c1 a1 t2"},{"1":"0.7377049","2":"0.1112487","3":"4.768147e-01","4":"0.8966881","5":"","6":"","_rn_":"p gnord c1 a2 t3"},{"1":"0.5172413","2":"0.1251483","3":"2.863173e-01","4":"0.7410289","5":"","6":"","_rn_":"p gnord c1 a3 t4"},{"1":"0.6897375","2":"0.1130732","3":"4.410917e-01","4":"0.8622990","5":"","6":"","_rn_":"p gnord c1 a4 t5"},{"1":"0.8715597","2":"0.0842866","3":"6.080347e-01","4":"0.9674089","5":"","6":"","_rn_":"p gnord c1 a5 t6"},{"1":"0.8717951","2":"0.1166268","3":"4.679756e-01","4":"0.9813324","5":"","6":"","_rn_":"p gnord c1 a6 t7"},{"1":"0.7089500","2":"393.9232300","3":"1.354979e-308","4":"1.0000000","5":"","6":"","_rn_":"p gnord c1 a7 t8"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

PIM pour CJS.

```r
PIMS(cjs.martinet,"Phi")
```

```
## group = colonienord 
##    1  2  3  4  5  6  7
## 1  1  2  3  4  5  6  7
## 2     2  3  4  5  6  7
## 3        3  4  5  6  7
## 4           4  5  6  7
## 5              5  6  7
## 6                 6  7
## 7                    7
## group = coloniesud 
##    1  2  3  4  5  6  7
## 1  1  2  3  4  5  6  7
## 2     2  3  4  5  6  7
## 3        3  4  5  6  7
## 4           4  5  6  7
## 5              5  6  7
## 6                 6  7
## 7                    7
```

Fait tourner modèle avec param constants. 

```r
phip.martinet <- mark(martinet.proc,
                      martinet.ddl,
                      model.parameters = list(Phi = phi, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~1)p(~1) 
## 
## Npar :  2
## -2lnL:  372.8533
## AICc :  376.9136
## 
## Beta
##                  estimate        se       lcl      ucl
## Phi:(Intercept) 0.8524384 0.1753794 0.5086948 1.196182
## p:(Intercept)   0.8881232 0.2391869 0.4193169 1.356929
## 
## 
## Real Parameter Phi
## Group:colonienord 
##           1         2         3         4         5         6         7
## 1 0.7010784 0.7010784 0.7010784 0.7010784 0.7010784 0.7010784 0.7010784
## 2           0.7010784 0.7010784 0.7010784 0.7010784 0.7010784 0.7010784
## 3                     0.7010784 0.7010784 0.7010784 0.7010784 0.7010784
## 4                               0.7010784 0.7010784 0.7010784 0.7010784
## 5                                         0.7010784 0.7010784 0.7010784
## 6                                                   0.7010784 0.7010784
## 7                                                             0.7010784
## 
## Group:coloniesud 
##           1         2         3         4         5         6         7
## 1 0.7010784 0.7010784 0.7010784 0.7010784 0.7010784 0.7010784 0.7010784
## 2           0.7010784 0.7010784 0.7010784 0.7010784 0.7010784 0.7010784
## 3                     0.7010784 0.7010784 0.7010784 0.7010784 0.7010784
## 4                               0.7010784 0.7010784 0.7010784 0.7010784
## 5                                         0.7010784 0.7010784 0.7010784
## 6                                                   0.7010784 0.7010784
## 7                                                             0.7010784
## 
## 
## Real Parameter p
## Group:colonienord 
##           2         3         4         5         6         7         8
## 1 0.7085027 0.7085027 0.7085027 0.7085027 0.7085027 0.7085027 0.7085027
## 2           0.7085027 0.7085027 0.7085027 0.7085027 0.7085027 0.7085027
## 3                     0.7085027 0.7085027 0.7085027 0.7085027 0.7085027
## 4                               0.7085027 0.7085027 0.7085027 0.7085027
## 5                                         0.7085027 0.7085027 0.7085027
## 6                                                   0.7085027 0.7085027
## 7                                                             0.7085027
## 
## Group:coloniesud 
##           2         3         4         5         6         7         8
## 1 0.7085027 0.7085027 0.7085027 0.7085027 0.7085027 0.7085027 0.7085027
## 2           0.7085027 0.7085027 0.7085027 0.7085027 0.7085027 0.7085027
## 3                     0.7085027 0.7085027 0.7085027 0.7085027 0.7085027
## 4                               0.7085027 0.7085027 0.7085027 0.7085027
## 5                                         0.7085027 0.7085027 0.7085027
## 6                                                   0.7085027 0.7085027
## 7                                                             0.7085027
```

```r
phip.martinet$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.7010784","2":"0.0367538","3":"0.6245005","4":"0.7678449","5":"","6":"","_rn_":"Phi gnord c1 a0 t1"},{"1":"0.7085027","2":"0.0493985","3":"0.6033198","4":"0.7952602","5":"","6":"","_rn_":"p gnord c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

PIM pour CJS.

```r
PIMS(phip.martinet,"Phi")
```

```
## group = colonienord 
##    1  2  3  4  5  6  7
## 1  1  1  1  1  1  1  1
## 2     1  1  1  1  1  1
## 3        1  1  1  1  1
## 4           1  1  1  1
## 5              1  1  1
## 6                 1  1
## 7                    1
## group = coloniesud 
##    1  2  3  4  5  6  7
## 1  1  1  1  1  1  1  1
## 2     1  1  1  1  1  1
## 3        1  1  1  1  1
## 4           1  1  1  1
## 5              1  1  1
## 6                 1  1
## 7                    1
```

Modèle avec 2 classes d'âge sur la survie.

```r
# create 0, 1+ age variable
martinet.ddl <- add.design.data(martinet.proc,
                                martinet.ddl, # add 2 age-class structure to design matrix
                             "Phi",
                             type = "age",
                             bins = c(0, 1, 7),
                             name = "ageclass",
                             right = FALSE)
```

On spécifie une survie qui dépend de l'âge.

```r
phi.age <- list(formula=~ageclass) # age effect on survival
```

On ajuste le modèle avec survie âge-dépendante et prob de recapture constante.

```r
CJSage.martinet <- mark(martinet.proc,
                        martinet.ddl,
                        model.parameters = list(Phi = phi.age, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~ageclass)p(~1) 
## 
## Npar :  3
## -2lnL:  372.846
## AICc :  378.9672
## 
## Beta
##                     estimate        se        lcl       ucl
## Phi:(Intercept)    0.8749553 0.3191398  0.2494413 1.5004693
## Phi:ageclass[1,7] -0.0339140 0.3988105 -0.8155825 0.7477546
## p:(Intercept)      0.8823123 0.2487228  0.3948155 1.3698090
## 
## 
## Real Parameter Phi
## Group:colonienord 
##           1         2         3         4         5         6         7
## 1 0.7057757 0.6986845 0.6986845 0.6986845 0.6986845 0.6986845 0.6986845
## 2           0.7057757 0.6986845 0.6986845 0.6986845 0.6986845 0.6986845
## 3                     0.7057757 0.6986845 0.6986845 0.6986845 0.6986845
## 4                               0.7057757 0.6986845 0.6986845 0.6986845
## 5                                         0.7057757 0.6986845 0.6986845
## 6                                                   0.7057757 0.6986845
## 7                                                             0.7057757
## 
## Group:coloniesud 
##           1         2         3         4         5         6         7
## 1 0.7057757 0.6986845 0.6986845 0.6986845 0.6986845 0.6986845 0.6986845
## 2           0.7057757 0.6986845 0.6986845 0.6986845 0.6986845 0.6986845
## 3                     0.7057757 0.6986845 0.6986845 0.6986845 0.6986845
## 4                               0.7057757 0.6986845 0.6986845 0.6986845
## 5                                         0.7057757 0.6986845 0.6986845
## 6                                                   0.7057757 0.6986845
## 7                                                             0.7057757
## 
## 
## Real Parameter p
## Group:colonienord 
##           2         3         4         5         6         7         8
## 1 0.7073012 0.7073012 0.7073012 0.7073012 0.7073012 0.7073012 0.7073012
## 2           0.7073012 0.7073012 0.7073012 0.7073012 0.7073012 0.7073012
## 3                     0.7073012 0.7073012 0.7073012 0.7073012 0.7073012
## 4                               0.7073012 0.7073012 0.7073012 0.7073012
## 5                                         0.7073012 0.7073012 0.7073012
## 6                                                   0.7073012 0.7073012
## 7                                                             0.7073012
## 
## Group:coloniesud 
##           2         3         4         5         6         7         8
## 1 0.7073012 0.7073012 0.7073012 0.7073012 0.7073012 0.7073012 0.7073012
## 2           0.7073012 0.7073012 0.7073012 0.7073012 0.7073012 0.7073012
## 3                     0.7073012 0.7073012 0.7073012 0.7073012 0.7073012
## 4                               0.7073012 0.7073012 0.7073012 0.7073012
## 5                                         0.7073012 0.7073012 0.7073012
## 6                                                   0.7073012 0.7073012
## 7                                                             0.7073012
```

```r
CJSage.martinet$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.7057757","2":"0.0662714","3":"0.5620390","4":"0.8176445","5":"","6":"","_rn_":"Phi gnord c1 a0 t1"},{"1":"0.6986845","2":"0.0463273","3":"0.6010232","4":"0.7811451","5":"","6":"","_rn_":"Phi gnord c1 a1 t2"},{"1":"0.7073012","2":"0.0514922","3":"0.5974414","4":"0.7973493","5":"","6":"","_rn_":"p gnord c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

PIM pour CJS avec âge.

```r
PIMS(CJSage.martinet,"Phi")
```

```
## group = colonienord 
##    1  2  3  4  5  6  7
## 1  1  2  2  2  2  2  2
## 2     1  2  2  2  2  2
## 3        1  2  2  2  2
## 4           1  2  2  2
## 5              1  2  2
## 6                 1  2
## 7                    1
## group = coloniesud 
##    1  2  3  4  5  6  7
## 1  1  2  2  2  2  2  2
## 2     1  2  2  2  2  2
## 3        1  2  2  2  2
## 4           1  2  2  2
## 5              1  2  2
## 6                 1  2
## 7                    1
```

Maintenant on passe au gros modèle ${phi(a.g), p(g.t)}$, avec interaction âge et groupe sur la survie, et groupe et temps sur la recapture. 

On définit les paramètres.

```r
phi.a.g <- list(formula=~ageclass*colonie) # age and colonie effect on survival
p.g.t <- list(formula=~colonie*time) # age and colonie effect on survival
```

On ajuste le modèle. 

```r
gros.mod <- mark(martinet.proc,
                        martinet.ddl,
                        model.parameters = list(Phi = phi.a.g, p = p.g.t))
```

```
## 
## Output summary for CJS model
## Name : Phi(~ageclass * colonie)p(~colonie * time) 
## 
## Npar :  18  (unadjusted=16)
## -2lnL:  340.7324
## AICc :  380.4701  (unadjusted=375.67296)
## 
## Beta
##                                 estimate          se          lcl         ucl
## Phi:(Intercept)                0.1691814   0.5256401   -0.8610733   1.1994361
## Phi:ageclass[1,7]              0.4793005   0.7462042   -0.9832597   1.9418607
## Phi:coloniesud                 1.4022426   0.7054865    0.0194890   2.7849962
## Phi:ageclass[1,7]:coloniesud  -1.0377703   0.9299069   -2.8603879   0.7848473
## p:(Intercept)                 16.7883840 168.1883700 -312.8608400 346.4376000
## p:coloniesud                 -14.8688640 168.1892900 -344.5198900 314.7821600
## p:time3                      -15.8914700 168.1912700 -345.5463700 313.7634300
## p:time4                      -17.0323450 168.1903500 -346.6854300 312.6207400
## p:time5                      -18.1895500 168.1927000 -347.8472500 311.4681500
## p:time6                      -16.7415970 168.1926900 -346.3992700 312.9160800
## p:time7                       10.1814060   0.0000000   10.1814060  10.1814060
## p:time8                      -17.4087180 168.1881100 -347.0574200 312.2399900
## p:coloniesud:time3            14.9484550 168.1931900 -314.7102100 344.6071200
## p:coloniesud:time4            15.2782600 168.1918700 -314.3778100 344.9343300
## p:coloniesud:time5            17.6006430 168.1947800 -312.0611300 347.2624200
## p:coloniesud:time6            17.1682490 168.1967800 -312.4974500 346.8339500
## p:coloniesud:time7           -10.4082040   0.0000000  -10.4082040 -10.4082040
## p:coloniesud:time8            15.3736270 168.1892700 -314.2773600 345.0246100
## 
## 
## Real Parameter Phi
## Group:colonienord 
##           1         2         3         4         5         6         7
## 1 0.5421948 0.6566683 0.6566683 0.6566683 0.6566683 0.6566683 0.6566683
## 2           0.5421948 0.6566683 0.6566683 0.6566683 0.6566683 0.6566683
## 3                     0.5421948 0.6566683 0.6566683 0.6566683 0.6566683
## 4                               0.5421948 0.6566683 0.6566683 0.6566683
## 5                                         0.5421948 0.6566683 0.6566683
## 6                                                   0.5421948 0.6566683
## 7                                                             0.5421948
## 
## Group:coloniesud 
##           1         2         3         4         5         6         7
## 1 0.8279865 0.7335979 0.7335979 0.7335979 0.7335979 0.7335979 0.7335979
## 2           0.8279865 0.7335979 0.7335979 0.7335979 0.7335979 0.7335979
## 3                     0.8279865 0.7335979 0.7335979 0.7335979 0.7335979
## 4                               0.8279865 0.7335979 0.7335979 0.7335979
## 5                                         0.8279865 0.7335979 0.7335979
## 6                                                   0.8279865 0.7335979
## 7                                                             0.8279865
## 
## 
## Real Parameter p
## Group:colonienord 
##           2         3         4         5         6 7         8
## 1 0.9999999 0.7103149 0.4393105 0.1976311 0.5116945 1 0.3497055
## 2           0.7103149 0.4393105 0.1976311 0.5116945 1 0.3497055
## 3                     0.4393105 0.1976311 0.5116945 1 0.3497055
## 4                               0.1976311 0.5116945 1 0.3497055
## 5                                         0.5116945 1 0.3497055
## 6                                                   1 0.3497055
## 7                                                     0.3497055
## 
## Group:coloniesud 
##           2         3         4        5         6         7         8
## 1 0.8720849 0.7264143 0.5412648 0.790942 0.9126294 0.8445818 0.4711394
## 2           0.7264143 0.5412648 0.790942 0.9126294 0.8445818 0.4711394
## 3                     0.5412648 0.790942 0.9126294 0.8445818 0.4711394
## 4                               0.790942 0.9126294 0.8445818 0.4711394
## 5                                        0.9126294 0.8445818 0.4711394
## 6                                                  0.8445818 0.4711394
## 7                                                            0.4711394
```

```r
gros.mod$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.5421948","2":"1.304742e-01","3":"2.971152e-01","4":"0.7684245","5":"","6":"","_rn_":"Phi gnord c1 a0 t1"},{"1":"0.6566683","2":"1.044159e-01","3":"4.355453e-01","4":"0.8258110","5":"","6":"","_rn_":"Phi gnord c1 a1 t2"},{"1":"0.8279865","2":"6.701750e-02","3":"6.568196e-01","4":"0.9236986","5":"","6":"","_rn_":"Phi gsud c1 a0 t1"},{"1":"0.7335979","2":"5.103750e-02","3":"6.227168e-01","4":"0.8212461","5":"","6":"","_rn_":"Phi gsud c1 a1 t2"},{"1":"0.9999999","2":"8.603863e-06","3":"1.337412e-136","4":"1.0000000","5":"","6":"","_rn_":"p gnord c1 a1 t2"},{"1":"0.7103149","2":"2.128975e-01","3":"2.439766e-01","4":"0.9490602","5":"","6":"","_rn_":"p gnord c1 a2 t3"},{"1":"0.4393105","2":"2.616152e-01","3":"8.901730e-02","4":"0.8626850","5":"","6":"","_rn_":"p gnord c1 a3 t4"},{"1":"0.1976311","2":"1.847870e-01","3":"2.447850e-02","4":"0.7074113","5":"","6":"","_rn_":"p gnord c1 a4 t5"},{"1":"0.5116945","2":"2.865201e-01","3":"9.968220e-02","4":"0.9084067","5":"","6":"","_rn_":"p gnord c1 a5 t6"},{"1":"1.0000000","2":"0.000000e+00","3":"1.000000e+00","4":"1.0000000","5":"","6":"","_rn_":"p gnord c1 a6 t7"},{"1":"0.3497055","2":"2.284815e-01","3":"6.981360e-02","4":"0.7939477","5":"","6":"","_rn_":"p gnord c1 a7 t8"},{"1":"0.8720849","2":"1.169520e-01","3":"4.662154e-01","4":"0.9815557","5":"","6":"","_rn_":"p gsud c1 a1 t2"},{"1":"0.7264143","2":"1.196541e-01","3":"4.492852e-01","4":"0.8962812","5":"","6":"","_rn_":"p gsud c1 a2 t3"},{"1":"0.5412648","2":"1.239404e-01","3":"3.072684e-01","4":"0.7583751","5":"","6":"","_rn_":"p gsud c1 a3 t4"},{"1":"0.7909420","2":"1.016563e-01","3":"5.313676e-01","4":"0.9265995","5":"","6":"","_rn_":"p gsud c1 a4 t5"},{"1":"0.9126294","2":"8.004570e-02","3":"5.935313e-01","4":"0.9867936","5":"","6":"","_rn_":"p gsud c1 a5 t6"},{"1":"0.8445818","2":"1.129750e-01","3":"5.014502e-01","4":"0.9670622","5":"","6":"","_rn_":"p gsud c1 a6 t7"},{"1":"0.4711394","2":"1.002331e-01","3":"2.882240e-01","4":"0.6621486","5":"","6":"","_rn_":"p gsud c1 a7 t8"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

PIM pour survie et détection dans le gros modèle.

```r
PIMS(gros.mod,"Phi")
```

```
## group = colonienord 
##    1  2  3  4  5  6  7
## 1  1  2  2  2  2  2  2
## 2     1  2  2  2  2  2
## 3        1  2  2  2  2
## 4           1  2  2  2
## 5              1  2  2
## 6                 1  2
## 7                    1
## group = coloniesud 
##    1  2  3  4  5  6  7
## 1  3  4  4  4  4  4  4
## 2     3  4  4  4  4  4
## 3        3  4  4  4  4
## 4           3  4  4  4
## 5              3  4  4
## 6                 3  4
## 7                    3
```

```r
PIMS(gros.mod,"p")
```

```
## group = colonienord 
##    2  3  4  5  6  7  8
## 1  5  6  7  8  9 10 11
## 2     6  7  8  9 10 11
## 3        7  8  9 10 11
## 4           8  9 10 11
## 5              9 10 11
## 6                10 11
## 7                   11
## group = coloniesud 
##    2  3  4  5  6  7  8
## 1 12 13 14 15 16 17 18
## 2    13 14 15 16 17 18
## 3       14 15 16 17 18
## 4          15 16 17 18
## 5             16 17 18
## 6                17 18
## 7                   18
```

La sélection de modèles.

```r
collect.models()
```

```
##                                        model npar     AICc DeltaAICc     weight
## 4                               Phi(~1)p(~1)    2 376.9136  0.000000 0.64807823
## 2                        Phi(~ageclass)p(~1)    3 378.9672  2.053631 0.23210645
## 3 Phi(~ageclass * colonie)p(~colonie * time)   18 380.4701  3.556533 0.10948031
## 1                         Phi(~time)p(~time)   14 385.1905  8.276948 0.01033501
##   Deviance
## 4 133.6472
## 2 133.6399
## 3 101.5263
## 1 115.7384
```


# Partie 3 : Hypothèses des modèles de capture-recapture, hétérogénéité et tests d'ajustement

Le but de cet exercice est de se familiariser avec les données de capture-recapture en population ouverte, d’ajuster par maximum de vraisemblance quelques modèles simples, de comparer ces modèles entre eux pour déterminer celui qui fournit la meilleure description des données et de tester la qualité de l’ajustement de ces modèles.

## Question 1

On simule 2 jeux de données de capture-recapture avec les paramètres de survie ($\phi$) et recapture ($p$) suivants :
* jeu de données G1 : $\phi = 0.8$, $p = 0.8$ ;
* jeu de données G2 : $\phi = 0.8$, $p = 0.2$.


```r
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
```



```r
set.seed(2021)
nind <- 500
nocc <- 8
G1 <- simul(nind = nind, nocc = nocc, phi = 0.8, p = 0.8)
G2 <- simul(nind = nind, nocc = nocc, phi = 0.8, p = 0.2)
```

Ajuster séparément à G1 et G2 le modèle ${\Phi(t), p(t)}$ appelé aussi le modèle de Cormack-Jolly-Seber (CJS). Que pouvez-vous vous dire sur l’estimation des paramètres ?


```r
G1marked <- data.frame(ch = tidyr::unite(G1, col = "ch", sep = ""), 
                       n = rep(1, nrow(G1)))
G2marked <- data.frame(ch = tidyr::unite(G2, col = "ch", sep = ""), 
                       n = rep(1, nrow(G2)))
```

On prépare les données. 

```r
G1.proc <- process.data(G1marked)
G2.proc <- process.data(G2marked)
G1.ddl <- make.design.data(G1.proc)
G2.ddl <- make.design.data(G2.proc)
```

On spécifie les paramètres. 

```r
phi <- list(formula=~1)  
p <- list(formula=~1)
```

On ajuste le modèle avec paramètres constants aux données G1.  

```r
cjs.G1 <- mark(G1.proc,
              G1.ddl,
              model.parameters = list(Phi = phi, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~1)p(~1) 
## 
## Npar :  2
## -2lnL:  3009.594
## AICc :  3013.601
## 
## Beta
##                 estimate        se      lcl      ucl
## Phi:(Intercept) 1.349691 0.0584381 1.235152 1.464229
## p:(Intercept)   1.417211 0.0761598 1.267938 1.566484
## 
## 
## Real Parameter Phi
##  
##           1         2         3         4         5         6         7
## 1 0.7940791 0.7940791 0.7940791 0.7940791 0.7940791 0.7940791 0.7940791
## 2           0.7940791 0.7940791 0.7940791 0.7940791 0.7940791 0.7940791
## 3                     0.7940791 0.7940791 0.7940791 0.7940791 0.7940791
## 4                               0.7940791 0.7940791 0.7940791 0.7940791
## 5                                         0.7940791 0.7940791 0.7940791
## 6                                                   0.7940791 0.7940791
## 7                                                             0.7940791
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7         8
## 1 0.8049008 0.8049008 0.8049008 0.8049008 0.8049008 0.8049008 0.8049008
## 2           0.8049008 0.8049008 0.8049008 0.8049008 0.8049008 0.8049008
## 3                     0.8049008 0.8049008 0.8049008 0.8049008 0.8049008
## 4                               0.8049008 0.8049008 0.8049008 0.8049008
## 5                                         0.8049008 0.8049008 0.8049008
## 6                                                   0.8049008 0.8049008
## 7                                                             0.8049008
```

```r
cjs.G1$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.7940791","2":"0.0095556","3":"0.7747191","4":"0.8121787","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"0.8049008","2":"0.0119598","3":"0.7803896","4":"0.8272818","5":"","6":"","_rn_":"p g1 c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Puis aux données G2. 

```r
cjs.G2 <- mark(G2.proc,
              G2.ddl,
              model.parameters = list(Phi = phi, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~1)p(~1) 
## 
## Npar :  2
## -2lnL:  2091.359
## AICc :  2095.374
## 
## Beta
##                  estimate        se       lcl       ucl
## Phi:(Intercept)  1.487792 0.1111557  1.269926  1.705657
## p:(Intercept)   -1.398919 0.0940821 -1.583320 -1.214518
## 
## 
## Real Parameter Phi
##  
##           1         2         3         4         5         6         7
## 1 0.8157466 0.8157466 0.8157466 0.8157466 0.8157466 0.8157466 0.8157466
## 2           0.8157466 0.8157466 0.8157466 0.8157466 0.8157466 0.8157466
## 3                     0.8157466 0.8157466 0.8157466 0.8157466 0.8157466
## 4                               0.8157466 0.8157466 0.8157466 0.8157466
## 5                                         0.8157466 0.8157466 0.8157466
## 6                                                   0.8157466 0.8157466
## 7                                                             0.8157466
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7         8
## 1 0.1979877 0.1979877 0.1979877 0.1979877 0.1979877 0.1979877 0.1979877
## 2           0.1979877 0.1979877 0.1979877 0.1979877 0.1979877 0.1979877
## 3                     0.1979877 0.1979877 0.1979877 0.1979877 0.1979877
## 4                               0.1979877 0.1979877 0.1979877 0.1979877
## 5                                         0.1979877 0.1979877 0.1979877
## 6                                                   0.1979877 0.1979877
## 7                                                             0.1979877
```

```r
cjs.G2$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.8157466","2":"0.0167072","3":"0.7807302","4":"0.8462721","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"0.1979877","2":"0.0149392","3":"0.1703258","4":"0.2289026","5":"","6":"","_rn_":"p g1 c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## Question 2

a) Grouper les jeux de données G1 et G2 pour obtenir le jeu de données G1+G2.


```r
G1plusG2 <- rbind(G1, G2)
```

b) Ajuster le modèle CJS à G1+G2. Que remarquez-vous concernant l’estimation des paramètres ?


```r
G1G2marked <- data.frame(ch = tidyr::unite(G1plusG2, col = "ch", sep = ""),
                         n = rep(1, nrow(G1plusG2)))
G1G2.proc <- process.data(G1G2marked)
G1G2.ddl <- make.design.data(G1G2.proc)
cjs.G1G2 <- mark(G1G2.proc,
                G1G2.ddl,
                model.parameters = list(Phi = phi, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~1)p(~1) 
## 
## Npar :  2
## -2lnL:  5825.357
## AICc :  5829.362
## 
## Beta
##                  estimate        se       lcl       ucl
## Phi:(Intercept) 1.1639805 0.0436060 1.0785127 1.2494483
## p:(Intercept)   0.3450607 0.0495103 0.2480206 0.4421009
## 
## 
## Real Parameter Phi
##  
##           1         2         3         4         5         6         7
## 1 0.7620552 0.7620552 0.7620552 0.7620552 0.7620552 0.7620552 0.7620552
## 2           0.7620552 0.7620552 0.7620552 0.7620552 0.7620552 0.7620552
## 3                     0.7620552 0.7620552 0.7620552 0.7620552 0.7620552
## 4                               0.7620552 0.7620552 0.7620552 0.7620552
## 5                                         0.7620552 0.7620552 0.7620552
## 6                                                   0.7620552 0.7620552
## 7                                                             0.7620552
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7         8
## 1 0.5854193 0.5854193 0.5854193 0.5854193 0.5854193 0.5854193 0.5854193
## 2           0.5854193 0.5854193 0.5854193 0.5854193 0.5854193 0.5854193
## 3                     0.5854193 0.5854193 0.5854193 0.5854193 0.5854193
## 4                               0.5854193 0.5854193 0.5854193 0.5854193
## 5                                         0.5854193 0.5854193 0.5854193
## 6                                                   0.5854193 0.5854193
## 7                                                             0.5854193
```

```r
cjs.G1G2$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.7620552","2":"0.0079070","3":"0.7462124","4":"0.7772043","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"0.5854193","2":"0.0120163","3":"0.5616892","4":"0.6087595","5":"","6":"","_rn_":"p g1 c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Modèle avec survie qui dépend du temps.

```r
phi.time <- list(formula=~time)  
cjs.G1G2 <- mark(G1G2.proc,
                G1G2.ddl,
                model.parameters = list(Phi = phi.time, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~time)p(~1) 
## 
## Npar :  8
## -2lnL:  5792.723
## AICc :  5808.782
## 
## Beta
##                  estimate        se        lcl       ucl
## Phi:(Intercept) 0.7598157 0.0909049  0.5816420 0.9379894
## Phi:time2       0.3979970 0.1933472  0.0190366 0.7769574
## Phi:time3       0.7072565 0.2137484  0.2883097 1.1262033
## Phi:time4       0.6334193 0.2291567  0.1842721 1.0825665
## Phi:time5       0.4558443 0.2336743 -0.0021573 0.9138459
## Phi:time6       0.8182721 0.3428348  0.1463158 1.4902284
## Phi:time7       1.0221790 0.5473009 -0.0505309 2.0948888
## p:(Intercept)   0.3870597 0.0505148  0.2880506 0.4860688
## 
## 
## Real Parameter Phi
##  
##           1        2        3         4         5         6        7
## 1 0.6813137 0.760935 0.812612 0.8011082 0.7712989 0.8289335 0.855943
## 2           0.760935 0.812612 0.8011082 0.7712989 0.8289335 0.855943
## 3                    0.812612 0.8011082 0.7712989 0.8289335 0.855943
## 4                             0.8011082 0.7712989 0.8289335 0.855943
## 5                                       0.7712989 0.8289335 0.855943
## 6                                                 0.8289335 0.855943
## 7                                                           0.855943
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7         8
## 1 0.5955747 0.5955747 0.5955747 0.5955747 0.5955747 0.5955747 0.5955747
## 2           0.5955747 0.5955747 0.5955747 0.5955747 0.5955747 0.5955747
## 3                     0.5955747 0.5955747 0.5955747 0.5955747 0.5955747
## 4                               0.5955747 0.5955747 0.5955747 0.5955747
## 5                                         0.5955747 0.5955747 0.5955747
## 6                                                   0.5955747 0.5955747
## 7                                                             0.5955747
```

```r
cjs.G1G2$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.6813137","2":"0.0197378","3":"0.6414452","4":"0.7186933","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"0.7609350","2":"0.0259835","3":"0.7063779","4":"0.8081089","5":"","6":"","_rn_":"Phi g1 c1 a1 t2"},{"1":"0.8126120","2":"0.0294916","3":"0.7479048","4":"0.8637363","5":"","6":"","_rn_":"Phi g1 c1 a2 t3"},{"1":"0.8011082","2":"0.0335598","3":"0.7271892","4":"0.8588852","5":"","6":"","_rn_":"Phi g1 c1 a3 t4"},{"1":"0.7712989","2":"0.0379757","3":"0.6886254","4":"0.8372107","5":"","6":"","_rn_":"Phi g1 c1 a4 t5"},{"1":"0.8289335","2":"0.0470411","3":"0.7166459","4":"0.9027614","5":"","6":"","_rn_":"Phi g1 c1 a5 t6"},{"1":"0.8559430","2":"0.0668935","3":"0.6723169","4":"0.9450756","5":"","6":"","_rn_":"Phi g1 c1 a6 t7"},{"1":"0.5955747","2":"0.0121673","3":"0.5715188","4":"0.6191799","5":"","6":"","_rn_":"p g1 c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## Question 3

A l’aide du package `R2ucare`, tester la qualité de l’ajustement du modèle CJS aux données G1, G2 et G1+G2. Quelles sont vos conclusions ?

G1

```r
overall_CJS(G1, rep(1,nrow(G1)))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["chi2"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["degree_of_freedom"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["p_value"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"3.327","2":"9","3":"0.95","_rn_":"Gof test for CJS model:"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

G2

```r
overall_CJS(G2, rep(1,nrow(G2)))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["chi2"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["degree_of_freedom"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["p_value"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"15.041","2":"14","3":"0.375","_rn_":"Gof test for CJS model:"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

G1G2

```r
overall_CJS(G1plusG2, rep(1,nrow(G1plusG2)))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["chi2"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["degree_of_freedom"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["p_value"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"150.342","2":"15","3":"0","_rn_":"Gof test for CJS model:"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## Question 4

Il peut y avoir des animaux en transit sur la zone d’étude.

a) Pour créer artificiellement une telle situation, rajouter 50 individus en transit (i.e. possédant une histoire avec un seul événement de capture) à chaque date dans G1. 


```r
G1transit <- as.matrix(G1)
ntransients <- 50
for (j in 1:nocc){
   zeros <- matrix(0, nrow = ntransients, ncol = nocc)
   zeros[, j] <- 1
   G1transit <- rbind(G1transit, zeros)
}
G1transit <- data.frame(y = G1transit)
```


```r
dim(G1transit)
```

```
## [1] 900   8
```



```r
head(G1transit)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["y.y.1"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["y.y.2"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["y.y.3"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["y.y.4"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["y.y.5"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["y.y.6"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["y.y.7"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["y.y.8"],"name":[8],"type":["dbl"],"align":["right"]}],"data":[{"1":"1","2":"1","3":"0","4":"0","5":"1","6":"0","7":"1","8":"1","_rn_":"1"},{"1":"1","2":"0","3":"0","4":"0","5":"0","6":"0","7":"0","8":"0","_rn_":"2"},{"1":"1","2":"0","3":"0","4":"0","5":"0","6":"0","7":"0","8":"0","_rn_":"3"},{"1":"1","2":"0","3":"0","4":"0","5":"0","6":"0","7":"0","8":"0","_rn_":"4"},{"1":"1","2":"1","3":"0","4":"0","5":"0","6":"0","7":"0","8":"0","_rn_":"5"},{"1":"1","2":"1","3":"1","4":"0","5":"0","6":"0","7":"0","8":"0","_rn_":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>



```r
tail(G1transit)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["y.y.1"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["y.y.2"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["y.y.3"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["y.y.4"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["y.y.5"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["y.y.6"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["y.y.7"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["y.y.8"],"name":[8],"type":["dbl"],"align":["right"]}],"data":[{"1":"0","2":"0","3":"0","4":"0","5":"0","6":"0","7":"0","8":"1","_rn_":"895"},{"1":"0","2":"0","3":"0","4":"0","5":"0","6":"0","7":"0","8":"1","_rn_":"896"},{"1":"0","2":"0","3":"0","4":"0","5":"0","6":"0","7":"0","8":"1","_rn_":"897"},{"1":"0","2":"0","3":"0","4":"0","5":"0","6":"0","7":"0","8":"1","_rn_":"898"},{"1":"0","2":"0","3":"0","4":"0","5":"0","6":"0","7":"0","8":"1","_rn_":"899"},{"1":"0","2":"0","3":"0","4":"0","5":"0","6":"0","7":"0","8":"1","_rn_":"900"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


b) Faire tourner le modèle CJS à ces nouvelles données avec `RMark`. Quelles sont vos conclusions concernant les estimations ?


```r
G1transitmarked <- data.frame(ch = tidyr::unite(G1transit, col = "ch", sep = ""), 
                              n = rep(1, nrow(G1transit)))
```


```r
G1transit.proc <- process.data(G1transitmarked)
G1transit.ddl <- make.design.data(G1transit.proc)
```

Ajuste le modèle. 

```r
cjs.G1transit <- mark(G1transit.proc,
                     G1transit.ddl,
                     model.parameters = list(Phi = phi, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~1)p(~1) 
## 
## Npar :  2
## -2lnL:  3793.282
## AICc :  3797.288
## 
## Beta
##                  estimate        se       lcl       ucl
## Phi:(Intercept) 0.7871844 0.0479482 0.6932058 0.8811629
## p:(Intercept)   1.1885539 0.0770665 1.0375036 1.3396042
## 
## 
## Real Parameter Phi
##  
##           1         2         3         4         5         6         7
## 1 0.6872264 0.6872264 0.6872264 0.6872264 0.6872264 0.6872264 0.6872264
## 2           0.6872264 0.6872264 0.6872264 0.6872264 0.6872264 0.6872264
## 3                     0.6872264 0.6872264 0.6872264 0.6872264 0.6872264
## 4                               0.6872264 0.6872264 0.6872264 0.6872264
## 5                                         0.6872264 0.6872264 0.6872264
## 6                                                   0.6872264 0.6872264
## 7                                                             0.6872264
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7         8
## 1 0.7664823 0.7664823 0.7664823 0.7664823 0.7664823 0.7664823 0.7664823
## 2           0.7664823 0.7664823 0.7664823 0.7664823 0.7664823 0.7664823
## 3                     0.7664823 0.7664823 0.7664823 0.7664823 0.7664823
## 4                               0.7664823 0.7664823 0.7664823 0.7664823
## 5                                         0.7664823 0.7664823 0.7664823
## 6                                                   0.7664823 0.7664823
## 7                                                             0.7664823
```

```r
cjs.G1transit$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.6872264","2":"0.0103063","3":"0.6666797","4":"0.7070632","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"0.7664823","2":"0.0137939","3":"0.7383680","4":"0.7924248","5":"","6":"","_rn_":"p g1 c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Idem avec survie qui dépend du temps.

```r
cjs.G1transit <- mark(G1transit.proc,
                     G1transit.ddl,
                     model.parameters = list(Phi = phi.time, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~time)p(~1) 
## 
## Npar :  8
## -2lnL:  3776.066
## AICc :  3792.138
## 
## Beta
##                   estimate        se        lcl        ucl
## Phi:(Intercept)  1.1097470 0.1223381  0.8699643  1.3495298
## Phi:time2       -0.3581903 0.1908887 -0.7323321  0.0159515
## Phi:time3       -0.2686698 0.1890295 -0.6391678  0.1018281
## Phi:time4       -0.4461148 0.1934616 -0.8252994 -0.0669301
## Phi:time5       -0.4810592 0.2040143 -0.8809273 -0.0811912
## Phi:time6       -0.3850168 0.2250947 -0.8262023  0.0561688
## Phi:time7       -0.8490586 0.2307665 -1.3013610 -0.3967562
## p:(Intercept)    1.1866979 0.0786387  1.0325661  1.3408297
## 
## 
## Real Parameter Phi
##  
##           1         2        3         4         5         6         7
## 1 0.7520819 0.6795178 0.698692 0.6600759 0.6521919 0.6736478 0.5648055
## 2           0.6795178 0.698692 0.6600759 0.6521919 0.6736478 0.5648055
## 3                     0.698692 0.6600759 0.6521919 0.6736478 0.5648055
## 4                              0.6600759 0.6521919 0.6736478 0.5648055
## 5                                        0.6521919 0.6736478 0.5648055
## 6                                                  0.6736478 0.5648055
## 7                                                            0.5648055
## 
## 
## Real Parameter p
##  
##         2       3       4       5       6       7       8
## 1 0.76615 0.76615 0.76615 0.76615 0.76615 0.76615 0.76615
## 2         0.76615 0.76615 0.76615 0.76615 0.76615 0.76615
## 3                 0.76615 0.76615 0.76615 0.76615 0.76615
## 4                         0.76615 0.76615 0.76615 0.76615
## 5                                 0.76615 0.76615 0.76615
## 6                                         0.76615 0.76615
## 7                                                 0.76615
```

```r
cjs.G1transit$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.7520819","2":"0.0228105","3":"0.7047383","4":"0.7940527","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"0.6795178","2":"0.0270283","3":"0.6244073","4":"0.7300381","5":"","6":"","_rn_":"Phi g1 c1 a1 t2"},{"1":"0.6986920","2":"0.0305397","3":"0.6356993","4":"0.7549905","5":"","6":"","_rn_":"Phi g1 c1 a2 t3"},{"1":"0.6600759","2":"0.0337817","3":"0.5911056","4":"0.7228668","5":"","6":"","_rn_":"Phi g1 c1 a3 t4"},{"1":"0.6521919","2":"0.0371981","3":"0.5762203","4":"0.7211351","5":"","6":"","_rn_":"Phi g1 c1 a4 t5"},{"1":"0.6736478","2":"0.0419754","3":"0.5867403","4":"0.7500641","5":"","6":"","_rn_":"Phi g1 c1 a5 t6"},{"1":"0.5648055","2":"0.0489543","3":"0.4676276","4":"0.6572466","5":"","6":"","_rn_":"Phi g1 c1 a6 t7"},{"1":"0.7661500","2":"0.0140892","3":"0.7374131","4":"0.7926263","5":"","6":"","_rn_":"p g1 c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

c) Tester l’ajustement du modèle CJS à ces mêmes données avec `R2ucare`. Interpréter en particulier la composante 3.SR du test.


```r
overall_CJS(G1transit, rep(1,nrow(G1transit)))
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["chi2"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["degree_of_freedom"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["p_value"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"543.606","2":"15","3":"0","_rn_":"Gof test for CJS model:"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
test2ct(G1transit, rep(1,nrow(G1transit)))
```

```
## $test2ct
##      stat        df     p_val sign_test 
##     1.135     5.000     0.951     0.600 
## 
## $details
##   component dof  stat p_val signed_test  test_perf
## 1         2   1 0.112 0.737      -0.335 Chi-square
## 2         3   1 0.003 0.953       0.055 Chi-square
## 3         4   1 0.721 0.396       0.849 Chi-square
## 4         5   1 0.139 0.709       0.373 Chi-square
## 5         6   1  0.16  0.69         0.4     Fisher
```

```r
test3sr(G1transit, rep(1,nrow(G1transit)))
```

```
## $test3sr
##      stat        df     p_val sign_test 
##   540.279     6.000     0.000    23.140 
## 
## $details
##   component    stat p_val signed_test  test_perf
## 1         2  96.827     0        9.84 Chi-square
## 2         3 103.329     0      10.165 Chi-square
## 3         4  88.333     0       9.399 Chi-square
## 4         5   94.62     0       9.727 Chi-square
## 5         6 100.743     0      10.037 Chi-square
## 6         7  56.427     0       7.512 Chi-square
```


d) Faire tourner un modèle à 2 classes d’âge sur la survie $\phi(a2*t)$ avec `RMark`. Vos conclusions ?


```r
G1transit.ddl <- make.design.data(G1transit.proc)
# create 0, 1+ age variable
G1transit.ddl <- add.design.data(G1transit.proc,
                             G1transit.ddl, # add 2 age-class structure to design matrix
                             "Phi",
                             type = "age",
                             bins = c(0, 1, nocc - 1),
                             name = "ageclass",
                             right = FALSE)
```

On spécifie une survie qui dépend de l'âge.

```r
phi.age <- list(formula=~ageclass) # age effect on survival
```

On ajuste le modèle.

```r
cjsage.G1transit <- mark(G1transit.proc,
                     G1transit.ddl,
                     model.parameters = list(Phi = phi.age, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~ageclass)p(~1) 
## 
## Npar :  3
## -2lnL:  3604.772
## AICc :  3610.784
## 
## Beta
##                     estimate        se        lcl      ucl
## Phi:(Intercept)   -0.1000537 0.0738121 -0.2447254 0.044618
## Phi:ageclass[1,7]  1.4656701 0.1046905  1.2604768 1.670864
## p:(Intercept)      1.3783584 0.0769186  1.2275979 1.529119
## 
## 
## Real Parameter Phi
##  
##           1         2         3         4         5         6         7
## 1 0.4750074 0.7966710 0.7966710 0.7966710 0.7966710 0.7966710 0.7966710
## 2           0.4750074 0.7966710 0.7966710 0.7966710 0.7966710 0.7966710
## 3                     0.4750074 0.7966710 0.7966710 0.7966710 0.7966710
## 4                               0.4750074 0.7966710 0.7966710 0.7966710
## 5                                         0.4750074 0.7966710 0.7966710
## 6                                                   0.4750074 0.7966710
## 7                                                             0.4750074
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7         8
## 1 0.7987272 0.7987272 0.7987272 0.7987272 0.7987272 0.7987272 0.7987272
## 2           0.7987272 0.7987272 0.7987272 0.7987272 0.7987272 0.7987272
## 3                     0.7987272 0.7987272 0.7987272 0.7987272 0.7987272
## 4                               0.7987272 0.7987272 0.7987272 0.7987272
## 5                                         0.7987272 0.7987272 0.7987272
## 6                                                   0.7987272 0.7987272
## 7                                                             0.7987272
```

```r
cjsage.G1transit$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.4750074","2":"0.0184069","3":"0.4391222","4":"0.5111526","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"0.7966710","2":"0.0113847","3":"0.7734445","4":"0.8180764","5":"","6":"","_rn_":"Phi g1 c1 a1 t2"},{"1":"0.7987272","2":"0.0123656","3":"0.7733979","4":"0.8218774","5":"","6":"","_rn_":"p g1 c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

D'une autre façon. 

```r
G1transit.ddl <- make.design.data(G1transit.proc)
#max age 4
G1transit.ddl$Phi$max.age <- as.factor((G1transit.ddl$Phi$Age < 1) * G1transit.ddl$Phi$Age + (G1transit.ddl$Phi$Age>0) * 1)
phi.max.age <- list(formula=~max.age)
cjsaget.G1transit <- mark(G1transit.proc,
                     G1transit.ddl,
                     model.parameters = list(Phi = phi.max.age, p = p))
```

```
## 
## Output summary for CJS model
## Name : Phi(~max.age)p(~1) 
## 
## Npar :  3
## -2lnL:  3604.772
## AICc :  3610.784
## 
## Beta
##                   estimate        se        lcl      ucl
## Phi:(Intercept) -0.1000537 0.0738121 -0.2447254 0.044618
## Phi:max.age1     1.4656701 0.1046905  1.2604767 1.670864
## p:(Intercept)    1.3783584 0.0769186  1.2275979 1.529119
## 
## 
## Real Parameter Phi
##  
##           1         2         3         4         5         6         7
## 1 0.4750074 0.7966710 0.7966710 0.7966710 0.7966710 0.7966710 0.7966710
## 2           0.4750074 0.7966710 0.7966710 0.7966710 0.7966710 0.7966710
## 3                     0.4750074 0.7966710 0.7966710 0.7966710 0.7966710
## 4                               0.4750074 0.7966710 0.7966710 0.7966710
## 5                                         0.4750074 0.7966710 0.7966710
## 6                                                   0.4750074 0.7966710
## 7                                                             0.4750074
## 
## 
## Real Parameter p
##  
##           2         3         4         5         6         7         8
## 1 0.7987272 0.7987272 0.7987272 0.7987272 0.7987272 0.7987272 0.7987272
## 2           0.7987272 0.7987272 0.7987272 0.7987272 0.7987272 0.7987272
## 3                     0.7987272 0.7987272 0.7987272 0.7987272 0.7987272
## 4                               0.7987272 0.7987272 0.7987272 0.7987272
## 5                                         0.7987272 0.7987272 0.7987272
## 6                                                   0.7987272 0.7987272
## 7                                                             0.7987272
```

```r
PIMS(cjsaget.G1transit,"Phi")
```

```
## group = Group 1 
##    1  2  3  4  5  6  7
## 1  1  2  2  2  2  2  2
## 2     1  2  2  2  2  2
## 3        1  2  2  2  2
## 4           1  2  2  2
## 5              1  2  2
## 6                 1  2
## 7                    1
```

```r
cjsaget.G1transit$results$real
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["estimate"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["se"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["lcl"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["ucl"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["fixed"],"name":[5],"type":["chr"],"align":["left"]},{"label":["note"],"name":[6],"type":["chr"],"align":["left"]}],"data":[{"1":"0.4750074","2":"0.0184069","3":"0.4391222","4":"0.5111526","5":"","6":"","_rn_":"Phi g1 c1 a0 t1"},{"1":"0.7966710","2":"0.0113847","3":"0.7734445","4":"0.8180764","5":"","6":"","_rn_":"Phi g1 c1 a1 t2"},{"1":"0.7987272","2":"0.0123656","3":"0.7733979","4":"0.8218774","5":"","6":"","_rn_":"p g1 c1 a1 t2"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Supprime fichiers créés en cours de route.

```r
cleanup(ask = FALSE)
```

