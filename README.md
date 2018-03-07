# ProdFacto
Production d'analyses factorielles (ACM)

## Utilisation

**Installation du package :**
```{r}
if (!require("devtools")) install.packages("devtools", dep=T)
devtools::install_github("Grisoudre/ProdFacto")
```

**Ouverture de l'application :**
```{r}
library(ProdFacto)
ProdACM()
```
Puis, sur la nouvelle fenêtre, cliquer sur "Open in browser".

## Contenu

### Existant

**Fonction :**

ProdACM() : Fonction ouvrant une fenêtre interactive permettant de produire une ACM.

**Données :**

  - hdv2003b : Sélection de 1000 individus à partir de la table hdv2003 du package questionr (extrait de l'enquête "histoire de vie 2003", Insee)
  - PratiquesCultSport : Extrait de l'enquête "Participation culturelle et sportive", Insee, 2003
  - Dico_PartiquesCultSport : Dictionnaire des codes des variables conservées.

### A faire

**Fonction :**

ProdACP() : Fonction ouvrant une fenêtre interactive permettant de produire une ACP.
