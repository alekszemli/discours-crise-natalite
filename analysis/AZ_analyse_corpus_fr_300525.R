library(R.temis)
library(dplyr)
library(ggplot2)
library(questionr)
library(data.table)
library(stringr)
library(tidyr)
library(tidytext)

# Télécharger le corpus et voir les stats descriptives -------------------------

corpus <- R.temis::import_corpus("C:/rdocs/Corpus_fr.txt", format="alceste", 
                                 language="fr")
print(meta(corpus))

freq(meta(corpus)$presse) 

# graphique par presse

data1 <- data.frame(
  presse = factor(c("Le Figaro", "Les Échos", "La Croix", "Le Monde", 
                  "Libération")),
  perc = c(35.6, 30.8, 19.2, 9.6, 4.8)
)

data1$color_group <- cut(
  data1$perc,
  breaks = c(0, 10, 20, 100),
  labels = c("<10%", "10–20%", ">20%"),
  right = FALSE
)

ggplot(data1, aes(x = presse, y = perc, fill = color_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("<10%" = "#deebf7", "10–20%" = "#9ecae1", ">20%" = "#08519c"),
    name = NULL
  ) +
  labs(
    title = "Publications dans la presse française par titre du journal",
    x = NULL,
    y = "Pourcentage (%)"
  ) +
  theme_minimal()

freq(meta(corpus)$an)

# graphique par an
data <- data.frame(
  year = factor(c(2020, 2021, 2022, 2023, 2024, 2025)),
  perc = c(11.5, 19.2, 5.8, 21.2, 31.7, 10.6)
)

data$color_group <- cut(
  data$perc,
  breaks = c(0, 10, 20, 100),
  labels = c("<10%", "10–20%", ">20%"),
  right = FALSE
)

ggplot(data, aes(x = year, y = perc, fill = color_group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("<10%" = "#deebf7", "10–20%" = "#9ecae1", ">20%" = "#08519c"),
    name = NULL
  ) +
  labs(
    title = "Publications dans la presse française selon l'année",
    x = NULL,
    y = "Pourcentage (%)"
  ) +
  theme_minimal()

freq(meta(corpus)$quart)

# Extraire les métadonnées
year <- meta(corpus)$an
quart <- meta(corpus)$quart

# Créer un tableau de répartition (table croisée)
table_quart_year <- table(year, quart)

# Afficher le tableau
print(table_quart_year)

# Nuage de mots ---------------------------------------------------------------
dtmsmo <-build_dtm(corpus, remove_stopwords = T )
dtmsmo
dic <-dictionary(dtmsmo)
word_freq <- colSums(as.matrix(dtmsmo))
word_freq_df <- data.frame(word = names(word_freq), frequency = word_freq)
filtered_words <- word_freq_df[word_freq_df$frequency >= 90, ]
sorted_words <- filtered_words[order(-filtered_words$frequency), ]
print(sorted_words)
asupp <- c("entre", "si", "aussi", "alors", "donc", "où", "quand", 
           "hui", "ainsi", "dont")
dtmsmo2 <-dtmsmo[, !colnames(dtmsmo)  %in% asupp]
cloud<-word_cloud(dtmsmo2, n=100, min.freq=10)

# Lemmatisation ---------------------------------------------------------------
corpus_d <-split_documents(corpus,1)
dtmsmo_d <-build_dtm(corpus_d, remove_stopwords=T)
dtmsmo_d2 <-dtmsmo_d[, !colnames(dtmsmo_d)  %in% asupp]
dtmsmo_d2 # 1306 paragraphes et 7566 termes uniques

lexique3 <- read.csv("C:/rdocs/Lexique382.csv", fileEncoding = "UTF-8", header = TRUE,  sep = ";")
lexique3 <- arrange(lexique3, desc("freqlivres"))
lexique3 <- lexique3[!duplicated(lexique3$ortho),]

lexique3[lexique3$ortho == "plus", ]
lexique3[lexique3$ortho == "plus", "lemme"] <- "plus"

voc_actif <- lexique3[lexique3$cgram %in% c("ADV", "VER", "ADJ", "NOM"),]
dic_total <- merge(dic, voc_actif, by.x="row.names", by.y="ortho", all.x=TRUE)
dic_total <- mutate(dic_total, Term=coalesce(lemme, Term))
rownames(dic_total) <- dic_total$Row.names
nr <- filter(dic_total, is.na(lemme))
head(dic_total)

dic_total3 <- dic_total[!is.na(dic_total$cgramortho), ]
dtmlem2 <- combine_terms(dtmsmo, dic_total3)
dtmlem22<-dtmlem2[, !colnames(dtmlem2) %in% asupp]
dtmlem2_d <- combine_terms(dtmsmo_d,dic_total3)
dtmlem2_d2<-dtmlem2_d[, !colnames(dtmlem2_d) %in% asupp]
dtmlem2_d2 # 1306 paragraphes et 4113 termes uniques

frequent_terms(dtmlem22, n=20)

# 10 lemmes les plus fréquents -------------------------------------------------


# 1. Conversion de la DTM en data.table
dtm_df <- as.data.table(as.matrix(dtmlem2_d2))

# 2. Calcul de la fréquence totale par lemme (somme sur tous les documents)
lemma_freq <- data.table(lemma = colnames(dtm_df),
                         frequency = colSums(dtm_df))

# 3. Extraire les 10 lemmes les plus fréquents
top10_lemmas <- lemma_freq[order(-frequency)][1:10]

# 4. Catégorisation des fréquences pour la palette de couleurs
top10_lemmas[, freq_range := cut(frequency,
                                 breaks = c(-Inf, 350, 450, 550, Inf),
                                 labels = c("<350", "351–450", "451–550", ">550"))]

# 5. Ordonner les facteurs (pour que le barplot respecte l’ordre décroissant)
top10_lemmas$lemma <- factor(top10_lemmas$lemma, 
                             levels = top10_lemmas$lemma[order(top10_lemmas$frequency)])

# 6. Palette en bleu
blue_palette <- c("<350" = "#deebf7",     # bleu clair
                  "351–450" = "#9ecae1", # bleu moyen
                  "451–550" = "#3182bd",# bleu soutenu
                  ">550" = "#08519c")    # bleu foncé

# 7. Génération du graphique
ggplot(top10_lemmas, aes(x = lemma, y = frequency, fill = freq_range)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = blue_palette, name = "Fréquence") +
  coord_flip() +
  labs(title = "Top 10 des lemmes les plus fréquents dans la presse française",
       x = NULL,
       y = NULL) +
  theme_minimal(base_size = 14)

# Coocurences des lemmes les plus fréquents ------------------------------------

dict_lem_occur <- dic_total3[which(dic_total3[, "Occurrences"] > 5), ]
dtm2_lem2_filtre <- combine_terms(dtmsmo, dict_lem_occur)
dtm2_lem2_filtre2<-dtm2_lem2_filtre[, !colnames(dtm2_lem2_filtre) %in% asupp]
dtm2_d_lem2_filtre <- combine_terms(dtmsmo_d, dict_lem_occur)
dtm2_d_lem2_filtre
dtm2_d_lem2_filtre2<-dtm2_d_lem2_filtre[, !colnames(dtm2_d_lem2_filtre) %in% asupp]

cooc_terms(dtm2_d_lem2_filtre2,"enfant",n = 10)

cooc_terms(dtm2_d_lem2_filtre2,"femme",n = 10)

cooc_terms(dtm2_d_lem2_filtre2,"pouvoir",n = 10)

cooc_terms(dtm2_d_lem2_filtre2,"baisse",n = 10)

# Les lemmes fréquents par année -----------------------------------------------


# dtmlem2_d2 : ta matrice DTM (documents x termes)
# years_paragraph : liste de l'année pour chaque document

# 1. Convertir la DTM en data.table
dtm_df <- as.data.table(as.matrix(dtmlem2_d2))

# 2. Transformer la liste 'years_paragraph' en vecteur simple
years_vector <- unlist(years_paragraph)

# 3. Ajouter la colonne 'year' au data.table
dtm_df[, year := years_vector]

# 4. Transformer en format long
dtm_long <- melt(dtm_df, id.vars = "year", variable.name = "lemma", value.name = "frequency")

# 5. S'assurer que 'year' est un vecteur simple de type character (pas liste)
dtm_long[, year := as.character(year)]
dtm_long[, lemma := as.character(lemma)]

# 6. Regrouper et sommer la fréquence par année et lemme
freq_by_year <- dtm_long[, .(frequency = sum(frequency)), by = .(year, lemma)]

# 7. Pour chaque année, sélectionner les 10 lemmes les plus fréquents
top_lemmas <- freq_by_year %>%
  group_by(year) %>%
  slice_max(order_by = frequency, n = 10, with_ties = FALSE) %>%
  ungroup()

# 8. Faire le graphique
ggplot(top_lemmas, aes(x = reorder(lemma, frequency), y = frequency, fill = year)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 des lemmes les plus fréquents selon l'année dans le nouveau corpus",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  facet_wrap(~ year, scales = "free_y")

# Lemmes fréquents par presse --------------------------------------------------

# 1. Conversion de la DTM lemmatisée en data.table
dtm_df <- as.data.table(as.matrix(dtmlem2_d2))  # DTM par paragraphes

# 2. Création du vecteur des titres de presse associés à chaque paragraphe
# Vérifie que ce vecteur est dans le même ordre que dtm_df (i.e., correspondance 1:1)
presse_vector <- rep(meta(corpus)$presse, each = 1)  # Ajuster 'each' si les documents ont été splittés

presse_orig <- meta(corpus)$presse

# Créer un vecteur presse_vector correspondant à chaque paragraphe de corpus_d
# Chaque document a été découpé, on regarde à quoi correspond chaque paragraphe
presse_vector <- meta(corpus_d)$presse

# 3. Ajouter la colonne 'presse' à la DTM
dtm_df[, presse := presse_vector]

# 4. Format long pour regroupement
dtm_long <- melt(dtm_df, id.vars = "presse", variable.name = "lemma", value.name = "frequency")

# 5. Nettoyage des variables
dtm_long[, presse := as.character(presse)]
dtm_long[, lemma := as.character(lemma)]

# 6. Regrouper et sommer la fréquence par presse et lemme
freq_by_presse <- dtm_long[, .(frequency = sum(frequency)), by = .(presse, lemma)]

# 7. Sélection des 10 lemmes les plus fréquents par titre de presse
top_lemmas_presse <- freq_by_presse %>%
  group_by(presse) %>%
  slice_max(order_by = frequency, n = 10, with_ties = FALSE) %>%
  ungroup()

top_lemmas_presse$presse <- recode(top_lemmas_presse$presse,
                                    "libéra" = "La Libération",
                                    "échos" = "Les Échos",
                                    "monde" = "Le Monde", 
                                    "figaro" = "Le Figaro", 
                                    "croix" = "La Croix")

# Fixer l'ordre des facettes
top_lemmas_presse$presse <- factor(top_lemmas_presse$presse,
                                   levels = c("La Croix", "La Libération", 
                                              "Le Monde", "Le Figaro", 
                                              "Les Échos"))


# 8. Visualisation par facettes
ggplot(top_lemmas_presse, aes(x = reorder(lemma, frequency), y = frequency, fill = presse)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 10 des lemmes les plus fréquents selon le titre de presse",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  facet_wrap(~ presse, scales = "free_y")

# 11-20 
top_lemmas_11_20 <- freq_by_presse %>%
  group_by(presse) %>%
  arrange(desc(frequency)) %>%
  mutate(rank = row_number()) %>%
  filter(rank >= 11 & rank <= 20) %>%
  ungroup()

# Переименование названий изданий
top_lemmas_11_20$presse <- recode(top_lemmas_11_20$presse,
                                  "libéra" = "La Libération",
                                  "échos"  = "Les Échos",
                                  "monde"  = "Le Monde", 
                                  "figaro" = "Le Figaro", 
                                  "croix"  = "La Croix")

# Установка порядка для facet_wrap
top_lemmas_11_20$presse <- factor(top_lemmas_11_20$presse,
                                  levels = c("La Croix", "La Libération", 
                                             "Le Monde", "Le Figaro", 
                                             "Les Échos"))

# 8. BIS — Визуализация по facettes для 11–20 лемм
ggplot(top_lemmas_11_20, aes(x = reorder(lemma, frequency), y = frequency, fill = presse)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Lemmes classés 11–20 selon le titre de presse",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  facet_wrap(~ presse, scales = "free_y")
