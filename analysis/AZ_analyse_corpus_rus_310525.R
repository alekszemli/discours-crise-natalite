library(tm)
library(stringr)
library(data.table)
library(udpipe)
library(R.temis)
library(ggplot2)
library(dplyr)
library(tidytext)
library(wordcloud)
library(questionr)

# Inroducing the corpus --------------------------------------------------------
corpus <- R.temis::import_corpus("C:/rdocs/Corpus_rus.txt", format="alceste", 
                                 language="russian")

meta_no_date <- meta(corpus)[, !(names(meta(corpus)) %in% "Date")]
print(meta_no_date)

# an
freq(meta(corpus)$an)

data <- data.frame(
  year = factor(c(2020, 2021, 2022, 2023, 2024, 2025)),
  perc = c(12.6, 7.0, 7.6, 25.2, 44.9, 2.7)
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
    title = "Publications dans la presse russe selon l'année",
    x = NULL,
    y = "Pourcentage (%)"
  ) +
  theme_minimal()

#presse
freq(meta(corpus)$presse)

data1 <- data.frame(
  presse = factor(c("Kommersant", "Meduza", "Nezavisimaia", "Novaia")),
  perc = c(78.7, 4.0, 7.0, 10.3)
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
    title = "Publications dans la presse russe par titre du journal",
    x = NULL,
    y = "Pourcentage (%)"
  ) +
  theme_minimal()

#quart
freq(meta(corpus)$quart)

# an x quart

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
asupp <- c("это", "тыс", "млн", "которые", "однако", "пока", "которых", 
           "числе", "хотя")
dtmsmo2 <-dtmsmo[, !colnames(dtmsmo)  %in% asupp]
cloud<-word_cloud(dtmsmo2, n=100, min.freq=10)

# Lemmatisation ---------------------------------------------------------------

# Загрузка модели
model <- udpipe_load_model("C:/rdocs/russian-gsd-ud-2.5-191206.udpipe")


# Пример: путь к файлу
filepath <- "C:/rdocs/Corpus_rus.txt"

# Чтение файла и перекодировка в UTF-8
lines <- readLines(filepath, encoding = "Windows-1251")
lines <- iconv(lines, from = "Windows-1251", to = "UTF-8")

# Разбиваем по документам
doc_starts <- grep("^\\*\\*\\*\\*", lines)
doc_ends <- c(doc_starts[-1] - 1, length(lines))

docs <- mapply(function(start, end) lines[start:end], doc_starts, doc_ends, SIMPLIFY = FALSE)

# Извлекаем текст и мета-данные
corpus <- lapply(docs, function(doc_lines) {
  header <- doc_lines[1]
  text <- paste(doc_lines[-1], collapse = " ")
  list(
    meta = header,
    text = text
  )
})


lemm_results <- lapply(seq_along(corpus), function(i) {
  doc <- corpus[[i]]
  ann <- udpipe_annotate(model, x = doc$text, doc_id = paste0("doc_", i))
  df <- as.data.frame(ann)
  df$meta <- doc$meta
  return(df)
})

lemmatized <- rbindlist(lemm_results, fill = TRUE)

head(lemmatized[, .(doc_id, token, lemma, upos, meta)])


# Фильтрация: убираем NA, пустые строки, и технические токены
cleaned <- lemmatized[!is.na(lemma) & lemma != "" & grepl("[а-яА-Я]", lemma)]

# Группируем по леммам
freq_table <- cleaned[, .N, by = lemma][order(-N)]

# Определим список частей речи, которые хотим исключить
exclude_upos <- c("ADP", "AUX", "CCONJ", "DET", "PART", "PRON", "SCONJ", "PUNCT")

# Фильтруем по частям речи (исключаем служебные части речи)
filtered_lemmas <- lemmatized[!upos %in% exclude_upos, ]

# Группируем леммы по частоте
freq_filtered <- filtered_lemmas[, .N, by = lemma][order(-N)]

# Объединяем частоты и очищенные данные
freq_filtered_cleaned <- merge(freq_filtered, freq_table, by = "lemma", all.x = TRUE)

# Убираем все строки с нулевой или отрицательной частотой
freq_filtered_cleaned <- freq_filtered_cleaned[N.x > 0, ]

# Если очищенный набор данных пуст, выводим сообщение и прекращаем выполнение
if (nrow(freq_filtered_cleaned) == 0) {
  stop("После фильтрации остались только стоп-слова или данные с нулевой частотой. Облачко не будет построено.")
}

# Определяем максимальную частоту слов в очищенном наборе
max_freq <- max(freq_filtered_cleaned$N.x)

# Задаем минимальную частоту, если max_freq меньше 1
min_freq_value <- ifelse(max_freq < 1, 1, 1)  # Устанавливаем min.freq равным 1

# Создаем облако слов без служебных частей речи
wordcloud(words = freq_filtered_cleaned$lemma,
          freq = freq_filtered_cleaned$N.x,
          min.freq = min_freq_value,  # Используем минимальное значение min.freq
          scale = c(4, 0.8),
          random.order = FALSE,
          max.words = 100,
          rot.per = 0)

# GRAPHIQUE DES FREQUENCES -----------------------------------------------------

filtered_lemmas <- filtered_lemmas[lemma != "процент-знак"]

top10_lemmas <- filtered_lemmas[, .N, by = lemma][order(-N)][1:10]
setnames(top10_lemmas, "N", "frequency")

top10_lemmas[, freq_range := cut(frequency,
                                 breaks = c(-Inf, 500, 1000, 1500, Inf),
                                 labels = c("<500", "500–1000", "1000–1500", ">1500"))]

# Упорядочиваем факторы
top10_lemmas$lemma <- factor(top10_lemmas$lemma, 
                             levels = top10_lemmas$lemma[order(top10_lemmas$frequency)])

# Палитра в голубых тонах (от светлого к тёмному)
blue_palette <- c("<500" = "#deebf7",     # светло-голубой
                  "500–1000" = "#9ecae1", # средне-голубой
                  "1000–1500" = "#3182bd",# насыщенный
                  ">1500" = "#08519c")    # тёмно-синий

# Построение графика
ggplot(top10_lemmas, aes(x = lemma, y = frequency, fill = freq_range)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = blue_palette, name = "Couleurs par fréquences") +
  coord_flip() +
  labs(x = " ",
       y = " ") +
  theme_minimal(base_size = 14)

# COOCURENCES ------------------------------------------------------------------

# 1. Создаём тексты на основе лемм, исключая "процент-знак"
docs_lemmatized <- lemmatized[lemma != "процент-знак", 
                              .(text = paste(lemma, collapse = " ")), 
                              by = doc_id]

# 2. Создаём корпус из лемм
corpus_tm <- VCorpus(VectorSource(docs_lemmatized$text))

# 3. Назначаем метаданные (если они нужны)
for (i in seq_along(corpus_tm)) {
  meta(corpus_tm[[i]], tag = "doc_id") <- docs_lemmatized$doc_id[i]
}

# 4. Преобразования текста
corpus_tm <- tm_map(corpus_tm, content_transformer(tolower))
corpus_tm <- tm_map(corpus_tm, removePunctuation)
corpus_tm <- tm_map(corpus_tm, removeNumbers)
corpus_tm <- tm_map(corpus_tm, stripWhitespace)

# 5. Создание DocumentTermMatrix (на основе лемм)
dtm <- build_dtm(corpus_tm)

# 6. Анализ коокурренций с R.temis (вокруг слова "женщина", 10 самых частых)
cooc_femme <- cooc_terms(dtm, term = "женщина", n = 10)

# 7. Просмотр результата
print(cooc_femme)

# рождаемость
cooc2 <- cooc_terms(dtm, term = "рождаемость", n = 10)
print(cooc2)
# семья
cooc3 <- cooc_terms(dtm, term = "семья", n = 10)
print(cooc3)
# россия
cooc4 <- cooc_terms(dtm, term = "россия", n = 10)
print(cooc4)

# Lemmes frequents selon l'an 


# Укажите интересующие части речи
include_upos <- c("NOUN", "ADJ", "ADV", "VERB")

# Фильтрация по нужным частям речи
filtered_lemmas <- lemmatized[upos %in% include_upos]

# Извлекаем год из мета-данных
filtered_lemmas[, year := str_extract(meta, "an_\\d{4}") %>% str_replace("an_", "")]

# Группировка: считаем частоту каждой леммы по годам
freq_by_year <- filtered_lemmas %>%
  group_by(year, lemma) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  arrange(year, desc(frequency))

# Берём топ-10 лемм по каждому году
top_lemmas <- freq_by_year %>%
  group_by(year) %>%
  slice_max(order_by = frequency, n = 10, with_ties = FALSE) %>%
  ungroup()

# Для отображения на графике — делаем порядок лемм внутри года
top_lemmas <- top_lemmas %>%
  group_by(year) %>%
  mutate(lemma = reorder_within(lemma, frequency, year)) %>%
  ungroup()

# Построение графика
ggplot(top_lemmas, aes(x = lemma, y = frequency, fill = year)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ year, scales = "free_y") +
  labs(title = "Lemmes les plus fréquents selon l'année de publication dans la presse russe",
       x = NULL,
       y = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")

# Lemmes frequents selon le titre de journal

# Укажите интересующие части речи
include_upos <- c("NOUN", "ADJ", "ADV", "VERB")

# Фильтрация по нужным частям речи
filtered_lemmas <- lemmatized[upos %in% include_upos]

# Извлекаем название газеты из мета-данных
filtered_lemmas[, presse := str_extract(meta, "presse_\\w+") %>% str_replace("presse_", "")]

# Переименование сокращённых названий на полные
filtered_lemmas[, presse_label := case_when(
  presse == "komm"   ~ "Kommersant",
  presse == "meduza" ~ "Meduza",
  presse == "novaya" ~ "Novaïa gazeta",
  presse == "nezav"  ~ "Nezavisimaïa gazeta",
  TRUE               ~ presse  # на случай других значений
)]

# Группировка: считаем частоту лемм по изданиям
freq_by_presse <- filtered_lemmas %>%
  group_by(presse_label, lemma) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  arrange(presse_label, desc(frequency))

# Выбираем топ-10 лемм для каждого издания
top_lemmas_presse <- freq_by_presse %>%
  group_by(presse_label) %>%
  slice_max(order_by = frequency, n = 10, with_ties = FALSE) %>%
  ungroup()

# Для корректной сортировки внутри каждой панели
top_lemmas_presse <- top_lemmas_presse %>%
  group_by(presse_label) %>%
  mutate(lemma = reorder_within(lemma, frequency, presse_label)) %>%
  ungroup()

# Построение графика
ggplot(top_lemmas_presse, aes(x = lemma, y = frequency, fill = presse_label)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ presse_label, scales = "free_y") +
  labs(
    title = "Lemmes les plus fréquents selon le titre de journal",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")

## 11 to 20 the most frequent lemmes

freq_by_presse <- filtered_lemmas %>%
  group_by(presse_label, lemma) %>%
  summarise(frequency = n(), .groups = 'drop') %>%
  arrange(presse_label, desc(frequency)) %>%
  group_by(presse_label) %>%
  mutate(rank = row_number()) %>%
  filter(rank >= 11 & rank <= 20) %>%
  ungroup()

# Подготовка к отображению
freq_by_presse <- freq_by_presse %>%
  group_by(presse_label) %>%
  mutate(lemma = reorder_within(lemma, frequency, presse_label)) %>%
  ungroup()

# Построение графика
ggplot(freq_by_presse, aes(x = lemma, y = frequency, fill = presse_label)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ presse_label, scales = "free_y") +
  labs(
    title = "Леммы с 11 по 20 по частоте по изданиям",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")
