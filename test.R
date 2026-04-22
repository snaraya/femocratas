# Bigramas

Posibles combinaciones de palabras a tener en consideración para el análisis:
  
  ```{r}

bigrama <- docs  |> 
  unnest_tokens(bigrama, text, token = "ngrams", n = 2) |> 
  count(bigrama) |> 
  filter(n > 5)

bigrama_sep <- bigrama  |> 
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ")

# Eliminar stopwords en bigramas

stop_es <- tibble(word = stopwords("es"))

stop_es <- stop_es |>
  mutate(
    word = stri_trans_general(word, "latin-ascii"),
    word = str_remove_all(word, "[[:punct:]]"),
    word = str_to_lower(word)
  )

# Base bigramas limpia

bigrama_limpios <- bigrama_sep  |> 
  filter(!str_detect(palabra1, "\\d"),
         !str_detect(palabra2, "\\d"))  |> 
  filter(nchar(palabra1) > 2,
         nchar(palabra2) > 2) |> 
  filter(!palabra1 %in% stop_es$word,
         !palabra2 %in% stop_es$word)

# Limpiar bigramas manualmente

writexl::write_xlsx(bigrama_limpios, "bigramas.xlsx")


# Cargamos los bigramas revisados

bigramas_limpios <- readxl::read_xlsx("bigramas_rev.xlsx")

# Generamos vector

bigramas <- bigramas_limpios |> 
  filter(!is.na(mantener)) |> 
  mutate(frase = paste(palabra1, palabra2)) |> 
  select(frase) |> 
  pull()

rm(bigrama, bigrama_limpios, bigrama_sep)

```



Agregamos los bigramas encontrados anteriormente:
  
  ```{r}

# vector "bigramas" obtenido desde bigramas

bigramas

toks_comp <- tokens_compound(
  toks,
  pattern = phrase(bigramas)
)
```




```{r}
# Quitar términos de dos o menos letras

toks <- tokens_keep(toks, pattern = "^[[:alpha:]]{2,}$", valuetype = "regex") # Siglas de partidos
```