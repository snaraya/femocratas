
# Estandarización de nombres

norm_id <- function(x) {
  
  library(stringr)
  library(stringi)
  
  x |> 
    str_to_lower() |> 
    str_trim() |> 
    stri_trans_general("Latin-ASCII") |> 
    str_replace_all("[[:space:][:punct:]]+", "_")  |> 
    str_replace_all("_+", "_") |> 
    str_remove_all("^_|_$")
}


# Búsqueda por palabras clave

find_col <- function(target, keywords) {
  # busca por keywords en nombres normalizados
  hits <- names(nms_norm)[sapply(names(nms_norm), function(nm) {
    any(stringr::str_detect(nms_norm[[nm]], paste0(keywords, collapse="|")))
  })]
  if (length(hits)) hits[1] else NA_character_
}

# Resumir grupo

resumir_grupo <- function(df, group_var){
  stopifnot(group_var %in% names(df))
  df  |> 
    dplyr::group_by(.data[[group_var]])  |> 
    dplyr::summarise(
      n_docs = dplyr::n(),
      defensa_pct    = round(mean(defensa_bin,    na.rm = TRUE)*100, 1),
      antigenero_pct = round(mean(antigenero_bin, na.rm = TRUE)*100, 1),
      brechas_pct    = round(mean(brechas_bin,    na.rm = TRUE)*100, 1)
    ) %>% dplyr::arrange(dplyr::desc(n_docs))
}


# Diccionario temático

gen_palabras <- function(x) {
  x <- trimws(as.character(x))
  x <- x[nchar(x) > 0 & !is.na(x)]
  unique(x)
}

defensa_terms <- gen_palabras(
  c(
    "defend*",
    "sosten*",
    "mantener*",
    "preserv*",
    "garantiz*",
    "promov*",
    "implement*",
    "articul*",
    "coordina*",
    "incid*",
    "estrateg*",
    "alianz*",
    "presupuest*",
    "program*",
    "politic*",
    "public*",
    "derech*"
  )
)
antigenero_terms <- gen_palabras(
  c(
    "retroces*",
    "recort*",
    "derog*",
    "elimin*",
    "desfinanc*",
    "ajuste*",
    "antigener*",
    "wok*",
    "milei",
    "decret*",
    "anulacion*"
  )
)
brechas_terms <- gen_palabras(
  c(
    "brech*",
    "obstacul*",
    "barrera*",
    "resistenc*",
    "traba*",
    "limitacion*",
    "falta*",
    "recurso*",
    "apoyo*",
    "violenc*",
    "masculin*",
    "logic*",
    "cuid*",
    "sobrecarg*"
  )
)

dict_fem <- quanteda::dictionary(list(
  defensa    = defensa_terms,
  antigenero = antigenero_terms,
  brechas    = brechas_terms
))
