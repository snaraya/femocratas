
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

# Funciones gráficos heatmap chi2

plot_keyness_heatmap <- function(dfm, 
                                 var_autonomia,
                                 n_top = 20,
                                 titulo = "",
                                 subtitulo = "") {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(quanteda.textstats)
  
  # Crear grupo dinámico
  docvars(dfm, "grupo") <- paste(
    docvars(dfm)$pais,
    docvars(dfm)[[var_autonomia]],
    sep = "_"
  )
  
  grupos <- unique(docvars(dfm)$grupo)
  
  # Calcular keyness
  key_list <- lapply(grupos, function(g) {
    target <- docvars(dfm)$grupo == g
    
    textstat_keyness(dfm, target = target) |>
      mutate(grupo = g)
  })
  
  key_all <- bind_rows(key_list)
  
  # Seleccionar top palabras
  top_words <- key_all |>
    group_by(feature) |>
    summarise(max_chi2 = max(abs(chi2)), .groups = "drop") |>
    slice_max(max_chi2, n = n_top, with_ties = FALSE) |>
    pull(feature)
  
  # Preparar datos
  df_plot <- key_all |>
    filter(feature %in% top_words) |>
    mutate(
      feature = str_to_title(gsub("_", " ", feature)),
      pais = str_split(grupo, "\\_", simplify = TRUE)[,1],
      tipo = str_split(grupo, "\\_", simplify = TRUE)[,2]
    )
  
  # Gráfico
  ggplot(df_plot, aes(x = pais, y = reorder(feature, chi2), fill = chi2)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradient2(
      low = "#e63946",
      mid = "#f1faee",
      high = "#1d3557",
      midpoint = 0,
      name = "Keyness (chi²)"
    ) +
    facet_wrap(~tipo) +
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_family = "Roboto") +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )
}


# Tablas de frecuencia

library(dplyr)
library(tidyr)
library(writexl)
library(rlang)

tabla_frecuencias <- function(data, var, carpeta = "outputs") {
  
  var_quo <- enquo(var)
  var_name <- quo_name(var_quo)
  
  # Tabla base
  tabla <- data |> 
    count(pais, !!var_quo) |> 
    pivot_wider(names_from = pais,
                values_from = n,
                values_fill = 0) |> 
    mutate(total_fila = rowSums(across(-!!var_quo)))
  
  # Totales por columna
  totales_col <- tabla |>
    summarise(across(-!!var_quo, sum)) |>
    mutate(!!var_quo := "Total")
  
  # Tabla final
  tabla_final <- bind_rows(tabla, totales_col)
  
  # Guardar archivo
  ruta <- paste0(carpeta, "/tablas_frecuencia_", var_name, ".xlsx")
  
  write_xlsx(tabla_final, path = ruta)
  
  return(tabla_final)
}


guardar_fig <- function(plot, nombre_archivo, carpeta = "figs",
                        width_px = 750, height_px = 450, dpi = 300) {
  
  ruta <- file.path(carpeta, nombre_archivo)
  
  ggplot2::ggsave(
    filename = ruta,
    plot = last_plot(),
    width = width_px / dpi,
    height = height_px / dpi,
    units = "in",
    dpi = dpi
  )
  
}
