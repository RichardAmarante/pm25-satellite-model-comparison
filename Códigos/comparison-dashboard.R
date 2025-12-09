suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
  library(geobr)
  library(sf)
  library(scales)
  library(stringr)
  library(plotly)
  library(htmltools)
  library(fresh)
  library(irr)
  library(rsconnect)
  library(leaflet)
  library(shiny)
  library(shinydashboard)
})

# Paleta de Cores
pal_verde_base <- c("#064E3B", "#0E7C66", "#10B981", "#34D399", "#A7F3D0")
pal_verde <- grDevices::colorRampPalette(pal_verde_base)

hex2rgba <- function(hex, alpha = 0.7){ # converte cor HEX pra rgba (eu uso no plotly)
  rgb <- col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.3f)", rgb[1], rgb[2], rgb[3], alpha)
}

tema_amazonia <- create_theme(
  adminlte_color(
    light_blue = pal_verde_base[3],
    green      = pal_verde_base[3],
    aqua       = pal_verde_base[4]
  ),
  adminlte_sidebar(
    dark_bg       = pal_verde_base[1],
    dark_hover_bg = pal_verde_base[2],
    dark_color    = "white",
    dark_submenu_color       = "white",
    dark_submenu_hover_color = "#A7F3D0"
  ),
  adminlte_global(
    content_bg  = "#ECF0F5",
    box_bg      = "white",
    info_box_bg = "white"
  ),
  adminlte_vars(
    border_radius_base = "8px"
  )
)

# Vetor com as abreviações de meses pra não ficar repetindo constantemente c("Jan","Fev",...)
mes_labels <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")

# Leitura e preparação dos dados
UF_ALVO <- 13
UF_SIGLA  <- "AM"

arq_cams_2022 <- "daily_pm25_all_regions_2022.csv"
arq_cams_2023 <- "daily_pm25_all_regions_2023.csv"
arq_don_2022  <- "Donkelar_dados_completos_consolidado_2022.xlsx"
arq_don_2023  <- "Donkelar_dados_completos_consolidado_2023.xlsx"

# Função para garantir o mesmo comportamento de data, tipo YYYY-MM-DD
parse_date_any <- function(x){
  d1 <- suppressWarnings(lubridate::ymd(x, quiet = TRUE))
  d2 <- suppressWarnings(lubridate::dmy(x, quiet = TRUE))
  d1[is.na(d1)] <- d2[is.na(d1)]
  d1
}

ler_cams <- function(arquivo) { # Função para ler os dados do CAMS
  readr::read_csv2(
    arquivo,
    locale = readr::locale(decimal_mark = ".", grouping_mark = ","),
    show_col_types = FALSE, progress = FALSE
  )
}

ler_don <- function(arquivo){ # Função para ler os dados do Donkelar
  readxl::read_xlsx(arquivo) %>% 
    transmute(
      cod_mun7 = as.integer(CD_MUN),
      nome_muni = NM_MUN,
      uf = as.integer(stringr::str_sub(cod_mun7, 1, 2)),
      ano = as.integer(Ano),
      mes = as.integer(Mes),
      pm25 = as.numeric(`Media_PM25`)
    ) %>% 
    filter(uf == UF_ALVO)
}

# Colocando shape e o dicionário de munícipios do Amazonas pelo Geobr
muni_am_sf  <- geobr::read_municipality(code_muni = "AM", year = 2020)

muni_am_dic <- muni_am_sf %>% 
  sf::st_drop_geometry() %>% 
  transmute(
    cod_mun7  = as.integer(code_muni),
    nome_muni = name_muni
  )

muni_am_sf <- muni_am_sf %>% mutate(code_muni = as.integer(code_muni))

# CAMS 2022/2023: mudando de diário para mensal municipal

cams_long_2022 <- ler_cams(arq_cams_2022) %>% 
  rename(Date = 1) %>% 
  pivot_longer(-Date, names_to = "CodRes", values_to = "pm25") %>% 
  mutate(
    cod_mun7 = as.integer(str_remove(CodRes, "^ID_")),
    uf       = as.integer(str_sub(cod_mun7, 1, 2)),
    date     = parse_date_any(Date),
    ano      = year(date),
    mes      = month(date)
  ) %>% 
  select(-Date) %>% 
  filter(uf == UF_ALVO)

cams_long_2023 <- ler_cams(arq_cams_2023) %>% 
  rename(Date = 1) %>% 
  pivot_longer(-Date, names_to = "CodRes", values_to = "pm25") %>% 
  mutate(
    cod_mun7 = as.integer(str_remove(CodRes, "^ID_")),
    uf       = as.integer(str_sub(cod_mun7, 1, 2)),
    date     = parse_date_any(Date),
    ano      = year(date),
    mes      = month(date)
  ) %>% 
  select(-Date) %>% 
  filter(uf == UF_ALVO)

cams_muni_mensal_2022 <- cams_long_2022 %>% 
  group_by(cod_mun7, uf, ano, mes) %>% 
  summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop")

cams_muni_mensal_2023 <- cams_long_2023 %>% 
  group_by(cod_mun7, uf, ano, mes) %>% 
  summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop")

cams_municipal_mensal_am <- bind_rows(cams_muni_mensal_2022, cams_muni_mensal_2023) %>% 
  left_join(muni_am_dic, by = "cod_mun7") %>% 
  mutate(
    fonte     = "CAMS",
    uf_sigla  = UF_SIGLA,
    cod_muni6 = as.integer(str_sub(cod_mun7, 1, 6)),   # volta a usar 6 dígitos (exatamente igual ao original)
    date      = ymd(paste(ano, mes, "01"))
  ) %>% 
  select(date, cod_muni6, uf_sigla, nome_muni, ano, mes, pm25, fonte, cod_mun7, uf)

cams_municipal_anual_am <- cams_municipal_mensal_am %>% 
  group_by(cod_mun7, cod_muni6, nome_muni, uf_sigla, ano) %>% 
  summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop") %>% 
  mutate(fonte = "CAMS") %>% 
  select(cod_muni6, uf_sigla, nome_muni, ano, pm25, fonte, cod_mun7)

# Donkelar 2022/2023

don_muni_mensal_2022 <- ler_don(arq_don_2022)
don_muni_mensal_2023 <- ler_don(arq_don_2023)

don_municipal_mensal_am <- bind_rows(don_muni_mensal_2022, don_muni_mensal_2023) %>% 
  mutate(
    fonte     = "Donkelar",
    uf_sigla  = UF_SIGLA,
    cod_muni6 = as.integer(str_sub(cod_mun7, 1, 6)),  # também 6 dígitos como no código original
    date      = ymd(paste(ano, mes, "01"))
  ) %>% 
  select(date, cod_muni6, uf_sigla, nome_muni, ano, mes, pm25, fonte, cod_mun7, uf)

don_municipal_anual_am <- don_municipal_mensal_am %>% 
  group_by(cod_mun7, cod_muni6, nome_muni, uf_sigla, ano) %>% 
  summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop") %>% 
  mutate(fonte = "Donkelar") %>% 
  select(cod_muni6, uf_sigla, nome_muni, ano, pm25, fonte, cod_mun7)

# Juntando as duas bases e cortando os dados acima do percentil 99, pra tirar possíveis erros absurdos de medição
mensal_ambos_amazonas <- bind_rows(
  cams_municipal_mensal_am %>%
    select(date, cod_muni6, uf_sigla, nome_muni, ano, mes, pm25, fonte, cod_mun7),
  don_municipal_mensal_am %>%
    select(date, cod_muni6, uf_sigla, nome_muni, ano, mes, pm25, fonte, cod_mun7)
)

corte <- mensal_ambos_amazonas %>% 
  group_by(ano, fonte) %>% 
  summarise(p99 = quantile(pm25, 0.99, na.rm = TRUE), .groups = "drop")

mensal_ambos_corte <- mensal_ambos_amazonas %>% 
  left_join(corte, by = c("ano", "fonte")) %>% 
  filter(pm25 <= p99) %>% 
  select(-p99)

# Categorização das concentrações do PM2.5 através de rótulos
breaks_pm25 <- c(0, 5, 10, 15, 25, 35)

labels_pm25 <- c(
  "Excelente (0–5]",
  "Bom (5–10]",
  "Moderado (10–15]",
  "Ruim (15–25]",
  "Péssimo (25–35]"
)

cat_cols <- pal_verde(5)[5:1] 
names(cat_cols) <- labels_pm25 # Ordenando os rótulos das categorias nessa ordem, de Excelente em cima, até Péssimo (em baixo).

pm25_em_categorica <- function(x){
  x2 <- pmin(pmax(x, 0), 35 - 1e-8) 
  out <- cut(
    x2,
    breaks = breaks_pm25,
    labels = labels_pm25,
    include.lowest = TRUE,
    right = TRUE
  )
  factor(out, levels = labels_pm25)  # Só pra garantr que o fator tem sempre os 5 níveis
}

# Criando os mapas com ggplot (não estou mais utilizando, passei a usar o Leaflet)
plot_mapa_cat_anual <- function(df_anual, ano_sel, titulo, muni_sf){
  df_map <- muni_sf %>% 
    left_join(
      df_anual %>%
        filter(ano == ano_sel) %>%
        mutate(pm25_cat = pm25_em_categorica(pm25)),
      by = c("code_muni" = "cod_mun7")
    )
  
  ggplot(df_map) +
    geom_sf(aes(fill = pm25_cat), color = "white", size = 0.05) +
    scale_fill_manual(
      values = cat_cols,
      breaks = labels_pm25,
      drop = FALSE,
      na.value = "grey90",
      name = "PM2.5 (µg/m³)"
    ) +
    guides(fill = guide_legend(override.aes = list(shape = 22, size = 5))) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text  = element_text(size = 11),
      legend.key.height = grid::unit(18, "pt"),
      legend.key.width  = grid::unit(28, "pt")
    ) +
    labs(title = NULL)
}

plot_mapa_cat_bienio <- function(df_mensal, titulo, muni_sf) {
  anual <- df_mensal %>%
    filter(ano %in% c(2022, 2023)) %>%
    group_by(cod_mun7, cod_muni6, nome_muni, uf_sigla) %>%
    summarise(
      pm25 = mean(pm25, na.rm = TRUE),
      .groups = "drop"
    )
  
  df_map <- muni_sf %>%
    left_join(
      anual %>%
        mutate(
          pm25_cat = pm25_em_categorica(pm25),
          cod_mun7 = as.integer(cod_mun7)
        ),
      by = c("code_muni" = "cod_mun7")
    )
  
  ggplot(df_map) +
    geom_sf(aes(fill = pm25_cat), color = "white", size = 0.05) +
    scale_fill_manual(
      values = cat_cols,
      breaks = labels_pm25,
      drop = FALSE,
      na.value = "grey90",
      name = "PM2.5 (µg/m³)"
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(shape = 22, size = 5)
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      legend.key.height = grid::unit(18, "pt"),
      legend.key.width = grid::unit(28, "pt")
    ) +
    labs(title = NULL)
}

# Criando os mapas finais, que serão feitos através do Leaflet

prep_mapa_bienio_leaf <- function(df_mensal, muni_sf) {
  anual <- df_mensal %>%
    filter(ano %in% c(2022, 2023)) %>%
    group_by(cod_mun7, cod_muni6, nome_muni, uf_sigla) %>%
    summarise(
      pm25 = mean(pm25, na.rm = TRUE),
      .groups = "drop"
    )
  
  muni_sf %>%
    left_join(
      anual %>%
        mutate(
          cod_mun7 = as.integer(cod_mun7),
          pm25_cat = pm25_em_categorica(pm25)
        ),
      by = c("code_muni" = "cod_mun7")
    )
}

# Preparando as bases já prontas para o leaflet (uma pra cada fonte)
mapa_cams_leaf <- prep_mapa_bienio_leaf(cams_municipal_mensal_am, muni_am_sf)
mapa_don_leaf  <- prep_mapa_bienio_leaf(don_municipal_mensal_am, muni_am_sf)

fazer_leaflet_mapa <- function(sf_obj) {
  # Paleta com as mesmas cores e labels dos mapas do ggplot
  pal_leaf <- colorFactor(
    palette = cat_cols,
    domain  = labels_pm25,
    ordered = TRUE
  )
  
  # Caixinha que aparece quando passa o cursor no município
  lab_html <- sprintf(
    "<strong>%s</strong><br/>PM2.5 Médio (µg/m³): %s<br/>Categoria: %s",
    sf_obj$nome_muni,
    ifelse(
      is.na(sf_obj$pm25),
      "NA",
      scales::number(sf_obj$pm25, accuracy = 0.1, decimal.mark = ",")
    ),
    sf_obj$pm25_cat
  ) %>%
    lapply(htmltools::HTML)
  
  leaflet(sf_obj, options = leafletOptions(zoomControl = TRUE)) %>%
    addProviderTiles("CartoDB.Voyager") %>%
    setView(lng = -64, lat = -4, zoom = 4.8) %>%
    addPolygons(
      fillColor = ~pal_leaf(pm25_cat),
      weight = 0.8,
      opacity = 1,
      color   = "black",
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 1.6,
        color = "#222222",
        fillOpacity = 1,
        bringToFront = TRUE
      ),
      label = lab_html,
      labelOptions = labelOptions(
        style = list("font-size" = "12px"),
        direction = "auto"
      )
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal_leaf,
      values  = ~pm25_cat,
      title = "PM2.5 (µg/m³)",
      opacity = 1
    )
}


# Construindo todos os gráficos que vamos apresentar no dashboard

# Série temporal mensal média
serie_bienio <- mensal_ambos_corte %>%
  filter(ano %in% c(2022, 2023)) %>%
  group_by(fonte, mes) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    mes_lab = factor(
      mes,
      levels = 1:12,
      labels = mes_labels
    )
  )

# Base para o boxplot
muni_mes_bienio_long <- mensal_ambos_corte %>%
  filter(ano %in% c(2022, 2023)) %>%
  group_by(nome_muni, mes, fonte) %>%
  summarise(
    Media = mean(pm25, na.rm = TRUE),
    SD = sd(pm25, na.rm = TRUE),
    .groups = "drop"
  )

# Boxplot: preparar fator de mês e limites de eixo
bienio_box <- muni_mes_bienio_long %>%
  mutate(
    mes_fac = factor(
      mes,
      levels = 1:12,
      labels = mes_labels
    )
  )

lim_pm_bienio <- range(bienio_box$Media, na.rm = TRUE)

# Bases "bianuais" por município (médias de 2022–2023) pra colocar na comparação 2 do dashboard
cams_muni_bienio <- cams_municipal_mensal_am %>%
  filter(ano %in% c(2022, 2023)) %>%
  group_by(cod_mun7, nome_muni) %>%
  summarise(pm25_cams = mean(pm25, na.rm = TRUE), .groups = "drop")

don_muni_bienio <- don_municipal_mensal_am %>%
  filter(ano %in% c(2022, 2023)) %>%
  group_by(cod_mun7, nome_muni) %>%
  summarise(pm25_don = mean(pm25, na.rm = TRUE), .groups = "drop")

wide_bienio <- cams_muni_bienio %>%
  inner_join(don_muni_bienio, by = c("cod_mun7", "nome_muni")) %>%
  mutate(
    diferenca = pm25_don - pm25_cams,
    absdiferenca = abs(diferenca),
    media_par = (pm25_cams + pm25_don) / 2
  )

# Pares de CAMS vs Donkelar por município e mês, pra conseguir a comparação 1
cams_m_trim <- mensal_ambos_corte %>%
  filter(fonte == "CAMS", ano %in% c(2022, 2023)) %>%
  select(cod_mun7, nome_muni, ano, mes, pm25_cams = pm25)

don_m_trim <- mensal_ambos_corte %>%
  filter(fonte == "Donkelar", ano %in% c(2022, 2023)) %>%
  select(cod_mun7, nome_muni, ano, mes, pm25_don = pm25)

pares_mensais <- inner_join(
  cams_m_trim, don_m_trim,
  by = c("cod_mun7", "ano", "mes", "nome_muni")
) %>%
  mutate(
    diferenca = pm25_don - pm25_cams, # Donkelar − CAMS
    media_par = (pm25_cams + pm25_don) / 2
  )

n_pairs <- nrow(pares_mensais)


# Análises estatísticas (correlação e ICC, para ambas comparações)

# Comparação 1: pares município×mês
teste_spearman <- suppressWarnings(cor.test(
  pares_mensais$pm25_don,
  pares_mensais$pm25_cams,
  method = "spearman"
))

icc_mensal <- irr::icc(
  pares_mensais[, c("pm25_don", "pm25_cams")],
  model = "twoway",
  type  = "agreement",
  unit  = "single"
)

rho_spear  <- unname(teste_spearman$estimate)
p_spear    <- teste_spearman$p.value

# Comparação 2: médias bianuais por município
teste_spearman2 <- cor.test(
  wide_bienio$pm25_don,
  wide_bienio$pm25_cams,
  method = "spearman"
)

icc_bienio <- irr::icc(
  wide_bienio[, c("pm25_don", "pm25_cams")],
  model = "twoway",
  type  = "agreement",
  unit  = "single"
)

rho_spear2 <- unname(teste_spearman2$estimate)
p_spear2   <- teste_spearman2$p.value

classifica_icc <- function(v){
  if (is.na(v)) return("")
  if (v < 0.50) "baixa"
  else if (v < 0.75) "moderada"
  else if (v < 0.90) "boa"
  else "excelente"
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "PM2.5 no Amazonas — CAMS x Donkelar (2022–2023)"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Análise Mensal", icon = icon("bar-chart"),
               menuSubItem("Gráfico de Barras", tabName = "subitem1"),
               menuSubItem("Série",              tabName = "subitem2"),
               menuSubItem("Boxplot",            tabName = "subitem3")),
      
      menuItem("Análise de Concordância", icon = icon("bar-chart"),
               menuSubItem("Comparação 1", tabName = "subitem4"),
               menuSubItem("Comparação 2", tabName = "subitem5")),
      
      menuItem("Mapas", icon = icon("map"),
               menuSubItem("Comparação de Mapas", tabName = "subitem6"))
    )
  ),
  
  dashboardBody(
    fresh::use_theme(tema_amazonia),
    tags$head(
      tags$style(HTML("
        .box-header { 
          padding-bottom: 5px;
        }
        .box-body {
          padding-top: 5px;
        }
      "))
    ),
    tabItems(
      
      tabItem("subitem1",
              fluidRow(
                box(
                  title = "PM2.5 médio por mês — CAMS x Donkelar (2022–2023)",
                  width = 12,
                  plotlyOutput("barras", height = "600px")
                )
              )
      ),
      
      tabItem("subitem2",
              fluidRow(
                box(
                  title = "Série Temporal Mensal — CAMS x Donkelar (2022–2023)",
                  width = 12,
                  plotlyOutput("serie", height = "600px")
                )
              )
      ),
      
      tabItem("subitem3",
              fluidRow(
                box(
                  title = "Boxplot Sazonal — CAMS x Donkelar (2022–2023)",
                  width = 12,
                  plotlyOutput("boxplot", height = "600px")
                )
              )
      ),
      
      tabItem("subitem4",
              fluidRow(
                box(
                  title = "Dispersão Mensal — CAMS x Donkelar (2022–2023)",
                  width = 6, plotlyOutput("dispersao1")
                ),
                box(
                  title = "Bland–Altman Mensal — CAMS x Donkelar (2022–2023)",
                  width = 6, plotlyOutput("blandaltman1")
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  title = "Descrição",
                  collapsible = TRUE,
                  
                  HTML("
                  <p>
                  O gráfico à esquerda mostra a dispersão das estimativas mensais do PM2,5 por município, em que pontos próximos da linha tracejada indicam maior concordância entre as fontes;<br>
                  O gráfico à direita apresenta o diagrama de Bland–Altman, em que as diferenças próximas de zero e dentro dos limites de concordância indicam boa concordância entre as fontes.
                  </p>
                  "),
                  
                  tags$hr(),
                  
                  tags$h4("Correlação de Spearman"),
                  div(
                    style = "
                      border-left: 4px solid #10B981;
                      background-color: #ECFDF5;
                      padding: 10px 15px;
                      margin-bottom: 10px;
                    ",
                    htmlOutput("resumo_teste_spearman")
                  ),
                  
                  tags$h4("Coeficiente de Correlação Intraclasse (ICC)"),
                  div(
                    style = "
                      border-left: 4px solid #10B981;
                      background-color: #ECFDF5;
                      padding: 10px 15px;
                    ",
                    htmlOutput("resumo_icc1")
                  )
                )
              )
      ),
      
      tabItem("subitem5",
              fluidRow(
                box(
                  title = "Dispersão das Médias Municipais — CAMS x Donkelar (2022–2023)",
                  width = 6, plotlyOutput("dispersao2")
                ),
                box(
                  title = "Bland–Altman das Médias Municipais — CAMS x Donkelar (2022–2023)",
                  width = 6, plotlyOutput("blandaltman2")
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  title = "Descrição",
                  collapsible = TRUE,
                  
                  HTML("
                  <p>
                  O gráfico à esquerda mostra a dispersão das estimativas médias do PM2,5 por município, em que pontos próximos da linha tracejada indicam maior concordância entre as fontes;<br>
                  O gráfico à direita apresenta o diagrama de Bland–Altman, em que as diferenças próximas de zero e dentro dos limites de concordância indicam boa concordância entre as fontes.
                  </p>
                  "),
                  
                  tags$hr(),
                  
                  tags$h4("Correlação de Spearman"),
                  div(
                    style = "
                      border-left: 4px solid #10B981;
                      background-color: #ECFDF5;
                      padding: 10px 15px;
                      margin-bottom: 10px;
                    ",
                    htmlOutput("resumo_teste_spearman2")
                  ),
                  
                  tags$h4("Coeficiente de Correlação Intraclasse (ICC)"),
                  div(
                    style = "
                      border-left: 4px solid #10B981;
                      background-color: #ECFDF5;
                      padding: 10px 15px;
                    ",
                    htmlOutput("resumo_icc2")
                  )
                )
              )
      ),
      
      tabItem("subitem6",
              fluidRow(
                box(
                  title = "Mapa CAMS (2022–2023)",
                  width = 6,
                  leafletOutput("mapaCAMS", height = "450px")   
                ),
                box(
                  title = "Mapa Donkelar (2022–2023)",
                  width = 6,
                  leafletOutput("mapaVON", height = "450px")
                )
              )
      )
    )
  )
)

# SERVER

server <- function(input, output, session) {
  
  # Tab 1: Barras (média bianual por mês e fonte)
  output$barras <- renderPlotly({
    col_cams_bar <- pal_verde(7)[1]
    col_don_bar  <- pal_verde(7)[4]
    
    plot_ly() %>%
      add_bars(
        data = serie_bienio %>% filter(fonte == "CAMS"),
        x = ~mes_lab, y = ~pm25,
        name = "CAMS",
        marker = list(color = col_cams_bar),
        hovertemplate = paste(
          "Fonte: CAMS<br>",
          "Mês: %{x}<br>",
          "PM2.5 médio: %{y:.2f} µg/m³",
          "<extra></extra>"
        )
      ) %>%
      add_bars(
        data = serie_bienio %>% filter(fonte == "Donkelar"),
        x = ~mes_lab, y = ~pm25,
        name = "Donkelar",
        marker = list(color = col_don_bar),
        hovertemplate = paste(
          "Fonte: Donkelar<br>",
          "Mês: %{x}<br>",
          "PM2.5 médio: %{y:.2f} µg/m³",
          "<extra></extra>"
        )
      ) %>%
      layout(
        barmode = "group",
        xaxis  = list(title = "Mês"),
        yaxis  = list(title = "PM2.5 médio (µg/m³)"),
        legend = list(
          title = list(text = "Fonte"),
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.2
        ),
        margin = list(b = 80)
      )
  })
  
  # Tab 2: Série temporal
  output$serie <- renderPlotly({
    col_cams_line <- pal_verde(7)[1]
    col_don_line  <- pal_verde(7)[4]
    
    plot_ly() %>%
      add_lines(data = serie_bienio %>% filter(fonte == "CAMS"),
                x = ~mes_lab, y = ~pm25, name = "CAMS",
                line = list(width = 4, color = col_cams_line)) %>%
      add_markers(
        data = serie_bienio %>% filter(fonte == "CAMS"),
        x = ~mes_lab, y = ~pm25, name = "CAMS",
        marker = list(size = 9, color = col_cams_line,
                      line = list(width = 1.2, color = "black")), showlegend = FALSE
      ) %>%
      add_lines(data = serie_bienio %>% filter(fonte == "Donkelar"),
                x = ~mes_lab, y = ~pm25, name = "Donkelar",
                line = list(width = 4, color = col_don_line)) %>%
      add_markers(
        data = serie_bienio %>% filter(fonte == "Donkelar"),
        x = ~mes_lab, y = ~pm25, name = "Donkelar",
        marker = list(size = 9, color = col_don_line,
                      line = list(width = 1.2, color = "black")), showlegend = FALSE
      ) %>%
      layout(
        xaxis  = list(title = "Mês"),
        yaxis  = list(title = "PM2.5 médio (µg/m³)"),
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.25
        ),
        margin = list(b = 80)
      )
  })
  
  # Tab 3: Boxplot sazonal
  output$boxplot <- renderPlotly({
    col_cams_fill <- hex2rgba(pal_verde(7)[1], 1)
    col_don_fill  <- hex2rgba(pal_verde(7)[4], 1)
    
    plot_ly() %>%
      add_trace(
        data = bienio_box %>% filter(fonte == "CAMS"),
        x = ~mes_fac, y = ~Media,
        type = "box", name = "CAMS",
        boxpoints = FALSE,
        fillcolor = col_cams_fill,
        line = list(color = "black", width = 1)
      ) %>%
      add_trace(
        data = bienio_box %>% filter(fonte == "Donkelar"),
        x = ~mes_fac, y = ~Media,
        type = "box", name = "Donkelar",
        boxpoints = FALSE,
        fillcolor = col_don_fill,
        line = list(color = "black", width = 1)
      ) %>%
      layout(
        boxmode = "group",
        yaxis  = list(title = "PM2.5 (µg/m³)", range = lim_pm_bienio),
        xaxis  = list(title = "Mês"),
        legend = list(
          title       = list(text = "Fonte"),
          orientation = "h",
          x           = 0.5,
          xanchor     = "center",
          y           = -0.2
        ),
        margin = list(b = 80)
      )
  })
  
  # Tab 4: Comparação 1 - dispersão (pares município×mês)
  output$dispersao1 <- renderPlotly({
    g <- ggplot(pares_mensais, aes(x = pm25_don, y = pm25_cams)) +
      geom_point(alpha = 0.6, size = 1.7, color = pal_verde(7)[2]) +
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed", color = "grey40") +
      labs(
        x = "Donkelar — PM2.5 (µg/m³)",
        y = "CAMS — PM2.5 (µg/m³)"
      ) +
      theme_minimal(base_size = 12)
    
    ggplotly(g)
  })
  
  # Tab 4: Comparação 1 - Bland–Altman (pares município×mês)
  output$blandaltman1 <- renderPlotly({
    bias         <- mean(pares_mensais$diferenca, na.rm = TRUE)
    sd_diferenca <- sd(pares_mensais$diferenca, na.rm = TRUE)
    loa_u        <- bias + 1.96 * sd_diferenca
    loa_l        <- bias - 1.96 * sd_diferenca
    
    g <- ggplot(pares_mensais, aes(x = media_par, y = diferenca)) +
      geom_point(alpha = 0.6, size = 1.5, color = pal_verde(7)[3]) +
      geom_hline(yintercept = bias, color = "black") +
      geom_hline(yintercept = loa_u, linetype = "dashed", color = "grey40") +
      geom_hline(yintercept = loa_l, linetype = "dashed", color = "grey40") +
      labs(
        x = "Média do par (µg/m³)",
        y = "Donkelar − CAMS (µg/m³)"
      ) +
      theme_minimal(base_size = 12)
    
    ggplotly(g)
  })
  
  # Tab 5: Comparação 2 - dispersão (médias bianuais por município)
  output$dispersao2 <- renderPlotly({
    g <- ggplot(wide_bienio, aes(x = pm25_don, y = pm25_cams)) +
      geom_point(alpha = 0.6, size = 1.7, color = pal_verde(7)[2]) +
      geom_abline(slope = 1, intercept = 0,
                  linetype = "dashed", color = "grey40") +
      labs(
        x = "Donkelar — PM2.5 médio (µg/m³)",
        y = "CAMS — PM2.5 médio (µg/m³)"
      ) +
      theme_minimal(base_size = 12)
    
    ggplotly(g)
  })
  
  # Tab 5: Comparação 2 - Bland–Altman (médias bianuais por município)
  output$blandaltman2 <- renderPlotly({
    bias2 <- mean(wide_bienio$diferenca, na.rm = TRUE)
    sd_diferenca2 <- sd(wide_bienio$diferenca, na.rm = TRUE)
    loa2_u <- bias2 + 1.96 * sd_diferenca2
    loa2_l <- bias2 - 1.96 * sd_diferenca2
    
    g <- ggplot(wide_bienio, aes(x = media_par, y = diferenca)) +
      geom_point(alpha = 0.6, size = 1.5, color = pal_verde(7)[3]) +
      geom_hline(yintercept = bias2, color = "black") +
      geom_hline(yintercept = loa2_u, linetype = "dashed", color = "grey40") +
      geom_hline(yintercept = loa2_l, linetype = "dashed", color = "grey40") +
      labs(
        x = "Média do par (µg/m³)",
        y = "Donkelar − CAMS (µg/m³)"
      ) +
      theme_minimal(base_size = 12)
    
    ggplotly(g)
  })
  
  # Saída "console" do teste de Spearman (se for inspecionar via output$)
  output$print_teste_spearman <- renderPrint({
    teste_spearman
  })
  
  output$print_teste_spearman2 <- renderPrint({
    teste_spearman2
  })
  
  # Texto interpretativo da correlação de Spearman (Comparação 1)
  output$resumo_teste_spearman <- renderText({
    r  <- round(rho_spear, 3)
    p  <- p_spear
    
    mag <- if (abs(r) < 0.3) {
      "fraca"
    } else if (abs(r) < 0.6) {
      "moderada"
    } else if (abs(r) < 0.8) {
      "moderada/forte"
    } else {
      "forte"
    }
    
    signif_txt <- if (p < 0.05) {
      "significativa (p < 0,05)"
    } else {
      "não significativa (p ≥ 0,05)"
    }
    
    paste0(
      "Correlação de Spearman: ρ = ", r,
      " — magnitude ", mag, ", ", signif_txt, "."
    )
  })
  
  # Texto interpretativo da correlação de Spearman (Comparação 2)
  output$resumo_teste_spearman2 <- renderText({
    r <- round(rho_spear2, 3)
    p <- p_spear2
    
    mag <- if (abs(r) < 0.3) {
      "fraca"
    } else if (abs(r) < 0.6) {
      "moderada"
    } else if (abs(r) < 0.8) {
      "moderada/forte"
    } else {
      "forte"
    }
    
    signif_txt <- if (p < 0.05) {
      "significativa (p < 0,05)"
    } else {
      "não significativa (p ≥ 0,05)"
    }
    
    paste0(
      "Correlação de Spearman: ρ = ", r,
      " — magnitude ", mag, ", ", signif_txt, "."
    )
  })
  
  # ICC Comparação 1 (pares município×mês)
  output$print_icc1 <- renderPrint({
    icc_mensal
  })
  
  output$resumo_icc1 <- renderText({
    v  <- round(icc_mensal$value, 3)
    li <- round(icc_mensal$lbound, 3)
    ls <- round(icc_mensal$ubound, 3)
    p  <- icc_mensal$p.value
    
    mag <- classifica_icc(v)
    signif_txt <- if (p < 0.05) "significativa (p < 0,05)" else "não significativa (p ≥ 0,05)"
    
    paste0(
      "ICC (modelo two-way, agreement, unidade single): ", v,
      ". IC 95% = [ ", li, " ; ", ls, " ]. — Concordância ", mag,
      ", ", signif_txt, "."
    )
  })
  
  # ICC Comparação 2 (médias bianuais por município)
  output$print_icc2 <- renderPrint({
    icc_bienio
  })
  
  output$resumo_icc2 <- renderText({
    v  <- round(icc_bienio$value, 3)
    li <- round(icc_bienio$lbound, 3)
    ls <- round(icc_bienio$ubound, 3)
    p  <- icc_bienio$p.value
    
    mag <- classifica_icc(v)
    signif_txt <- if (p < 0.05) "significativa (p < 0,05)" else "não significativa (p ≥ 0,05)"
    
    paste0(
      "ICC (modelo two-way, agreement, unidade single): ", v,
      ". IC 95% = [ ", li, " ; ", ls, " ]. — Concordância ", mag,
      ", ", signif_txt, "."
    )
  })
  
  # Tab 6: Mapas categorizados (CAMS x Donkelar, média 2022–2023)
  output$mapaCAMS <- leaflet::renderLeaflet({
    fazer_leaflet_mapa(mapa_cams_leaf)
  })
  
  output$mapaVON <- leaflet::renderLeaflet({
    fazer_leaflet_mapa(mapa_don_leaf)
  })
  
}





shinyApp(ui = ui, server = server)
