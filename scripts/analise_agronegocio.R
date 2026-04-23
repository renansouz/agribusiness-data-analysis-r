setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacotes <- c("readxl", "ggplot2", "dplyr", "scales", "gridExtra")

pacotes_faltando <- pacotes[!sapply(pacotes, requireNamespace, quietly = TRUE)]

if (length(pacotes_faltando) > 0) {
  message("Instalando pacotes: ", paste(pacotes_faltando, collapse = ", "))
  install.packages(
    pacotes_faltando,
    repos = "https://cran.r-project.org",
    dependencies = TRUE
  )
}

suppressPackageStartupMessages({
  library(readxl)
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(gridExtra)
})

message("Pacotes carregados com sucesso!\n")

# 1. BASE DE DADOS
dados <- read_excel("../data/dados_agro.xlsx")

cat("=============================================================\n")
cat("  ANALISE EXPLORATORIA - AGRONEGOCIO BRASILEIRO\n")
cat("=============================================================\n\n")

# SECAO A - VARIAVEL QUANTITATIVA CONTINUA: Producao (toneladas)
x <- dados$producao_toneladas

cat("-------------------------------------------------------------\n")
cat("  VARIAVEL: Producao (toneladas)\n")
cat("-------------------------------------------------------------\n\n")

# A.1 MEDIDAS DE TENDENCIA CENTRAL
cat(">> MEDIDAS DE TENDENCIA CENTRAL\n")

media   <- mean(x)
mediana <- median(x)

freq_tab <- table(round(x, -3))
moda_val <- as.numeric(names(which.max(freq_tab)))

cat(sprintf(
  "   Media    : %s toneladas\n",
  format(round(media, 2), big.mark = ".", decimal.mark = ",")
))
cat(sprintf(
  "   Mediana  : %s toneladas\n",
  format(round(mediana, 2), big.mark = ".", decimal.mark = ",")
))
cat(sprintf(
  "   Moda     : aprox. %s toneladas (classe de 1.000 t)\n\n",
  format(moda_val, big.mark = ".", decimal.mark = ",")
))

# A.2 MEDIDAS DE DISPERSAO
cat(">> MEDIDAS DE DISPERSAO\n")

variancia <- var(x)
desv_pad  <- sd(x)
cv        <- (desv_pad / media) * 100
amplitude <- max(x) - min(x)
iqr_val   <- IQR(x)

cat(sprintf(
  "   Variancia           : %s\n",
  format(round(variancia, 2), big.mark = ".", decimal.mark = ",")
))
cat(sprintf(
  "   Desvio Padrao (s)   : %s t\n",
  format(round(desv_pad, 2), big.mark = ".", decimal.mark = ",")
))
cat(sprintf("   Coef. de Variacao   : %.2f%%\n", cv))
cat(sprintf(
  "   Amplitude Total     : %s t\n",
  format(round(amplitude, 2), big.mark = ".", decimal.mark = ",")
))
cat(sprintf(
  "   Amplitude IQ (AIQ)  : %s t\n\n",
  format(round(iqr_val, 2), big.mark = ".", decimal.mark = ",")
))

# A.3 MEDIDAS SEPARATRIZES
cat(">> MEDIDAS SEPARATRIZES\n")

q1    <- quantile(x, 0.25)
q2    <- quantile(x, 0.50)
q3    <- quantile(x, 0.75)
decis <- quantile(x, probs = seq(0.1, 0.9, 0.1))
p5    <- quantile(x, 0.05)
p95   <- quantile(x, 0.95)

cat("   -- Quartis --\n")
cat(sprintf("      Q1 (25): %s t\n", format(round(q1, 2), big.mark = ".", decimal.mark = ",")))
cat(sprintf("      Q2 (50): %s t\n", format(round(q2, 2), big.mark = ".", decimal.mark = ",")))
cat(sprintf("      Q3 (75): %s t\n", format(round(q3, 2), big.mark = ".", decimal.mark = ",")))

cat("\n   -- Decis --\n")
for (i in 1:9) {
  cat(sprintf(
    "      D%d (%d0): %s t\n",
    i, i,
    format(round(decis[i], 2), big.mark = ".", decimal.mark = ",")
  ))
}

cat("\n   -- Percentis extremos --\n")
cat(sprintf("      P5  : %s t\n", format(round(p5, 2), big.mark = ".", decimal.mark = ",")))
cat(sprintf("      P95 : %s t\n\n", format(round(p95, 2), big.mark = ".", decimal.mark = ",")))

# A.4 ANALISE GRAFICA - Variavel Quantitativa (Producao)
cat(">> ANALISE GRAFICA - gerando plots para 'producao_toneladas'...\n\n")

tema_agro <- theme_minimal(base_family = "sans") +
  theme(
    plot.title       = element_text(face = "bold", size = 13, color = "#1B5E20"),
    plot.subtitle    = element_text(size = 9, color = "#555555"),
    axis.title       = element_text(size = 10, color = "#1B5E20"),
    axis.text        = element_text(size = 9),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

p1 <- ggplot(dados, aes(x = producao_toneladas / 1e6)) +
  geom_histogram(bins = 8, fill = "#2E7D32", color = "white", alpha = 0.85) +
  geom_vline(
    xintercept = media / 1e6,
    color = "#E53935",
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = mediana / 1e6,
    color = "#1565C0",
    linewidth = 1,
    linetype = "dotdash"
  ) +
  annotate(
    "text",
    x = media / 1e6 + 0.25,
    y = Inf,
    vjust = 2,
    label = "Media",
    color = "#E53935",
    size = 3.2
  ) +
  annotate(
    "text",
    x = mediana / 1e6 - 0.25,
    y = Inf,
    vjust = 3.5,
    label = "Mediana",
    color = "#1565C0",
    size = 3.2
  ) +
  labs(
    title = "Histograma - Producao por Observacao",
    subtitle = "Linhas: Media (vermelha) e Mediana (azul)",
    x = "Producao (milhoes de toneladas)",
    y = "Frequencia"
  ) +
  tema_agro

p2 <- ggplot(dados, aes(x = "", y = producao_toneladas / 1e6)) +
  geom_boxplot(
    fill = "#A5D6A7",
    color = "#1B5E20",
    width = 0.5,
    outlier.color = "#E53935",
    outlier.size = 2.5
  ) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#E53935") +
  labs(
    title = "Boxplot - Producao (toneladas)",
    subtitle = "Losango vermelho = Media | Boxplot = Quartis",
    y = "Producao (milhoes de toneladas)",
    x = ""
  ) +
  tema_agro +
  theme(axis.text.x = element_blank())

p3 <- ggplot(dados, aes(x = producao_toneladas / 1e6)) +
  geom_density(fill = "#81C784", color = "#2E7D32", alpha = 0.6) +
  geom_rug(color = "#1B5E20", alpha = 0.5) +
  geom_vline(
    xintercept = q1 / 1e6,
    linetype = "dashed",
    color = "#6A1B9A",
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = q3 / 1e6,
    linetype = "dashed",
    color = "#6A1B9A",
    linewidth = 0.8
  ) +
  annotate(
    "text",
    x = q1 / 1e6,
    y = Inf,
    vjust = 2,
    hjust = 1.1,
    label = "Q1",
    color = "#6A1B9A",
    size = 3
  ) +
  annotate(
    "text",
    x = q3 / 1e6,
    y = Inf,
    vjust = 2,
    hjust = -0.1,
    label = "Q3",
    color = "#6A1B9A",
    size = 3
  ) +
  labs(
    title = "Curva de Densidade - Producao",
    subtitle = "Linhas tracejadas: Q1 e Q3",
    x = "Producao (milhoes de toneladas)",
    y = "Densidade"
  ) +
  tema_agro

p4 <- ggplot(dados, aes(sample = producao_toneladas)) +
  stat_qq(color = "#2E7D32", size = 2) +
  stat_qq_line(color = "#E53935", linewidth = 1) +
  labs(
    title = "QQ-Plot - Normalidade",
    subtitle = "Pontos proximos a linha = distribuicao aproximadamente normal",
    x = "Quantis Teoricos",
    y = "Quantis Amostrais"
  ) +
  tema_agro

png("graficos_quantitativo.png", width = 1400, height = 1100, res = 140)
grid.arrange(
  p1, p2, p3, p4,
  ncol = 2,
  top = grid::textGrob(
    "Analise GrafiSca - Producao (toneladas) | Agronegocio Brasileiro",
    gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#1B5E20")
  )
)
dev.off()

message("graficos_quantitativo.png salvo em: ", getwd())

# SECAO B - VARIAVEL QUALITATIVA ORDINAL: Nivel Tecnologico
cat(">> ANALISE GRAFICA - Variavel Qualitativa Ordinal: Nivel Tecnologico\n")

dados$nivel_tecnologico <- factor(
  dados$nivel_tecnologico,
  levels = c("Muito Baixo", "Baixo", "Médio", "Alto", "Muito Alto"),
  ordered = TRUE
)

freq_nivel <- dados %>%
  count(nivel_tecnologico) %>%
  mutate(
    pct   = n / sum(n) * 100,
    label = paste0(n, "\n(", round(pct, 1), "%)")
  )

cores_nivel <- c(
  "Muito Baixo" = "#B71C1C",
  "Baixo"       = "#EF6C00",
  "Médio"       = "#F9A825",
  "Alto"        = "#558B2F",
  "Muito Alto"  = "#1B5E20"
)

p5 <- ggplot(freq_nivel, aes(x = nivel_tecnologico, y = n, fill = nivel_tecnologico)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = label), vjust = -0.3, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = cores_nivel) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Frequencia por Nivel Tecnologico",
    subtitle = "Variavel Qualitativa Ordinal - Adocao de Tecnologia no Campo",
    x = "Nivel Tecnologico",
    y = "Numero de Observacoes"
  ) +
  tema_agro

p6 <- ggplot(freq_nivel, aes(x = 2, y = n, fill = nivel_tecnologico)) +
  geom_col(color = "white", linewidth = 0.7) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 3.5,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(values = cores_nivel, name = "Nivel Tecnologico") +
  xlim(0.5, 2.5) +
  labs(
    title = "Distribuicao Percentual - Nivel Tecnologico",
    subtitle = "Grafico de rosca (donut chart)"
  ) +
  tema_agro +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

png("graficos_qualitativo.png", width = 1400, height = 600, res = 140)
grid.arrange(
  p5, p6,
  ncol = 2,
  top = grid::textGrob(
    "Analise Grafica - Nivel Tecnologico | Agronegocio Brasileiro",
    gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#1B5E20")
  )
)
dev.off()

message("graficos_qualitativo.png salvo em: ", getwd())

cat("=============================================================\n")
cat("  Arquivos gerados na pasta do projeto:\n")
cat("    graficos_quantitativo.png  (4 paineis - Producao)\n")
cat("    graficos_qualitativo.png   (2 paineis - Nivel Tecnologico)\n")
cat("=============================================================\n")