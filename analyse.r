# Script original: gera estatísticas descritivas, testes de significância, correlações
# e muitos plots/tabelas (saídas em table/, plot/, demographic/).
#
# Observação: este script usa tikzDevice para gerar .tex de figuras; precisa LaTeX
# instalado para compilar .tex -> .pdf quando usar tikz(...). Também depende de
# pacotes: e1071, orddom, effsize, pracma, plotrix, scales, DescTools, etc.

library(tikzDevice)    # usa tikz() para gerar figuras em LaTeX
library(stringr)       # str_glue, strsplit helpers
library(DescTools)     # utilidades estatísticas (p.ex. para tabelas)
setOldClass("htest")   # registra 'htest' como classe antiga para usar em S4 slots
DESCRIPTIVE = "Descriptive"

# Definição S4 para armazenar estatísticas descritivas de um grupo
setClass(DESCRIPTIVE
  , slots = list
  (uid = "character"    # identificador do grupo (ex. "Control" / "Experiment")
    , num = "numeric"    # n (número de observações)
    , mea = "numeric"    # média
    , std = "numeric"    # desvio padrão
    , med = "numeric"    # mediana
    , mad = "numeric"    # median absolute deviation
    , min = "numeric"
    , max = "numeric"
    , ske = "numeric"    # skewness (assimetria)
    , kur = "numeric"    # kurtosis
    , swt = "htest"      # resultado de shapiro.test (objeto htest)
    , raw = "numeric"    # raw: valores limpos (sem NA) - usado em plots
    , sam = "numeric"    # sam: amostras originais alinhadas com fatores (pode incluir NAs)
  )
)

# descriptive_for_all: cria DESCRIPTIVE para todo o vetor (sem split por factor)
descriptive_for_all <- function(samples) {
  result = list()
  d <- samples[!is.na(samples)]
  s <- new(DESCRIPTIVE
           , uid = "0"
           , num = length(d)
           , mea = mean(d)
           , std = sd(d)
           , med = median(d)
           , mad = mad(d)
           , min = min(d)
           , max = max(d)
           , ske = e1071::skewness(d)       # e1071::skewness exige pacote e1071
           , kur = e1071::kurtosis(d)
           , swt = shapiro.test(d)          # Shapiro-Wilk para normalidade
           , raw = d
           , sam = samples
  )
  result <- append(result, s)
  return(result)
}  

# descriptive: calcula DESCRIPTIVE por níveis de 'factors' (levels(factors))
# - stopifnot garante mesmo comprimento de samples e factors
descriptive <- function(samples, factors) {
  stopifnot(length(samples) == length(factors))

  result = list()
  x <- samples[!is.na(samples)]
  f <- factors[!is.na(samples)]
  for (group in levels(f)) {
    d <- x[f == group]
    s <- new(DESCRIPTIVE
      , uid = group
      , num = length(d)
      , mea = mean(d)
      , std = sd(d)
      , med = median(d)
      , mad = mad(d)
      , min = min(d)
      , max = max(d)
      , ske = e1071::skewness(d)
      , kur = e1071::kurtosis(d)
      , swt = shapiro.test(d)
      , raw = d
      , sam = samples[f == group]
    )
    result <- append(result, s)
  }

  return(result)
}

# descriptive_slot_name: mapeia slot short-names para rótulos legíveis
descriptive_slot_name <- function(object, field) {
  stopifnot(class(object) == DESCRIPTIVE)
  stopifnot(any(slotNames(DESCRIPTIVE) == field))

  if (field == "num") { return("Number of observations") }
  if (field == "mea") { return("Mean") }
  if (field == "std") { return("Standard deviation") }
  if (field == "med") { return("Median") }
  if (field == "mad") { return("Median absolute deviation") }
  if (field == "min") { return("Minimum") }
  if (field == "max") { return("Maximum") }
  if (field == "ske") { return("Skew") }
  if (field == "kur") { return("Kurtosis") }
  if (field == "swt") { return("Shapiro-Wilk Test") }

  return("")
}

# descriptive_table: imprime uma tabela em formato org-mode (ou outro stream) com entradas
# - usa pracma::fprintf para formatar linhas (precisa do pacote pracma)
descriptive_table <- function(data, stream = "", dec = 4) {
  fprintf <- pracma::fprintf

  fprintf("|-\n", file = stream)
  for (field in slotNames(DESCRIPTIVE)) {
    if (field == "raw" || field == "sam") {
      next
    }

    name <- descriptive_slot_name(data[[1]], field)
    if (field == "swt") {
      name <- paste(name, "p-value", sep = " ")
    }

    fprintf("| %s ", name, file = stream, append = TRUE)
    for (i in 1:length(data)) {
      if (class(data[[i]]) != DESCRIPTIVE) {
        next
      }
      fprintf(" | ", file = stream, append = TRUE)

      val <- slot(data[[i]], field)
      if (field == "swt") {
        val <- val$p.value           # extrai p-value do resultado de Shapiro
      }

      fmt <- "%s"
      if (typeof(val) == "double") {
        if (field == "ske" || field == "kur") {
          dec <- 4
        }
        fmt <- paste("%.", as.character(dec), "f", sep = "")
      }
      fprintf(fmt, val, file = stream, append = TRUE);
    }
    fprintf(" |\n", file = stream, append = TRUE)

    if (field == "uid") {
      fprintf("|-\n", file = stream, append = TRUE)
    }
  }
  fprintf("|-\n", file = stream, append = TRUE)
}

# Funções para abrir/fechar device dos plots - suporta .pdf e .tex (tikz)
descriptive_plot_open <- function(stream, w, h) {
  if (stream != "") {
    if (grepl("\\.pdf$", stream)) {
      pdf(stream, width = w, height = h, paper = 'special')
    } else if (grepl("\\.tex$", stream)) {
      tikz(stream, width = w, height = h)
    } else {
      print(stream)
      print(w)
      print(h)
      stop("unsupported file extension to produce plot");
    }
  }
}

descriptive_plot_close <- function(stream) {
  if (stream != "") {
    dev.off()
  }
}

# descriptive_plot: dispatcher que escolhe tipo de plot por 'model' (kd/hg/bp/qq)
descriptive_plot <- function(model, object, colours, title, xmin = 0, xmax = 1, stream = "", w = 5.50, h = 2, xdelta = 10) {
  if (typeof(stream) == "list") {
    for (s in stream) {
      descriptive_plot(model, object, colours, title, xmin, xmax, s, w, h)
    }
  } else {
    if (grepl("\\.tex\\+pdf$", stream)) {
      path <- strsplit(stream, "\\.tex\\+pdf")
      s <- list(paste(path, ".tex", sep = ""), paste(path, ".pdf", sep = ""))
      descriptive_plot(model, object, colours, title, xmin, xmax, s, w, h)
    } else {
      if (model == "kd") {
        descriptive_plot_kernelden(object, colours, title, xmin, xmax, stream, w, h)
      } else if (model == "hg") {
        descriptive_plot_histogram(object, colours, title, xmin, xmax, stream, w, h, xdelta)
      } else if (model == "bp") {
        descriptive_plot_qqboxplot(object, colours, title, xmin, xmax, stream, w, h)
      } else if (model == "qq") {
        descriptive_plot_qqnormres(object, colours, title, xmin, xmax, stream, w, h)
      } else {
        stop(paste("unsupported model '", model, "' to produce plot", sep = ""))
      }
    }
  }
}

# Kernel density plot: desenha densidade para cada grupo (object é lista de DESCRIPTIVE)
descriptive_plot_kernelden <- function(object, colours, title, xmin, xmax, stream, w, h, cex = 0.85) {
  lbls <- list()
  data <- list()
  ymax <- -Inf
  ymin <- Inf

  for (i in 1:length(object)) {
    if (class(object[[i]]) != DESCRIPTIVE) {
      next
    }

    x <- object[[i]]@raw
    d <- density(x)
    ymax <- max(ymax, max(d$y))
    ymin <- min(ymin, min(d$y))
    data[[object[[i]]@uid]] <- d
    lbls <- append(lbls, object[[i]]@uid)
  }

  stopifnot(length(colours) == length(data))

  descriptive_plot_open(stream, w, h)
  tmp <- par(las = 1, bty = "l"
             # margins bottom, left, top and right
    #, mar = c(1.75, 2.5, 1, 0)
    #, mgp = c(-1, 0.75, 0)
    , mar = c(1.75, 3, 1, 0)
    , mgp = c(-1, 0.75, 0)
  )

  for (i in 1:length(data)) {
    d <- data[[i]]
    if (i == 1) {
      plot(d
        , col = colours[i]
        , main = ""
        , xlab = ""
        , ylim = c(floor(ymin), ymax)
        , xlim = c(floor(xmin), xmax)
        , cex.axis = cex
        , cex.lab = cex
      )
    } else {
      lines(d, col = colours[i])
    }
    fillcolour <- scales::alpha(colours[i], 0.25)
    polygon(d, col = fillcolour, border = colours[i], lwd = 2)
  }
  grid()
  descriptive_plot_legend(title, lbls, colours)

  par(tmp)
  descriptive_plot_close(stream)
}

# Histogram multi (usa plotrix::multhist)
descriptive_plot_histogram <- function(object, colours, title, xmin, xmax, stream, w, h, xdelta, cex = 0.85) {
  lbls <- list()
  data <- list()
  for (i in 1:length(object)) {
    if (class(object[[i]]) != DESCRIPTIVE) {
      next
    }

    data[[object[[i]]@uid]] <- object[[i]]@raw
    lbls <- append(lbls, object[[i]]@uid)
  }

  stopifnot(length(colours) == length(data))

  descriptive_plot_open(stream, w, h)

  tmp <- par(las = 1
             # margins bottom, left, top and right
    , mar = c(1.75, 2.5, 1, 0)
    , mgp = c(-1, 0.75, 0)
  )

  plotrix::multhist(data
    , col = colours
    , xlab = ""
    , ylab = "Frequency"
    , cex.axis = cex
    , cex.lab = cex
    , xlim = c(xmin, xmax)
    , breaks = xdelta
  )

  grid()
  descriptive_plot_legend(title, lbls, colours)

  par(tmp)
  descriptive_plot_close(stream)
}

# Boxplot horizontal (bp) para comparar grupos
descriptive_plot_qqboxplot <- function(object, colours, title, xmin, xmax, stream, w, h, cex = 0.85) {
  lbls <- list()
  data <- list()

  for (i in 1:length(object)) {
    if (class(object[[i]]) != DESCRIPTIVE) {
      next
    }

    x <- object[[i]]@raw
    data[[object[[i]]@uid]] <- x
    lbls <- append(lbls, object[[i]]@uid)
  }

  stopifnot(length(colours) == length(data))

  descriptive_plot_open(stream, w, h)
  tmp <- par(las = 1, bty = "l"
             # margins bottom, left, top and right
    , mar = c(1.75, 2.5, 1, 0)
    , mgp = c(-1, 0.75, 0)
  )

  boxplot(rev(data)
    , horizontal = T
    , col = rev(colours)
    , xlab = ""
    , ylab = "Groups"
    , ylim = c(xmin, xmax)
    , yaxt = 'n'
    , cex.axis = cex
    , cex.lab = cex
  )
  axis(2, labels = F, at = 1:length(data))
  grid()
  descriptive_plot_legend(title, lbls, colours)

  par(tmp)
  descriptive_plot_close(stream)
}

# QQ-norm residuals style plot (múltiplas janelas)
descriptive_plot_qqnormres <- function(object, colours, title, xmin, xmax, stream, w, h) {
  lbls <- list()
  data <- list()

  for (i in 1:length(object)) {
    if (class(object[[i]]) != DESCRIPTIVE) {
      next
    }

    x <- object[[i]]@raw
    data[[object[[i]]@uid]] <- x
    lbls <- append(lbls, object[[i]]@uid)
  }

  stopifnot(length(colours) == length(data))

  descriptive_plot_open(stream, w, h)
  tmp <- par(las = 1, bty = "l", mfrow = c(1, length(data))
             # margins bottom, left, top and right
    , mar = c(2, 1, 1, 0)
    , mgp = c(3, 0.75, 0)
    , oma = c(3, 5, 0, 0)
  )

  y <- qnorm(c(0.25, 0.75))
  for (i in 1:length(data)) {
    v <- data[[i]]
    x <- quantile(v, c(0.25, 0.75))
    k <- diff(y) / diff(x)
    d <- y[1L] - k * x[1L]

    if (i == 1) {
      qqnorm(v
        , col = colours[[length(data) - 1]]
        , main = ""
        , xlab = ""
        , ylab = ""
        , datax = TRUE
        , ylim = c(floor(xmin), xmax)
        , cex.axis = 1.25
        , cex.lab = 1.25
      )
      p <- par('usr')
      text((p[1] - p[2]) * 0.30
        , mean(p[3:4])
        , labels = "Theoretical Quantiles", xpd = NA, srt = 90, cex = 1.25)
      mtext("Sample Quantiles", side = 1, line = 1, cex = 0.85, outer = TRUE)
    } else {
      qqnorm(v
        , col = colours[[length(data) - 1]]
        , main = ""
        , ylab = ""
        , xlab = ""
        , datax = TRUE
        , ylim = c(floor(xmin), xmax)
        , yaxt = 'n'
        , cex.axis = 1.25
        , cex.lab = 1.25
      )
      axis(2, labels = F)
    }
    abline(d, k, col = colours[[length(data)]])

    grid()
    descriptive_plot_legend(lbls[[i]], "", rgb(0, 0, 0, 0), cex = 1.25, inset = 0.05)
  }

  par(tmp)
  descriptive_plot_close(stream)
}

# legenda reutilizável para plots de descriptivo
descriptive_plot_legend <- function(title, lbls, colours, cex = 0.85, inset = 0.0) {
  legend("topright"
    , title = title
    , legend = lbls
    , col = colours
    , border = colours
    , fill = colours
    , x.intersp = -1.5
    , lwd = 0
    , lty = 0
    , bg = rgb(0, 0, 0, 0)
    , box.lty = 0
    , horiz = F
    , cex = cex
    , inset = inset
  )
}

# Classe SIGNIFICANCE para armazenar resultados de diversas comparações
SIGNIFICANCE = "Significance"

setClass(SIGNIFICANCE
  , slots = list
  (raw = "list"     # lista dos DESCRIPTIVE originais (dois grupos)
    , wtt = "list"   # Welch t-tests (lista)
    , kst = "list"   # Kolmogorov-Smirnov tests (two-sample)
    , kwt = "list"   # Kruskal-Wallis
    , wct = "list"   # Wilcoxon two-sided
    , wcg = "list"   # Wilcoxon greater
    , wcl = "list"   # Wilcoxon less
    , ocd = "list"   # orddom::orddom (Cliff's delta-like ord dominance)
    , ecd = "list"   # (efsize?) reservado (não usado)
  )
)

# significance: recebe lista de DESCRIPTIVE (espera 2 grupos) e calcula vários testes
significance <- function(object) {
  stopifnot(typeof(object) == "list")

  data <- list()

  for (i in 1:length(object)) {
    if (class(object[[i]]) != DESCRIPTIVE) {
      next
    }

    x <- object[[i]]
    data <- append(data, x)
  }

  stopifnot(length(data) == 2)    # exige exatamente 2 grupos (control vs experiment)

  a = data[[1]]
  b = data[[2]]

  s <- new(SIGNIFICANCE
    , raw = data
    , wtt = list(t.test(a@raw, b@raw)
      #, t.test(a@raw, c@raw)
      #, t.test(b@raw, c@raw)
    )
    , kst = list(ks.test(a@raw, b@raw)
      #, ks.test(a@raw, c@raw)
      #, ks.test(b@raw, c@raw)
    )
    , kwt = list(kruskal.test(list(a@raw, b@raw))
      #, kruskal.test(list(a@raw, c@raw))
      #, kruskal.test(list(b@raw, c@raw))
    )
    , wct = list(wilcox.test(a@raw, b@raw, alternative = "two.sided")
      #, wilcox.test(a@raw, c@raw, alternative = "two.sided")
      #, wilcox.test(b@raw, c@raw, alternative = "two.sided")
    )
    , wcg = list(wilcox.test(a@raw, b@raw, alternative = "greater")
      #, wilcox.test(a@raw, c@raw, alternative = "greater")
      #, wilcox.test(b@raw, c@raw, alternative = "greater")
    )
    , wcl = list(wilcox.test(a@raw, b@raw, alternative = "less")
      #, wilcox.test(a@raw, c@raw, alternative = "less")
      #, wilcox.test(b@raw, c@raw, alternative = "less")
    )
    , ocd = list(orddom::orddom(a@raw, b@raw)
      #, orddom::orddom(a@raw, c@raw)
      #, orddom::orddom(b@raw, c@raw)
    )
           ## , ecd = list(effsize::cliff.delta(a@raw, b@raw)
           ##             , effsize::cliff.delta(a@raw, c@raw)
           ##             , effsize::cliff.delta(b@raw, c@raw)
           ##            )
  )

  return(s)
}

# significance_slot_name: mapeia slot short-names para rótulos
significance_slot_name <- function(object, field) {
  stopifnot(class(object) == SIGNIFICANCE)
  stopifnot(any(slotNames(SIGNIFICANCE) == field))

  if (field == "uid") { return("") }
  if (field == "wtt") { return("Welch t-test") }
  if (field == "kst") { return("Kolmogorov-Smirnov test") }
  if (field == "kwt") { return("Kruskal-Wallis rank sum test") }
  if (field == "wct") { return("Wilcoxon rank sum test") }
  if (field == "ocd") { return("Cliff's Delta") }
}

# significance_table: exporta resultados (apenas alguns 'kind's suportados)
# - kind = c("ocd") por padrão; realiza indexação por posição para extrair
#   valores de interesse; isso é frágil (índices magic numbers) e depende da
#   estrutura retornada por orddom::orddom (que é uma tabela).
significance_table <- function(object, kind = c("ocd"), stream = "", dec = 4) {
  stopifnot(class(object) == SIGNIFICANCE)
  stopifnot(length(object@raw) == 2)

  lbls <- list()
  lbls <- append(lbls, paste(object@raw[[1]]@uid, object@raw[[2]]@uid, sep = "/"))

  fprintf <- pracma::fprintf

  fprintf("|-\n", file = stream)
  fprintf("| ", file = stream, append = TRUE)
  for (i in 1:length(lbls)) {
    fprintf("| %s ", lbls[[i]], file = stream, append = TRUE)
  }
  fprintf("|\n", file = stream, append = TRUE)

  for (k in kind) {
    if (k == "ocd") {
      props <- c("d", "s", "v", "z", "cil", "cih", "p1", "p2", "p3", "p", "ap")

      value <- list()
      value[["d"]] <- 13
      value[["p1"]] <- 9
      value[["p2"]] <- 1 # dummy
      value[["p3"]] <- 10
      value[["p"]] <- 22
      value[["s"]] <- 17
      value[["v"]] <- 18
      value[["z"]] <- 20
      value[["cil"]] <- 15
      value[["cih"]] <- 16
      value[["ap"]] <- 3 # "type_title" will be overwritten by FDR calculation

      names <- list()
      names[["d"]] <- "Cliff's \\delta"
      names[["p1"]] <- "P(X>Y)" # " = p_1"
      names[["p2"]] <- "P(X=Y)" # " = p_2"
      names[["p3"]] <- "P(X<Y)" # " = p_3"
      names[["p"]] <- "p"
      names[["s"]] <- "s_{\\delta}"
      names[["v"]] <- "v_{\\delta}"
      names[["z"]] <- "z_{\\delta}"
      names[["cil"]] <- "CI (low)"
      names[["cih"]] <- "CI (high)"
      names[["ap"]] <- "p_{FDR}" # "FDR adjusted p"
    } else if (k == "kwt") {
      props <- c("cq", "p", "ap") #"d", "s", "v", "z", "cil", "cih", "p1", "p2", "p3", "p", "ap")

      value <- list()

      value[["p"]] <- 3
      value[["ap"]] <- 1 # dummy
      value[["cq"]] <- 1 # dummy

      names <- list()
      names[["cq"]] <- "Kruskal-Wallis \\chi^2"
      names[["p"]] <- "p"
      names[["ap"]] <- "FDR adjusted p"
    } else {
      stop("unsupported kind to dump table")
    }

    data <- slot(object, k);

    fprintf("|-\n", file = stream, append = TRUE)
    for (p in props) {
      fprintf("| %s", names[[p]], file = stream, append = TRUE)
      for (i in 1:length(lbls)) {
        fprintf(" | ", file = stream, append = TRUE)

        val <- as.double(data[[i]][[ value[[p]] ]])

        if (p == "p2") {
          val <- 1.0 -
            as.double(data[[i]][[ value[["p1"]] ]]) -
            as.double(data[[i]][[ value[["p3"]] ]])
        }
        ## if(p == "ap") {
        ##     pvalues <- c(as.double(data[[i]][[ value[["p"]] ]] ))
        ##     val <- p.adjust(pvalues, method = "BH", n = length(pvalues))
        ## }
        if (p == "cq" && k == "kwt") {
          val <- as.double(data[[i]][[ value[[p]] ]][[ 1 ]])
        }

        fmt <- "%s"
        if (typeof(val) == "double") {
          fmt <- paste("%.", as.character(dec), "f", sep = "")
        }
        fprintf(fmt, val, file = stream, append = TRUE);
      }
      fprintf(" |\n", file = stream, append = TRUE)
    }
  }

  fprintf("|-\n", file = stream, append = TRUE)
}


# significance_fdr: coleta p-values em 'kind' e aplica ajuste FDR (BH)
# Problema: a atribuição dos p-values ajustados de volta ao objeto é feita
# em variáveis locais (group) e não reatribui ao slot; portanto NÃO altera o objeto.
significance_fdr <- function(object, kind = c("wcl")) {
  pvalues <- c()

  for (s in object) {
    for (k in kind) {
      data <- slot(s, k);

      for (i in 1:length(data)) {
        group <- data[[ i ]]
        pvalue <- group[[ 3 ]]
        pvalues <- c(pvalues, pvalue)
      }
    }
  }

  # ajusta p-values
  fdr_adjusted_pvalues <- p.adjust(pvalues, method = "BH", n = length(pvalues))

  pos <- 1
  for (s in object) {
    for (k in kind) {
      data <- slot(s, k);

      for (i in 1:length(data)) {
        group <- data[[ i ]]
        group[[ 3 ]] <- fdr_adjusted_pvalues[[ pos ]]
        pos <- pos + 1
      }
    }
  }
  # Nota: aqui foi alterada a cópia local 'group' mas não escrito de volta em data[[i]]
  # e nem em slot(s, k) - portanto a alteração NÃO persiste. Para funcionar, é preciso
  # modificar data[[i]][[3]] <- new_pvalue e em seguida slot(s, k) <- data
}


# correlation: calcula Pearson, Spearman, Kendall entre pares de DESCRIPTIVE correspondentes
# - retorna lista: dataA, dataB, lbls, list(cor.test pearson), list(cor.test spearman), list(cor.test kendall)
correlation <- function(a, b) {
  lbls <- list()
  dataA <- list()
  dataB <- list()
  corp <- list()
  cors <- list()
  cork <- list()

  stopifnot(length(a) == length(b))

  for (i in 1:length(a)) {
    if (class(a[[i]]) != DESCRIPTIVE || class(b[[i]]) != DESCRIPTIVE) {
      next
    }

    x <- a[[i]]@sam
    dataA[[a[[i]]@uid]] <- x

    y <- b[[i]]@sam
    dataB[[b[[i]]@uid]] <- y

    lbl <- a[[i]]@uid
    stopifnot(lbl == b[[i]]@uid)
    lbls <- append(lbls, lbl)

    corp <- append(corp, cor.test(x, y, method = "pearson"))
    cors <- append(cors, cor.test(x, y, method = "spearman"))
    cork <- append(cork, cor.test(x, y, method = "kendall"))
  }

  stopifnot(length(dataA) == length(dataB))

  return(list(dataA, dataB, lbls, corp, cors, cork))
}

# correlation_plot: plota scatter + regressão linear por par do objeto retornado acima
correlation_plot <- function(object, colours, title, xlim, ylim, stream, w, h, cex = 1.25) {
  lbls <- object[[3]]
  dataA <- object[[1]]
  dataB <- object[[2]]

  stopifnot(length(colours) == length(dataA))
  stopifnot(length(colours) == length(dataB))

  descriptive_plot_open(stream, w, h)
  tmp <- par(las = 1, bty = "l", mfrow = c(1, length(dataA))
             # margins bottom, left, top and right
    , mar = c(2, 2, 1, 0.5)
    , mgp = c(3, 0.75, 0)
    , oma = c(3, 4, 0, 0)
  )

  for (i in 1:length(dataA)) {
    f <- dataA[[i]] ~ dataB[[i]]

    plot(f
      , col = colours[[length(colours) - 1]]
      , xlim = c(xlim[1], xlim[2])
      , ylim = c(ylim[1], ylim[2])
      , cex.axis = cex
      , cex.lab = cex
    )

    abline(lm(f)
      , col = colours[[length(colours)]]
    )

    grid()
    descriptive_plot_legend(lbls[[i]], "", rgb(0, 0, 0, 0), cex = 1.25, inset = 0.05)

    if (i == 1) {
      p <- par('usr')
      text((p[1] - p[2]) * 0.35
        , mean(p[3:4])
        , labels = "Correctness [1]", xpd = NA, srt = 90, cex = 1.25)
      mtext("Time [s]", side = 1, line = 1, cex = 0.85, outer = TRUE)
    }
  }

  par(tmp)
  descriptive_plot_close(stream)
}

# correlation_table: exporta tabela de correlações (para p, s, k)
correlation_table <- function(object, kind = c("p", "s", "k"), stream = "", dec = 4) {
  lbls <- object[[3]]

  fprintf <- pracma::fprintf
  fprintf("|-\n", file = stream)

  fprintf("| ", file = stream, append = TRUE)
  for (lbl in lbls) {
    fprintf("| %s ", lbl, file = stream, append = TRUE)
  }
  fprintf(" |\n", file = stream, append = TRUE)

  for (k in kind) {
    names <- list()
    props <- c("estimate", "p.value", "statistic")
    names[["p.value"]] <- "p"

    if (k == "p") {
      corr <- object[[4]]
      props <- c(props, "conf.int", "conf.int")
      names[["conf.int"]] <- "CI"
      names[["estimate"]] <- paste(strsplit(corr[["method"]], "'")[[1]][[1]], "'s \\rho", sep = "")
      names[["statistic"]] <- "t"
    } else if (k == "s") {
      corr <- object[[5]]
      names[["estimate"]] <- paste(strsplit(corr[["method"]], "'")[[1]][[1]], "'s \\rho", sep = "")
      names[["statistic"]] <- "S"
    } else if (k == "k") {
      corr <- object[[6]]
      names[["estimate"]] <- paste(strsplit(corr[["method"]], "'")[[1]][[1]], "'s \\tau", sep = "")
      names[["statistic"]] <- "z"
    } else {
      stop("invalid kind to print correlation table")
    }

    fprintf("|-\n", file = stream, append = TRUE)
    pos <- 2

    for (p in props) {
      name <- names[[p]]
      if (p == "conf.int") {
        if (pos == 1) {
          name <- paste(name, "(low)", sep = "")
        } else {
          name <- paste(name, "(high)", sep = "")
        }
      }

      fprintf("| %s ", name, file = stream, append = TRUE)

      for (i in 0:1) {
        r <- length(corr) / 2
        data <- corr[(1 + (i * r)):(r + (i * r))]

        val <- data[[p]]
        if (p == "conf.int") {
          val <- val[[pos]]
        }
        if (p == "estimate") {
          val <- val[[1]]
        }

        fmt <- "%s"
        if (typeof(val) == "double") {
          fmt <- paste("%.", as.character(dec), "f", sep = "")
        }
        fprintf(" | ", file = stream, append = TRUE)
        fprintf(fmt, val, file = stream, append = TRUE);
      }

      if (pos == 1) {
        pos <- 2
      } else {
        pos <- 1
      }
      fprintf(" |\n", file = stream, append = TRUE)
    }
  }
  fprintf("|-\n", file = stream, append = TRUE)
}

# cores predefinidas
colours <- c(rgb(0.8, 0.8, 0.8)
   , rgb(0.4, 0.4, 0.4)
)

# Funções de alto nível que geram tabelas / plots para demográfico, survey, tasks etc.
generate_descriptive_demographic_plot <- function(value, task_name) {
  print(str_glue("Generating demographic data for {task_name}..."))
  value_filtered = value[!is.na(value)]
  descriptive_value = descriptive(value_filtered, value_filtered)
  descriptive_table(descriptive_value, dec = 4, stream = str_glue("demographic/{task_name}_corr.org"))
  descriptive_plot("kd", descriptive_value, colours, str_glue("{task_name} [years]"), 0.9 * min(value_filtered), 1.1 * max(value_filtered), stream = str_glue("demographic/{task_name}_kd.tex+pdf"), w = 4.5, h = 1.5)
  descriptive_plot("hg", descriptive_value, colours, str_glue("{task_name} [years]"), 0, 45, stream = str_glue("demographic/{task_name}_hg.tex+pdf"), w = 5, h = 1.25, xdelta = 10)
}

# generate_descriptive_survey_plot: similar para variáveis de survey por grupo
generate_descriptive_survey_plot <- function(value, group, task_name, label) {
  print(str_glue("Generating plot for Task {task_name}..."))
  
  group_filtered <- group[!is.na(value)]
  value_filtered = value[!is.na(value)]
  
  descriptive_value = descriptive(value_filtered, group_filtered)
  descriptive_table(descriptive_value, dec = 4, stream = str_glue("table/{task_name}_corr.org"))
  descriptive_plot("kd", descriptive_value, colours, str_glue("{label} [a]"), 0.9 * min(value_filtered), 1.1 * max(value_filtered), stream = str_glue("plot/{task_name}_kd.tex+pdf"), w = 4.5, h = 1.5)
  descriptive_plot("hg", descriptive_value, colours, str_glue("{label} [a]"), 0, 45, stream = str_glue("plot/{task_name}_hg.tex+pdf"), w = 5, h = 1.25, xdelta = 10)
}

# generate_plots: função principal que gera tabelas/descritivas/significância/correlações
generate_plots <- function(correctness, timing, group, task_name) {
  
  print(str_glue("Generating plot for Task {task_name}..."))
  
  group_filtered <- group[!is.na(timing)]
  correctness_filtered = correctness[!is.na(timing)]
  timing_filtered = timing[!is.na(timing)]
  
  descriptive_correctness = descriptive(correctness_filtered, group_filtered)
  descriptive_table(descriptive_correctness, dec = 4, stream = str_glue("table/{task_name}_corr.org"))
  descriptive_plot("kd", descriptive_correctness, colours, "Correctness [1]", 0, 1.15, stream = str_glue("plot/{task_name}_corr_kd.tex+pdf"), w = 8, h = 1.25)
  descriptive_plot("bp", descriptive_correctness, colours, "Correctness [1]", 0, 1.3, stream = str_glue("plot/{task_name}_corr_bp.tex+pdf"), w = 8, h = 1.25)
  descriptive_plot("qq", descriptive_correctness, colours, "Correctness [1]", 0, 1, stream = str_glue("plot/{task_name}_corr_qq.tex+pdf"), w = 10, h = 4)
  descriptive_plot("hg", descriptive_correctness, colours, "Correctness [1]", 0, 60, stream = str_glue("plot/{task_name}_corr_hg.tex+pdf"), w = 5, h = 1.25, xdelta = 10)
  
  # timings can be NULL, so we must remove them from the list and also reduce the elements in the group vector
  descriptive_timing <- descriptive(timing_filtered, group_filtered)
  descriptive_table(descriptive_timing, dec = 2, stream = "table/es_time.org")
  descriptive_plot("kd", descriptive_timing, colours, "Time [m]", min(timing_filtered), 1.15*max(timing_filtered), stream = str_glue("plot/{task_name}_time_kd.tex+pdf"), w = 4.5, h = 1.25)
  descriptive_plot("bp", descriptive_timing, colours, "Time [m]", min(timing_filtered), 1.15*max(timing_filtered), stream = str_glue("plot/{task_name}_time_bp.tex+pdf"), w = 4.5, h = 1.25)
  
  significance_correctness <- significance(descriptive_correctness)
  significance_timing <- significance(descriptive_timing)
  sig_list = list(significance_correctness, significance_timing)
  significance_fdr(sig_list)   # tenta ajustar FDR (veja comentário de bug)
  
  significance_table(significance_correctness, dec = 4, stream = str_glue("table/{task_name}_corr_ht.org"))
  significance_table(significance_timing, dec = 4, stream = str_glue("table/{task_name}_time_ht.org"))
  C2TX1 <- correlation(descriptive_correctness, descriptive_timing)
  
  correlation_table(C2TX1)
  correlation_table(C2TX1, kind = c("s"), dec = 4, stream = str_glue("table/{task_name}_sc.org"))
  
  minTaskTiming = min(timing_filtered)
  maxTaskTiming = max(timing_filtered)
  
  correlation_plot(C2TX1, colours, "", c(minTaskTiming, maxTaskTiming), c(0, 1), str_glue("plot/{task_name}_sc.tex"), w = 10, h = 4)
  correlation_plot(C2TX1, colours, "", c(minTaskTiming, maxTaskTiming), c(0, 1), str_glue("plot/{task_name}_sc.pdf"), w = 10, h = 4)  
}

######################################################################
# Fluxo principal de execução (carrega RDS e chama funções)
getwd()
# data <- readRDS("Documents/api-ace/microservice_api_component_metrics_experiment/experiment_data/experiment-results.rds")
data <- readRDS('./experiment-results.rds')

attach(data)   # Prefeitura: attach pode causar conflitos; prefer data$col

summary(data)

if (!dir.exists("table")) { dir.create("table") }
if (!dir.exists("plot")) { dir.create("plot") }
if (!dir.exists("demographic")) { dir.create("demographic") }

# O script contém muitas partes comentadas (geração de plots por tarefa, demographics etc.)
# Aqui apenas gera as estatísticas agregadas principais:

print("Generating descriptive data")

ES_Correctness <- descriptive(Task.ES.fmeasure, Task.ES.Group)
descriptive_table(ES_Correctness, dec = 4, stream = "table/es.org")
generate_plots(Task.ES.fmeasure, Task.ES.Timing, Task.ES.Group, "es")

PM_Correctness <- descriptive(Task.PM.fmeasure, Task.PM.Group)
descriptive_table(PM_Correctness, dec = 4, stream = "table/pm.org")
generate_plots(Task.PM.fmeasure, Task.PM.Timing, Task.PM.Group, "pm")

ES_Timing <- descriptive(Task.ES.Timing, Task.ES.Group)
descriptive_table(ES_Timing, dec = 4, stream = "table/duration_statistics_es.org")

PM_Timing <- descriptive(Task.PM.Timing, Task.PM.Group)
descriptive_table(PM_Timing, dec = 4, stream = "table/duration_statistics_pm.org")

print("Descriptive data generated")
