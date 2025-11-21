# Este arquivo gera histogramas e gráficos de barras com informações demográficas
# a partir do RDS pré-processado (experiment-results.rds).

library(stringr)   # fornece str_glue() usado para construir nomes de ficheiros/strings

# Define duas cores utilizadas nos gráficos (tons de cinza)
light_gray <- rgb(0.8, 0.8, 0.8)
dark_gray  <- rgb(0.4, 0.4, 0.4)

# Vetor com as cores; usado nos plots se necessário
colours <- c(light_gray, dark_gray)

# Função plot_bar: desenha um gráfico de barras para uma variável categórica
plot_bar <- function(data, name) {
  print(data)                          # imprime o vetor bruto no console (pode ser muito verboso)
  data <- data[!is.na(data)]           # remove valores NA antes de plotar/contar
  pdf(str_glue("demographics/{name}.pdf"),width = 5, height = 3, paper = 'special')
  barplot(table(data))                 # plota um barplot com as contagens por categoria
  print(str_glue("BarPlot info for {name}:"))
  bar_table = table(data)              # tabela de frequências (usada para imprimir)
  print(bar_table)                     # imprime a tabela de frequências
  grid()                               # adiciona linhas de grade ao gráfico atual
  dev.off()                            # fecha o dispositivo gráfico (salva o PDF)
  print("Bar completed")               # mensagem de conclusão
}

# Função plot_histogram: desenha histograma para variável numérica e imprime estatísticas
plot_histogram <- function(data, name, file_name = name, breaks=15) {
  
  data <- data[!is.na(data)]           # remove NAs (hist() não lida bem com NAs)
  # data <- trim_q(data, 0.01, 0.999)  # trecho comentado para eventualmente remover outliers
  pdf(str_glue("demographics/{file_name}.pdf"),width = 5, height = 3, paper = 'special')
  # hist_info recebe o objecto retornado por hist() com contagens, breaks, mids, etc.
  hist_info = hist(data,
       main="",                         # título vazio (substitui por legendas externas)
       xlab=str_glue("{name} (years)"), # rótulo do eixo x dinâmico
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, # tamanhos de fonte
       col=c(light_gray),               # cor das barras
       # xlim=c(0,15),                  # exemplo comentado de limitação do eixo x
       breaks                           # número (ou definição) de bins passado por posição
  )
  grid()                               # adiciona linhas de grade
  dev.off()                            # fecha dispositivo gráfico -> grava PDF
  print(str_glue("Histogram info for {name}:"))
  # print(hist_info)                   # informação completa do hist está comentada (muito verbosa)
  median_data = median(data)           # calcula mediana do vetor (já sem NAs)
  print(str_glue('Median ({name}): {median_data}'))
  mean_data = mean(data)               # calcula média
  print(str_glue('Mean ({name}): {mean_data}'))
}

# imprime o diretório de trabalho atual (diagnóstico)
getwd()
# lê o RDS gerado pelo prepare.r (preprocessamento)
# data <- readRDS("Documents/.../experiment-results.rds")  # exemplo comentado
data <- readRDS('./experiment-results.rds')

attach(data)    # anexa data ao search path: permite referenciar colunas por nome direto (p.ex. Age)
summary(data)   # imprime sumário descritivo do data.frame (por colunas)

# garante existência da pasta de saída
if (!dir.exists("demographics")) { dir.create("demographics") }

# gera histogramas para várias variáveis demográficas (remover NAs é feito dentro das funções)
plot_histogram(Age, "Age", breaks=10)
plot_histogram(Experience.Programming, "Programming Experience")
plot_histogram(Experience.Modeling, "Modeling Experience")
# neste caso o 3º argumento fornece o nome do ficheiro, e breaks=8 especifica bins
plot_histogram(Experience.Software.Industry, "Exp. Working in Industry", "Experience Working in Industry", breaks=8)
plot_histogram(Experience.Security.Related, "Exp. Security Topcis", "Experience in Security Topics")
# atenção: breaks=50 pode ser demasiado grande para amostras pequenas; avalie n antes
plot_histogram(Distributed.Systems.Knowledge, "Exp. Distributed Systems", "Experience in Distributed Systems", breaks=50)
# gera gráfico de barras (Education é presumivelmente categórica)
plot_bar(Education, "Education")