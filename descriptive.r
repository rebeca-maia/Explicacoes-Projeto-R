# Este script gera estatísticas descritivas e plots (kernel density) para os dois projetos
# (ES e PM).

library(stringr)   # str_glue e helpers de string
library(dplyr)     # filter, etc.
library(nortest)   # ad.test (Anderson-Darling) para normalidade

# definição de paleta de cores usada nos gráficos
light_gray <- rgb(0.8, 0.8, 0.8)
light_gray_transparent <- scales::alpha(light_gray, 0.25)
dark_gray <- rgb(0.4, 0.4, 0.4)
dark_gray_transparent <- scales::alpha(dark_gray, 0.25)

colours <- c(light_gray, dark_gray)

# set_group: converte marcação 'x'/'X' em factor 'Control'/'Experiment'
# - Esperado: entrada é vetor com marcadores (ex.: data$ES.Control)
# - Retorna factor para facilitar split por níveis
set_group <- function(data) {
  group <- ifelse(data %in% c('x', 'X'), 'Control', 'Experiment')
  return (as.factor(group))
}

# generate_statistics_dataframe:
#   calcula várias estatísticas descritivas e testes de normalidade para um vetor
#   - project_name, group_name, variable apenas para rotulação/row do dataframe
#   - data: vetor numérico (NAs devem ser removidos antes de chamar)
#   - data_frame: dataframe sendo populado (append)
# Retorna data_frame atualizado.
generate_statistics_dataframe <- function(project_name, group_name, variable, data, data_frame) {
  data <- data[!is.na(data)]  # remove NAs antes de calcular
  # calcula estatísticas básicas
  Observations = length(data)
  Mean = round(mean(data),2)
  Median = round(median(data),2)
  Standard_deviation = round(sd(data),3)
  Variance = round(var(data), 3)
  Min = round(min(data),2)
  Max = round(max(data),2)
  Skewness = round(e1071::skewness(data), 3)    # e1071::skewness exige pacote e1071
  Kurtosis = round(e1071::kurtosis(data), 3)    # e1071::kurtosis exige pacote e1071
  Shapiro = round(shapiro.test(data)$p.value, 4)            # Shapiro-Wilk para normalidade
  Kolmogorow = round(ks.test(data, 'pnorm', mean=mean(data), sd=sd(data))$p.value, 4)
  Anderson = round(ad.test(data)$p.value, 4)                # Anderson-Darling

  # adiciona linha ao data_frame de saída
  data_frame[nrow(data_frame) + 1,] <- c(project_name, variable, group_name, Observations, Mean, Median, Standard_deviation, Variance, Min, Max, Skewness, Kurtosis, Shapiro, Kolmogorow, Anderson)
  return (data_frame)
}

# create_kernel_density_plot:
#   plota duas densidades kernel (experiment, control) com preenchimento e legenda.
#   - experiment, control: vetores numéricos (podem conter NAs -> filtrados)
#   - project, variable: rótulos para arquivo
#   - legend, title: strings para legenda/título
create_kernel_density_plot <- function(experiment, control, project, variable, legend, title) {
  # filtra NAs
  experiment <- experiment[!is.na(experiment)]
  control <- control[!is.na(control)]

  # proteger contra número insuficiente de observações
  if (length(experiment) < 2 || length(control) < 2) {
    warning(str_glue("Not enough data to plot kernel density for {project} {variable}: n_exp={length(experiment)}, n_ctrl={length(control)}"))
    return(invisible(NULL))
  }

  # abre PDF de saída (padrão: pasta descriptive/)
  pdf(str_glue("descriptive/{project}_{variable}_kernel_density.pdf"),width = 10, height = 3.5, paper = 'special')

  # ajusta parâmetros gráficos (movimenta posição de labels)
  par(mgp = c(-1.75, 1, 0))

  # pré-cálculo das densidades para fixar os eixos
  tmp_exp <- density(experiment)
  tmp_control <- density(control)
  ymin <- min(tmp_exp$y, tmp_control$y)
  ymax <- max(tmp_exp$y, tmp_control$y)

  experiment_kernel_density <- density(experiment)
  # plota densidade do experimento
  plot(experiment_kernel_density, 
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
       col=light_gray, 
       main = "", 
       xlab = "",
       bty = "L",
       ylim = c(ymin, ymax))
  # adiciona linha da densidade do controle
  control_kernel_density <- density(control)
  lines(control_kernel_density, col=dark_gray, lwd=1)
  # preenche polígonos para visualmente comparar
  polygon(control_kernel_density, col=light_gray_transparent, border=light_gray, lwd=2)
  polygon(experiment_kernel_density, col=dark_gray_transparent, border=dark_gray, lwd=2)
  # legenda fixa
  legend("topright", 
         title = legend,
         title.cex = 1.5,
         legend = c("Control", "Experimental"),
         col = colours,
         border = colours,
         fill = colours,
         x.intersp = -1.5,
         lwd = 0,
         lty = 0,
         bg = rgb(0, 0, 0, 0),
         # bty = 'o',
         box.lty = 0,
         cex = 1.3,
         horiz = FALSE)
  title(
    sub=title,
    cex.sub=1.75,
    line=3.75)
  grid()
  dev.off()
  print("completed")
}

# fluxo principal do script descriptive.r
getwd()

# cria diretorio de saída se inexistente
if (!dir.exists("descriptive")) { dir.create("descriptive") }

# carrega dados pré-processados gerados por prepare.r
data <- readRDS('./experiment-results.rds')

attach(data)   # NB: attach facilita uso interativo mas NÃO é recomendado em scripts (pode causar ambiguidade)

# ES Project: gera kernel density para Correctness e Response Time (ES)
data$ES.Group = set_group(data$ES.Control)
es_experiment_group = filter(data, ES.Group == 'Experiment')
es_control_group = filter(data, ES.Group == 'Control')
create_kernel_density_plot(es_experiment_group$Task.ES.fmeasure, es_control_group$Task.ES.fmeasure, "ES", "Correctness", "Correctness [0;1]", "(a) Overall Correctness - eShopOnContainers (ES)")
create_kernel_density_plot(es_experiment_group$Task.ES.Timing, es_control_group$Task.ES.Timing, "ES", "Response Time", "Response Time (min)", "(b) Overall Response Time - eShopOnContainers (ES)")

# PM Project: mesmo procedimento para o outro projeto
data$PM.Group = set_group(data$PM.Control)
pm_experiment_group = filter(data, PM.Group == 'Experiment')
pm_control_group = filter(data, PM.Group == 'Control')
create_kernel_density_plot(pm_experiment_group$Task.PM.fmeasure, pm_control_group$Task.PM.fmeasure, "PM", "Correctness", "Correctness [0;1]", "(a) Overall Correctness - Piggy Metrics (PM)")
create_kernel_density_plot(pm_experiment_group$Task.PM.Timing, pm_control_group$Task.PM.Timing, "PM", "Response Time", "Response Time (min)", "(b) Overall Response Time - Piggy Metrics (PM)")

# prepara dataframe que armazenará todas as estatísticas descritivas (linhas inseridas em seguida)
descriptive_data = data.frame(Project=character(),
                              Variable=character(),
                              Group=character(),
                              Observations=integer(),
                              Mean=double(),
                              Median=double(),
                              Standard_deviation=double(),
                              Variance=double(),
                              Min=double(),
                              Max=double(),
                              Skewness=double(),
                              Kurtosis=double(),
                              Shapiro=double(),
                              Kolmogorow=double(),
                              Anderson=double()
                              )

# popula descriptive_data com estatísticas de ES (experiment e control) e timings
descriptive_data = experiment_data = generate_statistics_dataframe("ES","Experiment", 'Correctness', es_experiment_group$Task.ES.fmeasure, descriptive_data)
descriptive_data = control_data = generate_statistics_dataframe("ES","Control", 'Correctness', es_control_group$Task.ES.fmeasure, descriptive_data)
descriptive_data = experiment_data = generate_statistics_dataframe("ES","Experiment", 'Response Time', es_experiment_group$Task.ES.Timing, descriptive_data)
descriptive_data = control_data = generate_statistics_dataframe("ES","Control", 'Response Timing', es_control_group$Task.ES.Timing, descriptive_data)

# popula descriptive_data com estatísticas de PM (experiment e control) e timings
descriptive_data = experiment_data = generate_statistics_dataframe("PM","Experiment", 'Correctness', pm_experiment_group$Task.PM.fmeasure, descriptive_data)
descriptive_data = control_data = generate_statistics_dataframe("PM","Control", 'Correctness', pm_control_group$Task.PM.fmeasure, descriptive_data)
descriptive_data = experiment_data = generate_statistics_dataframe("PM","Experiment", 'Response Time', pm_experiment_group$Task.PM.Timing, descriptive_data)
descriptive_data = control_data = generate_statistics_dataframe("PM","Control", 'Response Timing', pm_control_group$Task.PM.Timing, descriptive_data)
print(descriptive_data)

print('Completed')