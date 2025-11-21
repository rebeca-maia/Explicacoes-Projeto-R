# Este script realiza análises simples (QQplots, testes não-paramétricos, correlações)
# sobre o RDS gerado por prepare.r. 

library(stringr)   # para str_glue (formatação de strings)
library(DescTools)  # várias utilidades descritivas (nem sempre usadas diretamente)
library(dplyr)      # manipulação de dados (filter, etc.)
library(effsize)    # funções para tamanho de efeito (ex.: cliff.delta) - atualmente não usado ativamente

# Definição de cores reutilizadas para plots (tons de cinza)
light_gray <- rgb(0.8, 0.8, 0.8)
light_gray_transparent <- scales::alpha(light_gray, 0.25)
dark_gray <- rgb(0.4, 0.4, 0.4)
dark_gray_transparent <- scales::alpha(dark_gray, 0.25)

colours <- c(light_gray, dark_gray)

# create_qq_plot: gera um QQ-plot para verificar aproximação à normalidade
# - data: vetor numérico (NA removidos no início)
# - project_name, variable, group_name apenas para nome do ficheiro/legenda
create_qq_plot <- function(data, project_name, variable, group_name) {
  
  data <- data[!is.na(data)]  # remover NAs — qqnorm/qqline falham com NAs
  
  # Abre arquivo PDF para saída gráfica
  pdf(str_glue("analysis/{project_name}_{group_name}_{variable}_qq.pdf"),width = 10, height = 7.5, paper = 'special')
  
  # qqnorm desenha quantis amostrais vs quantis teóricos da N(0,1)
  qqnorm(data, 
         pch = 1, 
         frame = FALSE, 
         main='',
         # main=str_glue("{group_name} ({project_name}) - {variable}"),
         ylab='',
         xlab='')
  # qqline adiciona linha de referência (ajustada pela amostra)
  qqline(data, col = light_gray, lwd = 2)
  # legenda grande no canto superior esquerdo
  legend("topleft", 
         legend=c(str_glue("{group_name} ({project_name}) - \n{variable}")),
         x.intersp = -1.5,
         lwd = 0,
         lty = 0,
         bg = rgb(0, 0, 0, 0),
         bty = 'n',
         box.lty = 0,
         cex = 3.5,
         horiz = FALSE)
  grid()
  dev.off()
  print("completed")
}

# create_scatter_plot: plota scatter correctness ~ response_time e ajusta reta linear
# - usa a fórmula f <- correctness ~ response_time, plota e adiciona abline(lm(f))
create_scatter_plot <- function(project_name, group_name, correctness, response_time) {
  pdf(str_glue("analysis/{project_name}_{group_name}_scatter.pdf"),width = 10, height = 7.5, paper = 'special')
  
  f <- correctness ~ response_time
  
  # plot com labels vazios; usa axis padrão (sem ticks explicitados)
  plot(f, xlab="", ylab="", xaxt='n', yaxt='n')
  abline(lm(f))
  legend("bottomright", 
         legend=c(str_glue("{group_name} ({project_name})")),
         x.intersp = -1.5,
         lwd = 0,
         lty = 0,
         bg = rgb(0, 0, 0, 0),
         bty = 'n',
         box.lty = 0,
         cex = 3.5,
         horiz = FALSE)
  
  grid()
  dev.off()
  
  print("completed")
}

# create_correlation: calcula correlação de Spearman entre response_time e correctness
# - devolve o dataframe atualizado com coluna (Group_Name, Spearman_Rho, S, p)
create_correlation <- function(group_name, correctness, response_time, dataframe) {
  
  # usa cor.test com método spearman (rank-based, robusto contra não-normalidade)
  # Observação: é importante garantir que correctness e response_time tenham mesmos indices
  # e que NAs sejam removidos ou alinhados previamente.
  correlation = cor.test(response_time, correctness, method="spearman")
  
  # extrai valores de interesse do objecto cor.test
  Spearman_Rho=round(as.double(correlation$estimate[["rho"]]),3)
  Spearman_S=round(as.double(correlation$statistic[["S"]]),3)
  Spearman_P=round(as.double(correlation$p.value),3)
  
  # adiciona linha ao dataframe passado (note: mantém tipos conforme o dataframe)
  dataframe[nrow(dataframe) + 1,] <- c(group_name, Spearman_Rho, Spearman_S, Spearman_P)
  return (dataframe)
}

# analyze: executa testes entre control_data e experiment_data e regista p-values
# - data: a lista com os dois vetores (control, experiment) criada em main
# - project_name, variable: para rotulação
# - dataframe: dataframe que será populado com resultados
# - control_data, experiment_data: vetores numéricos (NAs já removidos no início da função)
analyze <- function(data, project_name, variable, dataframe, control_data, experiment_data) {
  
  # remover NAs explicitamente — evita erro em testes
  control_data <- control_data[!is.na(control_data)]
  experiment_data <- experiment_data[!is.na(experiment_data)]
  
  # Kruskal-Wallis: teste não-paramétrico para comparar grupos (aqui lista de dois vectores serve)
  Kruskal = round(kruskal.test(data)$p.value,3)
  # Wilcoxon (Mann-Whitney): teste não-paramétrico para duas amostras independentes
  Wilcox = round(wilcox.test(control_data, experiment_data)$p.value,3)
  
  # Preparação para cálculo de Cliff's delta — a implementação original está comentada.
  # X e Y são transformados em matrizes/colnames mas esse formato não é usado depois.
  x <-matrix(control_data)
  colnames(x)<-c("control")
  y <-matrix(experiment_data)
  colnames(y)<-c("experiment")
  # Código comentado que usaria orddom::orddom para calcular Cliff's delta:
  # cliff_delta = orddom::orddom(x,y, x.name="control", y.name="experiment")
  # CliffDelta = round(as.double(cliff_delta[13,1]),3)
  # CliffDeltaYLargerX = round(as.double(cliff_delta[10,2]),3)
  # CliffDeltaP = round(as.double(cliff_delta[22,2]),3)
  
  # atualmente valores placeholder a 0.0 (sem cálculo efetivo)
  CliffDelta = 0.0
  CliffDeltaYLargerX = 0.0
  CliffDeltaP = 0.0
  
  Brunner = NULL  # variavel não utilizada (possível placeholder para outro teste)
  
  # adiciona linha ao dataframe de resultados.
  # Atenção: usando c(...) as colunas ficam em geral como character; é melhor
  # inserir por coluna com tipos apropriados.
  dataframe[nrow(dataframe) + 1,] <- c(project_name, variable, Kruskal, Wilcox, CliffDelta, CliffDeltaYLargerX, CliffDeltaP)
  return (dataframe)
}


getwd()  # imprime working directory (diagnóstico)

if (!dir.exists("analysis")) { dir.create("analysis") }  # cria pasta de saída se ausente

# carrega dados pré-processados pelo prepare.r (experiment-results.rds)
# Nota: prepare.r deve ser executado antes para gerar esse RDS
data <- readRDS('./experiment-results.rds')

attach(data)  # torna colunas acessíveis por nome (uso arriscado; pode causar ambiguidade)

# ES Project ---------------------------------------------------------------
# usa função set_group — ela NÃO é definida neste ficheiro; está em prepare.r/descriptive.r
# portanto este script depende de prepare.r ter sido executado (ou de carregar funções)
data$ES.Group = set_group(data$ES.Control)
es_experiment_group = filter(data, ES.Group == 'Experiment')
es_control_group = filter(data, ES.Group == 'Control')

# gera QQ-plots para Correctness e Timing, para os dois grupos
create_qq_plot(es_experiment_group$Task.ES.fmeasure, "ES", "Correctness", "Experiment")
create_qq_plot(es_control_group$Task.ES.fmeasure, "ES", "Correctness", "Control")
create_qq_plot(es_experiment_group$Task.ES.Timing, "ES", "Response Time", "Experiment")
create_qq_plot(es_control_group$Task.ES.Timing, "ES", "Response Time", "Control")

# PM Project ---------------------------------------------------------------
data$PM.Group = set_group(data$PM.Control)
pm_experiment_group = filter(data, PM.Group == 'Experiment')
pm_control_group = filter(data, PM.Group == 'Control')

create_qq_plot(pm_experiment_group$Task.PM.fmeasure, "PM", "Correctness", "Experiment")
create_qq_plot(pm_control_group$Task.PM.fmeasure, "PM", "Correctness", "Control")
create_qq_plot(pm_experiment_group$Task.PM.Timing, "PM", "Response Time", "Experiment")
create_qq_plot(pm_control_group$Task.PM.Timing, "PM", "Response Time", "Control")

# Agrupa os vetores (listas) que serão passadas para análise
es_correctness = list(es_control_group$Task.ES.fmeasure, es_experiment_group$Task.ES.fmeasure)
es_timing = list(es_control_group$Task.ES.Timing, es_experiment_group$Task.ES.Timing)
pm_correctness = list(pm_control_group$Task.PM.fmeasure, pm_experiment_group$Task.PM.fmeasure)
pm_timing = list(pm_control_group$Task.PM.Timing, pm_experiment_group$Task.PM.Timing)

# Cria data.frame vazio para resultados (atenção aos tipos iniciais)
analysis_data = data.frame(Project=character(),
                           Variable=character(),
                           Kruskal=double(),
                           Wilcox=double(),
                           CliffDelta=double(),
                           CliffDeltaYLargerX=double(),
                           CliffDeltaP=double()
                           # Brunner=double()
                           )

# Executa análises e popula analysis_data
analysis_data = analyze(es_correctness, "ES", "Correctness", analysis_data, es_control_group$Task.ES.fmeasure, es_experiment_group$Task.ES.fmeasure)
analysis_data = analyze(es_timing, "ES", "Response Time", analysis_data, es_control_group$Task.ES.Timing, es_experiment_group$Task.ES.Timing)

analysis_data = analyze(pm_correctness, "PM", "Correctness", analysis_data, pm_control_group$Task.PM.fmeasure, pm_experiment_group$Task.PM.fmeasure)
analysis_data = analyze(pm_timing, "PM", "Response Time", analysis_data, pm_control_group$Task.PM.Timing, pm_experiment_group$Task.PM.Timing)
print(analysis_data)  # imprime tabela de resultados (atenção: colunas podem estar como character)

# Correlation --------------------------------------------------------------
# Prepara dataframe para resultados de correlação
correlation_data = data.frame(Group_Name=character(),
                              Spearman_Rho=double(),
                              Spearman_S=double(),
                              Spearman_P=double()
                              )

# Calcula correlações (Spearman) para cada combinação de grupo
correlation_data = create_correlation("ES Experimental", es_experiment_group$Task.ES.fmeasure, es_experiment_group$Task.ES.Timing, correlation_data)
correlation_data = create_correlation("ES Control", es_control_group$Task.ES.fmeasure, es_control_group$Task.ES.Timing, correlation_data)

correlation_data = create_correlation("PM Control", pm_control_group$Task.PM.fmeasure, pm_control_group$Task.PM.Timing, correlation_data)
correlation_data = create_correlation("PM Experimental", pm_experiment_group$Task.PM.fmeasure, pm_experiment_group$Task.PM.Timing, correlation_data)

print("correlation completed")

# Gera scatter plots para visualização das correlações + regressão linear
create_scatter_plot("ES", "Experimental", es_experiment_group$Task.ES.fmeasure, es_experiment_group$Task.ES.Timing)
create_scatter_plot("PM", "Experimental", pm_experiment_group$Task.PM.fmeasure, pm_experiment_group$Task.PM.Timing)

create_scatter_plot("ES", "Control", es_control_group$Task.ES.fmeasure, es_control_group$Task.ES.Timing)
create_scatter_plot("PM", "Control", pm_control_group$Task.PM.fmeasure, pm_control_group$Task.PM.Timing)

print("completed")