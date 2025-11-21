# Este script produz visualizações e algumas transformações para os itens do survey (likert),
# calcula uma métrica de "self-assessment" (correção ajustada por confiança) e gera
# plots (densidade kernel e barplots) por projeto/grupo.

library(stringr)   # str_glue helpers para formatar nomes de ficheiros/legendas
library(dplyr)     # filter, select
library(ggplot2)   # não é fortemente usado aqui, mas carregado (poderia substituir gráficos base)

# cores/estilos usados nos gráficos
light_gray <- rgb(0.8, 0.8, 0.8)
light_gray_transparent <- scales::alpha(light_gray, 0.5)
dark_gray <- rgb(0.4, 0.4, 0.4)
dark_gray_transparent <- scales::alpha(dark_gray, 0.5)

colours <- c(light_gray, dark_gray)

# Função utilitária: converte marcação 'x'/'X' para fator 'Control'/'Experiment'
# Entrada esperada: vetor com 'x' / 'X' em uma das categorias; retorna factor( "Control"/"Experiment" ).
set_group <- function(data) {
  group <- ifelse(data %in% c('x', 'X'), 'Control', 'Experiment')
  return (as.factor(group))
}

# create_kernel_density_plot:
#   - plota densidade kernel para experiment e control (com preenchimento)
#   - experiment/control são vetores numéricos (NAs removidos)
#   - project, variable usados para nome do ficheiro; legend/title para rótulos
create_kernel_density_plot <- function(experiment, control, project, variable, legend, title, xlim) {
  # filtra NAs
  experiment <- experiment[!is.na(experiment)]
  control <- control[!is.na(control)]
  
  # Abre PDF de saída
  pdf(str_glue("survey/{project}_{variable}_kernel_density.pdf"),width = 10, height = 3.5, paper = 'special')
  
  # move axis labels para dentro do plot
  par(mgp = c(-1.75, 1, 0))
  
  # pre-calc de densidades para definir os limites do y
  tmp_exp <- density(experiment)
  tmp_control <- density(control)
  ymin <- min(tmp_exp$y, tmp_control$y)
  ymax <- max(tmp_exp$y, tmp_control$y)
  
  experiment_kernel_density <- density(experiment)
  # desenha densidade do experimento
  plot(experiment_kernel_density, 
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
       col=light_gray, 
       main = "", 
       xlab = "",
       bty = "L",
       xlim=xlim,
       ylim=c(0,3)   # OBS: ylim fixo c(0,3) pode cortar ou expandir desnecessariamente
  )
  
  # adiciona densidade do controle
  control_kernel_density <- density(control)
  lines(control_kernel_density, col=dark_gray, lwd=1)
  # preenche polígonos com transparência
  polygon(control_kernel_density, col=light_gray_transparent, border=light_gray, lwd=2)
  polygon(experiment_kernel_density, col=dark_gray_transparent, border=dark_gray, lwd=2)
  # legenda
  legend("topright", 
         title = legend,
         # title.cex = 1.5,
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
         # inset = inset
         horiz = FALSE)
  title(
    sub=title,
    cex.sub=1.75,
    line=3.75)
  grid()
  dev.off()
  print("completed")
}

# plot_barplot:
#   - plota barras lado-a-lado para respostas Likert (1..5) por grupo (Control vs Experimental ou ES vs PM)
#   - data: vetor de scores (1..5), group: vetor com labels de grupo (factor ou char)
#   - label_position: onde posicionar legenda ('topleft', 'bottomright', etc.)
plot_barplot <- function(data, group, title, group_name1, group_name2, file_name, label_position, breaks=15) {
  
  # monta tabela: linhas = grupos, colunas = categorias 1:5 (mesmo que vazias)
  tab = table(group, factor(data, levels = 1:5))
  
  pdf(str_glue("survey/{file_name}.pdf"),width = 10, height = 4, paper = 'special')
  
  par(mar = c(3.5,2,6,2))
  plot = barplot(
    tab, 
    beside=TRUE, 
    col = c(light_gray_transparent, dark_gray_transparent), 
    main=title, 
    cex.lab=0.5, cex.axis=0.5, cex.main=1.5, cex.sub=0.5, cex.names=1.5,
    names.arg=c("totally disagree", "disagree", "neutral", "agree", "totally agree"),
    las=1
    )
  
  # legenda (usa label_position passado)
  legend(label_position,fill=colours,legend=c(group_name1, group_name2),box.lty = 0, cex = 1.3,)
  
  dev.off()
}

# revert_likert:
#   - converte escala Likert invertida: assume entrada 1..5 (onde 1 é melhor ou pior)
#   - retorna 6 - data para inverter; trata 0/NA como NA
revert_likert <- function(data) {
  result = 6 - data
  if (is.na(result)) {
    return (NA)
  } else if(result == 6) {
    # acontece quando data == 0 (valor inválido para Likert) -> devolve NA
    return (NA)
  } else {
    return (result)
  }
}

# calculate_confidence:
#   - transforma confiança (supostamente 1..5) em ajuste: (5 - confidence)/5
#   - a métrica de "assessment" = correctness - tmp
#   - Nota: correctness espera-se na escala [0,1] (fmeasure), confidence na escala 1..5
calculate_confidence <- function(correctness, confidence) {
  tmp = (5 - confidence)/5
  assessment = correctness - tmp
  return (assessment)
}

# Início do fluxo principal
getwd()
# carrega o dataset processado
data <- readRDS('./experiment-results.rds')

attach(data)   # NB: attach não recomendado em scripts; preferir data$col
summary(data)

# cria pasta de saída se inexistente
if (!dir.exists("survey")) { dir.create("survey") }

# marca projeto (ES ou PM) em que cada participante foi experimental (coluna ES.Experiment na RDS)
data$Experiment = ifelse(data$ES.Experiment %in% c('x', 'X'), 'ES', 'PM')

# Cria grupos (Control/Experiment) para cada projeto
# (set_group está definida em prepare.r/descriptive.r, por isso prepare.r deve ter sido executado)
data$ES.Group = set_group(data$ES.Control)
es_experiment_group = filter(data, ES.Group == 'Experiment')
es_control_group = filter(data, ES.Group == 'Control')

data$PM.Group = set_group(data$PM.Control)
pm_experiment_group = filter(data, PM.Group == 'Experiment')
pm_control_group = filter(data, PM.Group == 'Control')

# --- Self-assessment (Confidence) plots -----------------------------------
# calcula 'self assessment' = correctness - (5 - confidence)/5
# Para ES:
es_control_confidence = mapply(calculate_confidence, es_control_group$Task.ES.fmeasure, es_control_group$Task.ES.Confidence)
es_experiment_confidence = mapply(calculate_confidence, es_experiment_group$Task.ES.fmeasure, es_experiment_group$Task.ES.Confidence)
# gera densidade kernel para self-assessment entre grupos do projeto ES
create_kernel_density_plot(es_experiment_confidence, es_control_confidence, "ES", "Self Assessment", "Self Assessment [-1;1]", "(a) Self Assessment - eShopOnContainers (ES)", c(-1,1))

# Para PM:
pm_control_confidence = mapply(calculate_confidence, pm_control_group$Task.PM.fmeasure, pm_control_group$Task.PM.Confidence)
pm_experiment_confidence = mapply(calculate_confidence, pm_experiment_group$Task.PM.fmeasure, pm_experiment_group$Task.PM.Confidence)
create_kernel_density_plot(pm_experiment_confidence, pm_control_confidence, "PM", "Self Assessment", "Self Assessment [-1;1]", "(b) Self Assessment - Piggy Metrics (PM)", c(-1,1))


# --- Barplots para itens de survey (agrupados por Experimento) -------------
# Junta entradas de ambos projetos onde foram experimentais (ES ou PM)
es_experiment = dplyr::filter(data, Experiment == 'ES')
pm_experiment = dplyr::filter(data, Experiment == 'PM')

# Exemplo: "Visual Diagrams helped" (reverte escala e plota)
es_revert_likert = mapply(revert_likert, es_experiment$Task.ES.Helpful.Component.Diagram)
pm_revert_likert = mapply(revert_likert, pm_experiment$Task.PM.Helpful.Component.Diagram)
all_revert_likert = c(es_revert_likert, pm_revert_likert)
all_experiments = rbind(es_experiment,pm_experiment)
plot_barplot(all_revert_likert, all_experiments$Experiment, "The additional component diagram was helpful for solving the tasks.", "ES", "PM", "Component_Diagrams",'topleft')

# "Metrics were helpful"
es_revert_likert = mapply(revert_likert, es_experiment$Task.ES.Helpful.Metrics)
pm_revert_likert = mapply(revert_likert, pm_experiment$Task.PM.Helpful.Metrics)
all_revert_likert = c(es_revert_likert, pm_revert_likert)
plot_barplot(all_revert_likert, all_experiments$Experiment, "The additional metrics were helpful for solving the tasks.", "ES", "PM", "Metrics",'topleft')

# Security features understanding: plot por grupo dentro de projeto (ES e PM separadamente)
new_likert_scale = mapply(revert_likert, data$Task.ES.Understand.Security.Features)
plot_barplot(new_likert_scale, data$ES.Group, "It was easy for me to understand security features of eShopOnContainers.", "Control", "Experimental", "ES_Security_Features", 'topleft')

new_likert_scale = mapply(revert_likert, data$Task.PM.Understand.Security.Features)
plot_barplot(new_likert_scale, data$PM.Group, "It was easy for me to understand security features of Piggy Metrics.", "Control", "Experimental", "PM_Security_Features", 'topleft')

# Alguns trechos comentados: outras visualizações e checagens de escala Likert
# O script termina com print de 'completed'
print("completed")