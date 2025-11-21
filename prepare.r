# Script preprocesses a CSV of experiment responses and
# builds derived variables (group, correctness counts, f-measure, timings)
# then saves a cleaned RDS for later analysis.

library(purrr)   # used for map() in string-list processing

# Helper: map raw marker ('x'/'X') to group factor 'Control' or 'Experiment'
set_group <- function(data) {
  group <- ifelse(data %in% c('x', 'X'), 'Control', 'Experiment')
  # return as a factor so downstream code that expects factor levels works
  return (as.factor(group))
}

# Map gender markers from multiple boolean columns into a single text label
set_gender <- function(data) {
  gender <- ifelse(data$Gender.Male %in% c('x', 'X'), 'Male',
            ifelse(data$Gender.Female %in% c('x', 'X'), 'Female', 'Diverse'))
  return (gender)
}

# Map education columns into a single label (Bsc/Msc/PhD/None)
set_education <- function(data) {
  education <- ifelse(data$Education.BSc %in% 'x', 'Bsc',
               ifelse(data$Education.MSc %in% 'x', 'Msc',
               ifelse(data$Education.PhD %in% 'x', 'PhD', 'None')))
  print(education)  # diagnostic print; may be noisy for large datasets
  return (education)
}

# replaceMarker: general utility to convert an 'x' marker into a token (like 'a'),
# handle NA, optionally convert to factor, and set other values to elseTo.
replaceMarker <- function(data, from, to, asFactor = T, elseTo = '') {
  d <- as.character(data)        # work with character vector
  d[is.na(d)] <- ''              # normalize NA to empty string
  d[d == from] <- to             # replace 'from' marker with 'to'
  
  if (elseTo != '') {
    # replace any value that is not 'from' and not 'to' with elseTo
    d[d != from & d != to] <- elseTo
  }
  
  if (asFactor) {
    d <- as.factor(d)
  } else {
    d <- as.character(d)
  }
  return(d)
}

# Count how many elements of 'answer' (a list of character vectors) are present
# in 'correct' (vector of correct tokens). Returns numeric vector.
calculate_correctness_answer_count_for_strings <- function(answer, correct) {
  # map over the list of answer-strings -> for each element compute intersection length
  correct_answer_count = map(answer, function (x) length(intersect(x, correct)))
  return (correct_answer_count)
}

# Read CSV produced from the ODS spreadsheet
data = read.csv("experiment-results.csv", sep = ",", dec = ".", header = T, stringsAsFactors = F)
# NOTE: stringsAsFactors = F prevents automatic conversion to factors; later code
# explicitly converts when needed.

# Create convenience columns derived from marker columns
data$Task.ES.Group <- set_group(data$ES.Control)
data$Task.PM.Group <- set_group(data$PM.Control)
data$Gender <- set_gender(data)
data$Education <- set_education(data)

######### Begin Task Preparation #############

######### ES Tasks ###########################

######### Task ES.1
# For the multiple-choice items Task.ES.1.a ... .e we replace 'x' with the option label
a = replaceMarker(data$Task.ES.1.a, 'x', 'a', elseTo = '')
b = replaceMarker(data$Task.ES.1.b, 'x', 'b', elseTo = '')
c = replaceMarker(data$Task.ES.1.c, 'x', 'c', elseTo = '')
d = replaceMarker(data$Task.ES.1.d, 'x', 'd', elseTo = '')
e = replaceMarker(data$Task.ES.1.e, 'x', 'e', elseTo = '')

# correct_count: number of expected correct answers for this question
correct_count = 1

# answer_count: how many options participant marked (non-empty)
# NOTE: BUG: repeats d twice and omits e in original; this should use e not d twice.
answer_count = as.numeric(as.integer(a != '') + as.integer(b != '') + as.integer(c != '') + as.integer(d != '') + as.integer(d != '') + as.integer(e != ''))

# correct_answer_count: whether participant chose the correct option 'd'
correct_answer_count = as.numeric(d == 'd')

# store counts in data frame for later aggregated metrics
data$Task.ES.1.correct_answer_count = correct_answer_count
data$Task.ES.1.correct_count = correct_count
data$Task.ES.1.answer_count = answer_count

######### Task ES.2
# Here the responses are numeric (e.g., expected values 3, 0, 4)
# Use defensive NA handling: if the equality yields NA, replace with FALSE
a = ifelse(is.na(data$Task.ES.2.a == 3) == TRUE, FALSE, data$Task.ES.2.a == 3)
b = ifelse(is.na(data$Task.ES.2.b == 0) == TRUE, FALSE, data$Task.ES.2.b == 0)
c = ifelse(is.na(data$Task.ES.2.c == 4) == TRUE, FALSE, data$Task.ES.2.c == 4) 

correct_count = 3
answer_count = 3 # assumes participants answered all three (might be unsafe)
correct_answer_count = as.numeric(a) + as.numeric(b) + as.numeric(c)

data$Task.ES.2.correct_answer_count = correct_answer_count
data$Task.ES.2.correct_count = correct_count
data$Task.ES.2.answer_count = answer_count

######### Task ES.3
a = replaceMarker(data$Task.ES.3.a, 'x', 'a', elseTo = '')
b = replaceMarker(data$Task.ES.3.b, 'x', 'b', elseTo = '')
c = replaceMarker(data$Task.ES.3.c, 'x', 'c', elseTo = '')
d = replaceMarker(data$Task.ES.3.d, 'x', 'd', elseTo = '')
e = replaceMarker(data$Task.ES.3.e, 'x', 'e', elseTo = '')
correct_count = 2
answer_count = as.numeric(as.integer(a != '') + as.integer(b != '') + as.integer(c != '') + as.integer(d != '') + as.integer(d != '') + as.integer(e != ''))
# NOTE: same duplication bug here (d repeated); should be ... + as.integer(e != '')
correct_answer_count = as.numeric(b == 'b') + as.numeric(d == 'd')
data$Task.ES.3.correct_answer_count = correct_answer_count
data$Task.ES.3.correct_count = correct_count
data$Task.ES.3.answer_count = answer_count

######### Task ES.4 (free-text splitted by ';')
correct = list('identity')
answer = strsplit(data$Task.ES.4,"[;]") 
correct_count = 1 # only identify
answer_count = as.numeric(map(answer, function(x) length(x)))
correct_answer_count = as.numeric(calculate_correctness_answer_count_for_strings(answer, c(correct)))
data$Task.ES.4.correct_answer_count = correct_answer_count
data$Task.ES.4.correct_count = correct_count
data$Task.ES.4.answer_count = answer_count

######### Task ES.5 (multi-select)
correct = list('mobile-ordering','mobile-basket','webclient-ordering','webclient-basket')
correct_count = 4
answer = strsplit(data$Task.ES.5,"[;]") 
answer_count = as.numeric(map(answer, function(x) length(x)))
correct_answer_count = as.numeric(calculate_correctness_answer_count_for_strings(answer, c(correct)))
data$Task.ES.5.correct_answer_count = correct_answer_count
data$Task.ES.5.correct_count = correct_count
data$Task.ES.5.answer_count = answer_count

######### Task ES.6
a = replaceMarker(data$Task.ES.6.a, 'x', 'a', elseTo = '')
b = replaceMarker(data$Task.ES.6.b, 'x', 'b', elseTo = '')
c = replaceMarker(data$Task.ES.6.c, 'x', 'c', elseTo = '')
d = replaceMarker(data$Task.ES.6.d, 'x', 'd', elseTo = '')
e = replaceMarker(data$Task.ES.6.e, 'x', 'e', elseTo = '')
correct_count = 2
answer_count = as.numeric(as.integer(a != '') + as.integer(b != '') + as.integer(c != '') + as.integer(d != '') + as.integer(d != '') + as.integer(e != ''))
# NOTE: repeated d again - same bug
correct_answer_count = as.numeric(c == 'c') + as.numeric(e == 'e')
data$Task.ES.6.correct_answer_count = correct_answer_count
data$Task.ES.6.correct_count = correct_count
data$Task.ES.6.answer_count = answer_count

########## Task ES.7
# expects two items with numeric correct values (==4)
a = data$Task.ES.7.a == 4
a[is.na(a)] <- FALSE # replace NA with FALSE (assume unanswered is incorrect)
b = data$Task.ES.7.b == 4
b[is.na(b)] <- FALSE
correct_count = 2
answer_count = 2
correct_answer_count = as.numeric(a) + as.numeric(b)
data$Task.ES.7.correct_answer_count = correct_answer_count
data$Task.ES.7.correct_count = correct_count
data$Task.ES.7.answer_count = answer_count

######### PM Task ################################

######### Task PM.1
a = replaceMarker(data$Task.PM.1.a, 'x', 'a', elseTo = '')
b = replaceMarker(data$Task.PM.1.b, 'x', 'b', elseTo = '')
c = replaceMarker(data$Task.PM.1.c, 'x', 'c', elseTo = '')
d = replaceMarker(data$Task.PM.1.d, 'x', 'd', elseTo = '')
e = replaceMarker(data$Task.PM.1.e, 'x', 'e', elseTo = '')
correct_count = 2
answer_count = as.numeric(as.integer(a != '') + as.integer(b != '') + as.integer(c != '') + as.integer(d != '') + as.integer(d != '') + as.integer(e != ''))
# NOTE: repeated d appears here as well
correct_answer_count = as.numeric(b == 'b') + as.numeric(d == 'd')
data$Task.PM.1.correct_answer_count = correct_answer_count
data$Task.PM.1.correct_count = correct_count
data$Task.PM.1.answer_count = answer_count

####### Task PM.2 (multi-select)
correct = list('notification','statistics','account','oauth2')
correct_count = 4
answer = strsplit(data$Task.PM.2,"[;]") 
answer_count = as.numeric(map(answer, function(x) length(x)))
correct_answer_count = as.numeric(calculate_correctness_answer_count_for_strings(answer, c(correct)))
data$Task.PM.2.correct_answer_count = correct_answer_count
data$Task.PM.2.correct_count = correct_count
data$Task.PM.2.answer_count = answer_count

######## Task PM.3
a = replaceMarker(data$Task.PM.3.a, 'x', 'a', elseTo = '')
b = replaceMarker(data$Task.PM.3.b, 'x', 'b', elseTo = '')
c = replaceMarker(data$Task.PM.3.c, 'x', 'c', elseTo = '')
d = replaceMarker(data$Task.PM.3.d, 'x', 'd', elseTo = '')
e = replaceMarker(data$Task.PM.3.e, 'x', 'e', elseTo = '')
correct_count = 2
answer_count = as.numeric(as.integer(a != '') + as.integer(b != '') + as.integer(c != '') + as.integer(d != '') + as.integer(d != '') + as.integer(e != ''))
# repeated d bug again
correct_answer_count = as.numeric(c == 'c') + as.numeric(d == 'd')
data$Task.PM.3.correct_answer_count = correct_answer_count
data$Task.PM.3.correct_count = correct_count
data$Task.PM.3.answer_count = answer_count

######## Task PM.4 (multi-select)
correct = list('account-statistics','notification-account')
correct_count = 2
answer = strsplit(data$Task.PM.4,"[;]") 
answer_count = as.numeric(map(answer, function(x) length(x)))
correct_answer_count = as.numeric(calculate_correctness_answer_count_for_strings(answer, c(correct)))
data$Task.PM.4.correct_answer_count = correct_answer_count
data$Task.PM.4.correct_count = correct_count
data$Task.PM.4.answer_count = answer_count

######## Task PM.5 (boolean choices)
a = data$Task.PM.5.a == 4
a[is.na(a)] <- FALSE # treat NA as FALSE (not selected)
b = data$Task.PM.5.b == 2
correct_count = 2
answer_count = 2
correct_answer_count = as.numeric(a) + as.numeric(b)
data$Task.PM.5.correct_answer_count = correct_answer_count
data$Task.PM.5.correct_count = correct_count
data$Task.PM.5.answer_count = answer_count

######## Task PM.6
a = replaceMarker(data$Task.PM.6.a, 'x', 'a', elseTo = '')
b = replaceMarker(data$Task.PM.6.b, 'x', 'b', elseTo = '')
c = replaceMarker(data$Task.PM.6.c, 'x', 'c', elseTo = '')
correct_count = 1
answer_count = as.numeric(as.integer(a != '') + as.integer(b != '') + as.integer(c != '') + as.integer(d != '') + as.integer(d != '') + as.integer(e != ''))
# NOTE: here d,e referenced but PM.6 only defines a,b,c above. This is suspicious (copy-paste)
correct_answer_count = as.numeric(a == 'a')
data$Task.PM.6.correct_answer_count = correct_answer_count
data$Task.PM.6.correct_count = correct_count
data$Task.PM.6.answer_count = answer_count

######### Task PM.7
a = data$Task.PM.7.a == 4
b = data$Task.PM.7.b == 2
c = data$Task.PM.7.c == 2
correct_count = 3
answer_count = 3
correct_answer_count = as.numeric(a) + as.numeric(b) + as.numeric(c)
data$Task.PM.7.correct_answer_count = correct_answer_count
data$Task.PM.7.correct_count = correct_count
data$Task.PM.7.answer_count = answer_count

######### end of task preparation ##########################

# Aggregate timings: sum of description time + per-question timings
data$Task.ES.Timing = 
  data$Task.ES.Description.Timing + 
  data$Task.ES.1.Timing + data$Task.ES.2.Timing + 
  data$Task.ES.3.Timing + data$Task.ES.4.Timing + 
  data$Task.ES.5.Timing + data$Task.ES.6.Timing + 
  data$Task.ES.7.Timing

print(data$Task.ES.Timing)  # diagnostic print

data$Task.PM.Timing = 
  data$Task.PM.Description.Time + 
  data$Task.PM.1.Timing + data$Task.PM.2.Timing + 
  data$Task.PM.3.Timing + data$Task.PM.4.Timing + 
  data$Task.PM.5.Timing + data$Task.PM.6.Timing + 
  data$Task.PM.7.Timing

# Compute overall precision/recall/fmeasure for ES
data$Task.ES.recall = ( data$Task.ES.1.correct_answer_count + 
                          data$Task.ES.2.correct_answer_count + 
                          data$Task.ES.3.correct_answer_count + 
                          data$Task.ES.4.correct_answer_count + 
                          data$Task.ES.5.correct_answer_count + 
                          data$Task.ES.6.correct_answer_count + 
                          data$Task.ES.7.correct_answer_count) / 
  ( data$Task.ES.1.correct_count + 
      data$Task.ES.2.correct_count + 
      data$Task.ES.3.correct_count + 
      data$Task.ES.4.correct_count + 
      data$Task.ES.5.correct_count + 
      data$Task.ES.6.correct_count + 
      data$Task.ES.7.correct_count )
data$Task.ES.precision = ( data$Task.ES.1.correct_answer_count + 
                             data$Task.ES.2.correct_answer_count + 
                             data$Task.ES.3.correct_answer_count + 
                             data$Task.ES.4.correct_answer_count + 
                             data$Task.ES.5.correct_answer_count + 
                             data$Task.ES.6.correct_answer_count + 
                             data$Task.ES.7.correct_answer_count) / 
  ( data$Task.ES.1.answer_count + 
      data$Task.ES.2.answer_count + 
      data$Task.ES.3.answer_count + 
      data$Task.ES.4.answer_count + 
      data$Task.ES.5.answer_count + 
      data$Task.ES.6.answer_count + 
      data$Task.ES.7.answer_count )

# f-measure: harmonic mean; handle NA by replacing with 0
fmeasure = 2 * ((data$Task.ES.precision * data$Task.ES.recall) / (data$Task.ES.precision + data$Task.ES.recall))
fmeasure = ifelse(is.na(fmeasure) == TRUE, 0, fmeasure)
data$Task.ES.fmeasure = fmeasure

# Compute PM recall/precision/fmeasure analogously
data$Task.PM.recall = ( data$Task.PM.1.correct_answer_count + 
                          data$Task.PM.2.correct_answer_count + 
                          data$Task.PM.3.correct_answer_count + 
                          data$Task.PM.4.correct_answer_count + 
                          data$Task.PM.5.correct_answer_count + 
                          data$Task.PM.6.correct_answer_count + 
                          data$Task.PM.7.correct_answer_count) / 
  ( data$Task.PM.1.correct_count + 
      data$Task.PM.2.correct_count + 
      data$Task.PM.3.correct_count + 
      data$Task.PM.4.correct_count + 
      data$Task.PM.5.correct_count + 
      data$Task.PM.6.correct_count + 
      data$Task.PM.7.correct_count )
data$Task.PM.precision = ( data$Task.PM.1.correct_answer_count + 
                             data$Task.PM.2.correct_answer_count + 
                             data$Task.PM.3.correct_answer_count + 
                             data$Task.PM.4.correct_answer_count + 
                             data$Task.PM.5.correct_answer_count + 
                             data$Task.PM.6.correct_answer_count + 
                             data$Task.PM.7.correct_answer_count) / 
  ( data$Task.PM.1.answer_count + 
      data$Task.PM.2.answer_count + 
      data$Task.PM.3.answer_count + 
      data$Task.PM.4.answer_count + 
      data$Task.PM.5.answer_count + 
      data$Task.PM.6.answer_count + 
      data$Task.PM.7.answer_count )
fmeasure = 2 * ((data$Task.PM.precision * data$Task.PM.recall) / (data$Task.PM.precision + data$Task.PM.recall))
fmeasure = ifelse(is.na(fmeasure) == TRUE, 0, fmeasure)
data$Task.PM.fmeasure = fmeasure

# overall fmeasure and average timing across projects
data$Task.fmeasure = (data$Task.ES.fmeasure + data$Task.PM.fmeasure) / 2.0
data$Task.Timing = (data$Task.ES.Timing + data$Task.PM.Timing) /  2.0

print("correctness calculation completed")

# save the processed dataset for later analysis scripts to load quickly
saveRDS(data, "./experiment-results.rds")

print("Prepare completed")
# End of prepare.r