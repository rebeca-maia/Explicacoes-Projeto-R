# Install R dependencies
`Rscript install.r`

# Generate CSV file from ODS
`libreoffice --headless --convert-to csv experiment-results.ods`

# Generate org table from CSV
`cat experiment-results.csv | sed 's/^/\| /g' | sed 's/,/ \| /g' | sed 's/6232/ \|/g' > experiment-results.org`

# Load the experiment data from the CSV file, do some preprocessing and save as an RDS file
`Rscript prepare.r`

# Analyse the data and generate output to plot/ and table/
`R CMD BATCH analyse.r`
