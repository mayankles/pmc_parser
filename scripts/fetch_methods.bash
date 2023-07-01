### # ! / b in/bash

Rscript_bin="/mnt/c/Program Files/R/R-4.2.1/bin/Rscript.exe"
WD="/mnt/c/Users/mtand/OneDrive/Documents/Rprojects/PMC_parser"
RSCRIPT="fetch_n_methods.2.R"

cd $WD

FROM=$1
TO=$2

"$Rscript_bin" "$RSCRIPT" $FROM $TO

