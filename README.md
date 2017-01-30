## complete build

```bash
# make sure you have knitr/rmarkdown installed
Rscript -e 'install.packages("knitr")'

cd introduction
Rscript -e 'knitr::knit("scurves.Rnw")'
Rscript -e 'knitr::knit("agevectors.Rnw")'
cd ../modelling
Rscript -e 'knitr::knit("usm.Rnw")'
Rscript -e 'install.packages("markovchain", "HMM", "Hmisc");knitr::knit("realigriffiths.Rnw")'
cd ../questionnaire
# pvrank requires gmp-devel and mpfr-devel
Rscript -e 'install.packages("ordinal", "pvrank", "stargazer");knitr::knit("shetland.Rnw")'
cd ../conclusion
Rscript -e 'knitr::knit("asymmetricvariation.Rnw")'
cd ..
pdflatex thesis.tex
```
