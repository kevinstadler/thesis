Citation:

```
@phdthesis{Stadler2016thesis,
  author = {Stadler, Kevin},
  school = {The University of Edinburgh},
  title = {Direction and directedness in language change: an evolutionary model of selection by trend-amplification},
  year = {2016},
  url = {https://github.com/kevinstadler/thesis}
}
```

## Complete build

```bash
# make sure you have knitr/rmarkdown installed
Rscript -e 'install.packages("knitr")'

cd introduction
Rscript -e 'install.packages("devtools");devtools::install_github("kevinstadler/scurves");knitr::knit("scurves.Rnw")'
Rscript -e 'install.packages("xtable");knitr::knit("agevectors.Rnw")'
cd ../modelling
Rscript -e 'install.packages("functional");knitr::knit("usm.Rnw")'
Rscript -e 'install.packages("markovchain", "HMM", "Hmisc");knitr::knit("realigriffiths.Rnw")'
cd ../questionnaire
# pvrank requires gmp-devel and mpfr-devel
Rscript -e 'install.packages("ordinal", "pvrank", "stargazer");knitr::knit("shetland.Rnw")'
cd ../conclusion
Rscript -e 'knitr::knit("asymmetricvariation.Rnw")'
cd ..
lualatex thesis.tex
bibtex thesis.aux
lualatex thesis.tex
lualatex thesis.tex
```
