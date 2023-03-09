# Blog municipality data

This repository contains the code to generate the tables and figures used in
the blog on municipality data. This blog is published on Medium:
https://medium.com/p/f5904fbe6cae

## Running the code

The code can be run using the script `main.R`.

## Solving installation error

In some cases an installation error can occur when trying to install the
'grenswijzigen' package. When trying to install the package by
`devtools::install_github()` an error is returned containing this
message (amongst others):
```
In file(filename, "r", encoding = encoding) :
  cannot open file 'renv/activate.R': No such file or directory
```

This error seems to be caused by an erroneous interplay between 'renv' and 
'devtools'. In these circumstances it seems to be best to (temporarily) deactivate
renv:

```
renv::deactivate()
devtools::install_github("https://github.com/VNG-Realisatie/grenswijzigen")
renv::status()
renv::snapshot()
```

## Licentie

<a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/deed.nl"><img alt="Creative Commons-Licentie" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />Dit
werk valt onder een
<a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/deed.nl">Creative
Commons Naamsvermelding-NietCommercieel-GelijkDelen 4.0
Internationaal-licentie</a>.
