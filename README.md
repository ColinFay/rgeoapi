# rgeoapi
This package requests informations from the french GéoAPI inside R — https://api.beta.gouv.fr/api/geoapi.html

##GeoAPI 

Developped by Etalab, with La Poste, l’INSEE and OpenStreetMap, [GéoAPI](https://api.beta.gouv.fr/api/geoapi.html) is designed to make requests on the french geographic database.

rgeoapi was developped to facilitate your geographic projects by giving you acces to theses informations straight inside R.


##Install rgeoapi

Install this package directly in R : 

```{r, eval=FALSE, warning = FALSE, message=FALSE, error=FALSE}
devtools::install_github("ColinFay/rgeoapi")
```

##How rgeoapi works

This first version works with three functions. They all return a dataframe with these variables : 
* `name` : name(s) of the city
* `codeInsee`: the Insee Code
* `codesPostaux` : the postal code(s)
* `surface` : the city surface
* `lat`and `long`: the GPS coordinates of the city

###getByPC 

This function takes a french postal code, returns a dataframe.

```{r}
getByPC(35000)
```

###getByName 

This function takes a name, and returns total and partial matches of this name.

```{r}
getByName("Rennes")
```

###getByIC

The same as getByPC, but with the INSEE Code

```{r}
getByIC(35238)
```

###French Tutorial & contact

A french tutorial on [my website](http://colinfay.me/rgeoapi/).
Questions and feedbacks [welcome](mailto:contact@colinfay.me) !
