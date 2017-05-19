rgeoapi is now on [CRAN](https://cran.r-project.org/web/packages/rgeoapi/)

# rgeoapi
This package requests informations from the French GeoAPI inside R — https://api.gouv.fr/explorer/geoapi/

## GeoAPI 

Developped by Etalab, with La Poste, l’INSEE and OpenStreetMap, the [GeoAPI](https://api.gouv.fr/explorer/geoapi/) API is a JSON interface designed to make requests on the French geographic database.

rgeoapi was developped to facilitate your geographic projects by giving you access to these informations straight inside R. With `rgeoapi`, you can get any coordinate, size and population of a French city, to be used in your maps. 

For an optimal compatibility, all the names (especially outputs) used in this package are the same as the ones used in the GeoAPI. Please note that this package works only with French cities.

## Install rgeoapi

Install this package directly in R : 

```{r}
devtools::install_github("ColinFay/rgeoapi")
```

## How rgeoapi works

The version 1.0.0 works with eleven functions. Which are :  

* `ComByCode` Get City by INSEE Code

* `ComByCoord` Get City by Coordinates

* `ComByDep` Get Cities by Department

* `ComByName` Get City by Name

* `ComByPostal` Get City by Postal Code

* `ComByReg` Get Cities by Region

* `DepByCode` Get Department by INSEE Code

* `DepByName` Get Department by Name

* `DepByReg` Get Departments by Region

* `RegByCode` Get Region by INSEE Code

* `RegByName` Get Region by Name

## How the functions are constructed

In the [GeoAPI](https://api.gouv.fr/explorer/geoapi/), you can request for "Commune", "Département" or "Région". 
All the functions are constructed using this terminology : AByB.

* A being the output you need -- Com for "Commune" (refering to French cities), Dep for Département (for Department) and Reg for Région. 

* B being the request parameter -- Code for INSEE Code, Coord for Coordinates (WGS-84), Dep for Department, Name for name, Postal for Postal Code and Reg for Region.

## Some examples 

### ComByCoord 

Takes the latitude and longitude of a city, returns a data.frame with name, INSEE code, postal code, INSEE department code, INSEE region code, population (approx), surface (in hectares), lat and long (WGS-84).

```{r}
ComByCoord(lat = "48.11023", lon = "-1.678872") 
```

### DepByName 

This function takes a character string with the name of the department, and returns a data.frame with name, INSEE code, and region code. Partial matches are possible. In that case, pertinence scores are given.

```{r}
DepByName("morbihan")
DepByName("Il")
```

### RegByCode

This function takes an INSEE Code, returns a data.frame with name and region code.

```{r}
RegByCode(53)
```

### French Tutorial & contact

A French tutorial on [my website](http://colinfay.me/rgeoapi-v1/).
Questions and feedbacks [welcome](mailto:contact@colinfay.me) !
