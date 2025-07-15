# Compendio: 30 de agosto de 2024


## Descripción

Este repositorio contiene los insumos de datos utilizados para generar el compendio [A quiénes nos faltan](https://media.datacivica.org/pdf/aquienesnosfaltan-2024-DATACIVICA.pdf), publicado el 30 de agosto de 2024 en el marco del Día Internacional de las Víctimas de Desapariciones Forzadas. El compendio presenta un análisis de los datos existentes sobre las personas desaparecidas en México. Identifica patrones y tendencias en los datos del RNPDNO, y analiza también el subregistro y falta de completitud de registros en esta fuente oficial.

El repositorio contiene scripts y datos organizados en cuatro principales directorios: `import`, `clean`, `eda`, y `descriptives` [actualizar si es necesario].

## Directorios principales

* `clean`: Limpia y transforma los datos crudos para su posterior análisis y visualización.
* `descriptives`: Genera visualizaciones y figuras descriptivas finales.

### Organización de los directorios principales

Cada uno de los directorios principales (`import`, `clean`, `eda`, `descriptives`) puede contener los siguientes subdirectorios para organizar y estandarizar el flujo de datos en este proyecto:

* `input`: datos de insumo antes de ser transformados por los scripts de `src`.
  * `clean/input` contiene los datos crudos que son limpiados en `clean/src`
  * Todos los datos de `clean/input` fueron scrapeados de la página de la CNB [Estadística del RNDPNO por filtros](https://versionpublicarnpdno.segob.gob.mx/Dashboard/Index), utilizando [este crate](https://github.com/irvingfisica/reqrnpdno) de Morlan para scrapearlos de forma automatizada.
* `src`: scripts utilizados para transformar los datos de `input` y/o generar los productos en `output`.
  * `clean/src` contiene los scripts que limpian y transforman los datos scrapeados en `clean/input`.
  * `descriptives/src` contiene los scripts que generan las visualizaciones y figuras descriptivas en `descriptives/output`.
* `output`: datos y gráficas generadas por los scripts de `src`.
  * `descriptives/output` contiene las visualizaciones y figuras descriptivas generadas por los scripts de `descriptives/src`.

## Fuentes de datos

* **Registro Nacional de Personas Desaparecidas y No Localizadas (RNPDNO)**: se usaron dos formatos de esta misma fuente:

1. Datos scrapeados 22/7/2024 de la página [Estadística del RNDPNO por filtros](https://versionpublicarnpdno.segob.gob.mx/Dashboard/Index), utilizando [este crate](https://github.com/irvingfisica/reqrnpdno)

2. Versión pública del RNPDNO en formato JSON, actualizada 5/8/2023.

Cada visualización especifica la fuente de datos utilizada para su generación. 

* **Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública (ENVIPE)**: encuesta nacional realizada por el INEGI que proporciona información sobre la victimización y la percepción de seguridad en México. Incluye una pregunta sobre la desaparición forzada de integrantes de la vivienda durante el año anterior. Se utilizaron las encuestas publicadas 2013-2023.

## Licencia

Data Cívica 2024 ©

Para dudas sobre el contenido de este reposito, por favor contactar a Sierra Wells en sierra.wells@datacivica.org.

<!-- done -->
