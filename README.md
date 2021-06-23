# R-Script_and_Input-Datafiles_Ruett_Dietary_Pattern_Carbon_Footprint
The repository <a href="https://zenodo.org/badge/latestdoi/377050633"><img src="https://zenodo.org/badge/377050633.svg" alt="DOI"></a> contains input data files and the R-Script used for the analyses conducted in the following master thesis:
"Modelling Carbon Footprints of Western European Dietary Patterns Perfoming Probabilistic Simulation" by Johanna Manon Rütt, submitted 25/06/2021

This repository provides all data and information necessary to understand the thesis' carbon footprint probabilistic simulation, further calculations related to the carbon footprints and corresponding visualization.  

## File descriptions

### The Input Table

 ```Input_CO2dist.csv``` contains all greenhouse gas emission intensities and food consumption quantities applied in the ```Simulation``` function.

### The Legend File

```CO2e_label.csv``` contains the variable descriptions used in the simulation and further coding.

### The SUSLA File
```CALCSUSLA.csv``` and ```vegan.csv``` contain the carbon footrpints calculated by the calculator SUSLA, used as a basis for carbon footprint calculation. 

### Literature comparison files

```scenarios_lit.csv```, ```O_lit.csv```, ```VT_lit.csv``` and ```VN_lit.csv``` provide the carbon footprints identified in literature to which the thesis' results were compared to.

### The Model

```Modelling_Dietary_Pattern_CFs.R``` contains the entire code to run the model, to conduct further calculations carried out for carbon footprints and to vizualize the analyses in form of figures.  

### The R project

```Dietary_Pattern_Carbon_Footprints.Rproj``` is the corrsponding R project.


