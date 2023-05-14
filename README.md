# Diversity-Map
Repo for <a href = "https://www.bostonplans.org/research/mapping-diversity-in-boston">Diversity Map Index Tool</a>

## Folders
+ <b>output</b> : aggregated census data and calculated diversity score by dimension of diveristy and geography (e.g. lang_top10_city.csv" conains the data and diveristy scores for all comparable cities for 'Language Spoken at HOme')
+ <b>graph_files</b> : pie charts saved as widgets in an attempt to have interatvie pop-up iframes per census tract in Boston
+ <b>mapnew</b> : code containing the shiny app
+ <b>storymapanalysis</b> : data saved as csv or analyzed in a different way to make the associated <a href = "https://storymaps.arcgis.com/stories/bd384a8649094d2e80afeca6da90b92a">storymap</a>

## Important Files
+ <i>div_map_categories</i> : excel sheet used as a template / organizainal format to pull and aggregate census data
+ <i>dic_vars_pulling_script</i> : pulling script in R, using div_map_categories to aggregate data. Rerun this if any changes are made to variable categorizations. Writes to the output folder. 
+ <i>Variable Dictionary</i> : excel sheet with defintions of variables and naming conventions

