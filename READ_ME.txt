---------------------------------------------------------------------------
Description of the Supporting Information, including scripts and data to generate the results and visualisations, from Nest initiation and flooding in response to season and semi-lunar spring tides in a ground-nesting shorebird by Plaschke et al. 2019
---------------------------------------------------------------------------

WHEN USING any of the Supporting information, PLEASE CITE both the original paper and the Supporting information

Plaschke et al. (2019).  Nest initiation and flooding in response to season and semi-lunar spring tides in a ground-nesting shorebird.

Plaschke et al. (2019). Supporting information for 'Nest initiation and flooding in response to season and semi-lunar spring tides in a ground-nesting shorebird'. Open Science Framework.
	
For any publication making substantial use of the data, please contact Martin Bulla (bulla.mar@gmail.com) or Clemens Küpper (ckuepper@orn.mpg.de), as the authors welcome the opportunity for collaboration and wish to comment prior to publication.

---------------------------------------------------------------------------
CONTENT
1. Supplementary Information.
2.R scripts
    a) Analyses.R 
	b) Constants_Functions.R
	c) Prepare_Data.R
	d) Tides_and_lunar_data_scrape_code.R

3. Data
    a) SNPL_nest_bird_data.csv 
    b) tides_all.csv
    c) illumination_all.csv 
    d) moonphases.csv
---------------------------------------------------------------------------

1. Supplementary Information contains Figures and Tables discussed in the manuscript.

---------------------------------------------------------------------------

2. R-scripts 
a) Analyses.R - generates all the results, including supplementary Tables and test of model assumptions 
              - requires Constants_Functions.R, Prepare_Data.R, SNPL_nest_bird_data.csv, moonphases.csv, illumination_all.csv and tides_all.csv in the working directory (definde in "wd" part of the script)

b) Constants_Functions.R loads required packages, functions and constants needed for the analyses; is sourced from Analyses.R

c) Prepare_Data.R - prepares three data-frames used in the analyses (for their description see next section); is sourced from Analyses.R
				 
d) Tides_and_lunar_data_scrape_code.R contains the script used to scrape the tides and lunar data from the web; creates tides_all.csv, illumination_all.csv and moonphases.csv. Instead of generating these csvs data a new, you can use provided files: tides_all.csv, illumination_all.csv and moonphases.csv

---------------------------------------------------------------------------
Three data frames (tt, dd, nn) created via Analyses.R (i.e.Prepare_Data.R) used in the Analyses.R script:

- tt     contains the tide data 
pk: unique ID of the row (primary key)
year: year of observation
date: date of tide predictions
max_tide_height: Tide height in cm (the heights of the highest high tide for a given day)
m_start: start date-time of the moon cycle (starts at the datetime of a new moon)
m_end: end date-time of moon cycle (ends at the datetime of the next new moon)
moon_cycle: moon cycle number within the given year, count starts with first new moon of the year (1 - indicates the 1st full moon cycle of the year starting with new moon, 2 - starts with the next new moon and so on)
st_start: datetime when the given spring-tide cycle started
st_end: datetime when the given spring-tide cycle ended
st_cycle: spring-tide cycle number within the given year; count starts with first first-quarter of the moon of the year (1 - indicates the 1st spring tide cycle of the year starting with 1st quarter, 2 - starts with 3rd quarter and so on)
dur: duration of particular spring-tide cycle in days
days_after_nm: days after the last new moon
days_after_st: days after the last spring-tide
day_j: Julian date


- dd     contains the aggregated dataset with number of nests per day with tidal and lunar data.
year: year of observation
datetime_: date and time of the observation
n_nest: number of initiated nests at a given day
day_j: Julian date
st_start: datetime when the given spring-tide cycle started
st_end: datetime when the given spring-tide cycle ended
st_cycle: spring-tide cycle number within the given year; count starts with first first-quarter of the moon of the year (1 - indicates the 1st spring tide cycle of the year starting with 1st quarter, 2 - starts with 3rd quarter and so on)
dur: duration of particular spring-tide cycle in days
days_after_st: days after the last spring-tide
m_start: start date-time of the moon cycle (starts at the datetime of a new moon)
m_end: end date-time of moon cycle (ends at the datetime of the next new moon)
moon_cycle: moon cycle number within the given year, count starts with first new moon of the year (1 - indicates the 1st full moon cycle of the year starting with new moon, 2 - starts with the next new moon and so on)
days_after_nm: days after the last new moon
max_tide_height: Tide height in cm (the heights of the highest high tide for a given day)


- nn     contains the nesting with the corresponding tide and lunar data prepared for the analysis of semi-lunar spring-tide rhythms in the nesting schedules.
year: year of observation
nest: unique id of the nest (Year + Nesting Site + Nest Nr. merged together).
female: unique ID of incubating female. 
male: unique ID of incubating male. 
clutch_size_found: number of eggs in a clutch when eggs were floated (indicates wether clutch was complete when found and floated)
clutch_size: number of eggs in the complete clutch
floated: date when eggs were floated
fate: Fate (Hatched (=hatch), Unhatched (=unhatch), Abandoned (=aban), Predated (=pred), Flooded by tides (=tide), Flooded by rain (=rain), Trampled, Broken, Unknown)
lat: latitude of a particular nest.
lon: longitude of a particular nest.
float_1: floating stage of first egg of particular clutch
float_2: floating stage of second egg of particular clutch (if clutch size > 1, otherwise NA)
float_3: floating stage of third egg of particular clutch (if clutch size > 2, otherwise NA)
com: particular comments on clutches indicating unusual observation 
end_: date when clutch either hatched or failed
float_min: if more than one egg was floated, the smallest floating stage (equals the youngest egg of the clutch)
init: date of nest initiation (when the first egg was laid)
laid: start of incubation based on floating date and floating stages
pk: unique ID of the row (primary key)
st_start: date-time when given spring-tide cycle started
st_end:  date-time when given spring-tide cycle ended
st_cycle: spring-tide cycle number within the given year; count starts with first first-quarter of the moon of the year (1 - indicates the 1st spring tide cycle of the year starting with 1st quarter, 2 - starts with 3rd quarter and so on)
dur: duration of particular spring-tide cycle in days
st_cycle_c: "st_cycle", but year centered
days_after_st: days after the last spring-tide
days_after_st_r: days after the last spring-tide rounded to whole number
m_start: start date-time of the moon cycle (starts at the datetime of a new moon)
m_end: end date-time of moon cycle (ends at the datetime of the next new moon)
moon_cycle: moon cycle number within the given year, count starts with first new moon of the year (1 - indicates the 1st full moon cycle of the year starting with new moon, 2 - starts with the next new moon and so on)
days_after_nm: days after the last new moon
max_tide_height: Tide height in cm (the heights of the highest high tide for a given day)
illum_mid: lunar illumination on particular day



3. Data contains
	
a) SNPL_nest_bird_data.csv - coma delineated csv file containing raw nesting data derived from field observations.
year: year of observation
nest: unique id of the nest (Year + Nesting Site + Nest Nr. merged together).
female: unique ID of incubating female. 
male: unique ID of incubating male. 
clutch_size: number of eggs in the complete clutch
found: date when clutch found 
floated: date when eggs were floated
end: date when clutch either hatched or failed
fate: of the nest - Hatched (=hatch), Unhatched (=unhatch), Abandoned (=aban), Predated (=pred), Flooded by tides (=tide), Flooded by rain (=rain), Trampled, Broken, Unknown)
lat: latitude of a particular nest.
lon: longitude of a particular nest.
float_1: floating stage of first egg of particular clutch
float_2: floating stage of second egg of particular clutch (if clutch size > 1, otherwise NA)
float_3: floating stage of third egg of particular clutch (if clutch size > 2, otherwise NA)
com: particular comments on clutches indicating unusual observations  

b) 
tides_all.csv – contains raw data of tide predictions of Mazatlan derived from mobilegeographics.com
pk: unique ID of the row (primary key)
year: year of observation
date: date of tide predictions
event: moon phases (fq = First Quater, fm= Full Moon, lq= Last Quater, nm= New Moon)
high_1: tide prediction in cm of first high tide of the particular day
low_1: tide prediction in cm of first neap tide of the particular day
high_2: tide prediction in cm of second high tide of the particular day
low_2: tide prediction in cm of second neap tide of the particular day
high_3: tide prediction in cm of third high tide of the particular day
low_3: tide prediction in cm of third neap tide of the particular day
max_tide_height: Tide height in cm (the heights of the highest high tide for a given day)

c)
illumination_all.csv- contains data of lunar illumination predictions of Mazatlan derived from timeanddate.com
pk: unique ID of the row (primary key)
year: year of observation
meridian_passing: datetime of moon passing meridian
illumination_mp: illumination at meridan passing
noon: dummy variable used to create illumination_noon
illumination_noon: illumination at noon (interpolated)

d)
moonphases.csv- contains data of moon phases of Mazatlan derived from timeanddate.com
year: year of observation
datetime_: datetime of particular moon phase
event: moon phase (fq = First Quater, fm= Full Moon, lq= Last Quater, nm= New Moon)

	

