Report
================
Gwyneth Wei (gw2442); Emma Sexton (els2250); Olivia Wang (hw2852);
Mayuri Albal (ma4197) <br>
10 December 2022

### In-depth Analysis of Emma Sexton’s New York City 2022 Marathon Training

<br/>

#### Motivation: Provide an overview of the project goals and motivation.

<br/>

#### Related Work: Anything that inspired you, such as a paper, a web site, or something we discussed in class.

<br/>

#### Initial Questions: What questions are you trying to answer? How did these questions evolve over the course of the project? What new questions did you consider in the course of your analysis?

<br/>

#### Data: Source, scraping method, cleaning, etc.

Data was collected using the Garmin Forerunner 245 activity watch from
August 17, 2020 to November 6, 2022. Collected data was uploaded to
Strava.com after each activity. After granting permission, personal data
was downloaded from Strava.com. Each activity logged by the Garmin watch
was downloaded as a separate FIT file; therefore, using the `FITfileR`
package, data was imported into R.

##### *Importing Data*

Since each Garmin activity was downloaded as an individual FIT file, a
function was created to iterate across the multiple activities and
compile a single data frame. As shown below, the function was created to
read each activity FIT file into R and pull out the records (which
included data on latitude and longitude, distance, speed, heart rate,
and cadence), and then each second of activity was merged into a single
data frame. This function was applied to a list of all file names. A
`for` loop was used to generate a list of data frames, and then rows
were merged using `bind_rows` to create the final data frame,
`marathon_df`.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(FITfileR)
```

``` r
data_import <- function(list_data) {
  
  garmin_data <- readFitFile(list_data)
  
  garmin_activities_df <- records(garmin_data) %>% 
    bind_rows() %>% 
    arrange(timestamp)
  
  garmin_activities_df
  
}

garmin_fitfiles <- "activities/fit_files/garmin_data_"

list_of_fitfiles <- str_c(garmin_fitfiles, c("1":"329"), ".fit")

n = 329
datalist = list()
datalist = vector("list", length = n)

for (i in 1:n) {
  datalist[[i]] <- data_import(list_of_fitfiles[i])
}

marathon_df <- bind_rows(datalist)
```

##### *Data Cleaning*

After importing, the data frame was then cleaned to include activities
from the start of marathon training (March 31, 2022) to the day of the
New York City Marathon (November 6, 2022). Data were…

Other decisions we need to make:

\[ \] Need to decide if we want to keep each observation as a single
second for each activity

\[ \] Need to decide if we want to focus solely on training runs or
include walks and other activities (elliptical or biking)

<br/>

#### Exploratory Analysis: Visualizations, summaries, and exploratory statistical analyses. Justify the steps you took, and show any major changes to your ideas.

<br/>

#### Additional Analysis: If you undertake formal statistical analyses, describe these in detail

<br/>

#### Discussion: What were your findings? Are they what you expect? What insights into the data can you make?