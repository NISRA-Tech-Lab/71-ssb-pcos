# SSB PCOS RAP

## Checklist

Carry out the following updates each year to create new publication:

-   In your remote data directory place the raw SPSS data in the `Raw` folder

-   If new ONS data is available, place the .xlsx file in the `ONS` folder

-   Take a copy of the entire R Project by [duplicating the previous year's repository](https://github.com/new/import) on Github and name it for the new year.

-   Open RStudio. Click __File__ > __New Project__ > __Version Control__ > __Git__ and paste the new repository's URL in. Save your local clone of the Git repository under `C:\Users\USERNAME\Documents\R`.

-   In `code\config.R` file, update the following values:

    -   `data_filename` - This is the file name of the SPSS data that is stored in the `Raw` folder
    -   `password` - The password needed to open the Raw data
    -   `ons_filename` - The file name of the XLSX data for ONS that is stored in the `ONS` folder
    -   `current_year` - The reporting year for NISRA survey
    -   `ons_year` - The reporting year for ONS survey
    -   `pub_date` - Date of publication in "DD Mmm YYYY" format
    -   Check all other parameters are still correct. Adjust if needed.

-   Rename the following files for current year:

    -   `public-awareness-of-and-trust-in-official-statistics-northern-ireland-YYYY.Rmd`

    -   `public-awareness-of-and-trust-in-official-statistics-northern-ireland-YYYY-appendix-b.Rmd`

    -   `public-awareness-of-and-trust-in-official-statistics-northern-ireland-YYYY-appendix-c.Rmd`

-   Knit first draft of `public-awareness-of-and-trust-in-official-statistics-northern-ireland-YYYY.Rmd`.

-   After knitting report:

    -   QA the report and make any necessary changes in the Rmd document itself.
    -   Ensure that all automated text makes sense.

-   Knit updated `public-awareness-of-and-trust-in-official-statistics-northern-ireland-YYYY.Rmd` for final draft.

## Onloading a new user

New users will require the following software from the ITAssist Store (icon on your Desktop):

-   R version 4.4
-   RStudio
-   Inkscape

New users will also need a service request submitted to have the following software installed:

-   [Git for Windows](https://git-scm.com/download/win)

### Git

For more information on Git see the [RAP Skeleton Git infomation section](https://github.com/NISRA-Tech-Lab/rap-skeleton?tab=readme-ov-file#storing-your-r-project-in-a-github-repository)

Before you can clone a local copy of this Repository, first open RStudio and click on the Terminal tab.

Enter the following lines of code in the R terminal (one at a time, pressing Enter after each line):

```         
git config --global http.sslVerify false
git config --global http.proxy http://cloud-lb.nigov.net:8080
git config --global https.proxy https://cloud-lb.nigov.net:8080
git config --global user.name "YourUsername"
git config --global user.email firstname.lastname@nisra.gov.uk
```
### Cloning the repo

Click __File__ > __New Project__ > __Version Control__ > __Git__ and paste this repository's URL in. Save your local clone of the Git repository under `C:\Users\USERNAME\Documents\R`.

__To revisit the Project after closing RStudio, double click on the .Rproj file contained in the Project folder.__

### install necessary packages using renv

In RStudio, switch from the "Terminal" tab to the "Console" tab. Enter the code in the Console:

`renv::restore()`

and follow any on screen prompts in the Console.

## Key files and folders

### [code](code)

The code folder contains a number of sub-folders (detailed below) as well as some scripts:

-   [config.R](code/config.R) - This is where global settings can be declared. This file is referenced in a number of other scripts throughout the project. For more information see [the Checklist](#checklist)

-   [cookies.js](code/cookies.js) - This script enables the cookies prompt users will see when they first visit the report on the datavis server. Google Analytics page views are enabled through this script. Information on number of page hits can be obtained by contacting Dissemination Branch.

-   [path_to_data.R](code/path_to_data.R) - This should contain only the folder location of the data folder on your remote network drive. Folder paths should be given inside quotes, use forward slashes __( / )__ and the last character should be a forward slash.

-   [style.css](code/style.css) - Cascading Style Sheets (CSS) is used to format the layout of a html webpage. With CSS, you can control the color, font, the size of text, the spacing between elements, how elements are positioned and laid out, what background images or background colors are to be used, different displays for different devices and screen sizes, and much more. For more information see [CSS Tutorial](https://www.w3schools.com/css/default.asp), [HTML Styles - CSS](https://www.w3schools.com/html/html_css.asp) and [Apply custom CSS](https://bookdown.org/yihui/rmarkdown-cookbook/html-css.html)

### [code/functions](code/functions)

All of the functions used throughout the project are defined in this folder. This folder is automatically sourced any time `config.R` is called. The folder also contains the script `Styles.R` which provides definitions of individual cell formats for Excel (or ODS) based outputs.

### [code/html_publication](code/html_publication)

This directory contains all the code relating to the production of the main HTML Report. The diagram below outlines the process which takes place when you click __Knit__ on the main Rmd file. 

2022 has been used as an example year in the diagram below. In order to follow the process, start in the middle column of the diagram and read down the page.

<img src="data/images/Data%20flow.svg">

#### [public-awareness-of-and-trust-in-official-statistics-northern-ireland-2022.Rmd](code/html_publication/public-awareness-of-and-trust-in-official-statistics-northern-ireland-2022.Rmd)

In order to output a new version of the report:

1. Rename the Rmd file for the new year. The filename of the Rmd will also be the filename of the resulting HTML document.
2. Ensure `config.R` script is up to date.
3. Update any commentary text.
4. Save and click __Knit__.
 
##### Charts in the report

For all charts in the code the R charting package **Plotly** is used. Some helpful resources are below:

-   [Plotly R Open Source Graphing Library](https://plotly.com/r/#:~:text=Plotly's%20R%20graphing%20library%20makes,3D%20(WebGL%20based)%20charts.)
-   [Plotly R Library Basic Charts](https://plotly.com/r/basic-charts/)
-   [Plotly R Library Statistical Charts](https://plotly.com/r/statistical-charts/)
-   [GitHub Plotly](https://gist.github.com/aagarw30/800c4da26eebbe2331860872d31720c1)
-   [Cheatsheets for Plotly](https://images.plot.ly/plotly-documentation/images/r_cheat_sheet.pdf)

The code chunk for each chart generally follows a similar pattern. Normally a data frame created in the `data_prep.R` script is fed into the `plotly` function and named, for example `aware_nisra_data`. Chart parameters are set like chart `type`, `x` and `y` variables, `markers` and `line` aesthetics like font, size and colour. Other details about the chart like name, label text and [hover text](https://plotly.com/r/hover-text-and-formatting/) are set next. In charts with multiple lines or bars the [add_trace](https://plotly.com/r/creating-and-updating-figures/#adding-traces) function is used to add additional variables.

Following this there is a layout section that sets the aesthetics of the chart as a whole like axis styling, titles, legends, margins, fonts etc. [`layout` attributes in plotly](https://plotly.com/r/reference/layout/). Where required [chart annotations](https://plotly.com/r/text-and-annotations/) are added in the layout section. Annotations are used to label lines, bars and as axis titles to help meet accessibility requirements. After the layout section there is a line of code `config(displayModeBar = FALSE)` that removes plotly logos and other unnecessary tools. Finally the chart is called in the code chunk by displaying the name e.g. `chart_2_data`.

There are many different attributes that are used in the code that help to style and format the charts. The links below have more information on the different types of charts in the report and their corresponding styling attributes.

-   [Line Plots in R](https://plotly.com/r/line-charts/)
-   [Line chart plolty R attributes](https://plotly.com/r/reference/)
-   [Bar Charts in R](https://plotly.com/r/bar-charts/)
-   [Bar chart plolty R attributes](https://plotly.com/r/reference/bar/)
-   [Horizontal Bar Charts in R](https://plotly.com/r/horizontal-bar-charts/)

#### [data_prep.R](code/html_publication/data_prep.R)

This script performs a number of operations:

-   It first reads in the raw SPSS data file for NISRA and performs the necessary recodes.
-   It also reads the formatted ONS Excel file containing values.
-   It then outputs the variable checks into the project's __outputs__ folder.
-   It analyses the data and creates individual data frames for each of the charts in the Report.
-   It outputs the supplementary tables to the __outputs__ folder.

New variable recodes, data frames for charts and commentary values should be added to this script.

There is no need to run this script at any point as it will be called by `data_prep_for_ons.R`.

#### [check_raw_variables.R](code/html_publication/check_raw_variables.R)

This script produces frequency tables on raw variables as well as crosstabs comparing PCOS1c and PCOS1d with PCOS1.

There is no need to run this script as it is called at a specific point in the running of `data_prep.R`.

#### [check_created_variables.R](code/html_publication/check_created_variables.R)

This script produces frequency tables on recoded variables as well as crosstabs comparing the recoded variables with the raw variables.

There is no need to run this script as it is called at a specific point in the running of `data_prep.R`.

#### [supplementary_tables.R](code/html_publication/supplementary_tables.R)

This script produces crosstabs analysing our recoded variables with `AGE2`, `DERHIanalysis` and `EMPST2`.

There is no need to run this script as it is called at a specific point in the running of `data_prep.R`.

#### [trend_data_for_charts.R](code/html_publication/trend_data_for_charts.R)

This script reads in the `PCOS 2022 Charts.xlsx` file provided by SSB and converts each table into separate .RDS files.

Each .RDS file is stored in the `Trend/2021` directory on the remote drive. These are used during the analysis of the __2022__ data. The results of that analysis are saved in the folder `Trend/2022`. In 2023, that data will be used to formulate the trend data going and so on...

This script should only be run once. There is no need to run this script manually as it will be automatically triggered in `data_prep.R` in the event the 2021 trend data is missing from the remote drive.

#### [public-awareness-of-and-trust-in-official-statistics-northern-ireland-2022-appendix-b.Rmd](code/html_publication/public-awareness-of-and-trust-in-official-statistics-northern-ireland-2022-appendix-b.Rmd)

In order to output a new version of the Appendix B HTML document:

1. Rename the Rmd file for the new year. The filename of the Rmd will also be the filename of the resulting HTML document.
2. Ensure `config.R` script is up to date.
3. Add any new tables to the appropritate place in the document.
4. Save and click __Knit__.

#### [public-awareness-of-and-trust-in-official-statistics-northern-ireland-2022-appendix-c.Rmd](code/html_publication/public-awareness-of-and-trust-in-official-statistics-northern-ireland-2022-appendix-c.Rmd)

In order to output a new version of the Appendix C HTML document:

1. Rename the Rmd file for the new year. The filename of the Rmd will also be the filename of the resulting HTML document.
2. Ensure `config.R` script is up to date.
3. Add any new questions to the appropriate place in  document.
4. Save and click __Knit__.

#### [background-quality-report.Rmd](code/html_publication/background-quality-report.Rmd)

In order to output a new version of the Background Quality Report HTML document:

 1. Ensure `config.R` script is up to date.
 2. Update text in Rmd file.
 3. Save and click __Knit__.

### [code/infographic](code/infographic)

This folder contains code and templates needed to produce infographics. The infographics are output as PDF files.

The three template files:

-    Awareness - Infographic Template.svg
-    Overview - Infographic Template.svg
-    Trust - Infographic Template.svg

The templates can be edited using [Inkscape](https://inkscape.org/) (available on ITAssist Store). You can only make edits to the overall layout of the infographic canvas here. Adjusting individuaol graphics is done elsewhere in the code.

#### [infographic prep.R](code/infographic/infographic%20prep.R)

This script is used to prepare data frames that will be used to plot the various charts that appear in the three infographics.

There is no need to run this script as it is called at a specific point in the running of `infographic charts.R`.

#### [infographic charts.R](code/infographic/infographic%20charts.R)

This script takes the data frames created in `infographic prep.R` and produces charts using [ggplot2](https://ggplot2.tidyverse.org/reference/). It also uses the content of the data frames to output alt text on each figure.

The charts are then embedded in the infogrpahic templates, along with the alt text and these are then saved as PDF files in the `outputs` folder. The individual infographics are also saved in the outputs folder as .png images where they can be used in social media posts.

### [code/ministerial_sub](code/ministerial_sub)

Code in this folder produces the Ministerial Submission in Word Document format with automated values from the data. It can then be reviewed and saved to PDF.

#### [ministerial_sub_template.docx](code/ministerial_sub/ministerial_sub_template.docx)

This is a reference document that is used by the Rmd in this folder. Like CSS, it is used to set font styles for the Ministerial Submission.

#### [Submission - Public Awareness of and Trust in Official Statistics 2022.Rmd](code/ministerial_sub/Submission%20-%20Public%20Awareness%20of%20and%20Trust%20in%20Official%20Statistics%202022.Rmd)

When knitted this file will output the Ministerial Submisison in Word format. Where possible, values have been automated. Other areas of the text that may require further attention have been highlighed in red text.

### [code/ods_tables](code/ods_tables)

The code to produce the ODS tables is automatically executed when you Knit the main Rmd for the Report or for Appendix B. The scripts here can be amended to alter the ODS output.

#### [ods_trend_data.R](code/ods_tables/ods_trend_data.R)

A one-time run script to create trend data files in RDS format for all tables up to 2021. There are instructions elsewhere in the code to execute this script automatically should the 2021 Trend Data be missing in the remote location.

#### [data_prep_for_ods.R](code/ods_tables/data_prep_for_ods.R)

This script constructs data frames for all the tables in the ODS booklet using trend data from the previous year, current year NISRA data and the most recent year ONS data.

Trend values are appended to trend files for the current year so that they can be picked up the following year.

There is no need to run this script as it is called at a specific point in the running of `run_ods_tables.R`.

### [code/pfg_tables](code/pfg_tables)

The code in here will generate an Excel workbook with a tab for each of the questions required for the Data Portal.

#### [Historic Data to R.R](code/pfg_tables/Historic%20Data%20to%20R.R)

This script takes the final SPSS .SAV datasets from previous years (up to 2021) and converts them to RDS. Where required, variables have been recoded to match their 2022 formatting.

#### [PfG Table Output.R](code/pfg_tables/PfG%20Table%20Output.R)

This code will loop through the required variables, co-variates and available years' data to produce the Excel workbook containing the weighted values and their confidence intervals.

Learn more: [Creating & Formatting Excel Workbooks](https://datavis.nisra.gov.uk/techlab/yalcbs/Useful-R-Info.html#Excel_table_functions)

### [code/press_release](code/press_release)

Code in this folder produces the Press Release in Word Document format with automated values from the data. It can then be reviewed and saved to PDF.

#### [press_release_template.docx](code/ministerial_sub/press_release_template.docx)

This is a reference document that is used by the Rmd in this folder. Like CSS, it is used to set font styles for the Press Release.

#### [Public Awareness of and Trust in Official Statistics Press Release.Rmd](code/ministerial_sub/Public%20Awareness%20of%20and%20Trust%20in%20Official%20Statistics%20Press%20Release.Rmd)

When knitted this file will output the Press Release in Word format. Where possible, values have been automated. Other areas of the text that may require further attention have been highlighed in red text.

### [code/significance_testing](code/significance_testing)

There are two outputs produced: the exploratory significance output and the final significance output

#### [exploratory_significance_testing.R](code/significance_testing/exploratory_significance_testing.R)

This script will produce a list of Z Scores for a quick look at significance trends. Each questions appears in its own tab.

The main way to edit the inputs to this analysis is through the csv files and Excel document contained in the inputs folder.

The years being comapred can be changed setting the `comparison_year` and `analysis_year` values at the top of the script.

#### [weighted_trend.R](code/significance_testing/final_output/weighted_trend.R)

One time run to produce trend figures up to 2021, required for the significance analysis. This script will be executed automatically in `significance_testing.R` if the trend file is missing.

#### [significance_testing.R](code/significance_testing/final_output/weighted_trend.R)

This produces all the data frames required to output the final significance output. Data frames for new tables should be constructed in here. The functions used to output the data frames are found in [code/functions/significance_functions.R](code/functions/significance_functions.R)

This script is automatically execectured when `significance_outputs.R` is run.

#### [significance_outputs.R](code/significance_testing/final_output/significance_outputs.R)

This script will produce the final significance output workbook. Any data frames created in `significance_testing.R` need positioned on the page here.


### Links

The [NISRA Technology & Support Lab RAP Skeleton Git Page](https://github.com/NISRA-Tech-Lab/rap-skeleton) has lots of useful information that forms the basis for the development of this project.

A number of [cheatsheets](https://www.rstudio.com/resources/cheatsheets/) are available for R.

[RStudio IDE cheatsheet](https://raw.githubusercontent.com/rstudio/cheatsheets/main/rstudio-ide.pdf)

[Dynamic documents with rmarkdown cheatsheet](https://raw.githubusercontent.com/rstudio/cheatsheets/main/rmarkdown.pdf)

[Data transformation with dplyr cheatsheet](https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-transformation.pdf)

[Base R](http://github.com/rstudio/cheatsheets/blob/main/base-r.pdf)

[Data import with readr, readxl, and googlesheets4 cheatsheet](https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-import.pdf)

[Some useful tips for using the RStudio](https://www.dataquest.io/blog/rstudio-tips-tricks-shortcuts/) includes changing the appearance in global options.

Some useful [KeyBoard shortcuts](https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts-in-the-RStudio-IDE)
