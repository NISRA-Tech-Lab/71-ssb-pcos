# SSB PCOS RAP

## Checklist

Carry out the following updates each year to create new publication:

-   In your remote data directory place the raw SPSS data in the `Raw` folder

-   If new ONS data is available, place the .xlsx file in the `ONS` folder

-   Take a copy of the entire R Project by [duplicating this repository](https://github.com/new/import) on Github and name it for the new year.

-   In `code\config.R` file, update the following values:

    -   `data_filename` - This is the file name of the SPSS data that is stored in the `Raw` folder
    -   `password` - The password needed to open the Raw data
    -   `ons_filename` - The file name of the XLSX data for ONS that is stored in the `ONS` folder
    -   `current_year` - The reporting year for NISRA survey
    -   `ons_year` - The reporting year for ONS survey
    -   `title` - The title to appear at the top of the document
    -   `pub_date` - Date of publication in "DD Mmm YYYY" format
    -   Check all other parameters are still correct. Adjust if needed.

-   Rename the following files for current year:

    -   `public-awareness-of-and-trust-in-official-statistics-northern-ireland-YYYY.Rmd`

    -   `public-awareness-of-and-trust-in-official-statistics-northern-ireland-YYYY-appendix-b.Rmd`

    -   `public-awareness-of-and-trust-in-official-statistics-northern-ireland-YYYY-appendix-c.Rmd`

-   Knit first draft of `public-awareness-of-and-trust-in-official-statistics-northern-ireland-YYYY.Rmd`.

-   After knitting report:

    -   QA the report and make any necessary changes.
    -   Ensure that all automated text makes sense.

-   Knit updated `report.Rmd` for final draft.

-   Run `excel tables/run_excel_tables.R` script.

## Onloading a new user

### Git

For more information on Git look at the [RAP Skeleton Git infomation section](https://github.com/NISRA-Tech-Lab/rap-skeleton?tab=readme-ov-file#storing-your-r-project-in-a-github-repository)

Enter the following lines of code in the R terminal:

```         
git config --global http.sslVerify false
git config --global http.proxy http://cloud-lb.nigov.net:8080
git config --global https.proxy https://cloud-lb.nigov.net:8080
git config --global user.name "YourUsername"
git config --global user.email firstname.lastname@nisra.gov.uk
git update-index --assume-unchanged code/path_to_data.R
```

## Key files and folders

### [code](code)

The code folder contains a number of sub-folders (detailed below) as well as some scripts:

-   [config.R](code/config.R) - This is where global settings can be declared. This file is referenced in a number of other scripts throughout the project. For more information see [the Checklist](#checklist)

-   [cookies.js](code/cookies.js) - This script enables the cookies prompt users will see when they first visit the report on the datavis server. Google Analytics page views are enabled through this script. Information on number of page hits can be obtained by contacting Dissemination Branch.

-   [path_to_data.R](code/path_to_data.R) - This should contain only the folder location of the data folder on your remote network drive. Folder paths should be given inside quotes, use forward slashes __( / )__ and the last character should be a forward slash.

-   [style.css](code/style.css) - Cascading Style Sheets (CSS) is used to format the layout of a html webpage. With CSS, you can control the color, font, the size of text, the spacing between elements, how elements are positioned and laid out, what background images or background colors are to be used, different displays for different devices and screen sizes, and much more. For more information see [CSS Tutorial](https://www.w3schools.com/css/default.asp), [HTML Styles - CSS](https://www.w3schools.com/html/html_css.asp) and [Apply custom CSS](https://bookdown.org/yihui/rmarkdown-cookbook/html-css.html)

### [code/functions](code/functions)

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

#### [public-awareness-of-and-trust-in-official-statistics-northern-ireland-2022-appendix-b.Rmd](code/public-awareness-of-and-trust-in-official-statistics-northern-ireland-2022-appendix-b.Rmd)

In order to output a new version of the Appendix B HTML document:

1. Rename the Rmd file for the new year. The filename of the Rmd will also be the filename of the resulting HTML document.
2. Ensure `config.R` script is up to date.
3. Add any new tables to the appropritate place in the document.
4. Save and click __Knit__.

#### [public-awareness-of-and-trust-in-official-statistics-northern-ireland-2022-appendix-c.Rmd](code/public-awareness-of-and-trust-in-official-statistics-northern-ireland-2022-appendix-c.Rmd)

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

### [code/ministerial_sub](code/ministerial_sub)

### [code/ods_tables](code/ods_tables)

In order to produce the accompanying ODS outputs for this report, follow these steps:



Learn more: [Creating & Formatting Excel Workbooks](https://datavis.nisra.gov.uk/techlab/yalcbs/Useful-R-Info.html#Excel_table_functions)

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
