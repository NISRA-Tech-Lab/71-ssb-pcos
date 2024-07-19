# SSB PCOS RAP

## Checklist

Carry out the following updates each year to create new publication:

-   In your remote data directory place the raw SPSS data in the `Raw` folder

-   If new ONS data is available, place the .xlsx file in the `ONS` folder

-   Take a copy of the entire R Project by [duplicating this repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/duplicating-a-repository) on Github and name it for the new year.

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

### Key files and folders

| File                        | Purpose                                                          |
|------------------------------------|------------------------------------|
| `code/`                     | Main RMarkdown report; Appendices; and Background Quality Report |
| `code/excel_tables`         | Code to produce excel tables booklet                             |
| `code/infographic`          | Code to produce infographics                                     |
| `code/significance testing` | Code to produce significance outputs                             |

### Main Report Rmarkdown file - public-awareness-of-and-trust-in-official-statistics-northern-ireland-YYYY.Rmd

This `Rmd` file is the key file that is used to build the html report. When a report is knitted it is saved out to the `outputs` folder.

### Config file - config.R

`config.R` is the configuration file for the html report. This file is where specific report parameters can be set including the name of the department (DoF in this case) to set colours, the current year for the report, report title and the statistic type (e.g, Accredited Statistic, Official Statistic etc). There is also a section that loads in the required R packages using the `library` command. If packages are not loading check they are installed using the `install.packages()` function.

The rest of the config file relates to the structure and set-up of the report including logos, colour schemes and loading functions. These sections should not need to be changed normally.

### Data Prep Script - data_prep.R

The data prep script loads all the data for the report from SPSS and Excel files in the data folder. All data frames for charts are created and coded into his file. In addition variables for automating the text are created also. The script has a number of sections (denoted by a series of hash marks) as follows:

-   **Reading data from SPSS:** The password protected SPSS data is read in to R and stored under the value `data_raw`
-   **Recode variables:** This section replicates all steps in the original SPSS process in order to
    -   recode variables;
    -   set refusals to missing;
    -   and remove any cases where a respondent refused to answer all questions
-   **Create data frames for charts:** Separate data frames containing only the data needed to produce a particular chart are created. The data for Chart 1 will be stored under the name `chart_1_data` etc. Any trend series data up to the previous year is read in at this point, and data points for the new year are appended.
-   **Figures for commentary:** Individual values are calculated from the data and stored so they can be inserted in the report's commentary.

### Charts

Within the report there are 15 charts. There is one line chart, one grouped bar chart and the remaining charts are stacked bar charts.

For all charts in the code the R charting package **Plotly** is used. Some helpful resources are below:

-   [Plotly R Open Source Graphing Library](https://plotly.com/r/#:~:text=Plotly's%20R%20graphing%20library%20makes,3D%20(WebGL%20based)%20charts.)
-   [Plotly R Library Basic Charts](https://plotly.com/r/basic-charts/)
-   [Plotly R Library Statistical Charts](https://plotly.com/r/statistical-charts/)
-   [GitHub Plotly](https://gist.github.com/aagarw30/800c4da26eebbe2331860872d31720c1)
-   [Cheatsheets for Plotly](https://images.plot.ly/plotly-documentation/images/r_cheat_sheet.pdf)

The code chunk for each chart generally follows a similar pattern. Normally a data frame created in the `data_prep.R` script is fed into the `plotly` function and named, for example `chart_2_data`. Chart parameters are set like chart `type`, `x` and `y` variables, `markers` and `line` aesthetics like font, size and colour. Other details about the chart like name, label text and [hover text](https://plotly.com/r/hover-text-and-formatting/) are set next. In charts with multiple lines or bars the [add_trace](https://plotly.com/r/creating-and-updating-figures/#adding-traces) function is used to add additional variables.

Following this there is a layout section that sets the aesthetics of the chart as a whole like axis styling, titles, legends, margins, fonts etc. [`layout` attributes in plotly](https://plotly.com/r/reference/layout/). Where required [chart annotations](https://plotly.com/r/text-and-annotations/) are added in the layout section. Annotations are used to label lines, bars and as axis titles to help meet accessibility requirements. After the layout section there is a line of code `config(displayModeBar = FALSE)` that removes plotly logos and other unnecessary tools. Finally the chart is called in the code chunk by displaying the name e.g. `chart_2_data`.

There are many different attributes that are used in the code that help to style and format the charts. The links below have more information on the different types of charts in the report and their corresponding styling attributes.

-   [Line Plots in R](https://plotly.com/r/line-charts/)
-   [Line chart plolty R attributes](https://plotly.com/r/reference/)
-   [Bar Charts in R](https://plotly.com/r/bar-charts/)
-   [Bar chart plolty R attributes](https://plotly.com/r/reference/bar/)
-   [Pie Charts in R](https://plotly.com/r/pie-charts/)
-   [Pie chart plolty R attributes](https://plotly.com/r/reference/pie/)
-   [Horizontal Bar Charts in R](https://plotly.com/r/horizontal-bar-charts/)

### Download Buttons

In the functions folder the `f_embed_xl` R Script contains a function called `f_embed_xl()` that creates the Excel and CSV download links that are under each of the charts in the main report as well as all the tables listed in Appendix B. The function generates a hyper-link that sits behind the download button and allows a user to click the button and download data from the corresponding chart into csv or excel format.

In the code this function is called after each figure and has four parameters fed into it: - data (the data frame - created in the `data_prep.R` script), - title (the title to be given to the downloaded file), - data_style (style applied to the excel file - styles can be viewed in the `Style.R` script), - data_dir (the file path the downloaded files should be stored in - in the example below the file path is set to the `figdata` folder within the `outputs` folder).

```         
f_make_tables(data = figure_2.1_dl,
              title = paste0("Figure 2.1: Number of farms and area farmed,
              Northern Ireland,", currentyear - 40, "-", currentyear),
              data_style = ns_comma,
              data_dir = paste0(here(), "/outputs/figdata/"))  
```

### Excel Workbook

In order to produce the accompanying Excel outputs for this report, follow these steps:

-   Use Windows file explorer to open the R project file.
-   With the project open in R Studio, open the `excel tables/run_excel_tables.R` file.
-   Select all code and press `Run`
-   Any produced Excel document will be saved in the `outputs` folder.

Learn more: [Creating & Formatting Excel Workbooks](https://datavis.nisra.gov.uk/techlab/yalcbs/Useful-R-Info.html#Excel_table_functions)

### Other Project Files and Features

#### Functions Folder

f_banner(), f_header() and f_borderline() are functions used to create the html banner including logos and title, the Report header and the borderline used between each section. They can be edited within the `html_formatting.R` file in the functions folder. Each of these functions are called in the Report Rmd using inline code e.g.

```         
`r f_banner()`  
`r f_header()`
`r f_borderline()`
```

f_contact() is also in the `html_formatting.R` file and is used to create the contact section near the bottom of the report.

The f_email() function in the `f_email.R` script makes an email a live link.

#### Footer

The footer for the report is written in html code located at the bottom of the `report.Rmd` from lines 172 to 208. The footer contains numerous required links for NISRA reports. It is possible to add to the code to create additional links and information if required.

#### gitignore

The `.gitignore` file tells Git which files to ignore when committing your project to the GitHub repository e.g. data files. For more information see [What is Gitignore](https://www.freecodecamp.org/news/gitignore-what-is-it-and-how-to-add-to-repo/) and [Git Ignore and .gitignore](https://www.w3schools.com/git/git_ignore.asp?remote=github)

#### Cascading Style Sheets (CSS) file

Cascading Style Sheets (CSS) is used to format the layout of a html webpage. For the report there is `style.css` file in the code folder that can be updated to edit elements of the html report. With CSS, you can control the color, font, the size of text, the spacing between elements, how elements are positioned and laid out, what background images or background colors are to be used, different displays for different devices and screen sizes, and much more. For more information see [CSS Tutorial](https://www.w3schools.com/css/default.asp), [HTML Styles - CSS](https://www.w3schools.com/html/html_css.asp) and [Apply custom CSS](https://bookdown.org/yihui/rmarkdown-cookbook/html-css.html)

#### Cookies Banner

The cookies banner is called in the html code (see below) at the top of the `report.Rmd`. This code is in a javascript file called `cookies_script.js`.

```         
<div id = "cookie-banner"></div>
<script src = "cookies_script.js"></script>
```

Google analytics are included in the cookies banner code and these analytics can be accessed from Dissemination Branch.

### Git

For more information on Git look at the [RAP Skeleton Git infomation section](https://github.com/NISRA-Tech-Lab/rap-skeleton?tab=readme-ov-file#storing-your-r-project-in-a-github-repository)

Enter the following lines of code in the R terminal:

```         
git config --global http.sslVerify false
git config --global http.proxy http://cloud-lb.nigov.net:8080
git config --global https.proxy https://cloud-lb.nigov.net:8080
git config --global user.name "YourUsername"
git config --global user.email firstname.lastname@nisra.gov.uk
```

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
