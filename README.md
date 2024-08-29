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

### [code/functions](code/functions)

f_banner(), f_header() and f_borderline() are functions used to create the html banner including logos and title, the Report header and the borderline used between each section. They can be edited within the `html_formatting.R` file in the functions folder. Each of these functions are called in the Report Rmd using inline code e.g.

```         
`r f_banner()`  
`r f_header()`
`r f_borderline()`
```

f_contact() is also in the `html_formatting.R` file and is used to create the contact section near the bottom of the report.

The f_email() function in the `f_email.R` script makes an email a live link.

### [code/html_publication](code/html_publication)

<img src="data/images/Data%20flow.svg">

#### Charts

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
-   [Horizontal Bar Charts in R](https://plotly.com/r/horizontal-bar-charts/)

#### Download Buttons

### [code/infographic](code/infographic)

### [code/ministerial_sub](code/ministerial_sub)

### [code/ods_tables](code/ods_tables)

In order to produce the accompanying ODS outputs for this report, follow these steps:

-   Use Windows file explorer to open the R project file.
-   With the project open in R Studio, open the `excel tables/run_excel_tables.R` file.
-   Select all code and press `Run`
-   Any produced Excel document will be saved in the `outputs` folder.

Learn more: [Creating & Formatting Excel Workbooks](https://datavis.nisra.gov.uk/techlab/yalcbs/Useful-R-Info.html#Excel_table_functions)









### Other Project Files and Features








#### Cascading Style Sheets (CSS) file

Cascading Style Sheets (CSS) is used to format the layout of a html webpage. For the report there is `style.css` file in the code folder that can be updated to edit elements of the html report. With CSS, you can control the color, font, the size of text, the spacing between elements, how elements are positioned and laid out, what background images or background colors are to be used, different displays for different devices and screen sizes, and much more. For more information see [CSS Tutorial](https://www.w3schools.com/css/default.asp), [HTML Styles - CSS](https://www.w3schools.com/html/html_css.asp) and [Apply custom CSS](https://bookdown.org/yihui/rmarkdown-cookbook/html-css.html)





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
