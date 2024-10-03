# Styles for Page titles
pt <- createStyle(
  textDecoration = "bold",
  fontSize = 15,
  valign = "top"
)

pt_blue <- createStyle(
  textDecoration = "bold",
  fontSize = 15,
  valign = "top",
  fontColour = "#0066FF"
)

pt2 <- createStyle(
  textDecoration = "bold"
)

# Styles for column headers
ch <- createStyle(
  halign = "right",
  valign = "bottom",
  wrapText = TRUE,
  textDecoration = "bold"
)

ch_lined <- createStyle(
  halign = "right",
  valign = "bottom",
  wrapText = TRUE,
  textDecoration = "bold",
  border = "TopBottom",
  borderStyle = c("thin", "double")
)

ch_lined_left <- createStyle(
  halign = "left",
  valign = "bottom",
  wrapText = TRUE,
  textDecoration = "bold",
  border = "TopBottom",
  borderStyle = c("thin", "double")
)

# Style for column header (left aligned)
chl <- createStyle(
  halign = "left",
  valign = "bottom",
  wrapText = TRUE,
  textDecoration = "bold"
)

# Style for column header (center)
ch2 <- createStyle(
  halign = "center",
  valign = "bottom",
  wrapText = TRUE,
  textDecoration = "bold"
)

# Style for numbers
ns <- createStyle(
  numFmt = "#,##0",
  halign = "right"
)

ns_bold <- createStyle(
  numFmt = "#,##0",
  halign = "right",
  textDecoration = "bold"
)

ns_italic <- createStyle(
  numFmt = "#,##0",
  halign = "right",
  textDecoration = "italic",
  border = "TopBottom",
  borderStyle = c("thin", "thin")
)



# Style for decimal numbers
nsd <- createStyle(
  numFmt = "#,###.##",
  halign = "right"
)

ns3d <- createStyle(
  numFmt = "#,##0.000",
  halign = "right"
)

ns1d <- createStyle(
  numFmt = "#,##0.0",
  halign = "right"
)


ns_comma <- createStyle(
  numFmt = "#,##0",
  halign = "right"
)

ns_percent <- createStyle(
  numFmt = "#0.0",
  halign = "right"
)

num_resp <- createStyle(
  textDecoration = c("bold", "italic"),
  border = "TopBottom",
  borderStyle = c("thin", "thin")
)


# Style to right align cells
ra <- createStyle(halign = "right")

rab <- createStyle(halign = "right",
                   textDecoration = "bold")

# Style to left align cells
la <- createStyle(halign = "left")

# Style for contents
tw <- createStyle(wrapText = TRUE)

# Style for left aligned text headings
h3 <- createStyle(
  halign = "left",
  textDecoration = "bold",
  fontSize = 13
)
# small sample size style
sh <- createStyle(halign = "right", fgFill = "#d3d3d3")

sh_comma <- createStyle(halign = "right", fgFill = "#d3d3d3", numFmt = "#,##0")

sh_percent <- createStyle(halign = "right", fgFill = "#d3d3d3", numFmt = "#0.0")

# Style for column headers needed for maketable function
hs <- createStyle(
  halign = "right",
  wrapText = TRUE,
  textDecoration = "bold"
)

hs2 <- createStyle(
  halign = "left",
  wrapText = TRUE,
  textDecoration = "bold"
)

ts <- createStyle(
  textDecoration = "bold",
  fontSize = 14
)

# significance styles
sig <- createStyle(
  numFmt = "#,##0.000",
  halign = "right",
  fgFill = "#C6E0B4"
)

not_sig <- createStyle(
  numFmt = "#,##0.000",
  halign = "right",
  fgFill = "#FCD5B4"
)

grey <- createStyle(
  fgFill = "#D9D9D9"
)

# White text
wt <- createStyle(
  fontColour = "#ffffff"
)

ns_pfg <- createStyle(
  numFmt = "#,##0.0",
  halign = "center"
)
