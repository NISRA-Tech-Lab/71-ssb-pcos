f_comment_box <- function(colour_style = "red", text_words = NA) {
  if (!report_final) {
    div(
      p(class = paste0("statusbox_", colour_style), text_words)
    )
  }
}


f_borderline <- function() {
  div(
    div(class = "row", style = "display: flex; margin-top: 15px;
        margin-bottom: 10px"),
    div(class = "border", style = paste0(
      "background-color: var(--nics-three",
      "-bar-colour-1); float:left"
    )),
    div(class = "border", style = paste0(
      "background-color: var(--nics-three",
      "-bar-colour-2); float:left"
    )),
    div(class = "border", style = paste0(
      "background-color: var(--nics-three",
      "-bar-colour-3); float:right"
    )),
  )
}


if (!exists("nicstheme")) {
  departmental_link <- NULL
} else if (nicstheme == "dof") {
  departmental_link <- "https://www.finance-ni.gov.uk/topics/statistics-and-
  research-0"
} else if (nicstheme == "teo") {
  departmental_link <- "https://www.executiveoffice-ni.gov.uk/articles/national-
  statistics-protocol-and-compliance"
} else if (nicstheme == "daera") {
  departmental_link <- "https://www.daera-ni.gov.uk/landing-pages/statistics"
} else if (nicstheme == "dfc") {
  departmental_link <- "https://www.nisra.gov.uk/support/geography/neighbourhood
  -renewal-areas-dfc"
} else if (nicstheme == "de") {
  departmental_link <- "https://www.nisra.gov.uk/statistics/children-education-
  and-skills"
} else if (nicstheme == "dfe") {
  departmental_link <- "https://www.executiveoffice-ni.gov.uk/articles/national-
  statistics-protocol-and-compliance"
} else if (nicstheme == "dfi") {
  departmental_link <- "https://www.nisra.gov.uk/statistics/travel-and-
  transport"
} else if (nicstheme == "doh") {
  departmental_link <- "https://www.nisra.gov.uk/statistics/health-and-social-
  care/health-and-social-care-statistics"
} else if (nicstheme == "doj") {
  departmental_link <- "https://www.nisra.gov.uk/statistics/crime-and-
  justice/justice"
} else if (nicstheme == "bso") {
  departmental_link <- "https://www.nisra.gov.uk/contacts/business-services-
  organisation-information-unit"
} else {
  departmental_link <- NULL
}

f_banner <- function(subtitle = "") {
  div(
    div(
      style = "background-color: var(--nics-banner-bg); padding: 10px",
      div(
        class = "grid mtb",
        div(style = "display: flex; justify-content: center;
            align-items: center;", a(
          href = "https://nisra.gov.uk",
          img(
            src = nisra_logo,
            alt = "NISRA logo",
            width = "200px",
            height = "80px"
          )
        )),
        if (statistic_type == "as") {
          div(
            style = "display: flex; justify-content: center",
            img(src = acc_official_stats, alt = nat_alt, width = "100px")
          )
        },
        div(
          style = "display: flex; justify-content: center",
          a(
            href = departmental_link,
            img(src = dep_logo, alt = dep_alt, width = "200px")
          )
        )
      ),
      div(
        style = "display: flex; justify-content: center; text-align: center;",
        p(style = "color: #ffffff; font-size: 30px;
            text-transform: capitalize;", class = "toc-ignore", title)
      ),
      div(style = "font-size: 18px; color: #ffffff; display: flex;
          justify-content: center;  text-align: center;", subtitle)
    ),
    div(style = paste0("background-color: var(--nics-banner-highlight);
                       height: 9px; width: 100%;"))
  )
}

f_header <- function() {
  div(
    class = "header",
    if (pre_release == TRUE) {
      div(
        class = "prerelease-stripes", ".",
        div(
          class = "prerelease",
          p(style = "text-align: center", "OFFICIAL SENSITIVE - restricted
            official statistics"),
          p(style = "text-align: center", strong("For named individuals only -
                                                 do not forward or share")),
          p("Recipients are reminded that these are Official Statistics, which
            you have received pre-release access to under the Pre-Release Access
            to Official Statistics (NI) Order 2009.  Recipients of pre-release
            statistics are cautioned:"),
          p("- to ensure that the statistics, or any information based on them,
            or any indication of the content is not made available to anyone who
            has not been granted privileged access in advance of release;"),
          p("- not to seek changes to release dates; and"),
          p("- to make the statistician responsible for the statistics aware of
            any accidental release of the information to others immediately."),
          p("Wrongful release includes indications of the content, including
            descriptions such as 'favourable' or 'unfavourable'. Recipients
            should note that a list of those who receive privileged early access
            is publicly available on the website."),
          p("If you think you need to discuss and share with anyone not on the
            circulation list, first contact the lead statistician. Any
            accidental or wrongful release should be reported immediately and
            may lead to an inquiry.")
        )
      )
    } else {
      div()
    },
    div(
      class = "row", style = "display:flex",
      div(
        style = "width: 60%; padding-left:15px; font-size: 12pt;",
        strong("Status: "), a(href = "#status", statistic_type_text)
      ),
      div(
        style = "width: 40%; font-size: 12pt;",
        p(strong("Publication date: "), pub_date_words_dmy)
      )
    )
  )
}

f_contact <- function() {
  div(
    class = "header",
    div(
      class = "row", style = "display:flex",
      div(
        style = "width: 100%; padding-left:15px; font-size: 12pt;",
        p(strong("Published by: "), header_publisher),
        p(strong("Lead Statistician: "), lead_statistician),
        p(strong("Telephone: "), header_telephone),
        p(strong("Email: "), f_email(header_email))
      )
    )
  )
}

f_footer <- function() {
  HTML('
<footer style="color: #ffffff; background-color: #00205b;">
  <div class="col-wide" data-analytics="footer">
    <div class="row" style = "display: flex;">
      <div class="column left">
        <footerheading>Links</footerheading>
        <ul>
          <li><a href="https://www.nisra.gov.uk/" class="link" style="color: #ffffff">NISRA</a></li>
          <li><a href="https://www.nidirect.gov.uk" class="link" style="color: #ffffff">NIDirect</a></li>
          <li><a href="https://www.gov.uk/" class="link" style="color: #ffffff">GOV.UK</a></li>
          <li><a href="https://data.nisra.gov.uk/" class="link" style="color: #ffffff">NISRA Data Portal</a></li>
          <li><a href="https://build.nisra.gov.uk/" class="link" style="color: #ffffff">Census Flexible Table Builder</a></li>
        </ul>
      </div>
      <div class="column right">
        <footerheading>Follow NISRA</h3></footerheading>
        <ul>
          <li><a href="https://www.facebook.com/nisra.gov.uk" class="link" style="color: #ffffff">Facebook</a></li>
          <li><a href="https://twitter.com/NISRA" class="link" style="color: #ffffff">X (Twitter)</a></li>
          <li><a href="https://www.youtube.com/user/nisrastats" class="link" style="color: #ffffff">YouTube</a></li>
        </ul>
      </div>
    </div>
    <div class="flex-list">
      <ul>
        <li><a href="https://www.nisra.gov.uk/crown-copyright" class="link" style="color: #ffffff">&copy Crown Copyright</a></li>
        <li><a href="https://www.nisra.gov.uk/contact" class="link" style="color: #ffffff">Contact us</a></li>
        <li><a href="https://www.nisra.gov.uk/terms-and-conditions" class="link" style="color: #ffffff">Terms and conditions</a></li>
        <li><a href="https://www.nisra.gov.uk/cookies" class="link" style="color: #ffffff">Cookies</a></li>
        <li><a href="https://www.nisra.gov.uk/nisra-privacy-notice" class="link" style="color: #ffffff">Privacy</a></li>
        <li><a href="https://datavis.nisra.gov.uk/dissemination/accessibility-statement-visualisations.html" class="link" style="color: #ffffff">Accessibility Statement</a></li>
      </ul>
    </div>
  </div>
</footer>
')
}
