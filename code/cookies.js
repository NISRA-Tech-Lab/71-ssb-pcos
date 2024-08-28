let cookieBanner = document.getElementById('cookie-banner');
cookieBanner.style.display = "none";

if (window.location.protocol != "file:") {

  cookieBanner.classList.add("cookies-infobar");
  cookieBanner.style.width = "100%";
  cookieBanner.style.position = "fixed";
  cookieBanner.role = "cookies-information";
  
  cookieBanner.innerHTML =
    '<div class="container-fluid main-container">' +
  '<p><strong>Cookies on the Northern Ireland Statistics and Research Agency website</strong></p>' +
  '<p>This prototype web page places small amounts of information known as cookies on your device' +
  '. <a href="https://www.nisra.gov.uk/cookies" class="cookiesbarlink"><u>Find out more ' +
  'about cookies</u></a>.</p>' +
  '<button id="accept-cookies" class="cookies-infobar_btn">Accept cookies</button>' +
  '<button id="reject-cookies" class="cookies-infobar_btn_reject">Reject cookies</button>' +
  '</div>';
  
  function loadGoogleAnalytics() {
  
      (function(w, d, s, l, i){
        w[l] = w[l]||[];
        w[l].push({'gtm.start': new Date().getTime(),
                    event:'gtm.js'});
        var f = d.getElementsByTagName(s)[0],
            j = d.createElement(s),
            dl = l !='dataLayer'?'&l='+l: '';
        j.async = true;
        j.src = 'https://www.googletagmanager.com/gtm.js?id=' + i + dl;
        f.parentNode.insertBefore(j,f);
    })
    (window,document,'script','dataLayer','GTM-KF6WGSG');
  
  }
  
  today = new Date();
  
  document.getElementById('accept-cookies').onclick = function() {
    localStorage.setItem('cookie_answered', true);
    localStorage.setItem('cookie_date', today);
    cookieBanner.style.display = 'none';
    loadGoogleAnalytics();
  }
  
  document.getElementById('reject-cookies').onclick = function() {
    localStorage.setItem('cookie_answered', true);
    localStorage.setItem('cookie_date', today);
    cookieBanner.style.display = 'none';
  }
  
  function showCookieBanner() {
  
    diff = (today - new Date(localStorage.cookie_date)) / 1000 / 60 / 60 / 24;
  
    if (diff > 365) {
      localStorage.removeItem("cookie_answered");
      localStorage.removeItem("cookie_date");
    }
  
    if (!localStorage.cookie_answered) {
      cookieBanner.style.display = 'block';
    }
  };
  
  window.setInterval(function() {
    if (document.cookie == "") {
      localStorage.removeItem("cookie_answered");
      localStorage.removeItem("cookie_date");
    }
    showCookieBanner();
  } , 100)

}