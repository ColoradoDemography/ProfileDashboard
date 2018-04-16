
<!-- Google Analytics Script -->
(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
  a=s.createElement(o), m=s.getElementsByTagName(o)[0];
  a.async=1;
  a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

 <!-- Google Tag Manager --> 
(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
   new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
   j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
   'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
})(window,document,'script','dataLayer','UA-116669751-1');


  ga('create', 'UA-116669751-1', 'auto');
  ga('send', 'pageview');
  
   $(document).on('change', 'level', function(e) {
    ga('send', 'event', 'widget', 'select level', $(e.currentTarget).val());
    });
  
   $(document).on('change', 'unit', function(e) {
    ga('send', 'event', 'widget', 'select location', $(e.currentTarget).val());
    });
  
   $(document).on('click', 'outputPDF', function() {
    ga('send', 'event', 'widget', 'download Report');
   });
  
 
  
