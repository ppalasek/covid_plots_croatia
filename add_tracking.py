from bs4 import BeautifulSoup

from pathlib import Path

html_dir = Path('html')


for f in html_dir.glob('*.html'):
    print(f)
    
    with open(str(f), 'r', encoding='utf8') as html:
        soup = BeautifulSoup(html, 'lxml')
        
        ga = soup.new_tag('script')
        ga.string="""(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
              (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
              m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
              })(window,document,'script','//www.google-analytics.com/analytics.js','ga');
              ga('create', 'UA-201055349-1', 'auto');
              ga('send', 'pageview');"""

        soup.html.head.insert(0, ga)
        
        with open(str(f), "wb") as f_output:
            f_output.write(soup.prettify("utf-8"))
