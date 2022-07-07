# kc-keywords-elm

Elm based keyword display of publications and reserach of the Royal Conservatoire in The Hague.
You can view this application here:
[KonCon Research Results Keyword Page](https://www.researchcatalogue.net/view/496747/496748)

The raw data (data/internal_research.json) is fetched from the search function of the RC:
https://www.researchcatalogue.net/portal/search-result?fulltext=&title=&autocomplete=&keyword=&portal=6&statusprogress=0&statusprogress=1&statuspublished=0&statuspublished=1&includelimited=0&includelimited=1&includeprivate=0&type_research=research&resulttype=research&format=json&limit=250&page=0

You will have to install elm to build and run this program:
<https://guide.elm-lang.org/install/elm.html>

    $ elm reactor 
    
 and go to localhost:8000/index.html
 
To make a new version you will have to do:

`elm make src/Main.elm`


