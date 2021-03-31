/* these can be replaced with any advanced searches */

let publishedUrl = "portal/search-result?fulltext=&title=&autocomplete=&keyword=&portal=6&statusprogress=0&statuspublished=0&statuspublished=1&includelimited=0&includelimited=1&includeprivate=0&type_research=research&resulttype=research&format=json&limit=250&page=0";

let publishedUrl2 = "portal/search-result?fulltext=&title=&autocomplete=&keyword=&portal=6&statusprogress=0&statuspublished=0&statuspublished=1&includelimited=0&includelimited=1&includeprivate=0&type_research=research&resulttype=research&format=json&limit=250&page=1";

let publishedUrl3 = "portal/search-result?fulltext=&title=&autocomplete=&keyword=&portal=6&statusprogress=0&statuspublished=0&statuspublished=1&includelimited=0&includelimited=1&includeprivate=0&type_research=research&resulttype=research&format=json&limit=250&page=2";

let lectorate = "portal/search-result?fulltext=&title=&autocomplete=&keyword=KonCon+Lectorate&portal=&statusprogress=0&statusprogress=1&statuspublished=0&statuspublished=1&includelimited=0&includelimited=1&includeprivate=0&type_research=research&resulttype=research&format=json&limit=250&page=0";

let sonology = "portal/search-result?fulltext=&title=&autocomplete=&keyword=sonology&portal=6&statusprogress=0&statusprogress=1&statuspublished=0&includelimited=0&includeprivate=0&type_research=research&resulttype=research&format=json&limit=50&page=0";

let teachers = "portal/search-result?fulltext=&title=&autocomplete=&keyword=Research+by+teachers+of+the+Royal+Conservatoire&portal=&statusprogress=0&statusprogress=1&statuspublished=0&statuspublished=1&includelimited=0&includelimited=1&includeprivate=0&type_research=research&resulttype=research&format=json&limit=50&page=0"

function download(filename, text) {
    var pom = document.createElement('a');
    pom.setAttribute('href', 'data:application/json;charset=utf-8,' + encodeURIComponent(text));
    pom.setAttribute('download', filename);

    if (document.createEvent) {
        var event = document.createEvent('MouseEvents');
        event.initEvent('click', true, true);
        pom.dispatchEvent(event);
    }
    else {
        pom.click();
    }
}

let fetchData = function (url) {
    return fetch(url).then(response => response.json());
}

let fetchAll = function () {
    let jsonPromises = [publishedUrl,publishedUrl2,publishedUrl3,lectorate,sonology,teachers].map(fetchData) ;
    Promise.all(jsonPromises).then(values => { 
    	console.log(values);
        let flat = values.flat();
        let json = JSON.stringify(flat);
        download('internal-research.json',json);
    });
};

fetchAll();
