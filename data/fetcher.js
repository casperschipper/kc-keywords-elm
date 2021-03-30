let fetchData = function (url) {
    return fetch(url).then(response => response.json());
}

let publishedUrl = "portal/search-result?fulltext=&title=&autocomplete=&keyword=&portal=6&statusprogress=0&statuspublished=0&statuspublished=1&includelimited=0&includelimited=1&includeprivate=0&type_research=research&resulttype=research&format=json&limit=250&page=0";

let publishedUrl2 = "portal/search-result?fulltext=&title=&autocomplete=&keyword=&portal=6&statusprogress=0&statuspublished=0&statuspublished=1&includelimited=0&includelimited=1&includeprivate=0&type_research=research&resulttype=research&format=json&limit=250&page=1";

let publishedUrl3 = "portal/search-result?fulltext=&title=&autocomplete=&keyword=&portal=6&statusprogress=0&statuspublished=0&statuspublished=1&includelimited=0&includelimited=1&includeprivate=0&type_research=research&resulttype=research&format=json&limit=250&page=2";

let lectorate = "portal/search-result?fulltext=&title=&autocomplete=&keyword=KonCon+Lectorate&portal=&statusprogress=0&statusprogress=1&statuspublished=0&includelimited=0&includeprivate=0&type_research=research&resulttype=research&format=json&limit=50&page=0";

let sonology = "portal/search-result?fulltext=&title=&autocomplete=&keyword=sonology&portal=6&statusprogress=0&statusprogress=1&statuspublished=0&includelimited=0&includeprivate=0&type_research=research&resulttype=research&format=json&limit=50&page=0";

let fetchAll = function () {
    let jsonPromises = [publishedUrl,publishedUrl2,publishedUrl3,sonology].map(fetchData) ;
    Promise.all(jsonPromises).then(values => { console.log(values);
	                                       let flat = values.flat();
					       console.log(JSON.stringify(flat)); });
};

fetchAll();
