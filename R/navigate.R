library(rvest)

tryCatch2 = function(assignment, msg) {
	skip_to_next = FALSE
	tryCatch(x <- assignment, 
			 error = function(err) { print(paste0("error processing ", msg)); skip_to_next=TRUE }, 
			 warning = function(err) {},
			 message = function(err) {},
			 finally = function(err) {}
	)
	if (skip_to_next == TRUE) { next }
	return(x)
}


getAllPlayerIds <- function(countrycode=1){
	playerid <- c()
	base_url = "http://www.espncricinfo.com/ci/content/player/country.html?country=__CNTR__;alpha=__ALP__"
	base_url1 = sub("__CNTR__", countrycode, base_url)
	for (let in LETTERS)
	{
		base_url2 = sub( '__ALP__', let, base_url1)
		base_html <- tryCatch2(xml2::read_html(base_url2), base_url2)
		ahrefs <- tryCatch2(rvest::html_nodes(base_html, "div#ciHomeContent div#ciMainContainer div#ciHomeContentlhs div.ciPlayerbyCharCurvebg div.ciWPContainer div.ciPlayerbycapstable table tr table tr a"), base_url2)
		hrefs <- tryCatch2(rvest::html_attr(ahrefs, "href"),  base_url2)
		matched <- regexpr("\\d+", hrefs, perl=TRUE)
		x <- regmatches(hrefs, matched)
		playerid <- append(playerid, x, length(playerid))
	}
	return(playerid)
}
