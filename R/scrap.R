library(rvest)
library(R6)
library(stringr)
library(xml2)

Scrap <- R6::R6Class("Scrap",
	public = list(
		initialize = function(id){
			private$id = id
			private$base_url = "http://www.espncricinfo.com/india/content/player/__ID__.html"
			private$profile_url = stringr::str_replace(private$base_url, "__ID__", private$id)
			private$profile_html <- tryCatch2(xml2::read_html(private$profile_url), private$id)
		},
		finalize = function() {
			id = ""
			base_url = ""
			profile_url = ""
			profile_html = ""
			x = ""
			print(paste("finalize from Scrap ", " "))
		},
		tryCatch2 = function(assignment, id) {
			private$id <- id
			skip_to_next = FALSE
			private$x <- ""
			tryCatch(private$x <- assignment, 
				 error = function(err) { print(paste0("error processing ", id)); skip_to_next=TRUE }, 
				 warning = function(err) {},
				 message = function(err) {},
				 finally = function(err) {}
			)
			if (skip_to_next == TRUE) { next }
			return(private$x)
		},
		getPlayerIndex = function(){
			private$x=""
			private$x <- tryCatch2(rvest::html_nodes(private$profile_html, "div#ciHomeContent div#ciMainContainer div#ciHomeContentlhs div.pnl490M div.ciPlayernametxt div h1"), private$id)
			private$x <- rvest::html_nodes(private$profile_html, "div#ciHomeContent div#ciMainContainer div#ciHomeContentlhs div.pnl490M div.ciPlayernametxt div h1")
			private$x <- as.character(rvest::html_text(private$x))
			shortname <- stringr::str_trim(stringr::str_remove_all(private$x, "[\n\r\t]"))
			
			private$x=""
			private$x <- tryCatch2(rvest::html_nodes(private$profile_html, "div#ciHomeContent div#ciMainContainer div#ciHomeContentlhs div.pnl490M div.ciPlayernametxt div h3.PlayersSearchLink b"), private$id)

			private$x <- tryCatch2(rvest::html_text(private$x),private$id)
			country <- as.character(private$x)
			return(c(private$id, shortname, country))
			
		},
		getPlayerBasic = function(){
			private$x=""
			private$x <- tryCatch2(rvest::html_nodes(private$profile_html, "div#ciHomeContent div#ciMainContainer div#ciHomeContentlhs div.pnl490M div.ciPlayernametxt div h1"), private$id)
			private$x <- tryCatch2(rvest::html_text(private$x),private$id)
			shortname <- stringr::str_trim(stringr::str_remove_all(private$x, "[\n\r\t]"))
			
			private$x=""
			private$x <- tryCatch2(rvest::html_nodes(private$profile_html, "div#ciHomeContent div#ciMainContainer div#ciHomeContentlhs div.pnl490M div.ciPlayernametxt div h3.PlayersSearchLink b"),private$id)
			private$x <- tryCatch2(rvest::html_text(private$x),private$id)
			country <- as.character(private$x)
			
			private$x=""
			private$x <- tryCatch2(rvest::html_nodes(private$profile_html, "div#ciHomeContent div#ciMainContainer div#ciHomeContentlhs div.pnl490M div div p.ciPlayerinformationtxt span"),private$id)

			private$x <- tryCatch2(rvest::html_text(private$x),private$id)
			private$x <- stringr::str_trim(stringr::str_remove_all(private$x, "[\n\r\t]"))
			
			fullname <- private$x[1]
			dob_place <- stringr::str_replace_all(private$x[2], ",", "|")
			age <- private$x[3]
			role <- private$x[length(private$x)-2]
			batting_style <- private$x[length(private$x)-1]
			bowling_style <- private$x[length(private$x)]
			return(c(private$id, shortname, country, fullname, dob_place, age, role, batting_style, bowling_style))
		},
		getPlayerFull = function(){
			private$x=""
			private$x <- tryCatch2(rvest::html_nodes(private$profile_html, "div#ciHomeContent div#ciMainContainer div#ciHomeContentlhs div.pnl490M div.ciPlayernametxt div h1"), private$id)
			private$x <- tryCatch2(rvest::html_text(private$x),private$id)
			shortname <- stringr::str_trim(stringr::str_remove_all(private$x, "[\n\r\t]"))
			
			private$x=""
			private$x <- tryCatch2(rvest::html_nodes(private$profile_html, "div#ciHomeContent div#ciMainContainer div#ciHomeContentlhs div.pnl490M div.ciPlayernametxt div h3.PlayersSearchLink b"),private$id)
			private$x <- tryCatch2(rvest::html_text(private$x),private$id)
			country <- as.character(private$x)

			private$x=""
			private$x <- tryCatch2(rvest::html_nodes(private$profile_html, "div#ciHomeContent div#ciMainContainer div#ciHomeContentlhs div.pnl490M div div p.ciPlayerinformationtxt span"),private$id)
			private$x <- tryCatch2(rvest::html_text(private$x),private$id)
			private$x <- stringr::str_trim(stringr::str_remove_all(private$x, "[\n\r\t]"))
			
			fullname <- private$x[1]
			dob_place <- stringr::str_replace_all(private$x[2], ",", "|")
			age <- private$x[3]
			role <- private$x[length(private$x)-2]
			batting_style <- private$x[length(private$x)-1]
			bowling_style <- private$x[length(private$x)]
			
									
			private$x=""
			private$x <- tryCatch2(rvest::html_nodes(private$profile_html, "div#ciHomeContent div#ciMainContainer div#ciHomeContentlhs div.pnl490M table.engineTable tbody tr.data1 td"), private$id)

			private$x <- tryCatch2(rvest::html_text(private$x),private$id)
			
			batting_fielding_Test   <- private$x[2:15]
			batting_fielding_ODI    <- private$x[17:30]
			batting_fielding_T20I   <- private$x[32:45]
			batting_fielding_1st    <- private$x[47:60]
			batting_fielding_listA  <- private$x[62:75]
			batting_fielding_T20S   <- private$x[77:90]
			
			bowling_Test    <- private$x[92:104]
			bowling_ODI     <- private$x[106:118]
			bowling_T20I    <- private$x[120:132]
			bowling_1st     <- private$x[134:146]
			bowling_listA   <- private$x[148:160]
			bowling_T20s    <- private$x[162:174]

			return(c(c(private$id, shortname, country, fullname, dob_place, age, role, batting_style, bowling_style, private$x[2:15], private$x[17:30], private$x[32:45], private$x[47:60], private$x[62:75], private$x[77:90]) , 
					 c(private$id, shortname, country, fullname, dob_place, age, role, batting_style, bowling_style, private$x[92:104], private$x[106:118], private$x[120:132], private$x[134:146], private$x[148:160], private$x[162:174]) ))
		}
),
	private = list(
		id = "",
		base_url = "",
		profile_url = "",
		profile_html = "",
		x = ""
	)
)
