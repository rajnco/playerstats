library(RSQLite)

SQLight <- R6::R6Class("SQLight",
	public = list(
		initialize = function(dbpath, dbname) { 
			private$dbpath <- dbpath
			private$dbname <- dbname
			if (! dir.exists(private$dbpath) ) { 
				dir.create(private$dbpath)
			} 
			if (file.exists(paste0(private$dbpath, "/", private$dbname))) {
				print(paste0("Making use of alreay existing database ", private$dbname, " at ", private$dbpath) )
			} else { print(paste0("creating new database ", private$dbname, " at ", private$dbpath )) }
			private$connection <- RSQLite::dbConnect(RSQLite::SQLite(), paste0(private$dbpath, private$dbname))
		},	
		createTableSQL = function(sqlstmt) {
			private$sqlstmt = sqlstmt
			done <- RSQLite::dbSendQuery(private$connection, private$sqlstmt)
		},
		insertTableSQL = function(tablename, values){
			private$tablename = tablename
			private$values = values
			private$sqlstmt = paste("insert into ", private$tablename, " values (", private$values ,")")
			skip_to_next = FALSE
			done <- tryCatch(RSQLite::dbSendQuery(private$connection, private$sqlstmt),
							error = function(err) { print(paste0("insert failed : ", sqlstmt)); skip_to_next=TRUE }, 
							warning = function(err) {},
							message = function(err) {},
							finally = function(err) {}
			)
			if (skip_to_next == TRUE) { next }
			RSQLite::dbSendQuery(private$connection, private$sqlstmt)
			
		},
		createTable = function(tablename, fields) {
			private$tablename <- tablename
				if (RSQLite::dbExistsTable(private$connection, private$tablename)) {
					print(paste0("Table already exist ", private$tablename))
					print("You can call recreateTable method to recreate table")
				} else {
					done <- RSQLite::dbCreateTable(private$connection, private$tablename, fields=fields)
					print(paste0("Created table : ", private$tablename))
				}
		},
		recreateTable = function(tablename, fields) {
			private$tablename <- tablename
			private$fields <- fields
			if (! RSQLite::dbExistsTable(private$connection, private$tablename)) {
				print(paste0("Table not exist ", private$tablename))	
			} else {
				done <- RSQLite::dbRemoveTable(private$connection, private$tablename)
				print(paste0("Removed table : ", private$tablename))
			}
			done <- RSQLite::dbCreateTable(private$connection, private$tablename, fields=private$fields)
			print(paste0("Created table : ", private$tablename))
		},
		disconnectDB = function(){
			done <- RSQLite::dbDisconnect(private$connection)
		}
	),
	private = list(
		dbpath = "",
		dbname = "",
		connection = "",
		tablename= "",
		fields = "",
		values = "",
		sqlstmt = ""
	)
)

