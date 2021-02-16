database <- R6::R6Class(
  "Database",
  inherit = base_class,

  public = list(
    server = "character",
    database = "character",
    driver = "character",
    user = "character",
    password = "character",
    vaultParams = "list",
    connectionPool = "list",
    test = "logical",
    debug = 'logical',
    isConnected = "logical",
    messages = "list",
    qSuccess = "logical",

    #----------------------------------------------------------------
    # buildConnectionString -
    # ---------------------------------------------------------------
    buildConnectionString = function(){
      if( length(self$vaultParams) ){
        v = vault$new( argList = self$vaultParams )
        dbParams = v$open()
        self$database = dbParams$database
        self$driver = self$getDriver('')#dbParams$dbType)

        return(
          sprintf(
            "Driver=%s;Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            dbParams$driver,dbParams$server,dbParams$database,dbParams$user,dbParams$password
          )
        )
      } else {
        return(
          sprintf(
            "Driver=%s;Server=%s;Database=%s;Uid=%s;Pwd=%s;",
            self$driver,self$server,self$database,self$user,self$password
          )
        )
      }
    },


    #----------------------------------------------------------------
    # getDriver -
    # ---------------------------------------------------------------
    getDriver = function(dbType='sqlserver'){

      # Test for ODBC SQL Server Driver
      driver.exists = grepl('ODBC Driver [0-9][0-9] for SQL Server', odbc::odbcListDrivers()$name)
      driver.name.odbc = tail(odbc::odbcListDrivers()$name[driver.exists], n=1)

      # Test for SQL Server Native Driver is ODBC driver does not exist
      driver.exists = grepl('SQL Server Native Client ', odbc::odbcListDrivers()$name)
      driver.name.native = tail(odbc::odbcListDrivers()$name[driver.exists], n=1)
    },


    #----------------------------------------------------------------
    # connect -
    # ---------------------------------------------------------------
    connect = function(){
      iConn = length(self$connectionPool) + 1
      self$connectionPool[[iConn]] = odbc::dbConnect( odbc::odbc(),.connection_string = self$buildConnectionString() )
      self$isConnected = TRUE
      return( length(self$connectionPool) )
    },


    #----------------------------------------------------------------
    # getConnection -
    # ---------------------------------------------------------------
    getConnection = function(iConn=1, newConnection=FALSE){
      if(length(self$connectionPool) == 0 || newConnection) {
        iConn = self$connect()
      }
      return( self$connectionPool[[iConn]] )
    },



    #----------------------------------------------------------------
    # getTblPrimaryKeys -
    # ---------------------------------------------------------------
    getTblPrimaryKeys = function(tblName){
      return(
        query( sprintf("
          SELECT
  		      pk.TABLE_NAME,
  		      pk.COLUMN_NAME
		      FROM information_schema.KEY_COLUMN_USAGE as pk
		      WHERE pk.CONSTRAINT_NAME like 'PK%%' and
            pk.TABLE_NAME = '%s'",
        tblName) )
      )
    },



    #----------------------------------------------------------------
    # getTblColumnInfo -
    # ---------------------------------------------------------------
    getTblColumnInfo = function(tblName){
      return( odbc::dbListFields (
        conn = self$getConnection(),
        name = tblName

      ) )
    },


    #----------------------------------------------------------------
    # updateRecords - requires: dat, tablename, index
    # ---------------------------------------------------------------
    updateRecords = function(...){
    # odbc::sqlUpdate(
    #    channel = getConnection(),
    #    #test = test,
    #    #verbose = TRUE, # bug in RODBC where verbose causes problems with some queries safer to not use it
    #    ...
    #  )
    },


    #----------------------------------------------------------------
    # saveTable -
    # ---------------------------------------------------------------
    saveTable = function(iConn=1,...){
      odbc::dbWriteTable(
        conn = self$getConnection(iConn = iConn),
        #test = test,
        #verbose = TRUE, # bug in RODBC where verbose causes problems with some queries safer to not use it
        ...
      )
    },


    #----------------------------------------------------------------
    # truncate -
    # ---------------------------------------------------------------
    truncate = function( tblName ){
      self$query( sprintf("truncate table %s",tblName) )
    },


    #----------------------------------------------------------------
    # saveTables - transactioned batch table updates
    # ---------------------------------------------------------------
    saveTables = function(tblList, rownames=FALSE, colnames=FALSE, append=TRUE, addPK=FALSE, fast=TRUE){
      self$qSuccess = TRUE

      tryCatch({
        iConn = self$connect()
        odbc::dbBegin( self$getConnection(iConn) )
        for( tblName in names(tblList) ){

          saveTable(
            iConn = iConn,
            value = as.data.frame( tblList[[tblName]] ),
            name = tblName,
            row.names = rownames,
            append = append
          )
        }

      }, error = function(e){
        self$qSuccess = FALSE
        self$messages = append( self$messages, e)
        cat("\n\tERROR EXECUTING DATABASE QUERY - Rolling back data!\n")
        str(e)

      }, finally = {
        if(self$qSuccess){
          odbc::dbCommit( self$getConnection(iConn) )
        } else {
          odbc::dbRollback( self$getConnection(iConn) )
        }
        disconnect( iConn )
      })
      return(self$qSuccess)
    },


    #----------------------------------------------------------------
    # getTable -
    # ---------------------------------------------------------------
    getTable = function(tblName){
      self$query( sprintf("select * From %s",tblName) )
    },


    #----------------------------------------------------------------
    # query - executes the database query defined in the query string
    # ---------------------------------------------------------------
    query = function(query, iConn=1){
      odbc::dbGetQuery( self$getConnection(iConn=iConn), query )
    },


    #----------------------------------------------------------------
    # mergeTblData - imports data into a database table
    # ---------------------------------------------------------------
    mergeTblData = function(db.tblData, db.tblName){
      colNames = colnames(db.tblData)

      pks = self$getTblPrimaryKeys(db.tblName)$COLUMN_NAME

      valueStr = paste0("(",paste0("'",gsub("'{1}","''",db.tblData[1,]),"'", collapse=", "),")")
      if(nrow(db.tblData) > 1){
        for (iRow in 2:nrow(db.tblData)){
          valueStr = paste0(valueStr,",(",paste0("'",gsub("'{1}","''",db.tblData[iRow,]),"'", collapse=", "),")")
        }
      }
      valueStr=gsub(x=valueStr, pattern="'NA'", replacement="NULL")
      valueStr=gsub(x=valueStr, pattern="'',", replacement="NULL,")

      mergeStr = sprintf("
        merge %s as fs
        using (
          values %s
        ) as d (%s)
        on %s
        when not matched then
          insert (%s)
          values (%s)
        when matched then
          update set %s;",
        db.tblName,
        valueStr,
        paste(colNames, collapse = ", "),
        paste0(paste0("fs.",pks),paste0("=d.",pks),collapse = " and "),
        paste(colNames, collapse = ", "),
        paste(paste0("d.",colNames), collapse = ", "),
        paste0(paste0("fs.",colNames),paste0("=d.",colNames), collapse = ", ")
      )

      # Import data into database
      cat("\tMerging data into",db.tblName,"table...")
      qResult = self$analyzeResult( self$query(mergeStr), showError=TRUE )
      cat( ifelse(qResult == 0, "\tComplete!\n", "\tFailed!\n") )

      return( qResult )
    },


    #----------------------------------------------------------------
    # batchQuery -
    # ---------------------------------------------------------------
    batchQuery = function(queries, newConnection=TRUE, iConn=NULL, endBatch=TRUE, qSuccess=TRUE){

      qResult = list(success = qSuccess, result = NA, iConn=NA)

      tryCatch({
        if(newConnection){
          iConn = self$connect()
          odbc::dbBegin(  self$getConnection(iConn) )
        }
        qResult$iConn = iConn

        for(q in queries){
          if(qResult$success){
            qResult$result = self$query( q, iConn = iConn )
          } else {
            qResult$success = FALSE
          }
        }

      }, error = function(e){
        qResult$success = FALSE
        self$messages = append( self$messages, e)
        cat("\n\tERROR EXECUTING DATABASE QUERY\n")
        str(e)

      }, finally = {
        if(endBatch){
          if(qResult$success){
            odbc::dbCommit( self$getConnection(iConn) )
          } else {
            odbc::dbRollback( self$getConnection(iConn) )
          }
          self$disconnect( iConn )
        }
      })
      return(qResult)
    },


    #----------------------------------------------------------------
    # analyzeResult - (0) query success without output / (-1) query failure
    # ---------------------------------------------------------------
    analyzeResult = function(qResult=NULL, showError=FALSE){
      if(length(qResult) == 0) {
        return(0)

      } else if( class(qResult) == "character" ){
        cat('\n\t\tUnable to execute database query / interpret the provided parameters.\n')

        if(showError){
          cat('\n',qResult,'\n')
        }

        return(-1)

      }
    },


    #----------------------------------------------------------------
    # disconnect - closes the database connection
    # ---------------------------------------------------------------
    disconnect = function(iConn=NULL){
      try({
        if(!is.null(iConn)){
          odbc::dbDisconnect( self$getConnection(iConn) )
          self$connectionPool[iConn] = NULL
        } else {
          lapply( self$connectionPool, function(connection){
            odbc::dbDisconnect( connection )
          })
          self$connectionPool = list()
        }
      }, silent = TRUE)
    },


    #----------------------------------------------------------------
    # backupDB - backs up the database to the standard location
    # ---------------------------------------------------------------
    backupDB = function(){

    },


    #----------------------------------------------------------------
    # listBackups - lists the backups available for the given database
    # ---------------------------------------------------------------
    listBackups = function(){

    },


    #----------------------------------------------------------------
    # getTblColumns - returns an empty data.frame based on a given database table
    # ---------------------------------------------------------------
    getEmptyTable = function(tblName, iConn=1){
      # colNames = query(
      #   sprintf("
      #     Select COLUMN_NAME, DATA_TYPE
      #     From INFORMATION_SCHEMA.COLUMNS
      #     Where TABLE_NAME = '%s'"
      #     ,tblName
      #   ), iConn
      # )$COLUMN_NAME

      #return( data.frame( matrix( vector(), 0, length(colNames), dimnames = list(c(),colNames) ) ) )

      #Use this method as quick way of maintaining compatible data types
      return( self$query(sprintf("Select Top 1 * From %s", tblName), iConn)[!1,] )
    },


    #----------------------------------------------------------------
    # getTblColumns - returns an empty data.frame based on a given database table
    # ---------------------------------------------------------------
    getTblColumns = function(tblName){

      return( names( self$getEmptyTable( tblName ) ) )
    },

    #----------------------------------------------------------------
    # as.sql.in - concatenate list objects into an "IN" string for insertion into queries
    # ---------------------------------------------------------------
    as.sql.in = function(inList) {
      return( paste0("'", unlist( inList ), "'", collapse=", " ) )
    },

    #----------------------------------------------------------------
    # blanks.as.na - converts string blanks to R NA values across a dataframe
    # ---------------------------------------------------------------
    blanks.as.na = function(tblData){
      return(
        lapply( tblData, function(col){ gsub("",NA_character_,col) } )
      )
    },

    #----------------------------------------------------------------
    # NULLstr.as.na - converts string NULLs to R NA values across a dataframe
    # ---------------------------------------------------------------
    NULLstr.as.na = function(tblData){
      return(
        lapply( tblData, function(col){ gsub("NULL",NA_character_,col) } )
      )
    },

    #----------------------------------------------------------------
    # finalize - called when object is no longer being referenced
    # ---------------------------------------------------------------
    finalize = function(){
      try( lapply( self$connectionPool, function(connection){
        odbc::dbDisconnect( connection )
      }), silent = TRUE )
    }

  )
)
