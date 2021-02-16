
vault <- R6::R6Class(
  "vault",
  inherit = base_class,

  public = list(
    vaultPath = NULL,
    keyFile = NULL,


    save = function(secret){
      saveRDS(
        object = openssl::aes_cbc_encrypt(
          data = serialize( secret, connection = NULL ),
          key = self$readKey()
        ),
        file = self$vaultPath
      )
      invisible(self)
    },


    open = function() {
      return(
        unserialize(
          connection = openssl::aes_cbc_decrypt(
            data = readRDS( self$vaultPath ),
            key = self$readKey()
          )
        )
      )
    },


    updateValue = function(key,value) {
      secret = self$open()
      secret[[key]] = value
      self$save(secret)
    },


    updateKey = function(passphrase) {
      secret = self$open()
      self$generateKey(passphrase)
      self$save(secret)
    },


    readKey = function() {
      return( readRDS( self$keyFile ) )
    },


    generateKey = function(passphrase) {
      saveRDS(
        object = openssl::sha256( charToRaw(passphrase) ),
        file = self$keyFile
      )
    }
  )
)
