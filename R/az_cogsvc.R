az_cognitive_service <- R6::R6Class("az_cognitive_service", inherit=AzureRMR::az_resource,

public=list(
    list_keys=function()
    {
        unlist(private$res_op("listKeys", http_verb="POST"))
    },

    regen_key=function(key=1)
    {
        body=list(keyName=paste0("Key", key))
        unlist(private$res_op("regenerateKey", body=body, http_verb="POST"))
    },

    list_skus=function()
    {
        private$res_op("skus")$value
    },

    get_endpoint=function()
    {
        cognitive_endpoint(self$properties$endpoint, self$kind, key=self$list_keys()[1])
    }
))

