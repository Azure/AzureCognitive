#' Azure Cognitive Service resource class
#'
#' Class representing a cognitive service resource, exposing methods for working with it.
#'
#' @docType class
#' @section Methods:
#' The following methods are available, in addition to those provided by the [AzureRMR::az_resource] class:
#' - `list_keys()`: Return the access keys for this service.
#' - `get_endpoint()`: Return the service endpoint, along with an access key. See 'Endpoints' below.
#' - `regen_key(key)`: Regenerates (creates a new value for) an access key. The argument `key` can be 1 or 2.
#' - `list_service_tiers()`: List the service pricing tiers (SKUs) available for this service.
#'
#' @section Initialization:
#' Initializing a new object of this class can either retrieve an existing service, or create a new service on the host. Generally, the best way to initialize an object is via the `get_cognitive_service` and `create_cognitive_service` methods of the [az_resource_group] class, which handle the details automatically.
#'
#' @section Endpoints:
#' The client-side interaction with a cognitive service is via an _endpoint_. Endpoint interaction in AzureCognitive is implemented using S3 classes. You can create a new endpoint object via the `get_endpoint()` method, or with the standalone `cognitive_endpoint()` function. If you use the latter, you will also have to supply any necessary authentication credentials, eg a shared key or token.
#'
#' @seealso
#' [cognitive_endpoint], [create_cognitive_service], [get_cognitive_service]
#' @export
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

    list_service_tiers=function()
    {
        private$res_op("skus")$value
    },

    get_endpoint=function(key=self$list_keys()[1])
    {
        cognitive_endpoint(self$properties$endpoint, self$kind, key=key)
    }
))

