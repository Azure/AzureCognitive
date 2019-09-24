#' @import AzureRMR
NULL

globalVariables(c("self", "private"))

.onLoad <- function(libname, pkgname)
{
    add_methods()
}


#' Create, retrieve or delete an Azure Cognitive Service
#'
#' Methods for the [AzureRMR::az_resource_group] class.
#'
#' @rdname create_cognitive_service
#' @name create_cognitive_service
#' @aliases create_cognitive_service get_cognitive_service delete_cognitive_service
#' @section Usage:
#' ```
#' create_cognitive_service(name, service_type, service_tier, location = self$location,
#'                          subdomain = name, properties = list(), ...)
#' get_cognitive_service(name)
#' delete_cognitive_service(name, confirm = TRUE, wait = FALSE)
#' ```
#' @section Arguments:
#' - `name`: The name for the cognitive service resource.
#' - `service_type`: The type of service (or "kind") to create. See 'Details' below.
#' - `service_tier`: The pricing tier (SKU) for the service.
#' - `location`: The Azure location in which to create the service. Defaults to the resource group's location.
#' - `subdomain`: The subdomain name to assign to the service; defaults to the resource name. Set this to NULL if you don't want to assign the service a subdomain of its own.
#' - `properties`: For `create_cognitive_service`, an optional named list of other properties for the service.
#' - `confirm`: For `delete_cognitive_service`, whether to prompt for confirmation before deleting the resource.
#' - `wait`: For `delete_cognitive_service`, whether to wait until the deletion is complete before returning.
#'
#' @section Details:
#' These are methods to create, get or delete a cognitive service resource within a resource group.
#'
#' For `create_cognitive_service`, the type of service created can be one of the following:
#' - `ComputerVision`: generic computer vision service
#' - `Face`: face recognition
#' - `LUIS`: language understanding
#' - `CustomVision.Training`: Training endpoint for a custom vision service
#' - `CustomVision.Prediction`: Prediction endpoint for a custom vision service
#' - `ContentModerator`: Content moderation (text and images)
#' - `Text`: text analytics
#' - `TextTranslate`: text translation
#'
#' The possible tiers depend on the type of service created. Consult the Azure Cognitive Service documentation for more information. Usually there will be at least one free tier available.
#'
#' @section Value:
#' For `create_cognitive_service` and `get_cognitive_service`, an object of class `az_cognitive_service`.
#'
#' @seealso
#' [cognitive_endpoint], [call_cognitive_endpoint]
#'
#' [Azure Cognitive Services documentation](https://docs.microsoft.com/en-us/azure/cognitive-services/),
#' [REST API reference](https://docs.microsoft.com/en-us/rest/api/cognitiveservices/)
#' @examples
#' \dontrun{
#'
#' rg <- AzureRMR::get_azure_login()$
#'     get_subscription("sub_id")$
#'     get_resource_group("rgname")
#'
#' rg$create_cognitive_service("myvisionservice",
#'     service_type="ComputerVision", service_tier="F0")
#'
#' rg$create_cognitive_service("mylangservice",
#'     service_type="LUIS", service_tier="F0")
#'
#' rg$get_cognitive_service("myvisionservice")
#'
#' rg$delete_cognitive_service("myvisionservice")
#'
#'}
NULL

add_methods <- function()
{
    az_resource_group$set("public", "create_cognitive_service", overwrite=TRUE,
    function(name, service_type, service_tier, location=self$location, subdomain=name, properties=list(), ...)
    {
        if(!is.null(subdomain))
            properties <- utils::modifyList(properties, list(customSubDomainName=subdomain))

        az_cognitive_service$new(self$token, self$subscription, self$name,
            type="Microsoft.CognitiveServices/accounts",
            name=name,
            location=location,
            kind=service_type,
            sku=list(name=service_tier),
            properties=properties,
            ...)
    })

    az_resource_group$set("public", "get_cognitive_service", overwrite=TRUE,
    function(name)
    {
        az_cognitive_service$new(self$token, self$subscription, self$name,
            type="Microsoft.CognitiveServices/accounts",
            name=name)
    })

    az_resource_group$set("public", "delete_cognitive_service", overwrite=TRUE,
    function(name, confirm=TRUE, wait=FALSE)
    {
        az_cognitive_service$new(self$token, self$subscription, self$name,
            type="Microsoft.CognitiveServices/accounts",
            name=name,
            deployed_properties=list(NULL))$delete(confirm=confirm, wait=wait)
    })
}
