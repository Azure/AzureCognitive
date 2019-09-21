#' @import AzureRMR
NULL

globalVariables(c("self", "private"))

.onLoad <- function(libname, pkgname)
{
    add_methods()
}


add_methods <- function()
{
    az_resource_group$set("public", "create_cognitive_service", overwrite=TRUE,
    function(name, location=self$location, subdomain=name, kind="ComputerVision", sku="S1", properties=list(), ...)
    {
        if(!is.null(subdomain))
            properties <- utils::modifyList(properties, list(customSubDomainName=subdomain))

        az_cognitive_service$new(self$token, self$subscription, self$name,
            type="Microsoft.CognitiveServices/accounts",
            name=name,
            location=location,
            kind="ComputerVision",
            sku=list(name=sku),
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
