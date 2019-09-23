# AzureCognitive

A package to work with [Azure Cognitive Services](https://azure.microsoft.com/services/cognitive-services/). Both a Resource Manager interface and a client interface to the REST API are provided.

## Resource Manager interface

AzureCognitive extends the class framework provided by [AzureRMR](https://github.com/Azure/AzureRMR) to support Cognitive Services. You can create, retrieve, update, and delete cognitive service resources by calling the corresponding methods for a resource group.

```r
az <- AzureRMR::get_azure_login()
sub <- az$get_subscription("sub_id")
rg <- sub$get_resource_group("rgname")

# create a new Computer Vision service
rg$create_cognitive_service("myvisionservice",
    service_type="ComputerVision", service_tier="S1")

cogsvc <- rg$get_cognitive_service("myvisionservice")

# list subscription keys
cogsvc$list_keys()
```

## Client interface

AzureCognitive implements basic functionality for communicating with a cognitive service endpoint. It is meant to be used by other packages to support specific services, like Computer Vision, LUIS (language understanding), etc.

```r
# getting the endpoint from the resource object
endp <- cogsvc$get_endpoint()

# or standalone (must provide subscription key)
endp <- cognitive_endpoint("https://myvisionservice.cognitiveservices.azure.com/",
    service_type="ComputerVision", key="key")

# analyze an image
img_link <- "https://news.microsoft.com/uploads/2014/09/billg1_print.jpg"
call_cognitive_endpoint(endp,
    operation="analyze",
    body=list(url=img_link),
    options=list(details="celebrities"),
    http_verb="POST")
```
