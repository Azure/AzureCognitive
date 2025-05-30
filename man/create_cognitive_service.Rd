% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AzureCognitive.R
\name{create_cognitive_service}
\alias{create_cognitive_service}
\alias{get_cognitive_service}
\alias{delete_cognitive_service}
\title{Create, retrieve or delete an Azure Cognitive Service}
\description{
Methods for the \link[AzureRMR:az_resource_group]{AzureRMR::az_resource_group} class.
}
\section{Usage}{


\if{html}{\out{<div class="sourceCode">}}\preformatted{create_cognitive_service(name, service_type, service_tier, location = self$location,
                         subdomain = name, properties = list(), ...)
get_cognitive_service(name)
delete_cognitive_service(name, confirm = TRUE, wait = FALSE)
}\if{html}{\out{</div>}}
}

\section{Arguments}{

\itemize{
\item \code{name}: The name for the cognitive service resource.
\item \code{service_type}: The type of service (or "kind") to create. See 'Details' below.
\item \code{service_tier}: The pricing tier (SKU) for the service.
\item \code{location}: The Azure location in which to create the service. Defaults to the resource group's location.
\item \code{subdomain}: The subdomain name to assign to the service; defaults to the resource name. Set this to NULL if you don't want to assign the service a subdomain of its own.
\item \code{properties}: For \code{create_cognitive_service}, an optional named list of other properties for the service.
\item \code{confirm}: For \code{delete_cognitive_service}, whether to prompt for confirmation before deleting the resource.
\item \code{wait}: For \code{delete_cognitive_service}, whether to wait until the deletion is complete before returning.
}
}

\section{Details}{

These are methods to create, get or delete a cognitive service resource within a resource group.

For \code{create_cognitive_service}, the type of service created can be one of the following:
\itemize{
\item \code{CognitiveServices}: multiple service types
\item \code{ComputerVision}: generic computer vision service
\item \code{Face}: face recognition
\item \code{LUIS}: language understanding
\item \code{CustomVision.Training}: Training endpoint for a custom vision service
\item \code{CustomVision.Prediction}: Prediction endpoint for a custom vision service
\item \code{ContentModerator}: Content moderation (text and images)
\item \code{Text}: text analytics
\item \code{TextTranslate}: text translation
}

The possible tiers depend on the type of service created. Consult the Azure Cognitive Service documentation for more information. Usually there will be at least one free tier available.
}

\section{Value}{

For \code{create_cognitive_service} and \code{get_cognitive_service}, an object of class \code{az_cognitive_service}.
}

\examples{
\dontrun{

rg <- AzureRMR::get_azure_login()$
    get_subscription("sub_id")$
    get_resource_group("rgname")

rg$create_cognitive_service("myvisionservice",
    service_type="ComputerVision", service_tier="F0")

rg$create_cognitive_service("mylangservice",
    service_type="LUIS", service_tier="F0")

rg$get_cognitive_service("myvisionservice")

rg$delete_cognitive_service("myvisionservice")

}
}
\seealso{
\link{cognitive_endpoint}, \link{call_cognitive_endpoint}

\href{https://learn.microsoft.com/en-us/azure/cognitive-services/}{Azure Cognitive Services documentation},
\href{https://learn.microsoft.com/en-us/rest/api/cognitiveservices/}{REST API reference}
}
