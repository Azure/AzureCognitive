#' Object representing an Azure Cognitive Service endpoint
#'
#' @param url The URL of the endpoint.
#' @param type What type (or kind) of service the endpoint provides. See below for the services that AzureCognitive currently recognises.
#' @param key The key to use to authenticate with the endpoint.
#' @param aad_token. An Azure Active Directory (AAD) OAuth token, as an alternative to a key for the services that allow AAD authentication.
#' @param cognitive_token A Cognitive Service token, as another alternative to a key for the services that accept it.
#' @details
#' Currently, `cognitive_endpoint` recognises the following types (or kinds) of services:
#' - `ComputerVision`: generic computer vision service
#' - `Face`: face recognition
#' - `LUIS`: natural language understanding
#' - `CustomVision.Training`: Training endpoint for a custom vision service
#' - `CustomVision.Prediction`: Prediction endpoint for a custom vision service
#' - `ContentModerator`: Content moderation (text and images)
#' - `Text`: text analytics
#'
#' @return
#' An object inheriting from class `cognitive_endpoint`, that can be used to communicate with the REST endpoint. The object will have a subclass based on the type of service provided.
#'
#' @seealso
#' [call_cognitive_endpoint], [create_cognitive_service], [get_cognitive_service]
#' @export
cognitive_endpoint <- function(url, type, key=NULL, aad_token=NULL, cognitive_token=NULL)
{
    type <- normalize_cognitive_type(type)
    url <- httr::parse_url(url)
    url$path <- get_api_path(type)

    object <- list(url=url, key=key, aad_token=aad_token, cognitive_token=cognitive_token)
    class(object) <- c(paste0(type, "_endpoint"), "cognitive_endpoint")

    object
}


#' Call a Cognitive Service REST endpoint
#'
#' @param endpoint An object of class `cognitive_endpoint`.
#' @param operation The operation to perform.
#' @param options Any query parameters that the operation takes.
#' @param headers Any optional HTTP headers to include in the REST call. Note that `call_cognitive_endpoint` will handle authentication details automatically, so don't include them here.
#' @param body The body of the HTTP request for the REST call.
#' @param encode The encoding (really content-type) for the body. Can be `json` if the body is JSON text, or `raw` for a binary object.
#' @param http_verb The HTTP verb for the REST call.
#' @param http_status_handler How to handle a failed REST call. `stop`, `warn` and `message` will call the corresponding `*_for_status` handler in the httr package; `pass` will return the raw response object unchanged. The last one is mostly intended for debugging purposes.
#' @details
#' This function does the low-level work of constructing a HTTP request and then calling the REST endpoint. It is meant to be used by other packages that provide higher-level views of the service functionality.
#'
#' @return
#' For a successful REST call, the contents of the response. This will usually be a list, obtained by translating the raw JSON body into R. If the call returns a non-success HTTP status code, based on the `http_status_handler` argument.
#'
#' @seealso
#' [cognitive_endpoint], [create_cognitive_service], [get_cognitive_service]
#' @export
call_cognitive_endpoint <- function(endpoint, operation, options=list(), headers=list(), body=NULL, encode="json",
                                    http_verb=c("GET", "POST", "PUT", "PATCH", "DELETE", "HEAD"),
                                    http_status_handler=c("stop", "warn", "message", "pass"))
{
    url <- endpoint$url
    url$path <- file.path(url$path, operation)
    url$query <- options

    if(encode == "json")
    {
        # manually convert to json to avoid issues with nulls
        body <- jsonlite::toJSON(body, auto_unbox=TRUE, digits=22, null="null")
        encode <- "raw"
    }
    else if(encode == "raw")
        headers$`content-type` <- "application/octet-stream"

    headers <- add_cognitive_auth(endpoint, headers)
    verb <- match.arg(http_verb)
    res <- httr::VERB(verb, url, headers, body=body, encode=encode)

    process_cognitive_response(res, match.arg(http_status_handler))
}


add_cognitive_auth <- function(endpoint, headers)
{
    headers <- if(!is.null(endpoint$key))
        c(headers, `Ocp-Apim-Subscription-Key`=unname(endpoint$key))
    else if(is_azure_auth(endpoint$aad_token))
    {
        token <- endpoint$aad_token
        if(!token$validate())
            token$refresh()
        c(headers, Authorization=paste("Bearer", AzureAuth::extract_jwt(token)))
    }
    else stop("No supported authentication method found", call.=FALSE)

    do.call(httr::add_headers, headers)
}


process_cognitive_response <- function(response, handler)
{
    if(handler != "pass")
    {
        cont <- httr::content(response)
        handler <- get(paste0(handler, "_for_status"), getNamespace("httr"))
        handler(response, paste0("complete Cognitive Services operation. Message:\n",
                                 sub("\\.$", "", cognitive_error_message(cont))))

        cont
    }
    else response
}


cognitive_error_message <- function(cont)
{
    if(is.raw(cont))
        cont <- jsonlite::fromJSON(rawToChar(cont))

    msg <- if(is.character(cont))
        cont
    else if(is.list(cont))
    {
        if(is.character(cont$message))
            cont$message
        else if(is.list(cont$error) && is.character(cont$error$message))
            cont$error$message
        else ""
    }
    else ""

    paste0(strwrap(msg), collapse="\n")
}


# kind - api
# ComputerVision - vision/v2.0
# Face - face/v1.0
# LUIS - luis/v2.0
# CustomVision.Training - customvision/v3.0
# CustomVision.Prediction - customvision/v3.0
# ContentModerator - contentmoderator/moderate/v1.0
# Text - text/analytics/v2.0
get_api_path <- function(type)
{
    switch(type,
        computervision="vision/v2.0",
        face="face/v1.0",
        luis="luis/v2.0",
        customvision=, customvision_training=, customvision_prediction="customvision/v3.0",
        contentmoderator="contentmoderator/moderate/v1.0",
        text="text/analytics/v2.0",
        stop("Unknown cognitive service", call.=FALSE)
    )
}


normalize_cognitive_type <- function(type)
{
    tolower(gsub("[. ]", "_", type))
}
