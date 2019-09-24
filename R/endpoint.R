#' Object representing an Azure Cognitive Service endpoint
#'
#' @param url The URL of the endpoint.
#' @param service_type What type (or kind) of service the endpoint provides. See below for the services that AzureCognitive currently recognises.
#' @param key The key to use to authenticate with the endpoint.
#' @param aad_token An Azure Active Directory (AAD) OAuth token, as an alternative to a key for the services that allow AAD authentication.
#' @param cognitive_token A Cognitive Service token, as another alternative to a key for the services that accept it.
#' @details
#' Currently, `cognitive_endpoint` recognises the following service types:
#' - `ComputerVision`: generic computer vision service
#' - `Face`: face recognition
#' - `LUIS`: language understanding
#' - `CustomVision.Training`: Training endpoint for a custom vision service
#' - `CustomVision.Prediction`: Prediction endpoint for a custom vision service
#' - `ContentModerator`: Content moderation (text and images)
#' - `Text`: text analytics
#' - `TextTranslate`: text translation
#'
#' @return
#' An object of from class `cognitive_endpoint`, that can be used to communicate with the REST endpoint.
#'
#' @seealso
#' [call_cognitive_endpoint], [create_cognitive_service], [get_cognitive_service]
#' @examples
#' \dontrun{
#'
#' cognitive_service("https://myvisionservice.api.cognitive.azure.com",
#'     service_type="Computervision", key="key")
#'
#' cognitive_service("https://mylangservice.api.cognitive.azure.com",
#'     service_type="LUIS", key="key")
#'
#' # authenticating with AAD
#' token <- AzureAuth::get_azure_token("https://cognitiveservices.azure.com",
#'     tenant="mytenant", app="app_id", password="password")
#' cognitive_service("https://myvisionservice.api.cognitive.azure.com",
#'     service_type="Computervision", aad_token=token))
#'
#' }
#' @export
cognitive_endpoint <- function(url, service_type, key=NULL, aad_token=NULL, cognitive_token=NULL)
{
    service_type <- normalize_cognitive_type(service_type)
    url <- httr::parse_url(url)
    url$path <- get_api_path(service_type)

    object <- list(url=url, key=unname(key), aad_token=aad_token, cognitive_token=cognitive_token)
    class(object) <- "cognitive_endpoint"

    object
}


#' @export
print.cognitive_endpoint <- function(x, ...)
{
    cat("Azure Cognitive Service endpoint\n")
    cat("Service type:", sub("_.*$", "", class(x)[1]), "\n")
    cat("Endpoint URL:", httr::build_url(x$url), "\n")
    invisible(x)
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
#' @examples
#' \dontrun{
#'
#' endp <- cognitive_service("https://myvisionservice.api.cognitive.azure.com",
#'     service_type="Computervision", key="key")
#'
#' # analyze an online image
#' img_link <- "https://news.microsoft.com/uploads/2014/09/billg1_print.jpg"
#' call_cognitive_endpoint(endp,
#'     operation="analyze",
#'     body=list(url=img_link),
#'     options=list(details="celebrities"),
#'     http_verb="POST")
#'
#' # analyze an image on the local machine
#' img_raw <- readBin("image.jpg", "raw", file.info("image.jpg")$size)
#' call_cognitive_endpoint(endp,
#'     operation="analyze",
#'     body=img_raw,
#'     encode="raw",
#'     http_verb="POST")
#'
#' }
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
        headers$`content-type` <- "application/json"
    }
    else if(encode == "raw")
        headers$`content-type` <- "application/octet-stream"
    else stop("Unsupported encoding: ", encode, call.=FALSE)

    headers <- add_cognitive_auth(endpoint, headers)
    verb <- match.arg(http_verb)
    res <- httr::VERB(verb, url, headers, body=body, encode=encode)

    process_cognitive_response(res, match.arg(http_status_handler))
}


add_cognitive_auth <- function(endpoint, headers)
{
    headers <- if(!is.null(endpoint$key))
        c(headers, `Ocp-Apim-Subscription-Key`=unname(endpoint$key))
    else if(is_azure_token(endpoint$aad_token))
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


get_api_path <- function(type)
{
    switch(type,
        computervision="vision/v2.0",
        face="face/v1.0",
        luis="luis/v2.0",
        customvision=, customvision_training=, customvision_prediction="customvision/v3.0",
        contentmoderator="contentmoderator/moderate/v1.0",
        text="text/analytics/v2.0",
        texttranslate="translate",
        stop("Unknown cognitive service", call.=FALSE)
    )
}


normalize_cognitive_type <- function(type)
{
    tolower(gsub("[. ]", "_", type))
}
