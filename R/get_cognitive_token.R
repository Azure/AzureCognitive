#' Obtain authentication token for a cognitive service
#'
#' @param key The subscription key for the service.
#' @param region The Azure region where the service is located.
#' @param token_host Optionally, the URL for the token endpoint.
#' @export
get_cognitive_token <- function(key, region, token_host=NULL)
{
    if(is.null(token_host))
        token_host <- sprintf("https://%s.api.cognitive.microsoft.com/sts/v1.0/issueToken", normalize_region(region))

    hdrs <- httr::add_headers(`Ocp-Apim-Subscription-Key`=unname(key))
    res <- httr::POST(token_host, encode="form", hdrs)
    rawToChar(process_cognitive_response(res, "stop"))
}


normalize_region <- function(region)
{
    tolower(gsub(" ", "", region))
}
