context("Cognitive token auth")

tenant <- Sys.getenv("AZ_TEST_TENANT_ID")
app <- Sys.getenv("AZ_TEST_APP_ID")
password <- Sys.getenv("AZ_TEST_PASSWORD")
subscription <- Sys.getenv("AZ_TEST_SUBSCRIPTION")
storage <- Sys.getenv("AZ_TEST_STORAGE_ACCT")
svc_principal <- Sys.getenv("AZ_TEST_SVC_PRINCIPAL_ID")

if(tenant == "" || app == "" || password == "" || subscription == "" || storage == "" ||
   svc_principal == "")
    skip("Tests skipped: credentials not set")

rgname <- paste0(sample(letters, 10, TRUE), collapse="")
svcname <- paste0(sample(letters, 20, TRUE), collapse="")

az <- AzureRMR::az_rm$new(tenant=tenant, app=app, password=password)
sub <- az$get_subscription(subscription)
rg <- sub$create_resource_group(rgname, location="australiaeast")


test_that("Endpoint cogsvc token authentication works",
{
    cogsvc <- rg$create_cognitive_service(svcname, service_type="TextTranslation", service_tier="S1", location="global")

    # pause for Azure to catch up
    Sys.sleep(5)

    key <- cogsvc$list_keys()[1]
    tok <- get_cognitive_token(key, token_host=cogsvc$properties$endpoint)
    endp <- cognitive_endpoint(cogsvc$properties$endpoint, service_type="TextTranslation", cognitive_token=tok)
    expect_is(endp, "cognitive_endpoint")

    # manual hacking of text translation endpoint
    endp$url <- httr::parse_url("https://api.cognitive.microsofttranslator.com")
    res <- call_cognitive_endpoint(endp, "translate",
        options=list(`api-version`="3.0", from="en", to="de"),
        body=list(list(Text="Hello world")),
        http_verb="POST")
    expect_type(res, "list")
})

rg$delete(confirm=FALSE)

