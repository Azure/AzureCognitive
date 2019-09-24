context("Cognitive services")

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


test_that("Resource creation works",
{
    cogsvc <- rg$create_cognitive_service(svcname, service_type="ComputerVision", service_tier="S1")
    expect_is(cogsvc, "az_cognitive_service")

    cogsvc$add_role_assignment(svc_principal, "cognitive services user")
})

test_that("Resource methods work",
{
    cogsvc <- rg$get_cognitive_service(svcname)
    expect_is(cogsvc, "az_cognitive_service")

    expect_type(cogsvc$list_keys(), "character")
    expect_type(cogsvc$regen_key(2), "character")

    expect_type(cogsvc$list_service_tiers(), "list")
})

test_that("Endpoint works",
{
    cogsvc <- rg$get_cognitive_service(svcname)
    endp <- cogsvc$get_endpoint()
    expect_is(endp, "cognitive_endpoint")

    key <- cogsvc$list_keys()[1]
    endp2 <- cognitive_endpoint(cogsvc$properties$endpoint, service_type="ComputerVision", key=key)
    expect_identical(endp, endp2)

    img_url <- httr::parse_url(storage)
    img_url$path <- "cognitive/bill.jpg"
    res <- call_cognitive_endpoint(endp, "analyze", body=list(url=httr::build_url(img_url)), http_verb="POST")
    expect_type(res, "list")

    img_raw <- readBin("../resources/bill.jpg", "raw", file.info("../resources/bill.jpg")$size)
    res2 <- call_cognitive_endpoint(endp, "analyze", body=img_raw, encode="raw", http_verb="POST")
    expect_type(res2, "list")
})

test_that("Endpoint AAD authentication works",
{
    cogsvc <- rg$get_cognitive_service(svcname)
    tok <- AzureAuth::get_azure_token("https://cognitiveservices.azure.com", tenant, app, password)
    endp <- cognitive_endpoint(cogsvc$properties$endpoint, service_type="ComputerVision", aad_token=tok)
    expect_is(endp, "cognitive_endpoint")

    img_url <- httr::parse_url(storage)
    img_url$path <- "cognitive/bill.jpg"
    res <- call_cognitive_endpoint(endp, "analyze", body=list(url=httr::build_url(img_url)), http_verb="POST")
    expect_type(res, "list")
})

rg$delete(confirm=FALSE)
