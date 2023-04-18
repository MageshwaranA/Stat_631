library(shiny)
library(shiny.router)

home_page <- div(
  titlePanel("Dashboard"),
  p("This is a dashboard page")
)

settings_page <- div(
  titlePanel("Settings"),
  p("This is a settings page")
)

contact_page <- div(
  titlePanel("Contact"),
  p("This is a contact page")
)

router <- make_router(
  route("/", home_page),
  route("settings", settings_page),
  route("contact", contact_page)
)

ui <- fluidPage(
  tags$ul(
    tags$li(a(href = route_link("/"), "Dashboard")),
    tags$li(a(href = route_link("settings"), "Settings")),
    tags$li(a(href = route_link("contact"), "Contact"))
  ),
  router$ui
)

server <- function(input, output, session) {
  router$server(input, output, session)
}

shinyApp(ui, server)