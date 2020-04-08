library("shiny")
library("shiny.i18n")

translator = Translator$new(translation_json_path = "translation.json")
translator$set_translation_language("en")

shinyUI(fluidPage(uiOutput("content")))
