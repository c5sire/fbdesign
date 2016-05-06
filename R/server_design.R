#' server_design
#'
#' Design a fieldbook
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
#' @param dom target dom element name
#' @param values reactive values
#' @author Reinhard Simon
#' export
server_design <- function(input, output, session, dom="hot_fieldbook_design", values){
  output$fbDesign_crop <- shiny::renderUI({
    ct = fbcrops::get_crop_table()
    chc = as.list(ct$crop_name)
    nms = paste0(ct$crop_name, " (", ct$crop_id, ")")
    names(chc) = nms
    shiny::selectInput("designFieldbook_crop", "Crop", chc)
  })

  shiny::observe({
    shinyjs::toggle(condition = input$fbDesign_environment_type == "field",
                    selector = "#fbDesignNav li a[data-value=fbDesign_field]")
    shinyjs::toggle(condition = input$fbDesign_environment_type == "farmers_field",
                    selector = "#fbDesignNav li a[data-value=fbDesign_farmers_field]")
    shinyjs::toggle(condition = input$fbDesign_environment_type == "greenhouse",
                    selector = "#fbDesignNav li a[data-value=fbDesign_greenhouse]")
    shinyjs::toggle(condition = input$fbDesign_environment_type == "screenhouse",
                    selector = "#fbDesignNav li a[data-value=fbDesign_screenhouse]")
    shinyjs::toggle(condition = input$fbDesign_weather_cb,
                    selector = "#fbDesignNav li a[data-value=fbDesign_weather]")
    shinyjs::toggle(condition = input$fbDesign_soil_cb,
                    selector = "#fbDesignNav li a[data-value=fbDesign_soil]")
  })

  output$fbDesign_program <- shiny::renderUI({
    if (!is.null(input$designFieldbook_crop)) {
      tbl = fbcrops::get_crop_table()
      crp = tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]

      tbl = fbprogram::get_program_table()
      prg = tbl[tbl$crop_id == crp, ]

      if (nrow(prg) > 0 ) {
        lbl = paste0(prg$program_name, " (", prg$program_id, ")" )
        chc = as.list(prg$program_id)
        names(chc) = lbl
        shiny::selectInput("designFieldbook_program", "Investigation", chc)
      }
    }
  })

  output$fbDesign_phase <- shiny::renderUI({
    if (!is.null(input$designFieldbook_crop)) {
      tbl = fbcrops::get_crop_table()
      crp = tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]

      tbl = fbprstages::get_program_stage_table()
      prg = tbl[tbl$crop_id == crp, ]

      if (nrow(prg) > 0 ) {
        lbl = paste0(prg$program_stage_name, " (", prg$program_stage_id, ")" )
        chc = as.list(prg$program_stage_id)
        names(chc) = lbl
        shiny::selectInput("designFieldbook_phase", "Study", chc)
      }
    }
  })

  output$designFieldbook_genotypes <- shiny::renderUI({
    chc <- fbmaterials::list_material_lists(input$designFieldbook_crop, short = TRUE)
    shiny::selectInput("designFieldbook_trt1", "Genotype list", chc, width = 400)
  })

  fbdesign_id <- shiny::reactive({
    if (!is.null(input$designFieldbook_crop)) {
      tbl = fbcrops::get_crop_table()
      crop_id = tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]
      program_id = input$designFieldbook_program
      phase_id = input$designFieldbook_phase
      module_id = input$designFieldbook_module
      ayear = input$designFieldbook_year
      amonth = stringr::str_pad(input$designFieldbook_month, width = 2, side = "left",
                                pad = "0")
      sites = input$designFieldbook_sites
      out = paste0(crop_id, program_id, phase_id, module_id, ayear, amonth, "_", sites)
      paste(out, collapse = ", ")
    }
  })

  output$fbDesign_id <- shiny::renderText({
    fbdesign_id()
  })

  output$fbDesign_countrySite <- shiny::renderUI({
    locs = fbsites::get_site_table()
    if (nrow(locs) > 0 ) {
      chc = locs$shortn
      shiny::selectizeInput("designFieldbook_sites", label = "Field locations:",
                            choices = chc, selected = 1,  multiple = FALSE)
    }
  })

  output$fbDesign_variables <- shiny::renderUI({
    crp <- input$designFieldbook_crop
    mdl <- fbmodule::list_modules(crp)
    ids <- unlist(stringr::str_extract_all(mdl, "([A-Z]{2})"))
    vls <- mdl
    mdl = as.list(ids)
    names(mdl) <- vls

    #print(mdl)
    shiny::selectInput("designFieldbook_module", label = "Assay (fieldbook module):",
                       choices = mdl, selected = 1)
  })

  fbdraft <- shiny::reactive({
    try({
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...', value = 0, {
                     incProgress(3/15)
                     crp <- input$designFieldbook_crop
                     # print(crp)
                     # print(fbglobal::fname_material_lists(crp))
                     # print(input$designFieldbook_trt1)
                     incProgress(3/15)
                     fn =file.path(fbglobal::fname_material_lists(crp), input$designFieldbook_trt1)
                     # print(fn)
                     load(fn)
                     trt1 = table_materials$institutional_number
                     incProgress(3/15)
                     mdl = input$designFieldbook_module
                     mdl = stringr::str_extract(mdl, "([A-Z]{2})")[[1]]
                     vars = fbmodule::get_module_table(crp)
                     vars = vars[vars$module == mdl, "variable"]
                     # print(trt1)
                     # print(input$designFieldbook)
                     incProgress(3/15)

                     fb = design_fieldbook(design = input$designFieldbook,
                                           trt1 = trt1, trt1_label = "GENOTYPE",
                                           r = as.integer(input$designFieldbook_r),
                                           k = as.integer(input$designFieldbook_k),
                                           first = as.logical(input$designFieldbook_first),
                                           cont = as.logical(input$designFieldbook_cont),
                                           series = as.integer(input$designFieldbook_serie),
                                           zigzag = as.logical(input$designFieldbook_zigzag),
                                           variables = vars)
                     #print(fb)
                     fb[, 1] = as.integer(fb[, 1])

                    fb
                    } )
    })
  })


  shiny::observeEvent(input$fbDesign_draft, {
    fb = fbdraft()
    output$fbDesign_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(fb, readOnly = T)
    })
  })

  shiny::observeEvent(input$fbDesign_create, {
    #print("Heyoo")
    fb = fbdraft()
    try({
      fn = paste0(fbdesign_id(), ".rda")
      fp = file.path(fbglobal::fname_fieldbooks(input$designFieldbook_crop), fn)
      # print(fn)
      # print(fp)
      # print(str(fb))
      # print(head(fb))
      if(!file.exists(fp)) {
        saveRDS(fb, fp)
        values[["ph_fb_list"]] = NULL
        shinyBS::createAlert(session, "alert_fb_done", "fbdoneAlert", title = "Success",
                             content = "Fieldbook created.", append = FALSE)
      }

    })
  })

}
