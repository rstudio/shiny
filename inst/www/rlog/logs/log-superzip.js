var log = [
  {
    "action": "define",
    "reactId": "r3",
    "label": "output$map",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.84779
  },
  {
    "action": "invalidateStart",
    "reactId": "r3",
    "ctxId": "ctx1",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.84848
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286556.8487
  },
  {
    "action": "invalidateEnd",
    "reactId": "r3",
    "ctxId": "ctx1",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.84943
  },
  {
    "action": "define",
    "reactId": "r4",
    "label": "zipsInBounds",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.85145
  },
  {
    "action": "define",
    "reactId": "r5",
    "label": "plotObj",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.87167
  },
  {
    "action": "define",
    "reactId": "r6",
    "label": "output$histCentile",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.91259
  },
  {
    "action": "invalidateStart",
    "reactId": "r6",
    "ctxId": "ctx2",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.91288
  },
  {
    "action": "invalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx2",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.91308
  },
  {
    "action": "define",
    "reactId": "r7",
    "label": "plotObj",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.94274
  },
  {
    "action": "define",
    "reactId": "r8",
    "label": "output$scatterCollegeIncome",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.94486
  },
  {
    "action": "invalidateStart",
    "reactId": "r8",
    "ctxId": "ctx3",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.94513
  },
  {
    "action": "invalidateEnd",
    "reactId": "r8",
    "ctxId": "ctx3",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.9453
  },
  {
    "action": "define",
    "reactId": "r9",
    "label": "observe({\n    colorBy <- input$color\n    sizeBy <- input$size\n    if (colorBy == \"superzip\") {\n        colorData <- ifelse(zipdata$centile >= (100 - input$threshold), \n            \"yes\", \"no\")\n        pal <- colorFactor(\"viridis\", colorData)\n    }\n    else {\n        colorData <- zipdata[[colorBy]]\n        pal <- colorBin(\"viridis\", colorData, 7, pretty = FALSE)\n    }\n    if (sizeBy == \"superzip\") {\n        radius <- ifelse(zipdata$centile >= (100 - input$threshold), \n            30000, 3000)\n    }\n    else {\n        radius <- zipdata[[sizeBy]]/max(zipdata[[sizeBy]]) * \n            30000\n    }\n    leafletProxy(\"map\", data = zipdata) %>% clearShapes() %>% \n        addCircles(~longitude, ~latitude, radius = radius, layerId = ~zipcode, \n            stroke = FALSE, fillOpacity = 0.4, fillColor = pal(colorData)) %>% \n        addLegend(\"bottomleft\", pal = pal, values = colorData, \n            title = colorBy, layerId = \"colorLegend\")\n})",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.94683
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx4",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.94721
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx4",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.94746
  },
  {
    "action": "define",
    "reactId": "r10",
    "label": "observe({\n    leafletProxy(\"map\") %>% clearPopups()\n    event <- input$map_shape_click\n    if (is.null(event)) \n        return()\n    isolate({\n        showZipcodePopup(event$id, event$lat, event$lng)\n    })\n})",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.94852
  },
  {
    "action": "invalidateStart",
    "reactId": "r10",
    "ctxId": "ctx5",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.94877
  },
  {
    "action": "invalidateEnd",
    "reactId": "r10",
    "ctxId": "ctx5",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.94894
  },
  {
    "action": "define",
    "reactId": "r11",
    "label": "observe({\n    cities <- if (is.null(input$states)) \n        character(0)\n    else {\n        filter(cleantable, State %in% input$states) %>% \"City\"$NULL %>% \n            unique() %>% sort()\n    }\n    stillSelected <- isolate(input$cities[input$cities %in% cities])\n    updateSelectInput(session, \"cities\", choices = cities, selected = stillSelected)\n})",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.94996
  },
  {
    "action": "invalidateStart",
    "reactId": "r11",
    "ctxId": "ctx6",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.95023
  },
  {
    "action": "invalidateEnd",
    "reactId": "r11",
    "ctxId": "ctx6",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.95042
  },
  {
    "action": "define",
    "reactId": "r12",
    "label": "observe({\n    zipcodes <- if (is.null(input$states)) \n        character(0)\n    else {\n        cleantable %>% filter(State %in% input$states, is.null(input$cities) | \n            City %in% input$cities) %>% \"Zipcode\"$NULL %>% unique() %>% \n            sort()\n    }\n    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% \n        zipcodes])\n    updateSelectInput(session, \"zipcodes\", choices = zipcodes, \n        selected = stillSelected)\n})",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.95155
  },
  {
    "action": "invalidateStart",
    "reactId": "r12",
    "ctxId": "ctx7",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.95182
  },
  {
    "action": "invalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx7",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.95201
  },
  {
    "action": "define",
    "reactId": "r13",
    "label": "observe({\n    if (is.null(input$goto)) \n        return()\n    isolate({\n        map <- leafletProxy(\"map\")\n        map %>% clearPopups()\n        dist <- 0.5\n        zip <- input$goto$zip\n        lat <- input$goto$lat\n        lng <- input$goto$lng\n        showZipcodePopup(zip, lat, lng)\n        map %>% fitBounds(lng - dist, lat - dist, lng + dist, \n            lat + dist)\n    })\n})",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.95311
  },
  {
    "action": "invalidateStart",
    "reactId": "r13",
    "ctxId": "ctx8",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.95338
  },
  {
    "action": "invalidateEnd",
    "reactId": "r13",
    "ctxId": "ctx8",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.95355
  },
  {
    "action": "define",
    "reactId": "r14",
    "label": "output$ziptable",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.96774
  },
  {
    "action": "invalidateStart",
    "reactId": "r14",
    "ctxId": "ctx9",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.96802
  },
  {
    "action": "invalidateEnd",
    "reactId": "r14",
    "ctxId": "ctx9",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.96815
  },
  {
    "action": "enter",
    "reactId": "r3",
    "ctxId": "ctx10",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.98276
  },
  {
    "action": "exit",
    "reactId": "r3",
    "ctxId": "ctx10",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.99005
  },
  {
    "action": "enter",
    "reactId": "r6",
    "ctxId": "ctx11",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.99066
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r5",
    "ctxId": "ctx11",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.9918
  },
  {
    "action": "enter",
    "reactId": "r5",
    "ctxId": "ctx12",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.99218
  },
  {
    "action": "isolateEnter",
    "reactId": "r5",
    "ctxId": "ctx13",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.99259
  },
  {
    "action": "define",
    "reactId": "r2$output_histCentile_width",
    "label": "clientData$output_histCentile_width",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.99281
  },
  {
    "action": "dependsOn",
    "reactId": "r5",
    "depOnReactId": "r2$output_histCentile_width",
    "ctxId": "ctx13",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286556.99295
  },
  {
    "action": "define",
    "reactId": "r2$output_histCentile_height",
    "label": "clientData$output_histCentile_height",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.00722
  },
  {
    "action": "dependsOn",
    "reactId": "r5",
    "depOnReactId": "r2$output_histCentile_height",
    "ctxId": "ctx13",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.00736
  },
  {
    "action": "isolateExit",
    "reactId": "r5",
    "ctxId": "ctx13",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.00753
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r5",
    "ctxId": "ctx13",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.00766
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r5",
    "depOnReactId": "r2$output_histCentile_width",
    "ctxId": "ctx13",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.00778
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r5",
    "depOnReactId": "r2$output_histCentile_height",
    "ctxId": "ctx13",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.00794
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r5",
    "ctxId": "ctx13",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.00831
  },
  {
    "action": "define",
    "reactId": "r2$pixelratio",
    "label": "clientData$pixelratio",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.00855
  },
  {
    "action": "dependsOn",
    "reactId": "r5",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx12",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.00868
  },
  {
    "action": "dependsOn",
    "reactId": "r5",
    "depOnReactId": "r4",
    "ctxId": "ctx12",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.01049
  },
  {
    "action": "enter",
    "reactId": "r4",
    "ctxId": "ctx14",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.01087
  },
  {
    "action": "define",
    "reactId": "r1$map_bounds",
    "label": "input$map_bounds",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.01111
  },
  {
    "action": "dependsOn",
    "reactId": "r4",
    "depOnReactId": "r1$map_bounds",
    "ctxId": "ctx14",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.01123
  },
  {
    "action": "exit",
    "reactId": "r4",
    "ctxId": "ctx14",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.01575
  },
  {
    "action": "exit",
    "reactId": "r5",
    "ctxId": "ctx12",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.0247
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r2$output_histCentile_width",
    "ctxId": "ctx11",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.0252
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r2$output_histCentile_height",
    "ctxId": "ctx11",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.02561
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx11",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.02586
  },
  {
    "action": "exit",
    "reactId": "r6",
    "ctxId": "ctx11",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.02708
  },
  {
    "action": "enter",
    "reactId": "r8",
    "ctxId": "ctx15",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.02787
  },
  {
    "action": "dependsOn",
    "reactId": "r8",
    "depOnReactId": "r7",
    "ctxId": "ctx15",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.02891
  },
  {
    "action": "enter",
    "reactId": "r7",
    "ctxId": "ctx16",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.02924
  },
  {
    "action": "isolateEnter",
    "reactId": "r7",
    "ctxId": "ctx17",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.02961
  },
  {
    "action": "define",
    "reactId": "r2$output_scatterCollegeIncome_width",
    "label": "clientData$output_scatterCollegeIncome_width",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.02981
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r2$output_scatterCollegeIncome_width",
    "ctxId": "ctx17",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.02993
  },
  {
    "action": "define",
    "reactId": "r2$output_scatterCollegeIncome_height",
    "label": "clientData$output_scatterCollegeIncome_height",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.03012
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r2$output_scatterCollegeIncome_height",
    "ctxId": "ctx17",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.03023
  },
  {
    "action": "isolateExit",
    "reactId": "r7",
    "ctxId": "ctx17",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.03035
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r7",
    "ctxId": "ctx17",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.03046
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r7",
    "depOnReactId": "r2$output_scatterCollegeIncome_width",
    "ctxId": "ctx17",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.03058
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r7",
    "depOnReactId": "r2$output_scatterCollegeIncome_height",
    "ctxId": "ctx17",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.03073
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r7",
    "ctxId": "ctx17",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.03085
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx16",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.031
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r4",
    "ctxId": "ctx16",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.05455
  },
  {
    "action": "exit",
    "reactId": "r7",
    "ctxId": "ctx16",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.0586
  },
  {
    "action": "dependsOn",
    "reactId": "r8",
    "depOnReactId": "r2$output_scatterCollegeIncome_width",
    "ctxId": "ctx15",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.05885
  },
  {
    "action": "dependsOn",
    "reactId": "r8",
    "depOnReactId": "r2$output_scatterCollegeIncome_height",
    "ctxId": "ctx15",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.05904
  },
  {
    "action": "dependsOn",
    "reactId": "r8",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx15",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.05921
  },
  {
    "action": "exit",
    "reactId": "r8",
    "ctxId": "ctx15",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.06995
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx18",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.07067
  },
  {
    "action": "define",
    "reactId": "r1$color",
    "label": "input$color",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.07095
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx18",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.07109
  },
  {
    "action": "define",
    "reactId": "r1$size",
    "label": "input$size",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.07128
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx18",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.0714
  },
  {
    "action": "define",
    "reactId": "r1$threshold",
    "label": "input$threshold",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.07163
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx18",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.07176
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx18",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.2473
  },
  {
    "action": "enter",
    "reactId": "r10",
    "ctxId": "ctx19",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.24803
  },
  {
    "action": "define",
    "reactId": "r1$map_shape_click",
    "label": "input$map_shape_click",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.24978
  },
  {
    "action": "dependsOn",
    "reactId": "r10",
    "depOnReactId": "r1$map_shape_click",
    "ctxId": "ctx19",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.24989
  },
  {
    "action": "exit",
    "reactId": "r10",
    "ctxId": "ctx19",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25001
  },
  {
    "action": "enter",
    "reactId": "r11",
    "ctxId": "ctx20",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25046
  },
  {
    "action": "define",
    "reactId": "r1$states",
    "label": "input$states",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25062
  },
  {
    "action": "dependsOn",
    "reactId": "r11",
    "depOnReactId": "r1$states",
    "ctxId": "ctx20",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25072
  },
  {
    "action": "isolateEnter",
    "reactId": "r11",
    "ctxId": "ctx21",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.251
  },
  {
    "action": "define",
    "reactId": "r1$cities",
    "label": "input$cities",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25115
  },
  {
    "action": "dependsOn",
    "reactId": "r11",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx21",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25126
  },
  {
    "action": "isolateExit",
    "reactId": "r11",
    "ctxId": "ctx21",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.2514
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r11",
    "ctxId": "ctx21",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25151
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r11",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx21",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25161
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r11",
    "ctxId": "ctx21",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25173
  },
  {
    "action": "exit",
    "reactId": "r11",
    "ctxId": "ctx20",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25192
  },
  {
    "action": "enter",
    "reactId": "r12",
    "ctxId": "ctx22",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25235
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$states",
    "ctxId": "ctx22",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.2525
  },
  {
    "action": "isolateEnter",
    "reactId": "r12",
    "ctxId": "ctx23",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25277
  },
  {
    "action": "define",
    "reactId": "r1$zipcodes",
    "label": "input$zipcodes",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25294
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx23",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.2532
  },
  {
    "action": "isolateExit",
    "reactId": "r12",
    "ctxId": "ctx23",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25339
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r12",
    "ctxId": "ctx23",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.2535
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx23",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25362
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx23",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25375
  },
  {
    "action": "exit",
    "reactId": "r12",
    "ctxId": "ctx22",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25394
  },
  {
    "action": "enter",
    "reactId": "r13",
    "ctxId": "ctx24",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25442
  },
  {
    "action": "define",
    "reactId": "r1$goto",
    "label": "input$goto",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25457
  },
  {
    "action": "dependsOn",
    "reactId": "r13",
    "depOnReactId": "r1$goto",
    "ctxId": "ctx24",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25467
  },
  {
    "action": "exit",
    "reactId": "r13",
    "ctxId": "ctx24",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.25479
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286557.25489
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286557.25554
  },
  {
    "action": "invalidateStart",
    "reactId": "r4",
    "ctxId": "ctx14",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.34735
  },
  {
    "action": "invalidateStart",
    "reactId": "r5",
    "ctxId": "ctx12",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.34755
  },
  {
    "action": "invalidateStart",
    "reactId": "r6",
    "ctxId": "ctx11",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.34773
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286557.34854
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r5",
    "ctxId": "ctx11",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.3492
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r2$output_histCentile_width",
    "ctxId": "ctx11",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.34941
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r2$output_histCentile_height",
    "ctxId": "ctx11",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.34956
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx11",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.34973
  },
  {
    "action": "invalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx11",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.34994
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r5",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx12",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35013
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r5",
    "depOnReactId": "r4",
    "ctxId": "ctx12",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35035
  },
  {
    "action": "invalidateEnd",
    "reactId": "r5",
    "ctxId": "ctx12",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.3505
  },
  {
    "action": "invalidateStart",
    "reactId": "r7",
    "ctxId": "ctx16",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35069
  },
  {
    "action": "invalidateStart",
    "reactId": "r8",
    "ctxId": "ctx15",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35088
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r8",
    "depOnReactId": "r7",
    "ctxId": "ctx15",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35181
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r8",
    "depOnReactId": "r2$output_scatterCollegeIncome_width",
    "ctxId": "ctx15",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35208
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r8",
    "depOnReactId": "r2$output_scatterCollegeIncome_height",
    "ctxId": "ctx15",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35226
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r8",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx15",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35249
  },
  {
    "action": "invalidateEnd",
    "reactId": "r8",
    "ctxId": "ctx15",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35264
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r7",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx16",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35275
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r7",
    "depOnReactId": "r4",
    "ctxId": "ctx16",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35298
  },
  {
    "action": "invalidateEnd",
    "reactId": "r7",
    "ctxId": "ctx16",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35325
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r4",
    "depOnReactId": "r1$map_bounds",
    "ctxId": "ctx14",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35354
  },
  {
    "action": "invalidateEnd",
    "reactId": "r4",
    "ctxId": "ctx14",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35379
  },
  {
    "action": "enter",
    "reactId": "r6",
    "ctxId": "ctx25",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.35493
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r5",
    "ctxId": "ctx25",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.37618
  },
  {
    "action": "enter",
    "reactId": "r5",
    "ctxId": "ctx26",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.39393
  },
  {
    "action": "isolateEnter",
    "reactId": "r5",
    "ctxId": "ctx27",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.39466
  },
  {
    "action": "dependsOn",
    "reactId": "r5",
    "depOnReactId": "r2$output_histCentile_width",
    "ctxId": "ctx27",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.39497
  },
  {
    "action": "dependsOn",
    "reactId": "r5",
    "depOnReactId": "r2$output_histCentile_height",
    "ctxId": "ctx27",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.39534
  },
  {
    "action": "isolateExit",
    "reactId": "r5",
    "ctxId": "ctx27",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.39558
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r5",
    "ctxId": "ctx27",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.39579
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r5",
    "depOnReactId": "r2$output_histCentile_width",
    "ctxId": "ctx27",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.39602
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r5",
    "depOnReactId": "r2$output_histCentile_height",
    "ctxId": "ctx27",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.39628
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r5",
    "ctxId": "ctx27",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.39645
  },
  {
    "action": "dependsOn",
    "reactId": "r5",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx26",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.39661
  },
  {
    "action": "dependsOn",
    "reactId": "r5",
    "depOnReactId": "r4",
    "ctxId": "ctx26",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.39846
  },
  {
    "action": "enter",
    "reactId": "r4",
    "ctxId": "ctx28",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.39882
  },
  {
    "action": "dependsOn",
    "reactId": "r4",
    "depOnReactId": "r1$map_bounds",
    "ctxId": "ctx28",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.40747
  },
  {
    "action": "exit",
    "reactId": "r4",
    "ctxId": "ctx28",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.41005
  },
  {
    "action": "exit",
    "reactId": "r5",
    "ctxId": "ctx26",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44106
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r2$output_histCentile_width",
    "ctxId": "ctx25",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44128
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r2$output_histCentile_height",
    "ctxId": "ctx25",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44144
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx25",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44158
  },
  {
    "action": "exit",
    "reactId": "r6",
    "ctxId": "ctx25",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44256
  },
  {
    "action": "enter",
    "reactId": "r8",
    "ctxId": "ctx29",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44308
  },
  {
    "action": "dependsOn",
    "reactId": "r8",
    "depOnReactId": "r7",
    "ctxId": "ctx29",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44395
  },
  {
    "action": "enter",
    "reactId": "r7",
    "ctxId": "ctx30",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44449
  },
  {
    "action": "isolateEnter",
    "reactId": "r7",
    "ctxId": "ctx31",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44503
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r2$output_scatterCollegeIncome_width",
    "ctxId": "ctx31",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44526
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r2$output_scatterCollegeIncome_height",
    "ctxId": "ctx31",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44543
  },
  {
    "action": "isolateExit",
    "reactId": "r7",
    "ctxId": "ctx31",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44555
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r7",
    "ctxId": "ctx31",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44564
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r7",
    "depOnReactId": "r2$output_scatterCollegeIncome_width",
    "ctxId": "ctx31",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44574
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r7",
    "depOnReactId": "r2$output_scatterCollegeIncome_height",
    "ctxId": "ctx31",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44587
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r7",
    "ctxId": "ctx31",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44598
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx30",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.4461
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r4",
    "ctxId": "ctx30",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.44747
  },
  {
    "action": "exit",
    "reactId": "r7",
    "ctxId": "ctx30",
    "type": "observable",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.70754
  },
  {
    "action": "dependsOn",
    "reactId": "r8",
    "depOnReactId": "r2$output_scatterCollegeIncome_width",
    "ctxId": "ctx29",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.7078
  },
  {
    "action": "dependsOn",
    "reactId": "r8",
    "depOnReactId": "r2$output_scatterCollegeIncome_height",
    "ctxId": "ctx29",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.70814
  },
  {
    "action": "dependsOn",
    "reactId": "r8",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx29",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.70849
  },
  {
    "action": "exit",
    "reactId": "r8",
    "ctxId": "ctx29",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286557.70978
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286557.70993
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286557.71067
  },
  {
    "action": "valueChange",
    "reactId": "r1$threshold",
    "value": " int 6",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.49989
  },
  {
    "action": "invalidateStart",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.50003
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx18",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.50021
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286559.50037
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx18",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.50098
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx18",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.50113
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx18",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.50126
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx18",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.5014
  },
  {
    "action": "invalidateEnd",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.50153
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx32",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.50245
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx32",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.51584
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx32",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.51605
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx32",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.51624
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx32",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286559.67314
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286559.67335
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286559.6743
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286561.64174
  },
  {
    "action": "enter",
    "reactId": "r14",
    "ctxId": "ctx33",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286561.64311
  },
  {
    "action": "define",
    "reactId": "r1$minScore",
    "label": "input$minScore",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286561.67499
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$minScore",
    "ctxId": "ctx33",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286561.67515
  },
  {
    "action": "define",
    "reactId": "r1$maxScore",
    "label": "input$maxScore",
    "type": "reactiveValuesKey",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286561.676
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$maxScore",
    "ctxId": "ctx33",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286561.67613
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$states",
    "ctxId": "ctx33",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286561.67661
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx33",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286561.67751
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx33",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286561.67837
  },
  {
    "action": "exit",
    "reactId": "r14",
    "ctxId": "ctx33",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286562.01082
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286562.01099
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286562.01174
  },
  {
    "action": "valueChange",
    "reactId": "r1$states",
    "value": " chr \"AZ\"",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.6891
  },
  {
    "action": "invalidateStart",
    "reactId": "r1$states",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.68927
  },
  {
    "action": "invalidateStart",
    "reactId": "r11",
    "ctxId": "ctx20",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.68949
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286564.6897
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r11",
    "depOnReactId": "r1$states",
    "ctxId": "ctx20",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69039
  },
  {
    "action": "invalidateEnd",
    "reactId": "r11",
    "ctxId": "ctx20",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69056
  },
  {
    "action": "invalidateStart",
    "reactId": "r12",
    "ctxId": "ctx22",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69071
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$states",
    "ctxId": "ctx22",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69093
  },
  {
    "action": "invalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx22",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.6911
  },
  {
    "action": "invalidateStart",
    "reactId": "r14",
    "ctxId": "ctx33",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69124
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$minScore",
    "ctxId": "ctx33",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69215
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$maxScore",
    "ctxId": "ctx33",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69234
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$states",
    "ctxId": "ctx33",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69252
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx33",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69268
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx33",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69285
  },
  {
    "action": "invalidateEnd",
    "reactId": "r14",
    "ctxId": "ctx33",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.693
  },
  {
    "action": "invalidateEnd",
    "reactId": "r1$states",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69311
  },
  {
    "action": "enter",
    "reactId": "r11",
    "ctxId": "ctx34",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69408
  },
  {
    "action": "dependsOn",
    "reactId": "r11",
    "depOnReactId": "r1$states",
    "ctxId": "ctx34",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69432
  },
  {
    "action": "isolateEnter",
    "reactId": "r11",
    "ctxId": "ctx35",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69786
  },
  {
    "action": "dependsOn",
    "reactId": "r11",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx35",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.6982
  },
  {
    "action": "isolateExit",
    "reactId": "r11",
    "ctxId": "ctx35",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69866
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r11",
    "ctxId": "ctx35",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69896
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r11",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx35",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69913
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r11",
    "ctxId": "ctx35",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.69951
  },
  {
    "action": "exit",
    "reactId": "r11",
    "ctxId": "ctx34",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.70639
  },
  {
    "action": "enter",
    "reactId": "r12",
    "ctxId": "ctx36",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.70709
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$states",
    "ctxId": "ctx36",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.70731
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx36",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.71015
  },
  {
    "action": "isolateEnter",
    "reactId": "r12",
    "ctxId": "ctx37",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.71168
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx37",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.71189
  },
  {
    "action": "isolateExit",
    "reactId": "r12",
    "ctxId": "ctx37",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.71209
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r12",
    "ctxId": "ctx37",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.71223
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx37",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.71238
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx37",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.71255
  },
  {
    "action": "exit",
    "reactId": "r12",
    "ctxId": "ctx36",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.72365
  },
  {
    "action": "enter",
    "reactId": "r14",
    "ctxId": "ctx38",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.72458
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$minScore",
    "ctxId": "ctx38",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.77788
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$maxScore",
    "ctxId": "ctx38",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.77837
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$states",
    "ctxId": "ctx38",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.779
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx38",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.78041
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx38",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.78125
  },
  {
    "action": "exit",
    "reactId": "r14",
    "ctxId": "ctx38",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286564.95821
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286564.95836
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286564.959
  },
  {
    "action": "valueChange",
    "reactId": "r1$cities",
    "value": " chr \"Alpine\"",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43447
  },
  {
    "action": "invalidateStart",
    "reactId": "r1$cities",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43464
  },
  {
    "action": "invalidateStart",
    "reactId": "r12",
    "ctxId": "ctx36",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.4349
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286566.43512
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$states",
    "ctxId": "ctx36",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43585
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx36",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43603
  },
  {
    "action": "invalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx36",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43619
  },
  {
    "action": "invalidateStart",
    "reactId": "r14",
    "ctxId": "ctx38",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43632
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$minScore",
    "ctxId": "ctx38",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43718
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$maxScore",
    "ctxId": "ctx38",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43736
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$states",
    "ctxId": "ctx38",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43751
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx38",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43767
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx38",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43789
  },
  {
    "action": "invalidateEnd",
    "reactId": "r14",
    "ctxId": "ctx38",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43812
  },
  {
    "action": "invalidateEnd",
    "reactId": "r1$cities",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43828
  },
  {
    "action": "enter",
    "reactId": "r12",
    "ctxId": "ctx39",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.43998
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$states",
    "ctxId": "ctx39",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.44034
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx39",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.44314
  },
  {
    "action": "isolateEnter",
    "reactId": "r12",
    "ctxId": "ctx40",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.44555
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx40",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.44578
  },
  {
    "action": "isolateExit",
    "reactId": "r12",
    "ctxId": "ctx40",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.44598
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r12",
    "ctxId": "ctx40",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.44612
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx40",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.44626
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx40",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.44646
  },
  {
    "action": "exit",
    "reactId": "r12",
    "ctxId": "ctx39",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.44682
  },
  {
    "action": "enter",
    "reactId": "r14",
    "ctxId": "ctx41",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.44745
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$minScore",
    "ctxId": "ctx41",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.45064
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$maxScore",
    "ctxId": "ctx41",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.45142
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$states",
    "ctxId": "ctx41",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.45223
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx41",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.45362
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx41",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.45566
  },
  {
    "action": "exit",
    "reactId": "r14",
    "ctxId": "ctx41",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286566.47492
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286566.4751
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286566.47594
  },
  {
    "action": "valueChange",
    "reactId": "r1$zipcodes",
    "value": " chr \"85920\"",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.2808
  },
  {
    "action": "invalidateStart",
    "reactId": "r1$zipcodes",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28099
  },
  {
    "action": "invalidateStart",
    "reactId": "r14",
    "ctxId": "ctx41",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28123
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286568.28219
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$minScore",
    "ctxId": "ctx41",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28298
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$maxScore",
    "ctxId": "ctx41",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28333
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$states",
    "ctxId": "ctx41",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28353
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx41",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28369
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx41",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28385
  },
  {
    "action": "invalidateEnd",
    "reactId": "r14",
    "ctxId": "ctx41",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28401
  },
  {
    "action": "invalidateEnd",
    "reactId": "r1$zipcodes",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28414
  },
  {
    "action": "enter",
    "reactId": "r14",
    "ctxId": "ctx42",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28571
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$minScore",
    "ctxId": "ctx42",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28826
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$maxScore",
    "ctxId": "ctx42",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28888
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$states",
    "ctxId": "ctx42",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.28959
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx42",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.29129
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx42",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.2932
  },
  {
    "action": "exit",
    "reactId": "r14",
    "ctxId": "ctx42",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286568.31119
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286568.31135
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286568.31232
  },
  {
    "action": "valueChange",
    "reactId": "r1$threshold",
    "value": " int 5",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.34872
  },
  {
    "action": "invalidateStart",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.34889
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx32",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.34912
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286575.34933
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx32",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.35021
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx32",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.35042
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx32",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.3506
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx32",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.35079
  },
  {
    "action": "invalidateEnd",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.35091
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx43",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.35224
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx43",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.35253
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx43",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.35275
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx43",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.35298
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx43",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286575.42205
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286575.42222
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286575.42312
  },
  {
    "action": "valueChange",
    "reactId": "r1$threshold",
    "value": " int 6",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.39067
  },
  {
    "action": "invalidateStart",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.39084
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx43",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.39108
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286576.39129
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx43",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.39204
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx43",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.39223
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx43",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.3924
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx43",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.39256
  },
  {
    "action": "invalidateEnd",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.39267
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx44",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.39357
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx44",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.39386
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx44",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.39418
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx44",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.39446
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx44",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286576.45315
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286576.45328
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286576.45389
  },
  {
    "action": "valueChange",
    "reactId": "r1$threshold",
    "value": " int 8",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.08429
  },
  {
    "action": "invalidateStart",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.08457
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx44",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.08483
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286577.08507
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx44",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.08578
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx44",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.08596
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx44",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.08622
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx44",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.08646
  },
  {
    "action": "invalidateEnd",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.08666
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx45",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.08762
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx45",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.08783
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx45",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.08802
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx45",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.08823
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx45",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.17489
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286577.17503
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286577.17586
  },
  {
    "action": "valueChange",
    "reactId": "r1$threshold",
    "value": " int 9",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.74548
  },
  {
    "action": "invalidateStart",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.74565
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx45",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.74589
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286577.74611
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx45",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.74687
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx45",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.74707
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx45",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.74723
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx45",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.74738
  },
  {
    "action": "invalidateEnd",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.74753
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx46",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.74879
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx46",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.74912
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx46",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.74936
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx46",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.7496
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx46",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286577.94922
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286577.94937
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286577.95008
  },
  {
    "action": "valueChange",
    "reactId": "r1$threshold",
    "value": " int 10",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.5
  },
  {
    "action": "invalidateStart",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.50016
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx46",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.50039
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286578.5006
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx46",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.50142
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx46",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.50162
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx46",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.50178
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx46",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.50194
  },
  {
    "action": "invalidateEnd",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.50206
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx47",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.50308
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx47",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.50334
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx47",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.5036
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx47",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.50384
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx47",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286578.57866
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286578.57885
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286578.57961
  },
  {
    "action": "valueChange",
    "reactId": "r1$threshold",
    "value": " int 11",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.47317
  },
  {
    "action": "invalidateStart",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.47335
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx47",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.47359
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286579.4738
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx47",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.47461
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx47",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.47484
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx47",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.47503
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx47",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.47523
  },
  {
    "action": "invalidateEnd",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.47538
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx48",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.47642
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx48",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.47664
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx48",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.47686
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx48",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.47709
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx48",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286579.54842
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286579.54857
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286579.54933
  },
  {
    "action": "valueChange",
    "reactId": "r1$threshold",
    "value": " int 12",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.10166
  },
  {
    "action": "invalidateStart",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.10182
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx48",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.10205
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525286580.10232
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx48",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.10317
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx48",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.10346
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx48",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.10365
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx48",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.10381
  },
  {
    "action": "invalidateEnd",
    "reactId": "r1$threshold",
    "ctxId": "ctx",
    "type": "other",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.10393
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx49",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.10489
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx49",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.10509
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx49",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.1053
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx49",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.1055
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx49",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286580.17837
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525286580.17853
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525286580.17925
  },
  {
    "action": "invalidateStart",
    "reactId": "r3",
    "ctxId": "ctx10",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10495
  },
  {
    "action": "invalidateEnd",
    "reactId": "r3",
    "ctxId": "ctx10",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10509
  },
  {
    "action": "invalidateStart",
    "reactId": "r6",
    "ctxId": "ctx25",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10528
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r5",
    "ctxId": "ctx25",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10539
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r2$output_histCentile_width",
    "ctxId": "ctx25",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10554
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r2$output_histCentile_height",
    "ctxId": "ctx25",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10565
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx25",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10576
  },
  {
    "action": "invalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx25",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10587
  },
  {
    "action": "invalidateStart",
    "reactId": "r8",
    "ctxId": "ctx29",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10605
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r8",
    "depOnReactId": "r7",
    "ctxId": "ctx29",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10617
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r8",
    "depOnReactId": "r2$output_scatterCollegeIncome_width",
    "ctxId": "ctx29",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10629
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r8",
    "depOnReactId": "r2$output_scatterCollegeIncome_height",
    "ctxId": "ctx29",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.1064
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r8",
    "depOnReactId": "r2$pixelratio",
    "ctxId": "ctx29",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.1065
  },
  {
    "action": "invalidateEnd",
    "reactId": "r8",
    "ctxId": "ctx29",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.1066
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx49",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10679
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$color",
    "ctxId": "ctx49",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10693
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$size",
    "ctxId": "ctx49",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.1071
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r1$threshold",
    "ctxId": "ctx49",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10723
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx49",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10736
  },
  {
    "action": "invalidateStart",
    "reactId": "r10",
    "ctxId": "ctx19",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10757
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r10",
    "depOnReactId": "r1$map_shape_click",
    "ctxId": "ctx19",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10769
  },
  {
    "action": "invalidateEnd",
    "reactId": "r10",
    "ctxId": "ctx19",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10783
  },
  {
    "action": "invalidateStart",
    "reactId": "r11",
    "ctxId": "ctx34",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10802
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r11",
    "depOnReactId": "r1$states",
    "ctxId": "ctx34",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10813
  },
  {
    "action": "invalidateEnd",
    "reactId": "r11",
    "ctxId": "ctx34",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10825
  },
  {
    "action": "invalidateStart",
    "reactId": "r12",
    "ctxId": "ctx39",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10845
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$states",
    "ctxId": "ctx39",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10856
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx39",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10869
  },
  {
    "action": "invalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx39",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.1088
  },
  {
    "action": "invalidateStart",
    "reactId": "r13",
    "ctxId": "ctx24",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.109
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r13",
    "depOnReactId": "r1$goto",
    "ctxId": "ctx24",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10911
  },
  {
    "action": "invalidateEnd",
    "reactId": "r13",
    "ctxId": "ctx24",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10922
  },
  {
    "action": "invalidateStart",
    "reactId": "r14",
    "ctxId": "ctx42",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10942
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$minScore",
    "ctxId": "ctx42",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10953
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$maxScore",
    "ctxId": "ctx42",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10965
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$states",
    "ctxId": "ctx42",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10979
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$cities",
    "ctxId": "ctx42",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.10989
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r1$zipcodes",
    "ctxId": "ctx42",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.11
  },
  {
    "action": "invalidateEnd",
    "reactId": "r14",
    "ctxId": "ctx42",
    "type": "observer",
    "session": "4fb6817bd90798a7456dd60a7342a2ad",
    "time": 1525286583.11011
  }
]
