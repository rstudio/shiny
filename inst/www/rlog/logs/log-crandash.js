var log = [
  {
    "action": "define",
    "reactId": "r3",
    "label": "newLines",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.25291
  },
  {
    "action": "define",
    "reactId": "r4",
    "label": "reactive({\n    if (length(newLines()) == 0) \n        return()\n    read.csv(textConnection(newLines()), header = FALSE, stringsAsFactors = FALSE, \n        col.names = names(prototype)) %>% mutate(received = as.numeric(Sys.time()))\n})",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.28938
  },
  {
    "action": "define",
    "reactId": "r6",
    "label": "observe({\n    s <- signal()\n    isolate(rv$acc <- fun(rv$acc, s))\n})",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.29486
  },
  {
    "action": "invalidateStart",
    "reactId": "r6",
    "ctxId": "ctx1",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.29696
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525194292.29765
  },
  {
    "action": "invalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx1",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.31248
  },
  {
    "action": "define",
    "reactId": "r7",
    "label": "reactive(rv$acc)",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.31441
  },
  {
    "action": "define",
    "reactId": "r9",
    "label": "observe({\n    s <- signal()\n    isolate(rv$acc <- fun(rv$acc, s))\n})",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.31779
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx2",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.31868
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx2",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.3193
  },
  {
    "action": "define",
    "reactId": "r10",
    "label": "reactive(rv$acc)",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.32095
  },
  {
    "action": "define",
    "reactId": "r11",
    "label": "reactive({\n    df <- pkgStream()\n    if (!is.null(df) && nrow(df) > 0) {\n        ids <- paste(df$date, df$ip_id) %>% unique()\n        newIds <- !sapply(ids, bloomFilter$has)\n        total <<- total + length(newIds)\n        sapply(ids[newIds], bloomFilter$set)\n    }\n    total\n})",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.32683
  },
  {
    "action": "define",
    "reactId": "r12",
    "label": "output$rate",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.3336
  },
  {
    "action": "invalidateStart",
    "reactId": "r12",
    "ctxId": "ctx3",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.33515
  },
  {
    "action": "invalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx3",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.33635
  },
  {
    "action": "define",
    "reactId": "r13",
    "label": "output$count",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.38422
  },
  {
    "action": "invalidateStart",
    "reactId": "r13",
    "ctxId": "ctx4",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.385
  },
  {
    "action": "invalidateEnd",
    "reactId": "r13",
    "ctxId": "ctx4",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.38562
  },
  {
    "action": "define",
    "reactId": "r14",
    "label": "output$users",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.39038
  },
  {
    "action": "invalidateStart",
    "reactId": "r14",
    "ctxId": "ctx5",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.39162
  },
  {
    "action": "invalidateEnd",
    "reactId": "r14",
    "ctxId": "ctx5",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.3923
  },
  {
    "action": "define",
    "reactId": "r15",
    "label": "output$packagePlot",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.40006
  },
  {
    "action": "invalidateStart",
    "reactId": "r15",
    "ctxId": "ctx6",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.4013
  },
  {
    "action": "invalidateEnd",
    "reactId": "r15",
    "ctxId": "ctx6",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.4023
  },
  {
    "action": "define",
    "reactId": "r16",
    "label": "output$packageTable",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.40933
  },
  {
    "action": "invalidateStart",
    "reactId": "r16",
    "ctxId": "ctx7",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.4109
  },
  {
    "action": "invalidateEnd",
    "reactId": "r16",
    "ctxId": "ctx7",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.41202
  },
  {
    "action": "define",
    "reactId": "r17",
    "label": "output$downloadCsv",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.41865
  },
  {
    "action": "invalidateStart",
    "reactId": "r17",
    "ctxId": "ctx8",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.41999
  },
  {
    "action": "invalidateEnd",
    "reactId": "r17",
    "ctxId": "ctx8",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.4208
  },
  {
    "action": "define",
    "reactId": "r18",
    "label": "output$rawtable",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.42667
  },
  {
    "action": "invalidateStart",
    "reactId": "r18",
    "ctxId": "ctx9",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.42795
  },
  {
    "action": "invalidateEnd",
    "reactId": "r18",
    "ctxId": "ctx9",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.42875
  },
  {
    "action": "enter",
    "reactId": "r6",
    "ctxId": "ctx10",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.45267
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r4",
    "ctxId": "ctx10",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.4533
  },
  {
    "action": "enter",
    "reactId": "r4",
    "ctxId": "ctx11",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.45415
  },
  {
    "action": "dependsOn",
    "reactId": "r4",
    "depOnReactId": "r3",
    "ctxId": "ctx11",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.45547
  },
  {
    "action": "enter",
    "reactId": "r3",
    "ctxId": "ctx12",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.45688
  },
  {
    "action": "exit",
    "reactId": "r3",
    "ctxId": "ctx12",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.45907
  },
  {
    "action": "exit",
    "reactId": "r4",
    "ctxId": "ctx11",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.46
  },
  {
    "action": "isolateEnter",
    "reactId": "r6",
    "ctxId": "ctx13",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.46114
  },
  {
    "action": "define",
    "reactId": "r5$acc",
    "label": "reactiveValues2478$acc",
    "type": "reactiveValuesKey",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.46271
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx13",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.46335
  },
  {
    "action": "isolateExit",
    "reactId": "r6",
    "ctxId": "ctx13",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.46615
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r6",
    "ctxId": "ctx13",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.46679
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx13",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.46753
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx13",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.46818
  },
  {
    "action": "exit",
    "reactId": "r6",
    "ctxId": "ctx10",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.46864
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx14",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.4696
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r4",
    "ctxId": "ctx14",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.47016
  },
  {
    "action": "isolateEnter",
    "reactId": "r9",
    "ctxId": "ctx15",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.47091
  },
  {
    "action": "define",
    "reactId": "r8$acc",
    "label": "reactiveValues7513$acc",
    "type": "reactiveValuesKey",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.47157
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx15",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.47216
  },
  {
    "action": "isolateExit",
    "reactId": "r9",
    "ctxId": "ctx15",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.47274
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r9",
    "ctxId": "ctx15",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.47326
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx15",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.47378
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx15",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.47431
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx14",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.47475
  },
  {
    "action": "enter",
    "reactId": "r12",
    "ctxId": "ctx16",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.47565
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r7",
    "ctxId": "ctx16",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.47734
  },
  {
    "action": "enter",
    "reactId": "r7",
    "ctxId": "ctx17",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.47837
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx17",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.49119
  },
  {
    "action": "exit",
    "reactId": "r7",
    "ctxId": "ctx17",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.49202
  },
  {
    "action": "define",
    "reactId": "r1$rateThreshold",
    "label": "input$rateThreshold",
    "type": "reactiveValuesKey",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.49296
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$rateThreshold",
    "ctxId": "ctx16",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.49371
  },
  {
    "action": "exit",
    "reactId": "r12",
    "ctxId": "ctx16",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.49904
  },
  {
    "action": "enter",
    "reactId": "r13",
    "ctxId": "ctx18",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.49995
  },
  {
    "action": "dependsOn",
    "reactId": "r13",
    "depOnReactId": "r10",
    "ctxId": "ctx18",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.50166
  },
  {
    "action": "enter",
    "reactId": "r10",
    "ctxId": "ctx19",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.50265
  },
  {
    "action": "dependsOn",
    "reactId": "r10",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx19",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.50357
  },
  {
    "action": "exit",
    "reactId": "r10",
    "ctxId": "ctx19",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.50434
  },
  {
    "action": "exit",
    "reactId": "r13",
    "ctxId": "ctx18",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.50843
  },
  {
    "action": "enter",
    "reactId": "r14",
    "ctxId": "ctx20",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.50972
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r11",
    "ctxId": "ctx20",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.51189
  },
  {
    "action": "enter",
    "reactId": "r11",
    "ctxId": "ctx21",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.51299
  },
  {
    "action": "dependsOn",
    "reactId": "r11",
    "depOnReactId": "r4",
    "ctxId": "ctx21",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.51391
  },
  {
    "action": "exit",
    "reactId": "r11",
    "ctxId": "ctx21",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.51468
  },
  {
    "action": "exit",
    "reactId": "r14",
    "ctxId": "ctx20",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.51844
  },
  {
    "action": "enter",
    "reactId": "r15",
    "ctxId": "ctx22",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.51937
  },
  {
    "action": "dependsOn",
    "reactId": "r15",
    "depOnReactId": "r7",
    "ctxId": "ctx22",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.52083
  },
  {
    "action": "exit",
    "reactId": "r15",
    "ctxId": "ctx22",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.52279
  },
  {
    "action": "enter",
    "reactId": "r16",
    "ctxId": "ctx23",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.52376
  },
  {
    "action": "dependsOn",
    "reactId": "r16",
    "depOnReactId": "r7",
    "ctxId": "ctx23",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.52559
  },
  {
    "action": "exit",
    "reactId": "r16",
    "ctxId": "ctx23",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194292.56696
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525194292.56734
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525194292.56821
  },
  {
    "action": "invalidateStart",
    "reactId": "r3",
    "ctxId": "ctx12",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46002
  },
  {
    "action": "invalidateStart",
    "reactId": "r4",
    "ctxId": "ctx11",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46051
  },
  {
    "action": "invalidateStart",
    "reactId": "r11",
    "ctxId": "ctx21",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46103
  },
  {
    "action": "invalidateStart",
    "reactId": "r14",
    "ctxId": "ctx20",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46161
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525194293.46281
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r11",
    "ctxId": "ctx20",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.4641
  },
  {
    "action": "invalidateEnd",
    "reactId": "r14",
    "ctxId": "ctx20",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46462
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r11",
    "depOnReactId": "r4",
    "ctxId": "ctx21",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46518
  },
  {
    "action": "invalidateEnd",
    "reactId": "r11",
    "ctxId": "ctx21",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46564
  },
  {
    "action": "invalidateStart",
    "reactId": "r6",
    "ctxId": "ctx10",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46604
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r4",
    "ctxId": "ctx10",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46649
  },
  {
    "action": "invalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx10",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.4669
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx14",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46728
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r4",
    "ctxId": "ctx14",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46772
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx14",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46812
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r4",
    "depOnReactId": "r3",
    "ctxId": "ctx11",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46848
  },
  {
    "action": "invalidateEnd",
    "reactId": "r4",
    "ctxId": "ctx11",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46887
  },
  {
    "action": "invalidateEnd",
    "reactId": "r3",
    "ctxId": "ctx12",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.46918
  },
  {
    "action": "enter",
    "reactId": "r14",
    "ctxId": "ctx24",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.47003
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r11",
    "ctxId": "ctx24",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.49294
  },
  {
    "action": "enter",
    "reactId": "r11",
    "ctxId": "ctx25",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.50324
  },
  {
    "action": "dependsOn",
    "reactId": "r11",
    "depOnReactId": "r4",
    "ctxId": "ctx25",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.50424
  },
  {
    "action": "enter",
    "reactId": "r4",
    "ctxId": "ctx26",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.5056
  },
  {
    "action": "dependsOn",
    "reactId": "r4",
    "depOnReactId": "r3",
    "ctxId": "ctx26",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.50665
  },
  {
    "action": "enter",
    "reactId": "r3",
    "ctxId": "ctx27",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.508
  },
  {
    "action": "exit",
    "reactId": "r3",
    "ctxId": "ctx27",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.50989
  },
  {
    "action": "exit",
    "reactId": "r4",
    "ctxId": "ctx26",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.53361
  },
  {
    "action": "exit",
    "reactId": "r11",
    "ctxId": "ctx25",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.54549
  },
  {
    "action": "exit",
    "reactId": "r14",
    "ctxId": "ctx24",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.5506
  },
  {
    "action": "enter",
    "reactId": "r6",
    "ctxId": "ctx28",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.55181
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r4",
    "ctxId": "ctx28",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.55257
  },
  {
    "action": "isolateEnter",
    "reactId": "r6",
    "ctxId": "ctx29",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.55355
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx29",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.5545
  },
  {
    "action": "valueChange",
    "reactId": "r5$acc",
    "value": "'data.frame':\t45 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.57084
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r5$acc",
    "ctxId": "ctx29",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.57145
  },
  {
    "action": "invalidateStart",
    "reactId": "r7",
    "ctxId": "ctx17",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.57209
  },
  {
    "action": "invalidateStart",
    "reactId": "r15",
    "ctxId": "ctx22",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.57272
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r15",
    "depOnReactId": "r7",
    "ctxId": "ctx22",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.57404
  },
  {
    "action": "invalidateEnd",
    "reactId": "r15",
    "ctxId": "ctx22",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.57472
  },
  {
    "action": "invalidateStart",
    "reactId": "r16",
    "ctxId": "ctx23",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.57535
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r16",
    "depOnReactId": "r7",
    "ctxId": "ctx23",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.57673
  },
  {
    "action": "invalidateEnd",
    "reactId": "r16",
    "ctxId": "ctx23",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.5774
  },
  {
    "action": "invalidateStart",
    "reactId": "r12",
    "ctxId": "ctx16",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.578
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r7",
    "ctxId": "ctx16",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.57969
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$rateThreshold",
    "ctxId": "ctx16",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58043
  },
  {
    "action": "invalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx16",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58115
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r7",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx17",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.5819
  },
  {
    "action": "invalidateEnd",
    "reactId": "r7",
    "ctxId": "ctx17",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58268
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r6",
    "ctxId": "ctx29",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58331
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx29",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58392
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx29",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58454
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r5$acc",
    "ctxId": "ctx29",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.5851
  },
  {
    "action": "isolateExit",
    "reactId": "r6",
    "ctxId": "ctx29",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58559
  },
  {
    "action": "exit",
    "reactId": "r6",
    "ctxId": "ctx28",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58602
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx30",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58691
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r4",
    "ctxId": "ctx30",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58745
  },
  {
    "action": "isolateEnter",
    "reactId": "r9",
    "ctxId": "ctx31",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58817
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx31",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58879
  },
  {
    "action": "valueChange",
    "reactId": "r8$acc",
    "value": " num 45",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.58996
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r8$acc",
    "ctxId": "ctx31",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59056
  },
  {
    "action": "invalidateStart",
    "reactId": "r10",
    "ctxId": "ctx19",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59123
  },
  {
    "action": "invalidateStart",
    "reactId": "r13",
    "ctxId": "ctx18",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59187
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r13",
    "depOnReactId": "r10",
    "ctxId": "ctx18",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.5937
  },
  {
    "action": "invalidateEnd",
    "reactId": "r13",
    "ctxId": "ctx18",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59438
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r10",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx19",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59501
  },
  {
    "action": "invalidateEnd",
    "reactId": "r10",
    "ctxId": "ctx19",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59563
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r9",
    "ctxId": "ctx31",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59623
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx31",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59691
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx31",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59754
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r8$acc",
    "ctxId": "ctx31",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59809
  },
  {
    "action": "isolateExit",
    "reactId": "r9",
    "ctxId": "ctx31",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59858
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx30",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59902
  },
  {
    "action": "enter",
    "reactId": "r15",
    "ctxId": "ctx32",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.59991
  },
  {
    "action": "dependsOn",
    "reactId": "r15",
    "depOnReactId": "r7",
    "ctxId": "ctx32",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.60143
  },
  {
    "action": "enter",
    "reactId": "r7",
    "ctxId": "ctx33",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.60238
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx33",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.60323
  },
  {
    "action": "exit",
    "reactId": "r7",
    "ctxId": "ctx33",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.60395
  },
  {
    "action": "exit",
    "reactId": "r15",
    "ctxId": "ctx32",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.63893
  },
  {
    "action": "enter",
    "reactId": "r16",
    "ctxId": "ctx34",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.63991
  },
  {
    "action": "dependsOn",
    "reactId": "r16",
    "depOnReactId": "r7",
    "ctxId": "ctx34",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194293.64222
  },
  {
    "action": "exit",
    "reactId": "r16",
    "ctxId": "ctx34",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.01058
  },
  {
    "action": "enter",
    "reactId": "r12",
    "ctxId": "ctx35",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.01221
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r7",
    "ctxId": "ctx35",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.01413
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$rateThreshold",
    "ctxId": "ctx35",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.015
  },
  {
    "action": "exit",
    "reactId": "r12",
    "ctxId": "ctx35",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.01927
  },
  {
    "action": "enter",
    "reactId": "r13",
    "ctxId": "ctx36",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.02026
  },
  {
    "action": "dependsOn",
    "reactId": "r13",
    "depOnReactId": "r10",
    "ctxId": "ctx36",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.02277
  },
  {
    "action": "enter",
    "reactId": "r10",
    "ctxId": "ctx37",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.02396
  },
  {
    "action": "dependsOn",
    "reactId": "r10",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx37",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.02508
  },
  {
    "action": "exit",
    "reactId": "r10",
    "ctxId": "ctx37",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.02615
  },
  {
    "action": "exit",
    "reactId": "r13",
    "ctxId": "ctx36",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.03125
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525194294.0316
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525194294.03249
  },
  {
    "action": "invalidateStart",
    "reactId": "r3",
    "ctxId": "ctx27",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51062
  },
  {
    "action": "invalidateStart",
    "reactId": "r4",
    "ctxId": "ctx26",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.5111
  },
  {
    "action": "invalidateStart",
    "reactId": "r11",
    "ctxId": "ctx25",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51152
  },
  {
    "action": "invalidateStart",
    "reactId": "r14",
    "ctxId": "ctx24",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51196
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525194294.51321
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r11",
    "ctxId": "ctx24",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51451
  },
  {
    "action": "invalidateEnd",
    "reactId": "r14",
    "ctxId": "ctx24",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51501
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r11",
    "depOnReactId": "r4",
    "ctxId": "ctx25",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51563
  },
  {
    "action": "invalidateEnd",
    "reactId": "r11",
    "ctxId": "ctx25",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51611
  },
  {
    "action": "invalidateStart",
    "reactId": "r6",
    "ctxId": "ctx28",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51661
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r4",
    "ctxId": "ctx28",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51712
  },
  {
    "action": "invalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx28",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51755
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx30",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51794
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r4",
    "ctxId": "ctx30",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51839
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx30",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51882
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r4",
    "depOnReactId": "r3",
    "ctxId": "ctx26",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51919
  },
  {
    "action": "invalidateEnd",
    "reactId": "r4",
    "ctxId": "ctx26",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51959
  },
  {
    "action": "invalidateEnd",
    "reactId": "r3",
    "ctxId": "ctx27",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.51991
  },
  {
    "action": "enter",
    "reactId": "r14",
    "ctxId": "ctx38",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.52076
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r11",
    "ctxId": "ctx38",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.52333
  },
  {
    "action": "enter",
    "reactId": "r11",
    "ctxId": "ctx39",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.52482
  },
  {
    "action": "dependsOn",
    "reactId": "r11",
    "depOnReactId": "r4",
    "ctxId": "ctx39",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.52639
  },
  {
    "action": "enter",
    "reactId": "r4",
    "ctxId": "ctx40",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.52769
  },
  {
    "action": "dependsOn",
    "reactId": "r4",
    "depOnReactId": "r3",
    "ctxId": "ctx40",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.52874
  },
  {
    "action": "enter",
    "reactId": "r3",
    "ctxId": "ctx41",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.53001
  },
  {
    "action": "exit",
    "reactId": "r3",
    "ctxId": "ctx41",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.53178
  },
  {
    "action": "exit",
    "reactId": "r4",
    "ctxId": "ctx40",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.5351
  },
  {
    "action": "exit",
    "reactId": "r11",
    "ctxId": "ctx39",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.54096
  },
  {
    "action": "exit",
    "reactId": "r14",
    "ctxId": "ctx38",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.54906
  },
  {
    "action": "enter",
    "reactId": "r6",
    "ctxId": "ctx42",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.54999
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r4",
    "ctxId": "ctx42",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.55053
  },
  {
    "action": "isolateEnter",
    "reactId": "r6",
    "ctxId": "ctx43",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.55131
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx43",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.55209
  },
  {
    "action": "valueChange",
    "reactId": "r5$acc",
    "value": "'data.frame':\t97 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.56485
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r5$acc",
    "ctxId": "ctx43",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.56554
  },
  {
    "action": "invalidateStart",
    "reactId": "r7",
    "ctxId": "ctx33",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.56635
  },
  {
    "action": "invalidateStart",
    "reactId": "r15",
    "ctxId": "ctx32",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.56707
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r15",
    "depOnReactId": "r7",
    "ctxId": "ctx32",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.56853
  },
  {
    "action": "invalidateEnd",
    "reactId": "r15",
    "ctxId": "ctx32",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.56925
  },
  {
    "action": "invalidateStart",
    "reactId": "r16",
    "ctxId": "ctx34",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.56989
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r16",
    "depOnReactId": "r7",
    "ctxId": "ctx34",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.57133
  },
  {
    "action": "invalidateEnd",
    "reactId": "r16",
    "ctxId": "ctx34",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.572
  },
  {
    "action": "invalidateStart",
    "reactId": "r12",
    "ctxId": "ctx35",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.57263
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r7",
    "ctxId": "ctx35",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.57394
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$rateThreshold",
    "ctxId": "ctx35",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.5747
  },
  {
    "action": "invalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx35",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.57539
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r7",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx33",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.57601
  },
  {
    "action": "invalidateEnd",
    "reactId": "r7",
    "ctxId": "ctx33",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.57668
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r6",
    "ctxId": "ctx43",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.57729
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx43",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.57789
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx43",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.5785
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r5$acc",
    "ctxId": "ctx43",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.57923
  },
  {
    "action": "isolateExit",
    "reactId": "r6",
    "ctxId": "ctx43",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.57982
  },
  {
    "action": "exit",
    "reactId": "r6",
    "ctxId": "ctx42",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.58029
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx44",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.58144
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r4",
    "ctxId": "ctx44",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.58203
  },
  {
    "action": "isolateEnter",
    "reactId": "r9",
    "ctxId": "ctx45",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.58288
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx45",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.58358
  },
  {
    "action": "valueChange",
    "reactId": "r8$acc",
    "value": " num 97",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.58485
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r8$acc",
    "ctxId": "ctx45",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.58545
  },
  {
    "action": "invalidateStart",
    "reactId": "r10",
    "ctxId": "ctx37",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.5861
  },
  {
    "action": "invalidateStart",
    "reactId": "r13",
    "ctxId": "ctx36",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.58674
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r13",
    "depOnReactId": "r10",
    "ctxId": "ctx36",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.5881
  },
  {
    "action": "invalidateEnd",
    "reactId": "r13",
    "ctxId": "ctx36",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.58874
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r10",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx37",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.58936
  },
  {
    "action": "invalidateEnd",
    "reactId": "r10",
    "ctxId": "ctx37",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.58996
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r9",
    "ctxId": "ctx45",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.59054
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx45",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.59112
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx45",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.59171
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r8$acc",
    "ctxId": "ctx45",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.59226
  },
  {
    "action": "isolateExit",
    "reactId": "r9",
    "ctxId": "ctx45",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.59276
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx44",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.59321
  },
  {
    "action": "enter",
    "reactId": "r15",
    "ctxId": "ctx46",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.59414
  },
  {
    "action": "dependsOn",
    "reactId": "r15",
    "depOnReactId": "r7",
    "ctxId": "ctx46",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.59584
  },
  {
    "action": "enter",
    "reactId": "r7",
    "ctxId": "ctx47",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.59682
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx47",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.59768
  },
  {
    "action": "exit",
    "reactId": "r7",
    "ctxId": "ctx47",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.59848
  },
  {
    "action": "exit",
    "reactId": "r15",
    "ctxId": "ctx46",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.67001
  },
  {
    "action": "enter",
    "reactId": "r16",
    "ctxId": "ctx48",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.67092
  },
  {
    "action": "dependsOn",
    "reactId": "r16",
    "depOnReactId": "r7",
    "ctxId": "ctx48",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.6728
  },
  {
    "action": "exit",
    "reactId": "r16",
    "ctxId": "ctx48",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.68187
  },
  {
    "action": "enter",
    "reactId": "r12",
    "ctxId": "ctx49",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.68277
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r7",
    "ctxId": "ctx49",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.68432
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$rateThreshold",
    "ctxId": "ctx49",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.68523
  },
  {
    "action": "exit",
    "reactId": "r12",
    "ctxId": "ctx49",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.68901
  },
  {
    "action": "enter",
    "reactId": "r13",
    "ctxId": "ctx50",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.68999
  },
  {
    "action": "dependsOn",
    "reactId": "r13",
    "depOnReactId": "r10",
    "ctxId": "ctx50",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.69177
  },
  {
    "action": "enter",
    "reactId": "r10",
    "ctxId": "ctx51",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.6928
  },
  {
    "action": "dependsOn",
    "reactId": "r10",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx51",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.69371
  },
  {
    "action": "exit",
    "reactId": "r10",
    "ctxId": "ctx51",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.69448
  },
  {
    "action": "exit",
    "reactId": "r13",
    "ctxId": "ctx50",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194294.69885
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525194294.69922
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525194294.70012
  },
  {
    "action": "invalidateStart",
    "reactId": "r3",
    "ctxId": "ctx41",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.5331
  },
  {
    "action": "invalidateStart",
    "reactId": "r4",
    "ctxId": "ctx40",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.53359
  },
  {
    "action": "invalidateStart",
    "reactId": "r11",
    "ctxId": "ctx39",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.53401
  },
  {
    "action": "invalidateStart",
    "reactId": "r14",
    "ctxId": "ctx38",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.53443
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525194295.53563
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r11",
    "ctxId": "ctx38",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.53672
  },
  {
    "action": "invalidateEnd",
    "reactId": "r14",
    "ctxId": "ctx38",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.53719
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r11",
    "depOnReactId": "r4",
    "ctxId": "ctx39",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.53777
  },
  {
    "action": "invalidateEnd",
    "reactId": "r11",
    "ctxId": "ctx39",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.53833
  },
  {
    "action": "invalidateStart",
    "reactId": "r6",
    "ctxId": "ctx42",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.53877
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r4",
    "ctxId": "ctx42",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.53926
  },
  {
    "action": "invalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx42",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.53983
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx44",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.54031
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r4",
    "ctxId": "ctx44",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.54077
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx44",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.54131
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r4",
    "depOnReactId": "r3",
    "ctxId": "ctx40",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.54178
  },
  {
    "action": "invalidateEnd",
    "reactId": "r4",
    "ctxId": "ctx40",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.54231
  },
  {
    "action": "invalidateEnd",
    "reactId": "r3",
    "ctxId": "ctx41",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.54268
  },
  {
    "action": "enter",
    "reactId": "r14",
    "ctxId": "ctx52",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.54358
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r11",
    "ctxId": "ctx52",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.54598
  },
  {
    "action": "enter",
    "reactId": "r11",
    "ctxId": "ctx53",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.54727
  },
  {
    "action": "dependsOn",
    "reactId": "r11",
    "depOnReactId": "r4",
    "ctxId": "ctx53",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.54826
  },
  {
    "action": "enter",
    "reactId": "r4",
    "ctxId": "ctx54",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.54946
  },
  {
    "action": "dependsOn",
    "reactId": "r4",
    "depOnReactId": "r3",
    "ctxId": "ctx54",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.55047
  },
  {
    "action": "enter",
    "reactId": "r3",
    "ctxId": "ctx55",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.55178
  },
  {
    "action": "exit",
    "reactId": "r3",
    "ctxId": "ctx55",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.55379
  },
  {
    "action": "exit",
    "reactId": "r4",
    "ctxId": "ctx54",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.55708
  },
  {
    "action": "exit",
    "reactId": "r11",
    "ctxId": "ctx53",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.5616
  },
  {
    "action": "exit",
    "reactId": "r14",
    "ctxId": "ctx52",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.56592
  },
  {
    "action": "enter",
    "reactId": "r6",
    "ctxId": "ctx56",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.56685
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r4",
    "ctxId": "ctx56",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.5674
  },
  {
    "action": "isolateEnter",
    "reactId": "r6",
    "ctxId": "ctx57",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.56815
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx57",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.56891
  },
  {
    "action": "valueChange",
    "reactId": "r5$acc",
    "value": "'data.frame':\t142 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.58138
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r5$acc",
    "ctxId": "ctx57",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.58205
  },
  {
    "action": "invalidateStart",
    "reactId": "r7",
    "ctxId": "ctx47",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.58272
  },
  {
    "action": "invalidateStart",
    "reactId": "r15",
    "ctxId": "ctx46",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.58337
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r15",
    "depOnReactId": "r7",
    "ctxId": "ctx46",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.58498
  },
  {
    "action": "invalidateEnd",
    "reactId": "r15",
    "ctxId": "ctx46",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.58569
  },
  {
    "action": "invalidateStart",
    "reactId": "r16",
    "ctxId": "ctx48",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.5863
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r16",
    "depOnReactId": "r7",
    "ctxId": "ctx48",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.58785
  },
  {
    "action": "invalidateEnd",
    "reactId": "r16",
    "ctxId": "ctx48",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.58853
  },
  {
    "action": "invalidateStart",
    "reactId": "r12",
    "ctxId": "ctx49",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.58915
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r7",
    "ctxId": "ctx49",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.59044
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$rateThreshold",
    "ctxId": "ctx49",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.59111
  },
  {
    "action": "invalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx49",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.5918
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r7",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx47",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.5924
  },
  {
    "action": "invalidateEnd",
    "reactId": "r7",
    "ctxId": "ctx47",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.593
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r6",
    "ctxId": "ctx57",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.59359
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx57",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.5942
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx57",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.59481
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r5$acc",
    "ctxId": "ctx57",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.59539
  },
  {
    "action": "isolateExit",
    "reactId": "r6",
    "ctxId": "ctx57",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.59605
  },
  {
    "action": "exit",
    "reactId": "r6",
    "ctxId": "ctx56",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.59654
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx58",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.59756
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r4",
    "ctxId": "ctx58",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.59819
  },
  {
    "action": "isolateEnter",
    "reactId": "r9",
    "ctxId": "ctx59",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.59896
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx59",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.59959
  },
  {
    "action": "valueChange",
    "reactId": "r8$acc",
    "value": " num 142",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60078
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r8$acc",
    "ctxId": "ctx59",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60146
  },
  {
    "action": "invalidateStart",
    "reactId": "r10",
    "ctxId": "ctx51",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60213
  },
  {
    "action": "invalidateStart",
    "reactId": "r13",
    "ctxId": "ctx50",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60285
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r13",
    "depOnReactId": "r10",
    "ctxId": "ctx50",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60422
  },
  {
    "action": "invalidateEnd",
    "reactId": "r13",
    "ctxId": "ctx50",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60487
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r10",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx51",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60552
  },
  {
    "action": "invalidateEnd",
    "reactId": "r10",
    "ctxId": "ctx51",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60614
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r9",
    "ctxId": "ctx59",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60674
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx59",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60753
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx59",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60815
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r8$acc",
    "ctxId": "ctx59",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60872
  },
  {
    "action": "isolateExit",
    "reactId": "r9",
    "ctxId": "ctx59",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60923
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx58",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.60967
  },
  {
    "action": "enter",
    "reactId": "r15",
    "ctxId": "ctx60",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.61058
  },
  {
    "action": "dependsOn",
    "reactId": "r15",
    "depOnReactId": "r7",
    "ctxId": "ctx60",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.61218
  },
  {
    "action": "enter",
    "reactId": "r7",
    "ctxId": "ctx61",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.61317
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx61",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.61424
  },
  {
    "action": "exit",
    "reactId": "r7",
    "ctxId": "ctx61",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.61498
  },
  {
    "action": "exit",
    "reactId": "r15",
    "ctxId": "ctx60",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.62208
  },
  {
    "action": "enter",
    "reactId": "r16",
    "ctxId": "ctx62",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.62307
  },
  {
    "action": "dependsOn",
    "reactId": "r16",
    "depOnReactId": "r7",
    "ctxId": "ctx62",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.62496
  },
  {
    "action": "exit",
    "reactId": "r16",
    "ctxId": "ctx62",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.63928
  },
  {
    "action": "enter",
    "reactId": "r12",
    "ctxId": "ctx63",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.64034
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r7",
    "ctxId": "ctx63",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.64233
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$rateThreshold",
    "ctxId": "ctx63",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.64324
  },
  {
    "action": "exit",
    "reactId": "r12",
    "ctxId": "ctx63",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.6475
  },
  {
    "action": "enter",
    "reactId": "r13",
    "ctxId": "ctx64",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.64874
  },
  {
    "action": "dependsOn",
    "reactId": "r13",
    "depOnReactId": "r10",
    "ctxId": "ctx64",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.65062
  },
  {
    "action": "enter",
    "reactId": "r10",
    "ctxId": "ctx65",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.65178
  },
  {
    "action": "dependsOn",
    "reactId": "r10",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx65",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.65293
  },
  {
    "action": "exit",
    "reactId": "r10",
    "ctxId": "ctx65",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.65375
  },
  {
    "action": "exit",
    "reactId": "r13",
    "ctxId": "ctx64",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194295.65913
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525194295.65977
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525194295.66119
  },
  {
    "action": "invalidateStart",
    "reactId": "r3",
    "ctxId": "ctx55",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.55684
  },
  {
    "action": "invalidateStart",
    "reactId": "r4",
    "ctxId": "ctx54",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.55746
  },
  {
    "action": "invalidateStart",
    "reactId": "r11",
    "ctxId": "ctx53",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.55789
  },
  {
    "action": "invalidateStart",
    "reactId": "r14",
    "ctxId": "ctx52",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.55831
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525194296.55951
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r11",
    "ctxId": "ctx52",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56053
  },
  {
    "action": "invalidateEnd",
    "reactId": "r14",
    "ctxId": "ctx52",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56119
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r11",
    "depOnReactId": "r4",
    "ctxId": "ctx53",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56189
  },
  {
    "action": "invalidateEnd",
    "reactId": "r11",
    "ctxId": "ctx53",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56237
  },
  {
    "action": "invalidateStart",
    "reactId": "r6",
    "ctxId": "ctx56",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56277
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r4",
    "ctxId": "ctx56",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56323
  },
  {
    "action": "invalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx56",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56364
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx58",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56403
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r4",
    "ctxId": "ctx58",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56454
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx58",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56494
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r4",
    "depOnReactId": "r3",
    "ctxId": "ctx54",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.5653
  },
  {
    "action": "invalidateEnd",
    "reactId": "r4",
    "ctxId": "ctx54",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56569
  },
  {
    "action": "invalidateEnd",
    "reactId": "r3",
    "ctxId": "ctx55",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56602
  },
  {
    "action": "enter",
    "reactId": "r14",
    "ctxId": "ctx66",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56687
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r11",
    "ctxId": "ctx66",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.56908
  },
  {
    "action": "enter",
    "reactId": "r11",
    "ctxId": "ctx67",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.57021
  },
  {
    "action": "dependsOn",
    "reactId": "r11",
    "depOnReactId": "r4",
    "ctxId": "ctx67",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.57115
  },
  {
    "action": "enter",
    "reactId": "r4",
    "ctxId": "ctx68",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.57238
  },
  {
    "action": "dependsOn",
    "reactId": "r4",
    "depOnReactId": "r3",
    "ctxId": "ctx68",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.57356
  },
  {
    "action": "enter",
    "reactId": "r3",
    "ctxId": "ctx69",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.57531
  },
  {
    "action": "exit",
    "reactId": "r3",
    "ctxId": "ctx69",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.57768
  },
  {
    "action": "exit",
    "reactId": "r4",
    "ctxId": "ctx68",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.5808
  },
  {
    "action": "exit",
    "reactId": "r11",
    "ctxId": "ctx67",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.58599
  },
  {
    "action": "exit",
    "reactId": "r14",
    "ctxId": "ctx66",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.59013
  },
  {
    "action": "enter",
    "reactId": "r6",
    "ctxId": "ctx70",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.59112
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r4",
    "ctxId": "ctx70",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.59176
  },
  {
    "action": "isolateEnter",
    "reactId": "r6",
    "ctxId": "ctx71",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.59252
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx71",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.59338
  },
  {
    "action": "valueChange",
    "reactId": "r5$acc",
    "value": "'data.frame':\t178 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.60608
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r5$acc",
    "ctxId": "ctx71",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.60674
  },
  {
    "action": "invalidateStart",
    "reactId": "r7",
    "ctxId": "ctx61",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.6074
  },
  {
    "action": "invalidateStart",
    "reactId": "r15",
    "ctxId": "ctx60",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.6082
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r15",
    "depOnReactId": "r7",
    "ctxId": "ctx60",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.60963
  },
  {
    "action": "invalidateEnd",
    "reactId": "r15",
    "ctxId": "ctx60",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.61031
  },
  {
    "action": "invalidateStart",
    "reactId": "r16",
    "ctxId": "ctx62",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.6111
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r16",
    "depOnReactId": "r7",
    "ctxId": "ctx62",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.61252
  },
  {
    "action": "invalidateEnd",
    "reactId": "r16",
    "ctxId": "ctx62",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.61327
  },
  {
    "action": "invalidateStart",
    "reactId": "r12",
    "ctxId": "ctx63",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.61398
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r7",
    "ctxId": "ctx63",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.6155
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$rateThreshold",
    "ctxId": "ctx63",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.6162
  },
  {
    "action": "invalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx63",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.61697
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r7",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx61",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.61759
  },
  {
    "action": "invalidateEnd",
    "reactId": "r7",
    "ctxId": "ctx61",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.61819
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r6",
    "ctxId": "ctx71",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.61879
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx71",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.61938
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx71",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.61998
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r5$acc",
    "ctxId": "ctx71",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.62059
  },
  {
    "action": "isolateExit",
    "reactId": "r6",
    "ctxId": "ctx71",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.6211
  },
  {
    "action": "exit",
    "reactId": "r6",
    "ctxId": "ctx70",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.62164
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx72",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.62261
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r4",
    "ctxId": "ctx72",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.62317
  },
  {
    "action": "isolateEnter",
    "reactId": "r9",
    "ctxId": "ctx73",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.6239
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx73",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.62452
  },
  {
    "action": "valueChange",
    "reactId": "r8$acc",
    "value": " num 178",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.62564
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r8$acc",
    "ctxId": "ctx73",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.62624
  },
  {
    "action": "invalidateStart",
    "reactId": "r10",
    "ctxId": "ctx65",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.62689
  },
  {
    "action": "invalidateStart",
    "reactId": "r13",
    "ctxId": "ctx64",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.62761
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r13",
    "depOnReactId": "r10",
    "ctxId": "ctx64",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.62902
  },
  {
    "action": "invalidateEnd",
    "reactId": "r13",
    "ctxId": "ctx64",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.62968
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r10",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx65",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63026
  },
  {
    "action": "invalidateEnd",
    "reactId": "r10",
    "ctxId": "ctx65",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63086
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r9",
    "ctxId": "ctx73",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63144
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx73",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63211
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx73",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63274
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r8$acc",
    "ctxId": "ctx73",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63331
  },
  {
    "action": "isolateExit",
    "reactId": "r9",
    "ctxId": "ctx73",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63381
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx72",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63425
  },
  {
    "action": "enter",
    "reactId": "r15",
    "ctxId": "ctx74",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63517
  },
  {
    "action": "dependsOn",
    "reactId": "r15",
    "depOnReactId": "r7",
    "ctxId": "ctx74",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63662
  },
  {
    "action": "enter",
    "reactId": "r7",
    "ctxId": "ctx75",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63755
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx75",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63838
  },
  {
    "action": "exit",
    "reactId": "r7",
    "ctxId": "ctx75",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.63908
  },
  {
    "action": "exit",
    "reactId": "r15",
    "ctxId": "ctx74",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.64567
  },
  {
    "action": "enter",
    "reactId": "r16",
    "ctxId": "ctx76",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.64658
  },
  {
    "action": "dependsOn",
    "reactId": "r16",
    "depOnReactId": "r7",
    "ctxId": "ctx76",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.64838
  },
  {
    "action": "exit",
    "reactId": "r16",
    "ctxId": "ctx76",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.65775
  },
  {
    "action": "enter",
    "reactId": "r12",
    "ctxId": "ctx77",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.65865
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r7",
    "ctxId": "ctx77",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.66015
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$rateThreshold",
    "ctxId": "ctx77",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.66097
  },
  {
    "action": "exit",
    "reactId": "r12",
    "ctxId": "ctx77",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.66477
  },
  {
    "action": "enter",
    "reactId": "r13",
    "ctxId": "ctx78",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.66571
  },
  {
    "action": "dependsOn",
    "reactId": "r13",
    "depOnReactId": "r10",
    "ctxId": "ctx78",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.6708
  },
  {
    "action": "enter",
    "reactId": "r10",
    "ctxId": "ctx79",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.67194
  },
  {
    "action": "dependsOn",
    "reactId": "r10",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx79",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.67287
  },
  {
    "action": "exit",
    "reactId": "r10",
    "ctxId": "ctx79",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.67383
  },
  {
    "action": "exit",
    "reactId": "r13",
    "ctxId": "ctx78",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194296.67757
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525194296.67795
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525194296.67883
  },
  {
    "action": "invalidateStart",
    "reactId": "r3",
    "ctxId": "ctx69",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58199
  },
  {
    "action": "invalidateStart",
    "reactId": "r4",
    "ctxId": "ctx68",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58259
  },
  {
    "action": "invalidateStart",
    "reactId": "r6",
    "ctxId": "ctx70",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58319
  },
  {
    "action": "asyncStart",
    "session": null,
    "time": 1525194297.58381
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r4",
    "ctxId": "ctx70",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58483
  },
  {
    "action": "invalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx70",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58533
  },
  {
    "action": "invalidateStart",
    "reactId": "r9",
    "ctxId": "ctx72",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58576
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r4",
    "ctxId": "ctx72",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58626
  },
  {
    "action": "invalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx72",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58671
  },
  {
    "action": "invalidateStart",
    "reactId": "r11",
    "ctxId": "ctx67",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58716
  },
  {
    "action": "invalidateStart",
    "reactId": "r14",
    "ctxId": "ctx66",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58765
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r14",
    "depOnReactId": "r11",
    "ctxId": "ctx66",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58879
  },
  {
    "action": "invalidateEnd",
    "reactId": "r14",
    "ctxId": "ctx66",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58925
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r11",
    "depOnReactId": "r4",
    "ctxId": "ctx67",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.58966
  },
  {
    "action": "invalidateEnd",
    "reactId": "r11",
    "ctxId": "ctx67",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.59009
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r4",
    "depOnReactId": "r3",
    "ctxId": "ctx68",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.59046
  },
  {
    "action": "invalidateEnd",
    "reactId": "r4",
    "ctxId": "ctx68",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.59091
  },
  {
    "action": "invalidateEnd",
    "reactId": "r3",
    "ctxId": "ctx69",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.59126
  },
  {
    "action": "enter",
    "reactId": "r6",
    "ctxId": "ctx80",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.59215
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r4",
    "ctxId": "ctx80",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.59271
  },
  {
    "action": "enter",
    "reactId": "r4",
    "ctxId": "ctx81",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.59358
  },
  {
    "action": "dependsOn",
    "reactId": "r4",
    "depOnReactId": "r3",
    "ctxId": "ctx81",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.59433
  },
  {
    "action": "enter",
    "reactId": "r3",
    "ctxId": "ctx82",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.59538
  },
  {
    "action": "exit",
    "reactId": "r3",
    "ctxId": "ctx82",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.59689
  },
  {
    "action": "exit",
    "reactId": "r4",
    "ctxId": "ctx81",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.59975
  },
  {
    "action": "isolateEnter",
    "reactId": "r6",
    "ctxId": "ctx83",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.60059
  },
  {
    "action": "dependsOn",
    "reactId": "r6",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx83",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.60139
  },
  {
    "action": "valueChange",
    "reactId": "r5$acc",
    "value": "'data.frame':\t214 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.61332
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r5$acc",
    "ctxId": "ctx83",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.61395
  },
  {
    "action": "invalidateStart",
    "reactId": "r7",
    "ctxId": "ctx75",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.6146
  },
  {
    "action": "invalidateStart",
    "reactId": "r15",
    "ctxId": "ctx74",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.61523
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r15",
    "depOnReactId": "r7",
    "ctxId": "ctx74",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.6167
  },
  {
    "action": "invalidateEnd",
    "reactId": "r15",
    "ctxId": "ctx74",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.61736
  },
  {
    "action": "invalidateStart",
    "reactId": "r16",
    "ctxId": "ctx76",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.61798
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r16",
    "depOnReactId": "r7",
    "ctxId": "ctx76",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.61926
  },
  {
    "action": "invalidateEnd",
    "reactId": "r16",
    "ctxId": "ctx76",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.61989
  },
  {
    "action": "invalidateStart",
    "reactId": "r12",
    "ctxId": "ctx77",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62049
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r7",
    "ctxId": "ctx77",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62196
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r12",
    "depOnReactId": "r1$rateThreshold",
    "ctxId": "ctx77",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62273
  },
  {
    "action": "invalidateEnd",
    "reactId": "r12",
    "ctxId": "ctx77",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62337
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r7",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx75",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62396
  },
  {
    "action": "invalidateEnd",
    "reactId": "r7",
    "ctxId": "ctx75",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62464
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r6",
    "ctxId": "ctx83",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62535
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r6",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx83",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62596
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r6",
    "ctxId": "ctx83",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62662
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r5$acc",
    "ctxId": "ctx83",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62721
  },
  {
    "action": "isolateExit",
    "reactId": "r6",
    "ctxId": "ctx83",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62771
  },
  {
    "action": "exit",
    "reactId": "r6",
    "ctxId": "ctx80",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62815
  },
  {
    "action": "enter",
    "reactId": "r9",
    "ctxId": "ctx84",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62904
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r4",
    "ctxId": "ctx84",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.62958
  },
  {
    "action": "isolateEnter",
    "reactId": "r9",
    "ctxId": "ctx85",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63032
  },
  {
    "action": "dependsOn",
    "reactId": "r9",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx85",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63094
  },
  {
    "action": "valueChange",
    "reactId": "r8$acc",
    "value": " num 214",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63207
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r8$acc",
    "ctxId": "ctx85",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63273
  },
  {
    "action": "invalidateStart",
    "reactId": "r10",
    "ctxId": "ctx79",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63344
  },
  {
    "action": "invalidateStart",
    "reactId": "r13",
    "ctxId": "ctx78",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63412
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r13",
    "depOnReactId": "r10",
    "ctxId": "ctx78",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63546
  },
  {
    "action": "invalidateEnd",
    "reactId": "r13",
    "ctxId": "ctx78",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63611
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r10",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx79",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63674
  },
  {
    "action": "invalidateEnd",
    "reactId": "r10",
    "ctxId": "ctx79",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63734
  },
  {
    "action": "isolateInvalidateStart",
    "reactId": "r9",
    "ctxId": "ctx85",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63792
  },
  {
    "action": "dependsOnRemove",
    "reactId": "r9",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx85",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63854
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r9",
    "ctxId": "ctx85",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63914
  },
  {
    "action": "isolateInvalidateEnd",
    "reactId": "r8$acc",
    "ctxId": "ctx85",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.63969
  },
  {
    "action": "isolateExit",
    "reactId": "r9",
    "ctxId": "ctx85",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.64018
  },
  {
    "action": "exit",
    "reactId": "r9",
    "ctxId": "ctx84",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.64062
  },
  {
    "action": "enter",
    "reactId": "r14",
    "ctxId": "ctx86",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.64179
  },
  {
    "action": "dependsOn",
    "reactId": "r14",
    "depOnReactId": "r11",
    "ctxId": "ctx86",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.64361
  },
  {
    "action": "enter",
    "reactId": "r11",
    "ctxId": "ctx87",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.64465
  },
  {
    "action": "dependsOn",
    "reactId": "r11",
    "depOnReactId": "r4",
    "ctxId": "ctx87",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.64555
  },
  {
    "action": "exit",
    "reactId": "r11",
    "ctxId": "ctx87",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.64934
  },
  {
    "action": "exit",
    "reactId": "r14",
    "ctxId": "ctx86",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.65327
  },
  {
    "action": "enter",
    "reactId": "r15",
    "ctxId": "ctx88",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.65419
  },
  {
    "action": "dependsOn",
    "reactId": "r15",
    "depOnReactId": "r7",
    "ctxId": "ctx88",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.6556
  },
  {
    "action": "enter",
    "reactId": "r7",
    "ctxId": "ctx89",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.65656
  },
  {
    "action": "dependsOn",
    "reactId": "r7",
    "depOnReactId": "r5$acc",
    "ctxId": "ctx89",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.65739
  },
  {
    "action": "exit",
    "reactId": "r7",
    "ctxId": "ctx89",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.6581
  },
  {
    "action": "exit",
    "reactId": "r15",
    "ctxId": "ctx88",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.66466
  },
  {
    "action": "enter",
    "reactId": "r16",
    "ctxId": "ctx90",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.66555
  },
  {
    "action": "dependsOn",
    "reactId": "r16",
    "depOnReactId": "r7",
    "ctxId": "ctx90",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.66751
  },
  {
    "action": "exit",
    "reactId": "r16",
    "ctxId": "ctx90",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.67681
  },
  {
    "action": "enter",
    "reactId": "r12",
    "ctxId": "ctx91",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.67771
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r7",
    "ctxId": "ctx91",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.6792
  },
  {
    "action": "dependsOn",
    "reactId": "r12",
    "depOnReactId": "r1$rateThreshold",
    "ctxId": "ctx91",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.68003
  },
  {
    "action": "exit",
    "reactId": "r12",
    "ctxId": "ctx91",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.68414
  },
  {
    "action": "enter",
    "reactId": "r13",
    "ctxId": "ctx92",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.68507
  },
  {
    "action": "dependsOn",
    "reactId": "r13",
    "depOnReactId": "r10",
    "ctxId": "ctx92",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.68677
  },
  {
    "action": "enter",
    "reactId": "r10",
    "ctxId": "ctx93",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.68776
  },
  {
    "action": "dependsOn",
    "reactId": "r10",
    "depOnReactId": "r8$acc",
    "ctxId": "ctx93",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.68871
  },
  {
    "action": "exit",
    "reactId": "r10",
    "ctxId": "ctx93",
    "type": "observable",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.68948
  },
  {
    "action": "exit",
    "reactId": "r13",
    "ctxId": "ctx92",
    "type": "observer",
    "session": "a41fd95ac69dd22631d44fda82b320f3",
    "time": 1525194297.69332
  },
  {
    "action": "asyncStop",
    "session": null,
    "time": 1525194297.69368
  },
  {
    "action": "queueEmpty",
    "session": null,
    "time": 1525194297.69464
  }
  // ,
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx82",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.60259
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx81",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.60318
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx80",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.60376
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194298.6043
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx80",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.60544
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx80",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.60599
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx84",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.60646
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx84",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61065
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx84",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61134
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx87",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61176
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx86",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61224
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx86",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61341
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx86",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61386
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx87",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61426
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx87",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61467
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx81",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61503
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx81",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61542
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx82",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61574
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx94",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61676
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx94",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61733
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx95",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.6181
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx95",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61874
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx96",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.61955
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx96",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.62083
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx95",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.62332
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx97",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.62408
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx97",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.62485
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t242 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.63658
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx97",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.63722
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx89",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.63789
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx90",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.63855
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx90",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.63985
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx90",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.6405
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx91",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.64121
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx91",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.64248
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx91",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.64316
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx91",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.64378
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx88",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.64441
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx88",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.64566
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx88",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.6463
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx89",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.6469
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx89",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.64749
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx97",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.64807
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx97",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.64865
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx97",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.64924
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx97",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.64979
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx97",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.6503
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx94",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.65076
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx98",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.65171
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx98",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.65226
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx99",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.65298
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx99",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.6536
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 242",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.65476
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx99",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.65538
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx93",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.65603
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx92",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.65666
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx92",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.65794
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx92",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.65859
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx93",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.65917
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx93",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.65977
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx99",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.66035
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx99",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.66095
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx99",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.66163
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx99",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.6622
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx99",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.6627
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx98",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.66313
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx100",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.66408
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx100",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.66595
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx101",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.66746
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx101",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.66848
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx101",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.67314
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx100",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.6774
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx102",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.67858
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx102",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.68062
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx103",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.68174
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx103",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.68265
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx103",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.6838
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx102",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.69482
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx104",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.69585
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx104",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.69763
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx104",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.6985
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx104",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.70326
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx105",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.70429
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx105",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.70624
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx105",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.7135
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx106",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.71452
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx106",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.71656
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx107",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.71778
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx107",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.71875
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx107",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.71956
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx106",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194298.72365
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194298.72401
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194298.72495
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx96",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.62217
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx95",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.62258
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx94",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.623
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194299.62353
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx94",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.62472
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx94",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.62525
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx98",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.62572
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx98",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.62618
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx98",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.6266
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx101",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.627
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx100",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.62744
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx100",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.62878
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx100",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.62967
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx101",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.63022
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx101",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.63092
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx95",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.63133
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx95",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.63185
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx96",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.6322
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx108",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.6335
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx108",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.63424
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx109",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.63526
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx109",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.63617
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx110",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.63718
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx110",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.63857
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx109",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.64167
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx111",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.64265
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx111",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.6437
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t282 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.66185
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx111",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.66263
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx103",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.66332
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx102",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.66417
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx102",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.66567
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx102",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.66649
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx104",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.66727
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx104",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.66884
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx104",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.66956
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx104",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67025
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx105",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67106
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx105",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67249
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx105",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67317
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx103",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67379
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx103",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67466
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx111",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67528
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx111",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67595
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx111",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67659
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx111",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67722
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx111",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67796
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx108",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67847
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx112",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.67944
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx112",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68007
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx113",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68092
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx113",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68175
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 282",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68302
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx113",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68363
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx107",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68455
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx106",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68525
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx106",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68661
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx106",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68743
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx107",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68808
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx107",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68869
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx113",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68929
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx113",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.68994
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx113",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.69057
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx113",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.6912
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx113",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.69172
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx112",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.69217
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx114",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.69323
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx114",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.69527
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx115",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.69641
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx115",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.69741
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx115",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.70242
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx114",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.70657
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx116",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.70754
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx116",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.70964
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx117",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.71071
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx117",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.71165
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx117",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.71251
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx116",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.72323
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx118",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.72495
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx118",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.72696
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx118",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.72804
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx118",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.73323
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx119",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.73471
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx119",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.73646
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx119",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.74497
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx120",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.74618
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx120",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.74857
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx121",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.75015
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx121",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.75142
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx121",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.75233
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx120",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194299.75772
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194299.75816
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194299.75918
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx110",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.64347
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx109",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.64394
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx112",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.64451
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194300.64506
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx112",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.64653
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx112",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.64728
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx115",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.64775
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx114",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.6482
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx114",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.64942
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx114",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.64992
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx115",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65033
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx115",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65075
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx108",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65115
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx108",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65184
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx108",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65226
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx109",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65261
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx109",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65296
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx110",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65325
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx122",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65407
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx122",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65456
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx123",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65545
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx123",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65635
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx124",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65738
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx124",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.65876
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx123",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.6615
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx125",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.66224
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx125",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.6628
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 334",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.66386
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx125",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.66437
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx121",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.66494
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx120",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.66551
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx120",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.66667
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx120",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.66721
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx121",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.66769
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx121",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.66818
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx125",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.66883
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx125",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.66942
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx125",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.66995
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx125",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.67042
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx125",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.67084
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx122",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.6712
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx126",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.67197
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx126",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.67358
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx127",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.67452
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx127",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.6753
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx127",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.6792
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx126",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.70608
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx128",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.70693
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx128",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.70744
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx129",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.70809
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx129",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.70886
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t334 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.71982
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx129",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72035
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx117",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72088
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx116",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72155
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx116",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72295
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx116",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.7235
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx118",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72403
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx118",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72517
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx118",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72572
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx118",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72641
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx119",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72701
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx119",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72809
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx119",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72862
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx117",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72912
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx117",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.72964
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx129",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.73015
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx129",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.73064
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx129",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.73112
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx129",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.73157
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx129",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.73198
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx128",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.73271
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx130",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.7335
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx130",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.73534
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx131",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.73633
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx131",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.73723
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx131",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.73819
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx130",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.7419
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx132",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.74307
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx132",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.74482
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx133",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.74565
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx133",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.74647
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx133",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.74713
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx132",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.75785
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx134",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.75886
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx134",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.76085
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx134",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.76171
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx134",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.76509
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx135",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.76592
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx135",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.76719
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx135",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194300.77372
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194300.77402
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194300.77502
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx124",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.6598
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx123",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66026
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx127",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.6607
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx126",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66122
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194301.66243
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx126",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66348
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx126",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66404
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx127",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66475
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx127",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66529
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx128",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66577
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx128",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66637
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx128",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66696
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx122",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66747
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx122",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66794
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx122",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66845
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx123",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66884
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx123",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66926
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx124",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.66969
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx136",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.67058
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx136",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.67294
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx137",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.67434
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx137",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.67564
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx138",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.67729
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx138",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.67841
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx139",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.67972
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx139",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.68189
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx138",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.6853
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx137",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.69084
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx136",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.69521
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx140",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.69631
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx140",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.697
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx141",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.69786
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx141",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.69869
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t397 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.71213
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx141",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.71286
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx133",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.7136
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx132",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.71427
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx132",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.71586
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx132",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.71657
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx134",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.71728
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx134",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.71887
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx134",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.71986
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx134",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.72065
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx135",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.72141
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx135",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.72294
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx135",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.72367
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx133",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.72432
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx133",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.725
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx141",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.72573
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx141",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.72639
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx141",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.72702
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx141",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.72762
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx141",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.72814
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx140",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.72865
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx142",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.72976
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx142",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.73045
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx143",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.73127
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx143",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.732
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 397",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.73324
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx143",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.73396
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx131",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.73471
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx130",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.73538
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx130",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.73685
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx130",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.73767
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx131",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.73835
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx131",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.739
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx143",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.73962
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx143",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.74029
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx143",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.74101
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx143",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.74176
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx143",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.74231
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx142",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.7428
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx144",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.74733
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx144",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.74973
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx145",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.75086
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx145",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.75187
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx145",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.7527
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx144",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.76645
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx146",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.76787
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx146",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.76995
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx146",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.77087
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx146",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.77516
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx147",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.77619
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx147",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.77845
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx147",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.78809
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx148",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.78939
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx148",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.79165
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx149",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.79305
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx149",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.79416
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx149",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.79501
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx148",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194301.79943
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194301.79984
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194301.80082
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx139",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.68509
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx138",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.6856
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx137",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.68604
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx136",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.68649
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194302.68793
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx136",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.68919
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx136",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.68972
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx137",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69036
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx137",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69084
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx140",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69124
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx140",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69169
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx140",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69211
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx142",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69251
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx142",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69301
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx142",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69344
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx138",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.6938
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx138",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69419
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx139",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69451
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx150",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69541
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx150",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69776
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx151",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.69917
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx151",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.70036
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx152",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.70213
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx152",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.70321
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx153",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.70479
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx153",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.70694
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx152",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.71016
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx151",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.71564
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx150",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.72003
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx154",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.72138
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx154",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.72202
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx155",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.7229
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx155",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.72369
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t454 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.7364
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx155",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.73725
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx145",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.7381
  // }
  // ,
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx144",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.73879
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx144",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.74037
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx144",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.74112
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx146",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.74199
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx146",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.74356
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx146",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.7443
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx146",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.74505
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx147",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.74569
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx147",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.74707
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx147",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.74775
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx145",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.74841
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx145",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.74904
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx155",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.7497
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx155",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.75033
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx155",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.75095
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx155",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.75152
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx155",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.75203
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx154",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.75254
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx156",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.75353
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx156",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.75424
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx157",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.75507
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx157",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.75574
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 454",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.75706
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx157",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.75771
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx149",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.7585
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx148",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.75919
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx148",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.76072
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx148",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.7616
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx149",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.76225
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx149",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.76294
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx157",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.76356
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx157",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.76422
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx157",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.76495
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx157",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.76555
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx157",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.76618
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx156",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.76666
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx158",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.76786
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx158",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.77096
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx159",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.77211
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx159",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.77308
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx159",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.77387
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx158",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.78499
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx160",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.78611
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx160",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.78819
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx160",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.7891
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx160",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.79455
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx161",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.79565
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx161",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.7972
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx161",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.81177
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx162",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.813
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx162",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.81517
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx163",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.81649
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx163",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.81772
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx163",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.81884
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx162",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194302.82339
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194302.82378
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194302.82476
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx153",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71009
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx152",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.7106
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx151",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71106
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx150",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71149
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194303.71257
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx150",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.7135
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx150",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71392
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx151",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71435
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx151",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71485
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx154",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71525
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx154",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71569
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx154",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71617
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx156",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71655
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx156",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71698
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx156",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71738
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx152",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71773
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx152",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71814
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx153",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71856
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx164",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.71948
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx164",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.72186
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx165",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.72299
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx165",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.72401
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx166",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.72509
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx166",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.72592
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx167",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.72689
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx167",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.72848
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx166",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.73102
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx165",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.73475
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx164",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.73802
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx168",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.73901
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx168",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.7395
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx169",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.74013
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx169",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.74078
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t504 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75076
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx169",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75127
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx159",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75179
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx160",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75228
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx160",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75364
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx160",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75419
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx160",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75499
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx161",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75549
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx161",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75666
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx161",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75719
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx158",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75768
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx158",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75881
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx158",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75934
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx159",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.75981
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx159",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76028
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx169",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76086
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx169",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76149
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx169",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76205
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx169",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76258
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx169",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.7631
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx168",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76361
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx170",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76456
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx170",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76515
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx171",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76582
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx171",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76636
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 504",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76764
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx171",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76832
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx163",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.76924
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx162",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.77019
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx162",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.77163
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx162",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.7723
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx163",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.77281
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx163",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.77334
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx171",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.77384
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx171",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.77454
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx171",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.77532
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx171",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.77581
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx171",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.77627
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx170",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.77665
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx172",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.7774
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx172",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.77873
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx173",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.77972
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx173",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.78045
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx173",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.78119
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx172",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.78192
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx172",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.78526
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx174",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.78601
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx174",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.78718
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx174",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.79308
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx175",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.79383
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx175",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.79552
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx175",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.80546
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx176",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.80648
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx176",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.80812
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx177",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.80914
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx177",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.80994
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx177",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.81065
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx176",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194303.81417
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194303.81452
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194303.81546
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx167",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.73218
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx166",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.73278
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx165",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.73327
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx164",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.73376
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194304.73521
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx164",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.73679
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx164",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.73756
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx165",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.73823
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx165",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.73879
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx168",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.7394
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx168",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.74006
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx168",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.74056
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx170",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.74131
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx170",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.74188
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx170",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.74243
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx166",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.74288
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx166",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.74331
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx167",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.74365
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx178",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.74463
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx178",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.74655
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx179",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.7476
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx179",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.74851
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx180",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.74961
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx180",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.75061
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx181",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.75177
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx181",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.75345
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx180",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.75641
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx179",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.75987
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx178",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.7638
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx182",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.76492
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx182",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.76553
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx183",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.76631
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx183",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.76714
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t553 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.77986
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx183",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.78056
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx173",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.78168
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx172",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.78243
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx172",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.78425
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx172",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.78499
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx172",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.78575
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx174",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.78645
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx174",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.78791
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx174",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.78864
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx175",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.7894
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx175",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79084
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx175",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79156
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx173",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79221
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx173",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79291
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx183",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79355
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx183",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79417
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx183",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79481
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx183",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79543
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx183",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79597
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx182",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79669
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx184",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79794
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx184",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79857
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx185",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.79943
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx185",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.80017
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 553",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.80153
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx185",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.80218
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx177",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.80288
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx176",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.80354
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx176",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.80522
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx176",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.80625
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx177",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.807
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx177",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.8077
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx185",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.80835
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx185",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.80897
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx185",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.80968
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx185",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.81066
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx185",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.81122
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx184",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.81171
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx186",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.81265
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx186",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.81426
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx187",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.81525
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx187",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.81616
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx187",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.81699
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx186",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.81783
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx186",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.82243
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx188",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.82362
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx188",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.82549
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx188",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.83429
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx189",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.83537
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx189",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.83717
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx189",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.84708
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx190",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.84791
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx190",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.84934
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx191",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.85017
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx191",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.85091
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx191",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.85156
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx190",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194304.85503
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194304.85535
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194304.85633
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx181",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.7589
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx180",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.75938
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx182",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.75985
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194305.76039
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx182",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76173
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx182",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76225
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx184",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.7627
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx184",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76322
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx184",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76368
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx179",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76413
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx178",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76462
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx178",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76576
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx178",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76623
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx179",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76664
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx179",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.7671
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx180",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76748
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx180",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76805
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx181",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76841
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx192",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76933
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx192",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.76992
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx193",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.77077
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx193",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.7716
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx194",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.77255
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx194",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.77448
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx193",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.7775
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx195",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.77835
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx195",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.77914
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t606 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.79538
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx195",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.79604
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx187",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.79672
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx186",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.79741
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx186",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.79895
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx186",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.7997
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx186",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80037
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx188",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80111
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx188",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80258
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx188",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80327
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx189",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80392
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx189",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80522
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx189",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80588
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx187",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80648
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx187",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80725
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx195",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80785
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx195",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80846
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx195",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80906
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx195",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.80964
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx195",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.81015
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx192",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.8106
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx196",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.81151
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx196",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.81207
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx197",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.8128
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx197",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.81343
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 606",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.81458
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx197",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.81517
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx191",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.81582
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx190",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.81647
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx190",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.81779
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx190",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.81846
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx191",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.81906
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx191",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.81966
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx197",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.82025
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx197",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.82085
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx197",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.82171
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx197",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.82228
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx197",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.82279
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx196",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.82324
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx198",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.82416
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx198",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.82598
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx199",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.82703
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx199",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.82795
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx199",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.83294
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx198",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.83665
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx200",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.83758
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx200",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.83918
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx201",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.84023
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx201",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.84123
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx201",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.84212
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx200",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.84296
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx200",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.84743
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx202",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.84841
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx202",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.85061
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx202",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.85878
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx203",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.85986
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx203",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.86193
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx203",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.87216
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx204",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.8731
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx204",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.87486
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx205",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.8759
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx205",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.87682
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx205",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.87761
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx204",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194305.8812
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194305.88155
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194305.88234
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx194",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.77682
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx193",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.77728
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx192",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.77771
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194306.77825
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx192",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.7793
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx192",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.77977
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx196",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78018
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx196",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78064
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx196",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78126
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx199",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78175
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx198",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78221
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx198",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78342
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx198",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78391
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx199",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78433
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx199",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78479
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx193",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.7852
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx193",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.7858
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx194",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78615
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx206",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78708
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx206",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78765
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx207",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78857
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx207",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.78933
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx208",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.79057
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx208",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.7924
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx207",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.79661
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx209",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.79745
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx209",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.79823
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t631 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.81089
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx209",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.8116
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx201",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.81229
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx200",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.81295
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx200",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.81437
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx200",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.81508
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx200",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.81572
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx202",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.81636
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx202",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.81785
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx202",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.81855
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx203",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.81918
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx203",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82058
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx203",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82148
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx201",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82211
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx201",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82274
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx209",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82336
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx209",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82398
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx209",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82491
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx209",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.8256
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx209",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82616
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx206",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82687
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx210",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82787
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx210",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82847
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx211",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82925
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx211",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.82991
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 631",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.83133
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx211",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.83245
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx205",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.83365
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx204",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.83485
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx204",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.83633
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx204",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.83703
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx205",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.83764
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx205",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.83825
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx211",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.83884
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx211",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.83944
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx211",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.84004
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx211",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.84061
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx211",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.8412
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx210",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.84172
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx212",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.84265
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx212",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.8483
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx213",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.84942
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx213",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.85034
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx213",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.85389
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx212",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.85748
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx214",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.85849
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx214",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.86002
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx215",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.861
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx215",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.86197
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx215",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.86274
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx214",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.86356
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx214",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.86755
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx216",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.86847
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx216",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.86991
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx216",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.87812
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx217",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.87908
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx217",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.88092
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx217",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.89205
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx218",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.89302
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx218",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.89475
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx219",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.89578
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx219",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.89697
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx219",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.89791
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx218",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194306.90211
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194306.90251
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194306.90342
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx208",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.79838
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx207",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.79894
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx210",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.79942
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194307.79996
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx210",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80099
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx210",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80156
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx213",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80203
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx212",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80252
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx212",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80371
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx212",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80417
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx213",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80459
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx213",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80506
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx206",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80552
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx206",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80613
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx206",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80658
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx207",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80697
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx207",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80739
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx208",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80773
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx220",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80864
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx220",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.80935
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx221",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.81017
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx221",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.81084
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx222",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.81181
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx222",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.81337
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx221",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.81601
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx223",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.81694
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx223",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.81762
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 660",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.81882
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx223",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.81946
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx219",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.82016
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx218",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.82085
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx218",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.82251
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx218",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.82332
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx219",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.82415
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx219",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.8248
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx223",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.82548
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx223",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.8262
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx223",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.82685
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx223",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.82744
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx223",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.82796
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx220",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.82843
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx224",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.82998
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx224",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.83183
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx225",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.8329
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx225",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.83382
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx225",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.83761
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx224",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.84154
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx226",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.84289
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx226",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.84364
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx227",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.84446
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx227",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.84528
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t660 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.85784
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx227",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.85856
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx215",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.8593
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx214",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.86006
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx214",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.86175
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx214",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.86254
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx214",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.86326
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx216",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.86394
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx216",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.86559
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx216",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.86633
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx217",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.867
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx217",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.86844
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx217",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.86917
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx215",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.8698
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx215",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.87044
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx227",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.87109
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx227",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.87176
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx227",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.87238
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx227",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.87296
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx227",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.87347
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx226",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.87392
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx228",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.87484
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx228",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.87678
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx229",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.87782
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx229",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.87874
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx229",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.87996
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx228",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.8836
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx230",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.88448
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx230",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.88601
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx231",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.88701
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx231",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.89118
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx231",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.89214
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx230",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.89324
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx230",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.8978
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx232",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.89886
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx232",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.90093
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx232",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.90935
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx233",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.91034
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx233",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.91217
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx233",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194307.92385
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194307.92427
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194307.92512
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx222",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.8188
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx221",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.81943
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx220",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82005
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194308.8207
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx220",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82191
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx220",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82241
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx225",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82287
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx224",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82335
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx224",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82462
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx224",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82513
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx225",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82556
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx225",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82602
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx226",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82656
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx226",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82709
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx226",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82757
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx221",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82804
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx221",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82848
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx222",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82882
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx234",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.82973
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx234",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.83036
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx235",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.83117
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx235",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.83184
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx236",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.83271
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx236",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.83418
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx235",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.83685
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx237",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.83771
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx237",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.83841
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 698",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.83966
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx237",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84036
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx229",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.8413
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx228",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84201
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx228",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84347
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx228",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84418
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx229",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84481
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx229",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84544
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx237",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84607
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx237",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84668
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx237",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84733
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx237",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84792
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx237",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84843
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx234",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84887
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx238",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.84979
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx238",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.85159
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx239",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.85261
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx239",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.85354
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx239",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.85725
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx238",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.86124
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx240",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.86219
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx240",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.86275
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx241",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.86347
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx241",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.86424
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t698 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.87588
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx241",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.87663
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx231",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.87735
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx230",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.87801
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx230",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.87936
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx230",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.88005
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx230",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.88069
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx232",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.88132
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx232",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.88264
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx232",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.8833
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx233",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.88393
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx233",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.88522
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx233",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.88589
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx231",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.88655
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx231",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.8872
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx241",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.88783
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx241",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.88846
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx241",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.88908
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx241",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.88967
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx241",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.8902
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx240",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.89067
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx242",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.89162
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx242",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.89345
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx243",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.8945
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx243",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.89543
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx243",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.89625
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx242",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.89989
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx244",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.9008
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx244",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.90244
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx245",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.90348
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx245",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.90438
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx245",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.90525
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx244",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.90607
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx244",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.91049
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx246",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.91149
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx246",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.91344
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx246",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.92188
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx247",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.92312
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx247",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.92547
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx247",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194308.94144
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194308.94188
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194308.94276
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx236",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84016
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx235",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84076
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx234",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84135
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194309.84199
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx234",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84309
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx234",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.8436
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx239",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84406
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx238",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84457
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx238",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84578
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx238",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84637
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx239",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84682
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx239",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84728
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx240",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84781
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx240",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84833
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx240",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84877
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx235",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84916
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx235",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84959
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx236",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.84992
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx248",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.85082
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx248",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.85139
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx249",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.85217
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx249",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.85293
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx250",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.85386
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx250",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.85529
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx249",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.85827
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx251",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.85912
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx251",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.85978
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 733",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.86095
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx251",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.86183
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx243",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.86255
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx242",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.86339
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx242",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.86489
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx242",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.86559
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx243",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.86622
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx243",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.86688
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx251",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.86752
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx251",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.86814
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx251",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.86878
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx251",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.86937
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx251",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.8699
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx248",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.87038
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx252",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.87133
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx252",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.87321
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx253",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.87426
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx253",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.87517
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx253",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.8787
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx252",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.88247
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx254",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.88346
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx254",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.88403
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx255",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.88476
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx255",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.88553
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t733 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.89727
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx255",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.89794
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx245",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.89861
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx244",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.89925
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx244",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90057
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx244",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90127
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx244",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90193
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx246",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90254
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx246",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90385
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx246",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90452
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx247",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90514
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx247",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90647
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx247",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90715
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx245",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90774
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx245",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90835
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx255",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90895
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx255",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.90957
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx255",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.91018
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx255",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.91075
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx255",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.91128
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx254",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.91175
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx256",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.91269
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx256",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.91455
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx257",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.91558
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx257",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.91651
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx257",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.91743
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx256",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.92118
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx258",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.92213
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx258",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.92366
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx259",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.92465
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx259",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.92555
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx259",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.92632
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx258",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.92716
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx258",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.93097
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx260",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.9319
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx260",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.93331
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx260",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.94164
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx261",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.94269
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx261",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.9446
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx261",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194309.95548
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194309.95613
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194309.95717
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx250",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.85675
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx249",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.85724
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx248",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.85768
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194310.85818
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx248",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.85917
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx248",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.85962
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx253",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.86004
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx252",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.86052
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx252",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.8622
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx252",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.86283
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx253",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.86337
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx253",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.86405
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx254",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.86462
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx254",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.86966
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx254",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.87057
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx249",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.87133
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx249",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.87202
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx250",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.87254
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx262",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.87389
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx262",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.87473
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx263",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.87649
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx263",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.87763
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx264",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.87935
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx264",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.8812
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx263",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.88471
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx265",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.88559
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx265",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.88628
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 759",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.88775
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx265",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.88845
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx257",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.88918
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx256",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.88995
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx256",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.8917
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx256",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.89295
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx257",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.89414
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx257",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.89531
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx265",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.89672
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx265",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.89794
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx265",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.89884
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx265",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.89948
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx265",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.90002
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx262",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.90049
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx266",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.90163
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx266",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.90369
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx267",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.90491
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx267",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.90604
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx267",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.90899
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx266",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.9133
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx268",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.91429
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx268",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.9149
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx269",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.91572
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx269",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.91651
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t759 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93006
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx269",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93078
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx259",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93149
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx258",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93215
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx258",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93346
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx258",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93414
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx258",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93478
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx260",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93539
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx260",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93663
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx260",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93729
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx261",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93792
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx261",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93913
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx261",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.93978
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx259",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94038
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx259",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94097
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx269",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94158
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx269",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94218
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx269",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94278
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx269",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94334
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx269",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94385
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx268",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94429
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx270",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94518
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx270",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94711
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx271",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94815
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx271",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94907
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx271",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.94988
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx270",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.95374
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx272",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.95515
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx272",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.95688
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx273",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.95795
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx273",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.959
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx273",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.95985
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx272",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.96074
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx272",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.96494
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx274",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.96588
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx274",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.96745
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx274",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.97535
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx275",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.97702
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx275",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.97925
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx275",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194310.99001
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194310.99042
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194310.99129
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx264",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.88587
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx263",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.88647
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx267",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.88708
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx266",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.88757
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194311.88884
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx266",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.88991
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx266",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89045
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx267",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89096
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx267",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89143
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx268",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89182
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx268",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89227
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx268",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89268
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx262",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89307
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx262",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89349
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx262",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89389
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx263",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89423
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx263",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89461
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx264",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89493
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx276",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89564
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx276",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89728
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx277",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89818
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx277",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.89902
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx278",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.9
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx278",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.90083
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx279",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.902
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx279",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.90344
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx278",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.90609
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx277",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.91024
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx276",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.91745
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx280",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.91841
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx280",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.9189
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx281",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.91953
  // }
  // ,
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx281",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.92023
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t790 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.93065
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx281",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.93128
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx273",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.93182
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx272",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.93235
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx272",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.93346
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx272",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.93403
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx272",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.93456
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx274",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.93506
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx274",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.93617
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx274",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.93673
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx275",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.93723
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx275",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.93834
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx275",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.9389
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx273",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.9394
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx273",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.9399
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx281",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.94039
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx281",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.94088
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx281",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.94146
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx281",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.942
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx281",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.94257
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx280",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.94308
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx282",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.94394
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx282",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.94442
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx283",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.94506
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx283",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.94598
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 790",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.94755
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx283",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.9481
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx271",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.94868
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx270",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.94924
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx270",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.95106
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx270",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.95203
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx271",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.95274
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx271",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.95362
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx283",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.95451
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx283",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.95523
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx283",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.95599
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx283",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.95683
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx283",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.95756
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx282",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.9582
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx284",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.95938
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx284",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.96164
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx285",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.96264
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx285",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.9635
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx285",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.96425
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx284",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.96494
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx284",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.96851
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx286",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.96998
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx286",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.97153
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx286",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.978
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx287",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.97894
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx287",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.98066
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx287",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.9912
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx288",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.99229
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx288",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.99446
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx289",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.99546
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx289",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.99637
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx289",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194311.99736
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx288",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.00143
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194312.00189
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194312.00297
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx279",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.9093
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx278",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.90991
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx282",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91046
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194312.91098
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx282",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91214
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx282",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91263
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx277",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91309
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx276",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91359
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx276",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91481
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx276",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91533
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx277",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91576
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx277",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91624
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx280",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91683
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx280",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.9174
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx280",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91787
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx278",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91829
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx278",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91872
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx279",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.91911
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx290",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.92003
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx290",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.92063
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx291",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.92165
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx291",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.92254
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx292",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.92363
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx292",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.92522
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx291",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.92832
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx293",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.92921
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx293",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.92991
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 823",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.93117
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx293",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.93187
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx289",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.9326
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx288",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.93332
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx288",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.93479
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx288",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.93551
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx289",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.93616
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx289",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.93678
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx293",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.9374
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx293",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.93805
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx293",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.93869
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx293",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.93926
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx293",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.9398
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx290",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.94026
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx294",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.94124
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx294",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.94311
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx295",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.94419
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx295",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.94512
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx295",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.94901
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx294",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.95286
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx296",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.95386
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx296",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.95445
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx297",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.95521
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx297",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.95598
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t823 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.96812
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx297",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.9688
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx285",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.96949
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx284",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.9738
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx284",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.97545
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx284",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.97618
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx284",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.97686
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx286",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.9775
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx286",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.97895
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx286",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.97964
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx287",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.9803
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx287",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.98178
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx287",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.9825
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx285",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.98314
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx285",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.98381
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx297",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.98441
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx297",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.98502
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx297",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.98564
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx297",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.98621
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx297",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.98671
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx296",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.98725
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx298",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.98816
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx298",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.98992
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx299",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.99093
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx299",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.99184
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx299",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.99263
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx298",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.99644
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx300",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.99743
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx300",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194312.99944
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx301",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.00061
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx301",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.00225
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx301",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.00355
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx300",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.00448
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx300",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.00954
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx302",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.01093
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx302",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.01245
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx302",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.0214
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx303",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.02252
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx303",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.02472
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx303",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.03634
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194313.03673
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194313.03757
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx292",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.92877
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx291",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.92937
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx290",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.92996
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194313.9306
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx290",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93169
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx290",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93222
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx295",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93268
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx294",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93318
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx294",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93439
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx294",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93488
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx295",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93531
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx295",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93578
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx296",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93622
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx296",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93688
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx296",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93734
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx291",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93773
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx291",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93815
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx292",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93849
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx304",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.93939
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx304",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.94
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx305",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.94098
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx305",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.94195
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx306",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.94303
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx306",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.94445
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx305",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.94721
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx307",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.94814
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx307",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.94888
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 858",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95013
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx307",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95079
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx299",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95148
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx298",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95216
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx298",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95359
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx298",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95429
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx299",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95492
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx299",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95555
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx307",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95617
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx307",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95678
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx307",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95741
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx307",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95798
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx307",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.9585
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx304",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95895
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx308",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.95986
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx308",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.96171
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx309",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.96276
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx309",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.96369
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx309",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.96748
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx308",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.97136
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx310",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.97234
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx310",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.97291
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx311",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.97367
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx311",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.97444
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t858 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.98648
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx311",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.9872
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx301",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.9879
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx300",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.98858
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx300",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.98993
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx300",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.99062
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx300",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.99129
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx302",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.99191
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx302",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.99339
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx302",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.99407
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx303",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.9947
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx303",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.99601
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx303",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.99666
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx301",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.99728
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx301",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.99789
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx311",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.9985
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx311",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.99917
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx311",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194313.99979
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx311",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.00037
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx311",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.00088
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx310",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.00151
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx312",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.00264
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx312",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.0046
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx313",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.00578
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx313",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.00677
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx313",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.00759
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx312",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.0118
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx314",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.01273
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx314",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.01426
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx315",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.01527
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx315",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.01642
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx315",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.01734
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx314",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.01822
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx314",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.02858
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx316",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.02974
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx316",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.0313
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx316",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.04009
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx317",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.04132
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx317",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.04396
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx317",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.05477
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194314.05516
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194314.056
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx306",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95064
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx305",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95124
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx304",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95186
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194314.95247
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx304",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95356
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx304",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95405
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx309",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95458
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx308",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95509
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx308",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95636
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx308",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95685
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx309",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95728
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx309",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95774
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx310",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95825
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx310",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95879
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx310",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95925
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx305",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.95964
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx305",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.96006
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx306",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.9604
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx318",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.9613
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx318",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.96188
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx319",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.96272
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx319",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.96351
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx320",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.96441
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx320",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.96577
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx319",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.96862
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx321",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.96946
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx321",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.97013
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 887",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.97135
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx321",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.972
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx313",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.9727
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx312",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.97339
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx312",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.97488
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx312",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.97569
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx313",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.97643
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx313",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.97711
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx321",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.97774
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx321",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.97836
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx321",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.97899
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx321",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.97958
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx321",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.98011
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx318",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.98056
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx322",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.98148
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx322",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.98338
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx323",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.98446
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx323",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.98539
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx323",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.98905
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx322",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.99297
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx324",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.99392
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx324",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.99449
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx325",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.99522
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx325",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194314.99601
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t887 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.00865
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx325",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.00934
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx315",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.01003
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx314",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.01069
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx314",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.01205
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx314",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.01275
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx314",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.01339
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx316",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.01402
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx316",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.01534
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx316",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.01601
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx317",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.01666
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx317",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.01808
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx317",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.01878
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx315",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.0194
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx315",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.02015
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx325",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.02079
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx325",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.02154
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx325",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.02223
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx325",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.02293
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx325",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.02347
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx324",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.02395
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx326",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.02524
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx326",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.02724
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx327",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.02829
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx327",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.02922
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx327",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.03001
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx326",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.03382
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx328",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.03475
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx328",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.03637
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx329",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.03744
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx329",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.03847
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx329",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.03929
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx328",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.04013
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx328",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.04417
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx330",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.04512
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx330",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.04673
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx330",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.0556
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx331",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.05685
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx331",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.059
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx331",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.0754
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194315.07584
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194315.07677
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx320",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.9716
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx319",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.97219
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx318",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.9728
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194315.97332
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx318",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.97439
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx318",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.97487
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx323",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.97534
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx322",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.97583
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx322",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.97697
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx322",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.97744
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx323",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.97786
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx323",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.97831
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx324",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.97882
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx324",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.97934
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx324",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.97978
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx319",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.98018
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx319",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.98061
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx320",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.98096
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx332",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.9819
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx332",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.98249
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx333",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.98343
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx333",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.9843
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx334",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.98526
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx334",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.98676
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx333",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.98943
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx335",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99029
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx335",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99097
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 922",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99228
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx335",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99307
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx327",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99378
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx326",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99446
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx326",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99593
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx326",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99664
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx327",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99728
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx327",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99793
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx335",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99854
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx335",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99915
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx335",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194315.99976
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx335",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.0004
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx335",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.00092
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx332",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.00168
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx336",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.00293
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx336",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.00504
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx337",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.00616
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx337",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.00709
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx337",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.01106
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx336",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.01549
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx338",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.01647
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx338",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.01722
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx339",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.01824
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx339",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.01909
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t922 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.03253
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx339",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.0332
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx329",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.03403
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx330",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.03474
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx330",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.03624
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx330",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.03697
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx331",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.03761
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx331",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.03899
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx331",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.03969
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx328",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04032
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx328",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04176
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx328",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04246
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx328",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04316
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx329",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04379
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx329",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04443
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx339",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04504
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx339",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04567
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx339",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04629
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx339",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04686
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx339",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04737
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx338",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04782
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx340",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.04876
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx340",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.05082
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx341",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.05192
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx341",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.05293
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx341",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.05375
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx340",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.05753
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx342",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.05848
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx342",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.05989
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx343",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.06083
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx343",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.0619
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx343",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.06272
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx342",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.07126
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx344",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.07225
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx344",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.07417
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx344",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.08581
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx345",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.08684
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx345",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.08882
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx345",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.08983
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx345",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.09436
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194316.09476
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194316.09564
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx334",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.99156
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx333",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.9921
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx332",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.99258
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194316.99312
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx332",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.99448
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx332",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.99513
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx337",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.99567
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx336",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.99619
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx336",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.99752
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx336",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.99804
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx337",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.99851
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx337",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.99901
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx338",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194316.99957
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx338",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.00013
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx338",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.00064
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx333",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.00112
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx333",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.00178
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx334",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.00218
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx346",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.00323
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx346",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.00394
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx347",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.00486
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx347",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.00587
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx348",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.00684
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx348",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.00843
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx347",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.01163
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx349",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.01252
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx349",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.01336
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 946",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.01462
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx349",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.01527
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx341",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.01597
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx340",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.01699
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx340",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.01896
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx340",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.01986
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx341",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.02068
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx341",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.02162
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx349",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.02237
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx349",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.02304
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx349",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.0237
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx349",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.0243
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx349",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.02483
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx346",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.02531
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx350",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.02629
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx350",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.02876
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx351",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.02993
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx351",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.03085
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx351",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.03378
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx350",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.03756
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx352",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.03851
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx352",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.03926
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx353",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.0402
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx353",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.041
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t946 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.05342
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx353",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.05411
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx343",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.05478
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx342",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.05544
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx342",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.05677
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx342",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.05744
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx344",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.05806
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx344",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.05935
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx344",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06002
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx345",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06065
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx345",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06243
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx345",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.0632
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx345",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06387
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx343",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06448
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx343",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06509
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx353",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06569
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx353",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06629
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx353",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06698
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx353",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06756
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx353",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06808
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx352",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06854
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx354",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.06947
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx354",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.07131
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx355",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.07236
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx355",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.0736
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx355",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.07442
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx354",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.07805
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx356",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.07897
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx356",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.08037
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx357",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.0813
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx357",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.08215
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx357",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.08287
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx356",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.09169
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx358",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.09274
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx358",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.09489
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx358",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.10828
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx359",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.10988
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx359",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.11202
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx359",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.11305
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx359",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194317.11773
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194317.1186
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194317.11998
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx348",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.01312
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx347",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.01367
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx351",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.01416
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx350",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.01465
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194318.01595
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx350",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.01708
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx350",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.01767
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx351",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.01824
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx351",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.0188
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx352",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.01927
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx352",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.01978
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx352",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.02023
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx346",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.02065
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx346",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.02122
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx346",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.02185
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx347",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.02234
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx347",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.02282
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx348",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.02319
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx360",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.02418
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx360",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.02686
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx361",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.02812
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx361",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.02916
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx362",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.03036
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx362",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.03141
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx363",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.03266
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx363",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.03437
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx362",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.03749
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx361",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.04513
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx360",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.05856
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx364",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.05954
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx364",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.06011
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx365",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.06089
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx365",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.06183
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t971 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.07391
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx365",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.07462
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx357",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.07529
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx356",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.07595
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx356",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.07731
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx356",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.07799
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx358",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.07861
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx358",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.07988
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx358",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.08055
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx359",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.0812
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx359",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.08246
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx359",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.08314
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx359",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.08379
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx357",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.0844
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx357",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.08507
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx365",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.08582
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx365",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.08647
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx365",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.08712
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx365",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.08781
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx365",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.08836
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx364",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.08883
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx366",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.08976
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx366",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.09033
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx367",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.09107
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx367",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.09171
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 971",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.09288
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx367",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.09348
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx355",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.09417
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx354",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.09483
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx354",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.09617
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx354",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.09704
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx355",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.09768
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx355",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.09829
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx367",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.0989
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx367",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.09951
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx367",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.10013
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx367",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.10072
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx367",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.10126
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx366",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.10175
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx368",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.10279
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx368",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.10429
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx369",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.10524
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx369",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.10611
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx369",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.10685
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx368",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.11493
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx370",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.11589
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx370",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.11795
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx370",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.13027
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx371",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.13123
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx371",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.13277
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx371",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.13362
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx371",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.13838
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx372",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.13938
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx372",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.14149
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx373",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.14275
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx373",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.14379
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx373",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.14473
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx372",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194318.14889
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194318.14929
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194318.15016
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx363",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.03895
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx362",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.03958
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx361",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04016
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx360",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04066
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194319.04219
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx360",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04334
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx360",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.0439
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx361",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04448
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx361",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.045
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx364",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04544
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx364",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04612
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx364",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04661
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx366",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04706
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx366",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04754
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx366",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04801
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx362",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04841
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx362",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04893
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx363",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.04941
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx374",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.05037
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx374",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.05279
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx375",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.05404
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx375",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.05506
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx376",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.05627
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx376",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.05733
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx377",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.05864
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx377",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.06041
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx376",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.06363
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx375",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.06792
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx374",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.07199
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx378",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.07298
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx378",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.07357
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx379",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.07435
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx379",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.0752
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t995 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.09121
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx379",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.09188
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx369",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.09259
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx370",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.09332
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx370",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.09481
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx370",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.09554
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx371",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.09621
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx371",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.09787
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx371",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.09864
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx371",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.09936
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx368",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10005
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx368",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10159
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx368",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10228
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx369",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10289
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx369",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10352
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx379",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10412
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx379",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10477
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx379",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10544
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx379",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10606
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx379",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10658
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx378",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10704
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx380",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10795
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx380",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10851
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx381",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.10936
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx381",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11001
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 995",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11116
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx381",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11176
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx373",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11243
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx372",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11312
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx372",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11443
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx372",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11509
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx373",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.1157
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx373",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11631
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx381",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11693
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx381",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11753
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx381",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11814
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx381",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.1187
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx381",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11921
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx380",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.11966
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx382",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.12068
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx382",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.12291
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx383",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.12403
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx383",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.12496
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx383",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.12574
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx382",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.13862
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx384",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.1396
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx384",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.14197
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx384",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.14296
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx384",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.14702
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx385",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.14821
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx385",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.15024
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx385",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.1587
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx386",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.15969
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx386",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.16151
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx387",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.16262
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx387",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.16357
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx387",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.16438
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx386",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194319.16833
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194319.16873
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194319.16957
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx377",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.06525
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx376",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.06613
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx375",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.067
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx374",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.06756
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194320.06888
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx374",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07006
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx374",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07073
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx375",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07131
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx375",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07182
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx378",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07221
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx378",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07267
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx378",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.0731
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx380",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07349
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx380",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07392
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx380",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07433
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx376",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07469
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx376",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07511
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx377",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07541
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx388",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07625
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx388",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07808
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx389",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07898
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx389",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.07981
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx390",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.08073
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx390",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.08157
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx391",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.08255
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx391",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.084
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx390",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.08692
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx389",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.09182
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx388",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.09574
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx392",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.09659
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx392",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.09706
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx393",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.09767
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx393",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.09831
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t1040 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.10896
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx393",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.10955
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx383",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11023
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx382",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.111
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx382",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11219
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx382",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11279
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx384",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11332
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx384",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11443
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx384",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11498
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx384",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11551
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx385",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11601
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx385",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11717
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx385",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11773
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx383",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11825
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx383",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11877
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx393",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11931
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx393",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.11982
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx393",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12033
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx393",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12082
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx393",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12153
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx392",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12222
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx394",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12307
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx394",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12356
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx395",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12418
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx395",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12476
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 1040",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12572
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx395",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12626
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx387",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12681
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx386",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12736
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx386",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12847
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx386",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12903
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx387",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.12955
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx387",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.13009
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx395",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.1306
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx395",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.13518
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx395",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.13605
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx395",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.1366
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx395",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.13705
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx394",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.13749
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx396",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.13834
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx396",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.14014
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx397",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.14135
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx397",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.14263
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx397",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.14373
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx396",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.15552
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx398",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.15661
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx398",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.15834
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx398",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.15918
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx398",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.16331
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx399",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.16419
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx399",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.16603
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx399",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.17423
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx400",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.17534
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx400",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.17692
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx401",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.17791
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx401",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.1787
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx401",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.17937
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx400",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.1828
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194320.18314
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194320.18389
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r1$rateThreshold",
  //   "value": " num 20.8",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.76748
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.76808
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx398",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.76875
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194320.77015
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx398",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.77154
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx398",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.77224
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx398",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.77285
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.7734
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx402",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.78015
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx402",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.78216
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx402",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.78326
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx402",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194320.78793
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194320.78841
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194320.78936
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx391",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.08932
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx390",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.08993
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx389",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09048
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx388",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09098
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194321.09223
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx388",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09331
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx388",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09386
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx389",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09439
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx389",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09493
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx392",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09537
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx392",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09587
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx392",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09631
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx394",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09676
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx394",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09724
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx394",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09768
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx390",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09809
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx390",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09852
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx391",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09887
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx403",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.09981
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx403",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.10244
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx404",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.10397
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx404",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.10525
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx405",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.10656
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx405",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.10767
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx406",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.10899
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx406",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.11089
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx405",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.11409
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx404",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.11883
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx403",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.12318
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx407",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.12423
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx407",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.12483
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx408",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.12561
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx408",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.12641
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t1081 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.13877
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx408",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.13946
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx397",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.14015
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx402",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.14081
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx402",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.14224
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx402",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.14312
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx402",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.14383
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx396",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.1445
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx396",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.146
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx396",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.14674
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx399",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.14742
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx399",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.14882
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx399",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.14952
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx397",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15015
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx397",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15078
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx408",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.1514
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx408",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15201
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx408",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15264
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx408",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15325
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx408",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15377
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx407",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15423
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx409",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15527
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx409",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15596
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx410",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15677
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx410",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15744
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 1081",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15867
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx410",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.15933
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx401",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.16002
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx400",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.16068
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx400",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.1621
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx400",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.16279
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx401",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.1634
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx401",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.16402
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx410",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.16461
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx410",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.16522
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx410",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.16582
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx410",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.16646
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx410",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.16698
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx409",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.16744
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx411",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.16834
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx411",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.16995
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx412",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.17098
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx412",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.17209
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx412",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.17289
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx411",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.17374
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx411",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.1776
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx413",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.17862
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx413",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.18073
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx413",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.19332
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx414",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.19432
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx414",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.1958
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx414",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.20466
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx415",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.20589
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx415",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.20801
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx416",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.20933
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx416",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.21033
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx416",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.21118
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx415",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.21934
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194321.21975
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194321.22065
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r1$rateThreshold",
  //   "value": " num 26.7",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.66121
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.66179
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx411",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.66239
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194321.66401
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx411",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.66526
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx411",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.66593
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx411",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.66658
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.66711
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx417",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.6686
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx417",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.67038
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx417",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.67129
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx417",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194321.67612
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194321.67654
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194321.67742
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx406",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.11372
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx405",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.11432
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx404",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.11499
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx403",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.11558
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194322.11686
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx403",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.11803
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx403",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.11876
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx404",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.11941
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx404",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.12007
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx407",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.12056
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx407",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.12108
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx407",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.12155
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx409",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.12197
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx409",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.12245
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx409",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.1229
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx405",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.12331
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx405",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.12374
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx406",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.12409
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx418",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.12546
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx418",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.12775
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx419",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.12895
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx419",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.1301
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx420",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.13162
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx420",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.13274
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx421",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.13404
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx421",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.13574
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx420",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.1388
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx419",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.14097
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx418",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.14522
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx422",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.1463
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx422",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.14691
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx423",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.14767
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx423",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.14846
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t1091 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.16042
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx423",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.16112
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx412",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.1618
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx413",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.16247
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx413",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.16381
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx413",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.1645
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx414",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.16514
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx414",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.16655
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx414",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.16727
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx417",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.16791
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx417",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.16928
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx417",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.16997
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx417",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17065
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx412",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17128
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx412",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17191
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx423",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17252
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx423",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17314
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx423",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17376
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx423",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17444
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx423",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17507
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx422",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17557
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx424",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17664
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx424",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17722
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx425",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17796
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx425",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17861
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 1091",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.17976
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx425",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.18037
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx416",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.18112
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx415",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.18185
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx415",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.18322
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx415",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.18389
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx416",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.1845
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx416",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.18512
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx425",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.18572
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx425",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.18633
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx425",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.18703
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx425",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.18765
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx425",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.1882
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx424",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.18869
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx426",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.18968
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx426",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.19159
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx427",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.19265
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx427",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.19363
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx427",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.19445
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx426",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.20676
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx428",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.20787
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx428",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.20993
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx428",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.21894
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx429",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.21994
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx429",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.22169
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx429",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.22258
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx429",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.2271
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx430",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.22822
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx430",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.23019
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx431",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.2313
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx431",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.23225
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx431",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.23307
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx430",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.2372
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194322.24142
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194322.24265
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r1$rateThreshold",
  //   "value": " num 30.6",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.43005
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.43063
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx429",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.43123
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194322.43257
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx429",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.43367
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx429",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.43425
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx429",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.43477
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.43526
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx432",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.43644
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx432",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.43802
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx432",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.43887
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx432",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194322.44323
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194322.44365
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194322.44457
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx421",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.13704
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx420",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.13767
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx419",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.13828
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx418",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.13885
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194323.1401
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx418",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14133
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx418",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14191
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx419",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14247
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx419",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14301
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx422",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14345
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx422",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14394
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx422",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14439
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx424",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14481
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx424",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14529
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx424",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14576
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx420",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14616
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx420",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14658
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx421",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14693
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx433",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.14785
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx433",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.15005
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx434",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.15126
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx434",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.15236
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx435",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.15377
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx435",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.15485
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx436",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.15609
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx436",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.15798
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx435",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.1611
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx434",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.16585
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx433",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.16968
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx437",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.17069
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx437",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.17128
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx438",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.17203
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx438",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.17282
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t1160 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.18471
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx438",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.1854
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx427",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.18609
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx426",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.18677
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx426",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.18819
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx426",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.18889
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx428",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.18953
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx428",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19093
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx428",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19163
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx432",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19228
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx432",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19369
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx432",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19439
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx432",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19504
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx427",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19565
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx427",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19626
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx438",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19686
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx438",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19747
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx438",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.1981
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx438",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19868
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx438",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19919
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx437",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.19966
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx439",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.20059
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx439",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.20128
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx440",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.20213
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx440",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.20278
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 1160",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.20403
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx440",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.20472
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx431",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.20545
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx430",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.20612
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx430",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.20763
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx430",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.20835
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx431",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.20898
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx431",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.2096
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx440",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.21033
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx440",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.21097
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx440",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.2116
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx440",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.21218
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx440",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.21271
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx439",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.21317
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx441",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.2141
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx441",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.21602
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx442",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.21712
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx442",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.21805
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx442",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.21884
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx441",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.2301
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx443",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.23108
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx443",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.23269
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx443",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.24222
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx444",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.24338
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx444",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.24504
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx444",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.2459
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx444",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.2502
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx445",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.25131
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx445",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.25329
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx446",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.2544
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx446",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.25979
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx446",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.26095
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx445",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.26545
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194323.26591
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194323.26689
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r1$rateThreshold",
  //   "value": " int 34",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.46397
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.46455
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx444",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.46513
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194323.4665
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx444",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.46755
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx444",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.46813
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx444",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.46865
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.46913
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx447",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.47031
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx447",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.47189
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx447",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.47274
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx447",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194323.47727
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194323.47769
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194323.47861
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r1$rateThreshold",
  //   "value": " num 39.3",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.01144
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.01206
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx447",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.01266
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194324.01389
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx447",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.01495
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx447",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.01556
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx447",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.01619
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.01673
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx448",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.01804
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx448",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.02012
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx448",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.0215
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx448",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.02652
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194324.02697
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194324.0279
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx436",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16154
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx435",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16218
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx437",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16278
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194324.16341
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx437",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16445
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx437",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16494
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx439",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16539
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx439",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16594
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx439",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16643
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx434",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16689
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx433",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16735
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx433",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16847
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx433",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16895
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx434",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16938
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx434",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.16984
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx435",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.1703
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx435",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.17075
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx436",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.1711
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx449",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.17201
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx449",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.17261
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx450",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.17358
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx450",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.1745
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx451",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.17583
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx451",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.17729
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx450",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.17995
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx452",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.18084
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx452",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.18164
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t1186 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.19368
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx452",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.1944
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx442",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.19516
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx441",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.19584
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx441",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.19729
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx441",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.198
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx443",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.19863
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx443",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.19998
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx443",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20065
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx448",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20144
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx448",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20277
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx448",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20347
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx448",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20413
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx442",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20479
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx442",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20541
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx452",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20601
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx452",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20663
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx452",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20724
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx452",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20782
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx452",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20846
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx449",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20894
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx453",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.20987
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx453",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.21045
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx454",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.2112
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx454",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.21184
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 1186",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.213
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx454",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.21361
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx446",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.21429
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx445",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.21496
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx445",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.21635
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx445",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.21702
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx446",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.21764
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx446",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.21826
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx454",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.21887
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx454",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.2195
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx454",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.22018
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx454",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.2208
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx454",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.22138
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx453",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.2219
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx455",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.22286
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx455",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.22478
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx456",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.22589
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx456",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.22685
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx456",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.23039
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx455",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.23444
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx457",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.23555
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx457",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.23763
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx458",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.23876
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx458",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.23973
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx458",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.24056
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx457",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.25227
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx459",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.25327
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx459",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.26415
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx459",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.27534
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx460",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.27675
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx460",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.27873
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx460",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.27966
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx460",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.28425
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx461",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.28538
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx461",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.28732
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx462",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.28839
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx462",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.28934
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx462",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.29016
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx461",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194324.29449
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194324.29491
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194324.29578
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx451",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.184
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx450",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.18463
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx453",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.18523
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194325.18591
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx453",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.18696
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx453",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.18748
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx456",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.18796
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx455",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.18852
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx455",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.18982
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx455",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19033
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx456",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19077
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx456",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19123
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx449",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19177
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx449",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19239
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx449",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.1929
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx450",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19331
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx450",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19375
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx451",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19411
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx463",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19503
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx463",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19566
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx464",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19659
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx464",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19732
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx465",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19833
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx465",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.19982
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx464",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.20271
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx466",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.2038
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx466",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.2047
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 1219",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.20614
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx466",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.20689
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx462",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.20763
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx461",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.20837
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx461",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.21018
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx461",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.21095
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx462",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.2116
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx462",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.21224
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx466",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.21288
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx466",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.21354
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx466",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.21421
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx466",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.21484
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx466",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.21539
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx463",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.21587
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx467",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.21699
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx467",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.21884
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx468",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.2199
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx468",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.22083
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx468",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.2257
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx467",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.22963
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx469",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.23061
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx469",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.23119
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx470",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.23195
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx470",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.23277
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t1219 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.24523
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx470",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.24592
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx458",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.2466
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx457",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.24728
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx457",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.2487
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx457",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.24939
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx459",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25003
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx459",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25131
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx459",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25199
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx460",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25262
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx460",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25391
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx460",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25461
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx460",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25527
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx458",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25588
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx458",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25651
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx470",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25711
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx470",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25773
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx470",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25835
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx470",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25893
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx470",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25947
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx469",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.25995
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx471",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.26089
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx471",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.26278
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx472",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.26386
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx472",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.26481
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx472",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.26561
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx471",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.26939
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx473",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.2704
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx473",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.27231
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx474",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.27332
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx474",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.27425
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx474",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.27503
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx473",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.28649
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx475",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.28751
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx475",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.28916
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx475",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.29845
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx476",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.30404
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx476",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.30656
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx476",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.30751
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx476",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.31312
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194325.31368
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194325.31484
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r1$rateThreshold",
  //   "value": " num 35.1",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.85181
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.85253
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx476",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.85344
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194325.85533
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx476",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.8568
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx476",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.85773
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx476",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.85847
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.85915
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx477",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.86053
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx477",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.86238
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx477",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.86341
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx477",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194325.86831
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194325.86901
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194325.87054
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx465",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.20243
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx464",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.20305
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx463",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.20366
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194326.20427
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx463",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.20533
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx463",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.20585
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx468",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.20634
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx467",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.20685
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx467",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.20799
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx467",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.20844
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx468",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.20884
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx468",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.20926
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx469",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.20973
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx469",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.21039
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx469",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.21084
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx464",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.21123
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx464",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.21164
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx465",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.21199
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx478",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.21288
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx478",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.21348
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx479",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.21442
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx479",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.21538
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx480",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.21655
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx480",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.21838
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx479",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.22144
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx481",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.2225
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx481",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.22323
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 1255",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.22452
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx481",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.22519
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx472",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.22589
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx471",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.22658
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx471",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.22815
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx471",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.22896
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx472",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.22966
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx472",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.23036
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx481",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.23105
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx481",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.23173
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx481",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.23241
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx481",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.23308
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx481",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.23371
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx478",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.23427
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx482",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.23526
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx482",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.2373
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx483",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.23848
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx483",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.23945
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx483",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.24294
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx482",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.24694
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx484",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.24796
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx484",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.24856
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx485",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.24933
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx485",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.25012
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t1255 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.26329
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx485",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.26401
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx474",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.2647
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx473",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.26537
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx473",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.26674
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx473",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.26744
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx475",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.26808
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx475",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.26941
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx475",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.2701
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx477",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.27074
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx477",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.27203
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx477",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.27272
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx477",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.27337
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx474",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.27398
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx474",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.2746
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx485",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.2752
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx485",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.27581
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx485",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.27642
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx485",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.27701
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx485",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.27763
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx484",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.2781
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx486",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.27901
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx486",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.2808
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx487",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.28185
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx487",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.28279
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx487",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.28362
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx486",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.28782
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx488",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.28881
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx488",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.29071
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx489",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.29173
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx489",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.29268
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx489",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.29354
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx488",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.30739
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx490",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.30839
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx490",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.30995
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx490",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.31936
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx491",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.32059
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx491",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.32269
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx491",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.32363
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx491",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194326.33229
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194326.33276
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194326.33377
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx480",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.2232
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx479",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.22369
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx478",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.22414
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194327.22465
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx478",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.22567
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx478",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.22613
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx483",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.22656
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx482",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.22712
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx482",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.22831
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx482",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.22884
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx483",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.2293
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx483",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.22977
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx484",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.23027
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx484",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.23078
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx484",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.23123
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx479",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.23165
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx479",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.23207
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx480",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.23244
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx492",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.23335
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx492",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.23394
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx493",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.23488
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx493",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.2357
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx494",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.23692
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx494",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.23853
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx493",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.24142
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx495",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.24231
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx495",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.24302
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 1293",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.24424
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx495",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.24492
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx487",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.24564
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx486",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.24634
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx486",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.24776
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx486",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.24848
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx487",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.24921
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx487",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.24987
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx495",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.25051
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx495",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.25112
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx495",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.25182
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx495",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.25245
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx495",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.25299
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx492",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.25347
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx496",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.2545
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx496",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.25636
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx497",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.25744
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx497",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.25839
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx497",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.26218
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx496",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.26616
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx498",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.26732
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx498",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.26795
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx499",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.26873
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx499",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.26952
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t1293 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.28171
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx499",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.28241
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx489",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.28311
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx490",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.28377
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx490",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.28512
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx490",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.2858
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx491",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.28646
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx491",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.28789
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx491",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.28862
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx491",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.28927
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx488",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.28991
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx488",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.29119
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx488",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.29186
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx489",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.29249
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx489",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.29312
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx499",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.29373
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx499",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.29434
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx499",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.29496
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx499",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.29554
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx499",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.29606
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx498",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.29652
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx500",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.29743
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx500",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.29943
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx501",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.30047
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx501",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.30141
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx501",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.30221
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx500",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.30593
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx502",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.30687
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx502",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.30835
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx503",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.30931
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx503",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.31027
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx503",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.31111
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx502",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.32157
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx504",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.32278
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx504",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.32469
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx504",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.32564
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx504",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.32993
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx505",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.33109
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx505",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.3336
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx505",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.34606
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194327.34656
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194327.34749
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r1$rateThreshold",
  //   "value": " num 35.9",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.51976
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.52039
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx504",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.52102
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194327.52266
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx504",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.5239
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx504",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.52458
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx504",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.52513
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.52562
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx506",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.52683
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx506",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.52848
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx506",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.52967
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx506",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.53862
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194327.5393
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194327.5404
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r1$rateThreshold",
  //   "value": " num 36.8",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.94092
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.94166
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx506",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.94235
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194327.9439
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx506",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.94512
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx506",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.9458
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx506",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.94659
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.94715
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx507",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.94844
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx507",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.95009
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx507",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.95097
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx507",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194327.95532
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194327.95584
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194327.95705
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx494",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24037
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx493",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24102
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx492",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24162
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194328.24217
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx492",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24326
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx492",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24377
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx497",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24429
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx496",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24481
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx496",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24613
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx496",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24674
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx497",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24721
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx497",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24769
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx498",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24828
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx498",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24882
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx498",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24934
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx493",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.24975
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx493",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.25018
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx494",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.25054
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx508",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.25149
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx508",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.25218
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx509",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.25317
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx509",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.25393
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx510",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.25489
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx510",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.25652
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx509",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.25942
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx511",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.2603
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx511",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.26098
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 1323",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.26237
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx511",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.26305
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx501",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.26378
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx500",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.2645
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx500",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.26602
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx500",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.26676
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx501",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.26742
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx501",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.26807
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx511",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.26875
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx511",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.26945
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx511",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.27011
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx511",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.27072
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx511",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.27129
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx508",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.27177
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx512",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.27274
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx512",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.27464
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx513",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.27574
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx513",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.27678
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx513",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.27998
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx512",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.28391
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx514",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.28491
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx514",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.28549
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx515",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.28628
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx515",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.28708
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t1323 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.29977
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx515",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.3005
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx503",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.30123
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx502",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.30195
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx502",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.30348
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx502",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.30424
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx505",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.30494
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx505",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.30641
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx505",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.30716
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx507",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.30784
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx507",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.30921
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx507",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.30993
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx507",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.31061
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx503",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.31126
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx503",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.31203
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx515",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.31267
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx515",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.31329
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx515",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.31391
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx515",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.31449
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx515",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.31502
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx514",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.31548
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx516",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.31639
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx516",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.31821
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx517",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.31925
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx517",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.3202
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx517",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.32104
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx516",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.32498
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx518",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.32591
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx518",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.32735
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx519",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.32831
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx519",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.32917
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx519",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.32999
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx518",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.34036
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx520",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.34155
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx520",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.34372
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx520",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.35652
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx521",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.35763
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx521",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.35948
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx521",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.36035
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx521",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.36464
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194328.36513
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194328.36618
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r1$rateThreshold",
  //   "value": " num 37.3",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.37581
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.37643
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx521",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.37707
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194328.37858
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx521",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.37975
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx521",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.3804
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx521",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.38098
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r1$rateThreshold",
  //   "ctxId": "ctx",
  //   "type": "other",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.38159
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx522",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.38296
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx522",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.38478
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx522",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.38573
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx522",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194328.38996
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194328.39046
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194328.39142
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx510",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.25963
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx509",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26026
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx508",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26088
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194329.26152
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx508",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26258
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx508",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26312
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx513",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26361
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx512",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26412
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx512",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26533
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx512",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26583
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx513",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26637
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx513",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26688
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx514",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26744
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx514",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26802
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx514",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26851
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx509",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26892
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx509",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26936
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx510",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.26972
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx523",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.27066
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx523",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.27151
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx524",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.27262
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx524",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.27345
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx525",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.27455
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx525",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.276
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx524",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.27871
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx526",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.27984
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx526",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.28064
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 1360",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.28195
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx526",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.28262
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx517",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.28332
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx516",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.28401
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx516",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.28541
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx516",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.28612
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx517",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.28679
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx517",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.28745
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx526",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.28811
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx526",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.28876
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx526",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.28941
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx526",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.29003
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx526",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.29065
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx523",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.29116
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx527",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.29211
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx527",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.29399
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx528",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.29507
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx528",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.29601
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx528",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.30059
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx527",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.30477
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx529",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.30576
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx529",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.30635
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx530",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.30715
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx530",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.30796
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t1360 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.32025
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx530",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.32098
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx519",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.32177
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx520",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.32246
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx520",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.32388
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx520",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.32466
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx522",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.32532
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx522",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.32667
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx522",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.32738
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx522",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.32804
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx518",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.32869
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx518",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.32997
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx518",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.33066
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx519",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.33128
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx519",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.33191
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx530",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.33251
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx530",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.33313
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx530",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.33375
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx530",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.33434
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx530",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.33486
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx529",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.33535
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx531",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.33629
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx531",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.33809
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx532",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.33912
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx532",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.3401
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx532",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.34093
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx531",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.34464
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx533",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.34571
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx533",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.34803
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx534",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.34916
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx534",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.35056
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx534",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.35144
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx533",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.36489
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx535",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.36596
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx535",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.36797
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx535",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.36891
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx535",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.37307
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx536",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.37425
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx536",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.37605
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx536",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194329.38513
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194329.38557
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194329.38647
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx525",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.27756
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx524",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.27809
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx523",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.27859
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194330.27917
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx523",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.2804
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx523",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28104
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx528",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28171
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx527",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28224
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx527",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28365
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx527",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.2842
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx528",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28465
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx528",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28511
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx529",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28555
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx529",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28607
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx529",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28654
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx524",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28694
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx524",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28738
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx525",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28784
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx537",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28898
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx537",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.28979
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx538",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.29073
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx538",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.29148
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx539",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.29269
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx539",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.29429
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx538",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.29698
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx540",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.29785
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx540",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.29865
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 1390",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.29991
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx540",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.30058
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx532",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.30142
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx531",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.30214
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx531",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.30361
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx531",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.30435
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx532",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.30501
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx532",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.3057
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx540",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.30635
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx540",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.30704
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx540",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.30775
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx540",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.30846
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx540",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.30909
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx537",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.30964
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx541",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.3106
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx541",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.31248
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx542",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.31355
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx542",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.31471
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx542",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.31886
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx541",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.32304
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx543",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.3241
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx543",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.32469
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx544",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.32548
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx544",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.32627
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t1390 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.33854
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx544",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.33926
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx534",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.33995
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx533",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.34063
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx533",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.34222
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx533",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.34299
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx535",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.34365
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx535",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.34501
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx535",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.34571
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx535",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.34639
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx536",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.34703
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx536",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.34847
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx536",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.34917
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx534",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.3498
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx534",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.35044
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx544",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.35106
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx544",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.35169
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx544",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.35231
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx544",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.35291
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx544",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.35344
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx543",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.35391
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx545",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.35485
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx545",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.35667
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx546",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.35772
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx546",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.35867
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx546",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.35957
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx545",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.36377
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx547",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.3657
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx547",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.36796
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx548",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.36924
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx548",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.37044
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx548",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.37136
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx547",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.38487
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx549",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.38588
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx549",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.38762
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx549",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.38856
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx549",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.39303
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx550",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.39413
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx550",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.39591
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx550",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194330.40584
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194330.40628
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194330.40713
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r3",
  //   "ctxId": "ctx539",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.29986
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r4",
  //   "ctxId": "ctx538",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.30055
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r11",
  //   "ctxId": "ctx542",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.30124
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx541",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.30182
  // },
  // {
  //   "action": "asyncStart",
  //   "session": null,
  //   "time": 1525194331.30316
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx541",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.3044
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx541",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.30506
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx542",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.30563
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r11",
  //   "ctxId": "ctx542",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.3062
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx543",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.3067
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx543",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.30724
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx543",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.30774
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx537",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.30823
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx537",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.30877
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx537",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.30927
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx538",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.30971
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r4",
  //   "ctxId": "ctx538",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.31019
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r3",
  //   "ctxId": "ctx539",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.3106
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r14",
  //   "ctxId": "ctx551",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.31162
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx551",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.3177
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r11",
  //   "ctxId": "ctx552",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.31907
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r11",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx552",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.3201
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r4",
  //   "ctxId": "ctx553",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.32147
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r4",
  //   "depOnReactId": "r3",
  //   "ctxId": "ctx553",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.32261
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r3",
  //   "ctxId": "ctx554",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.32415
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r3",
  //   "ctxId": "ctx554",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.32619
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r4",
  //   "ctxId": "ctx553",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.32986
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r11",
  //   "ctxId": "ctx552",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.33409
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r14",
  //   "ctxId": "ctx551",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.33817
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r6",
  //   "ctxId": "ctx555",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.3392
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx555",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.33982
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r6",
  //   "ctxId": "ctx556",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.34061
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx556",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.34149
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r5$acc",
  //   "value": "'data.frame':\t1407 obs. of  11 variables:\n $ date     : chr  \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" \"2018-04-03\" ...\n $ time     : chr  \"17:04:53\" \"17:04:53\" \"17:04:53\" \"17:04:53\" ...\n $ size     : int  1001405 1010663 10738 118861 128969 134293 13482 135313 140743 14295775 ...\n $ r_version: chr  \"3.4.4\" \"3.4.4\" \"3.6.0\" NA ...\n $ r_arch   : chr  \"x86_64\" \"x86_64\" \"x86_64\" NA ...\n $ r_os     : chr  \"mingw32\" \"mingw32\" \"darwin17.5.0\" NA ...\n $ package  : chr  \"gmp\" \"debugme\" \"bindrcpp\" \"stringr\" ...\n $ version  : chr  \"0.5-13.1\" \"1.1.0\" \"0.2.2\" \"1.3.0\" ...\n $ country  : chr  \"IN\" \"US\" \"NL\" \"CA\" ...\n $ ip_id    : int  13067 43807 2435 10172 2435 21006 43802 43807 427 43792 ...\n $ received : num  1.53e+09 1.53e+09 1.53e+09 1.53e+09 1.53e+09 ...",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.35373
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx556",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.35443
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r7",
  //   "ctxId": "ctx548",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.35511
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx547",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.35578
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx547",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.35721
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx547",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.35791
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx549",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.35855
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx549",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.35988
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx549",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36058
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx549",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36135
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx550",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36203
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx550",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36339
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx550",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36407
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx548",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36469
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r7",
  //   "ctxId": "ctx548",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36531
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx556",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36594
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx556",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36662
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx556",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36724
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r5$acc",
  //   "ctxId": "ctx556",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36784
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r6",
  //   "ctxId": "ctx556",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36835
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r6",
  //   "ctxId": "ctx555",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.36882
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r9",
  //   "ctxId": "ctx557",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.3698
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx557",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37037
  // },
  // {
  //   "action": "isolateEnter",
  //   "reactId": "r9",
  //   "ctxId": "ctx558",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37112
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx558",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37177
  // },
  // {
  //   "action": "valueChange",
  //   "reactId": "r8$acc",
  //   "value": " num 1407",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37292
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx558",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37353
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r10",
  //   "ctxId": "ctx546",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.3742
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx545",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37487
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx545",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37618
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx545",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37687
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx546",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37749
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r10",
  //   "ctxId": "ctx546",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37812
  // },
  // {
  //   "action": "isolateInvalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx558",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37873
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx558",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37934
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx558",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.37997
  // },
  // {
  //   "action": "isolateInvalidateEnd",
  //   "reactId": "r8$acc",
  //   "ctxId": "ctx558",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.38058
  // },
  // {
  //   "action": "isolateExit",
  //   "reactId": "r9",
  //   "ctxId": "ctx558",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.38114
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r9",
  //   "ctxId": "ctx557",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.38164
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r16",
  //   "ctxId": "ctx559",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.38256
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx559",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.38444
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r7",
  //   "ctxId": "ctx560",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.38552
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r7",
  //   "depOnReactId": "r5$acc",
  //   "ctxId": "ctx560",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.38656
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r7",
  //   "ctxId": "ctx560",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.38746
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r16",
  //   "ctxId": "ctx559",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.39982
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r12",
  //   "ctxId": "ctx561",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.40081
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx561",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.40269
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx561",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.40367
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r12",
  //   "ctxId": "ctx561",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.40793
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r15",
  //   "ctxId": "ctx562",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.4091
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx562",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.41072
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r15",
  //   "ctxId": "ctx562",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.41975
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r13",
  //   "ctxId": "ctx563",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.42081
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx563",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.42281
  // },
  // {
  //   "action": "enter",
  //   "reactId": "r10",
  //   "ctxId": "ctx564",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.42391
  // },
  // {
  //   "action": "dependsOn",
  //   "reactId": "r10",
  //   "depOnReactId": "r8$acc",
  //   "ctxId": "ctx564",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.42486
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r10",
  //   "ctxId": "ctx564",
  //   "type": "observable",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.42566
  // },
  // {
  //   "action": "exit",
  //   "reactId": "r13",
  //   "ctxId": "ctx563",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194331.42953
  // },
  // {
  //   "action": "asyncStop",
  //   "session": null,
  //   "time": 1525194331.42997
  // },
  // {
  //   "action": "queueEmpty",
  //   "session": null,
  //   "time": 1525194331.4308
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r6",
  //   "ctxId": "ctx555",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24429
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r6",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx555",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24493
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r6",
  //   "ctxId": "ctx555",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24537
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r9",
  //   "ctxId": "ctx557",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24585
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r9",
  //   "depOnReactId": "r4",
  //   "ctxId": "ctx557",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24621
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r9",
  //   "ctxId": "ctx557",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24657
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r12",
  //   "ctxId": "ctx561",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24701
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx561",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24739
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r12",
  //   "depOnReactId": "r1$rateThreshold",
  //   "ctxId": "ctx561",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24786
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r12",
  //   "ctxId": "ctx561",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24827
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r13",
  //   "ctxId": "ctx563",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24873
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r13",
  //   "depOnReactId": "r10",
  //   "ctxId": "ctx563",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24912
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r13",
  //   "ctxId": "ctx563",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.24967
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r14",
  //   "ctxId": "ctx551",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.25016
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r14",
  //   "depOnReactId": "r11",
  //   "ctxId": "ctx551",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.25055
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r14",
  //   "ctxId": "ctx551",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.2509
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r15",
  //   "ctxId": "ctx562",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.25133
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r15",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx562",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.25167
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r15",
  //   "ctxId": "ctx562",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.25201
  // },
  // {
  //   "action": "invalidateStart",
  //   "reactId": "r16",
  //   "ctxId": "ctx559",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.25257
  // },
  // {
  //   "action": "dependsOnRemove",
  //   "reactId": "r16",
  //   "depOnReactId": "r7",
  //   "ctxId": "ctx559",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.25296
  // },
  // {
  //   "action": "invalidateEnd",
  //   "reactId": "r16",
  //   "ctxId": "ctx559",
  //   "type": "observer",
  //   "session": "a41fd95ac69dd22631d44fda82b320f3",
  //   "time": 1525194332.25331
  // }
]
