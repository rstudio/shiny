
## Questions
* √ Can an 'isolate' call ever cause any shiny dependency calculations other than read?
  * yes. yes it does
  * Isolate interactions should create dashed lines from the node causing the reading to happen to the node being read. It should not point to an `isolate` node.
  * On 'invalidate', these connections connections are dropped as well
* √ If `plotObj` connects to `output$plot`, why is the `dependsOn` after `plotObj` has calculated? Why not before it starts running?
* √ How big is a "massive" shiny dependency tree? (how many dependencies deep)
  * 20 layers deep
* √ How can a "invalidate" happen after an "exit" with the same context id?
```
  {
    "action": "exit",
    "id": "13",
    "session": "6026f69d772a5088a6bea5a390d05033",
    "time": 1522262332.70226
  },
  {
    "action": "invalidate",
    "id": "13",
    "session": "6026f69d772a5088a6bea5a390d05033",
    "time": 1522262332.70229
  },
```
  * Happens for an isolate computation. The isolate situation should be handled differently

* How can I capture that an observe has `set` a value. such as
```
observeEvent(input$plus, {
  val = value()
  value(val + 1)
})
```


## New Features
* Click to next cycle (no more running states)
* Click to previous cycle (no more running states)
* Able to Filter on family tree from selected node(s)
* Add a new entry of "Start Time" so a user could start in the middle of an playback
* Unhighlight a tree on cycle not involved in
* Set all major features with flags

## Labels
* Look at renaming id to something more consistent. It's being overloaded with two different types of "id"s
* "Isolate" calls do not provide any information when visually displayed. Would be better if the nodes were a different color (like yellow) when reading an 'isolate' value
* Should do a single, subtle pulse on value change
* Node should red on value change until running happens. Then turn half red (saying it was "it's fault")
* Current (most recently) running label needs to be displayed differently
  * ... to stand out that it is the one causing the actions to take place

## Positions
* Inputs should be to the left
* Outputs should be to the right
* Vertical changes should only help in distinguishing groups
* Run as a static force directed layout. Then solidify the positions.
  * Static Force Layout: https://bl.ocks.org/mbostock/1667139
  * With "thinking" progress bar (web workers)... https://bl.ocks.org/mbostock/01ab2e85e8727d6529d20391c0fd9a16
* Start with the graph displaying all known nodes with no connections
  * Helps know what the program even captured
* Determine separate groups within graph
* Pre analyze who connects to who and find a "fully connected" graph to find "optimal" positions
  * could be done with force directed graphs


## Super Groups
* Enter is a start computing, Exit is a stop computing.
  * Enter should contain super group id

### Super Group Rules
* Inputs and outputs all point to the super group when closed
* Inputs and outputs will point to the corresponding nodes when the super group is open
* Actions of a super group node (within the group) are skipped during run time, but the clock advances to recognize work was done
  * Could highlight node that 'work' is being done
* A super group may contain super group nodes (nesting)
* When a super group is opened, it's nodes must be displayed as a larger group.
  * Nodes should be layed out in similar routine to overall group within contianer
  * Could be done with slightly opaque background. The more layers, the thicker the background
    * Could be highlighted as a convex hull
* Super groups are closed by default, but could be flagged to be open


## Interactions
* Hover (node)- highlight all ancestors and descendants of the current branches
  * https://github.com/nylen/d3-process-map
* Hover (super group)- highlight group by adding border
* Click (node) - make highlight sticky
* Click (super group) - zoom into super group
* Click (zoomed super group) - zoom out of super group
  * Zoomable circle partitioning: https://bl.ocks.org/mbostock/7607535
* ?Click & Drag (node) - move node position
  * Click vs. Drag: https://bl.ocks.org/mbostock/a84aeb78fea81e1ad806
* ?Click & Drag (super group background) - move group position
* Double Click (super node) - open super group
* Double Click (super group background) - close super group (do not alter sub groups)


## Known Issues
* Feature request: for debugging, would be very useful to know which reactive caused invalidation: https://github.com/rstudio/shiny/issues/1846
* reactlog sometimes shows "time elapsed: NaN ms": https://github.com/rstudio/shiny/issues/1536
* Focusing on a single object in the react log: https://github.com/rstudio/shiny/issues/1532
* CLOSED - Improve graph drawing in reactive log visualizer: https://github.com/rstudio/shiny/issues/1279

## Known React Log Visualizations

* Filter... https://github.com/yonicd/shinyProf/tree/master/shiny

## Bl.ocks.org
* Dendrogram: https://bl.ocks.org/mbostock/ff91c1558bc570b08539547ccc90050b
* Force directed web worker: https://bl.ocks.org/mbostock/01ab2e85e8727d6529d20391c0fd9a16
* Convex hull: https://bl.ocks.org/mbostock/4341699
* Zoomable circle partitioning: https://bl.ocks.org/mbostock/7607535
* Click vs. Drag: https://bl.ocks.org/mbostock/a84aeb78fea81e1ad806
* Static Force Layout: https://bl.ocks.org/mbostock/1667139
