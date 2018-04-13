// Original: http://bl.ocks.org/dk8996/5449641
// trying to add zoom. currently broken. http://bl.ocks.org/stepheneb/1182434

try {
  log = __DATA__;
  time = String(__TIME__).toLowerCase() === 'true';
} catch (e) {}


var nodeNamesByNodeId = {}
var ganttData = [] // startDate, endDate, taskName, status
var enterExitMap = {}
var isolateMap = {}
var invalidMap = {}
var valueChangeMap = {}

var i, item, data;
for (i = 0; i < log.length; i++) {
  data = log[i]
  data.time = data.time * 1000
  switch(data.action) {

    case "nodeDef":
      nodeNamesByNodeId[data.nodeId] = data.label
      ganttData.push({
        startDate: new Date(data.time), endDate: new Date(data.time),
        nodeId: data.nodeId,
        status: "nodeDef",
        data: data
      })
      break;
    case "updateNodeLabel":
      nodeNamesByNodeId[data.nodeId] = data.label;
      break;
    case "valueChangeReactValueNames":
      ganttData.push({
        startDate: new Date(data.time), endDate: new Date(data.time),
        nodeId: data.nodeId,
        status: "valueChangeReactValueNames",
        data: data
      })
      break;
    case "valueChangeReactValueValues":
      // ganttData.push({
      //   startDate: new Date(data.time), endDate: new Date(data.time + 0.001),
      //   nodeId: data.nodeId,
      //   status: "valueChangeReactValueValues",
      //   data: data
      // })
      break;
    case "valueChangeReactValueKey":
      ganttData.push({
        startDate: new Date(data.time), endDate: new Date(data.time),
        nodeId: data.nodeId,
        status: "valueChangeReactValueKey",
        data: data
      })
      break;
    case "valueChangeStart":
      break;
      // valueChangeMap[data.nodeId] = data
      // break;
    case "valueChangeEnd":
      break;
      // var oldData = valueChangeMap[data.nodeId];
      // ganttData.push({
      //   startDate: new Date(oldData.time), endDate: new Date(data.time),
      //   nodeId: data.nodeId,
      //   status: "valueChange",
      //   data: data
      // })
      // break;
    case "invalidateStart":
      invalidMap[data.nodeId] = data
      break;
    case "invalidateEnd":
      var oldData = invalidMap[data.nodeId]
      ganttData.push({
        startDate: new Date(oldData.time), endDate: new Date(data.time),
        nodeId: data.nodeId,
        status: "invalidate",
        data: data
      })
      break;
    case "enter":
      if (data.type == "isolate") {
        isolateMap[data.nodeId] = data
      } else {
        enterExitMap[data.nodeId] = data
      }
      break;
    case "exit":
      var oldData, type
      if (data.type == "isolate") {
        type = "isolate"
        oldData = isolateMap[data.nodeId]
      } else {
        type = "work"
        oldData = enterExitMap[data.nodeId]
      }
      ganttData.push({
        startDate: new Date(oldData.time), endDate: new Date(data.time),
        nodeId: data.nodeId,
        status: type,
        data: data
      })
      break;
    case "depReactiveValueKey":
    case "dep":
    case "depOnRemove":
      break;

    default:
      console.error(data);
      throw data.action;
  }
}


var minTime = ganttData[0].time
var maxTime = ganttData[ganttData.length - 1].time
var ganttTaskMap = {}, ganttTasks = []
for (i = 0; i < ganttData.length; i++) {
  data = ganttData[i]
  switch(data.status) {
    case "valueChangeReactValueNames":
      data.taskName = "names(" + nodeNamesByNodeId[data.data.nodeId] + ")"
      break;
    // case "valueChangeReactValueValues":
    //   data.taskName = "names(" + nodeNamesByNodeId[data.data.nodeId] + ")"
    //   break;
    case "valueChangeReactValueKey":
      data.taskName = nodeNamesByNodeId[data.data.nodeId] + "$" + data.data.key
      break;
    default:
      data.taskName = nodeNamesByNodeId[data.data.nodeId]
      break;
  }
  ganttTaskMap[data.taskName] = true;
}
for (i in ganttTaskMap) ganttTasks.push(i)

// var tasks = [
// {"startDate":new Date("Sun Dec 09 01:36:45 EST 2012"),"endDate":new Date("Sun Dec 09 02:36:45 EST 2012"),"taskName":"E Job","status":"RUNNING"},
// {"startDate":new Date("Sun Dec 09 04:56:32 EST 2012"),"endDate":new Date("Sun Dec 09 06:35:47 EST 2012"),"taskName":"A Job","status":"RUNNING"},
// {"startDate":new Date("Sun Dec 09 06:29:53 EST 2012"),"endDate":new Date("Sun Dec 09 06:34:04 EST 2012"),"taskName":"D Job","status":"RUNNING"},
// {"startDate":new Date("Sun Dec 09 05:35:21 EST 2012"),"endDate":new Date("Sun Dec 09 06:21:22 EST 2012"),"taskName":"P Job","status":"RUNNING"},
// {"startDate":new Date("Sun Dec 09 05:00:06 EST 2012"),"endDate":new Date("Sun Dec 09 05:05:07 EST 2012"),"taskName":"D Job","status":"RUNNING"},
// {"startDate":new Date("Sun Dec 09 03:46:59 EST 2012"),"endDate":new Date("Sun Dec 09 04:54:19 EST 2012"),"taskName":"P Job","status":"RUNNING"},
// {"startDate":new Date("Sun Dec 09 04:02:45 EST 2012"),"endDate":new Date("Sun Dec 09 04:48:56 EST 2012"),"taskName":"N Job","status":"RUNNING"},
// {"startDate":new Date("Sun Dec 09 03:27:35 EST 2012"),"endDate":new Date("Sun Dec 09 03:58:43 EST 2012"),"taskName":"E Job","status":"SUCCEEDED"},
// {"startDate":new Date("Sun Dec 09 01:40:11 EST 2012"),"endDate":new Date("Sun Dec 09 03:26:35 EST 2012"),"taskName":"A Job","status":"SUCCEEDED"},
// {"startDate":new Date("Sun Dec 09 03:00:03 EST 2012"),"endDate":new Date("Sun Dec 09 03:09:51 EST 2012"),"taskName":"D Job","status":"SUCCEEDED"},
// {"startDate":new Date("Sun Dec 09 01:21:00 EST 2012"),"endDate":new Date("Sun Dec 09 02:51:42 EST 2012"),"taskName":"P Job","status":"SUCCEEDED"},
// {"startDate":new Date("Sun Dec 09 01:08:42 EST 2012"),"endDate":new Date("Sun Dec 09 01:33:42 EST 2012"),"taskName":"N Job","status":"FAILED"},
// {"startDate":new Date("Sun Dec 09 00:27:15 EST 2012"),"endDate":new Date("Sun Dec 09 00:54:56 EST 2012"),"taskName":"E Job","status":"SUCCEEDED"},
// {"startDate":new Date("Sun Dec 09 00:29:48 EST 2012"),"endDate":new Date("Sun Dec 09 00:44:50 EST 2012"),"taskName":"D Job","status":"SUCCEEDED"},
// {"startDate":new Date("Sun Dec 09 07:39:21 EST 2012"),"endDate":new Date("Sun Dec 09 07:43:22 EST 2012"),"taskName":"P Job","status":"RUNNING"},
// {"startDate":new Date("Sun Dec 09 07:00:06 EST 2012"),"endDate":new Date("Sun Dec 09 07:05:07 EST 2012"),"taskName":"D Job","status":"RUNNING"},
// {"startDate":new Date("Sun Dec 09 08:46:59 EST 2012"),"endDate":new Date("Sun Dec 09 09:54:19 EST 2012"),"taskName":"P Job","status":"RUNNING"},
// {"startDate":new Date("Sun Dec 09 09:02:45 EST 2012"),"endDate":new Date("Sun Dec 09 09:48:56 EST 2012"),"taskName":"N Job","status":"RUNNING"},
// {"startDate":new Date("Sun Dec 09 08:27:35 EST 2012"),"endDate":new Date("Sun Dec 09 08:58:43 EST 2012"),"taskName":"E Job","status":"SUCCEEDED"},
// {"startDate":new Date("Sun Dec 09 08:40:11 EST 2012"),"endDate":new Date("Sun Dec 09 08:46:35 EST 2012"),"taskName":"A Job","status":"SUCCEEDED"},
// {"startDate":new Date("Sun Dec 09 08:00:03 EST 2012"),"endDate":new Date("Sun Dec 09 08:09:51 EST 2012"),"taskName":"D Job","status":"SUCCEEDED"},
// {"startDate":new Date("Sun Dec 09 10:21:00 EST 2012"),"endDate":new Date("Sun Dec 09 10:51:42 EST 2012"),"taskName":"P Job","status":"SUCCEEDED"},
// {"startDate":new Date("Sun Dec 09 11:08:42 EST 2012"),"endDate":new Date("Sun Dec 09 11:33:42 EST 2012"),"taskName":"N Job","status":"FAILED"},
// {"startDate":new Date("Sun Dec 09 12:27:15 EST 2012"),"endDate":new Date("Sun Dec 09 12:54:56 EST 2012"),"taskName":"E Job","status":"SUCCEEDED"},
// {"startDate":new Date("Sat Dec 08 23:12:24 EST 2012"),"endDate":new Date("Sun Dec 09 00:26:13 EST 2012"),"taskName":"A Job","status":"KILLED"}];

var taskStatus = {
  "nodeDef": "bar-nodeDef",
  "valueChangeReactValueNames": "bar-valueChangeReactValueNames",
  "valueChangeReactValueValues": "bar-valueChangeReactValueValues",
  "valueChangeReactValueKey": "bar-valueChangeReactValueKey",
  "valueChange": "bar-valueChange",
  "invalidate": "bar-invalidate",
  "work": "bar",
  "isolate": "bar-isolate"
};

var taskNames = [ "D Job", "P Job", "E Job", "A Job", "N Job" ];

// tasks.sort(function(a, b) {
//     return a.endDate - b.endDate;
// });
// var maxDate = tasks[tasks.length - 1].endDate;
// tasks.sort(function(a, b) {
//     return a.startDate - b.startDate;
// });
// var minDate = tasks[0].startDate;

var format = "%H:%M:%S";

var newGanttData = [], time
for (i = 0; i < ganttData.length; i++) {
  time = ganttData[i].startDate.getSeconds()
  console.log(time)
  if (time >= 36 & time < 38) {
    newGanttData.push(ganttData[i])
  }
}
console.log(newGanttData)

var gantt = d3.gantt()
  .tasks(newGanttData)
  .taskTypes(ganttTasks)
  .taskStatus(taskStatus)
  .tickFormat(format);
var chart = gantt();

// chart.plot.call(d3.behavior.zoom().x(chart.x).y(chart.y).on("zoom", chart.redraw()))
