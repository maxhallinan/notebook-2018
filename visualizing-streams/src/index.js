import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
// import App from './App';
import registerServiceWorker from './registerServiceWorker';
import * as Rx from 'rxjs';
import { map, scan } from 'rxjs/operators';
import dagre from 'dagre';

// const tick$ = RxJs.timer(0, 1000);
// console.log(tick$);
// console.log(tick$.constructor.name);
// console.log(tick$._subscribe.toString());
// const mappedTick$ = tick$.pipe(map(x => x));
// console.log(mappedTick$);
// console.log(mappedTick$.operator.constructor.name);
// const scannedTick$ = mappedTick$.pipe(scan(x => x, 0));
// console.log(scannedTick$);
// console.log(scannedTick$.operator.constructor.name);
// const mergedTick$ = RxJs.combineLatest(tick$, scannedTick$);
// console.log(mergedTick$);
// console.log('foo')

/*
- do i have enough information to dynamically build a graph of streams?
- i have the names of operators at `observable.operator.constructor.name`
- i don't seem to be able to get the names of the Observable constructors
  like RxJs.timer
- how does the cycle devtools do this?
- cycle devtools only supports streams from the xstream library:
  https://github.com/cyclejs/cyclejs/blob/master/devtool/src/graphSerializer.ts#L329
- for each "sink", e.g. output stream, it creates a graph
- then it traverses that stream
- it traverses the stream by looking at `stream._prod`
  - there seem to be two main values: `stream._prod['ins']` and 
    `stream._prod['insArr']`
  - the old name for `stream._prod` was `stream._producer` but I'm not sure what 
    a producer is
  - there's three conditions
    - `ins`, 
    - `insArr`
    - default condition 
  - steps taken for three conditions
    - `ins`
      - create a graph node if there is not already a graph node for that producer
      - 
  - for all conditions, it sets a node on the graph if that node has not already
    been created
  - ok, i think that these `ins` and `insArr` are the source streams
  - the default condition is for the root stream
  - the code recursively traverses up the source streams after setting the 
    graph edge
  - i'm not sure i have enough information to do that
  - ah, so it doesn't label the intermediate steps
  - i'd need a way to figure out the label for each of my steps
  - could also create a static graph and then just visualize that
- my requirements
  - each node in the graph should be vertically aligned with the code example
    that describes its implementation
  - there should be lines between the graph
  - would be nice to draw the lines with an easing function
  - i basically need to start by creating the graph data structure
  - then i need to find the x/y coordinates of the nodes
  - then i need to draw the lines
  - after that, i need to make the thing "live"
  - show the data updating
  - i want to show the variable name for each stream
  - i want to show the operators between the streams
  - i want to show the data flowing through the streams
*/

// I can just hardcode the position of the nodes
// and I'd also have to pass the observable itself since i need to update the 
// flow as it changes
// so if i know where each of these are on the page, then i can draw their 
// the edges should be labeled too
// the edges should be labeled as code blocks
// show a list of data through the edge
// in terms of positioning, i don't know how to set the horizontal spacing
// the vertical spacing can be taken care of by the vertical offset
// then i could still use dagre for horizontal spacing?
// how would that work?
// i would know the width of the canvas
// i would know that the node width should be x% of that width
// then dagre would give me horizontal alignment when two or more nodes are at the same level in the hierarchy
// then i can change the vertical position to be whatever the offset is
// then i can draw that on the screen
// the operators are the node labels

/*
what are the tasks
1. prototype the directed graph
  - labeled nodes
  - labeled edges
  - prototype this with simple data
  - nice curving lines between the nodes
  - is there a way to make the lines go around the nodes?
  - maybe something like show a smaller edge label when you hover on the line?
2. prototype the server simulation
  - click to start connections
  - click to close connection
  - show how this effects the data at each point in the stream
3. somehow combine this with the graph visualization

to render the graph
- set height/width of each node
- find the position of each node on the canvas
- draw lines between the center of these positions
- draw the nodes at those positions
*/

/*
- see the behavior of the system
- see the entire state of the system across all the variables all at once
to be able to make comparisons, recognize things
- we need to be able to adjust the system, see how the behavior responds
- make associations between what we're changing and how the behavior responds
- we need multiple representations of the system, looking at the behavior in 
  different ways through different lenses
- we need to not just see the behavior but interact with it, measuring it, 
  searching it
- code doesn't matter - it's what the code is doing
*/
function createNode(node, graph) {
  graph.setNode(
    node.id,
    {
      height: 40,
      label: node.label,
      width: 60,
    }
  );

  return graph;
}

function createEdge(edge, graph) {
  graph.setEdge(
    edge.source,
    edge.target,
    {
      label: edge.label,
    }
  );

  return graph;
}

const nodes = [
  { 
    id: "connection$",
    label: "connection$",
  }, {
    id: "connectionCount$",
    label: "connectionCount$",
  }, {
    id: "socket$",
    label: "socket$",
  }, {
    id: "close$",
    label: "close$",
  }, {
    id: "closeCount$",
    label: "closeCount$",
  }, {
    id: "combinedCount$",
    label: "combinedCount$",
  }, {
    id: "currentCount$",
    label: "currentCount$",
  }, {
    id: "pause$",
    label: "pause$",
  }, {
    id: "tick$",
    label: "tick$",
  },
];

const edges = [
  {
    label: "scan(addOne, 0)",
    source: "connection$",
    target: "connectionCount$",
  }, {
    label: "map(head)",
    source: "connection$",
    target: "socket$",
  }, {
    label: "flatMap(toCloses)",
    source: "socket$",
    target: "close$",
  }, {
    label: "scan(addOne, 0)",
    source: "close$",
    target: "closeCount$",
  }, {
    label: "combineLatest",
    source: "connectionCount$",
    target: "combinedCount$"
  }, {
    label: "combineLatest",
    source: "closeCount$",
    target: "combinedCount$"
  }, {
    label: "map(subtract)",
    source: "combinedCount$",
    target: "currentCount$",
  }, {
    label: "map(isPaused)",
    source: "currentCount$",
    target: "pause$",
  }, {
    label: "switchMap(toTicks)",
    source: "pause$",
    target: "tick$",
  },
];

const graph = new dagre.graphlib.Graph();
graph.setGraph({ rankdir: 'LR', });
nodes.forEach((node) => createNode(node, graph));
edges.forEach((edge) => createEdge(edge, graph));
const graphLayout = dagre.layout(graph);
// graph.nodes().forEach((n) => console.log({ n: graph.node(n), }));
// graph.edges().forEach((e) => console.log({ e: graph.edge(e), }));

const PADDING = 20;

function Node(props, key) {
  const { node, } = props;
  const x = node.x - (node.width / 2) + PADDING;
  const y = node.y - (node.height / 2) + PADDING;

  return (
    <g>
      <rect 
        fill="transparent"
        height={node.height}
        key={key}
        stroke="black"
        strokeWidth="1.25px"
        width={node.width}
        x={x}
        y={y}
      />
      <text fontSize="10" x={x} y={y - 5}>{node.label}</text>
    </g>
  );
}

function Edge(props, key) {
  const { edge, } = props;
  const { points, } = edge;
  const [ point1, ...restPoints ] = points;
  const pathStart = `M${point1.x + PADDING} ${point1.y + PADDING}`;
  const pathRest = restPoints.map((point) => `L${point.x + PADDING} ${point.y + PADDING}`).join(``);
  const d = `${pathStart}${pathRest}`;
  return (
    <path 
      key={key}
      strokeWidth="1.25px" 
      d={d}
      stroke="black" 
      fill="transparent" 
    />
  );
}

function App(props) {
  const { graph, } = props;
  const edges = graph.edges().map((edge) => graph.edge(edge));
  const nodes = graph.nodes().map((node) => graph.node(node));

  return (
    <div style={{ width: "100%", maxWidth: "53.291em", padding: "20px"}}>
      <div>
        <label>Opened connections:</label>
        <input type="number" />
      </div>
      <div>
        <label>Closed connections:</label>
        <input type="number" />
      </div>
      <svg width="100%" height="500px">
        {edges.map((edge) => <Edge edge={edge} />)}
        {nodes.map((node) => <Node height={100} width={100} node={node} />)}
      </svg>
    </div>
  );
}

ReactDOM.render(<App graph={graph} />, document.getElementById('root'));

// click connect
// click disconnect
// need to vizualise the stream
// could rewrite the whole thing as 
