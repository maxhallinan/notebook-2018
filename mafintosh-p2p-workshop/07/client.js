const jsonStream = require('duplex-json-stream');
const streamSet = require('stream-set');
const topology = require('fully-connected-topology');

const nickname = process.argv[2];
const me = process.argv.slice(3);
const peers = process.argv.slice(4);

const swarm = topology(me, peers);
const connections = streamSet();

swarm.on('connection', function (socket, id) {
  console.log(`new connection from id ${id}`);
 
  socket = jsonStream(c);
  socket.on('data', function ({ username, msg, }) {
    console.log(`${username}> ${msg}`);
  });
});

process.stdin.on('data', function (data) {
  connections.forEach(function (socket) {
    const msg = data.toString().trim();
    socket.write({ nickname, msg, });
  });
});
