const topology = require('fully-connected-topology');

const ownHost = process.argv[2];
const peers = process.argv.slice(3);

const t = topology(ownHost, peers);

t.on('connection', function (connection, peer) {
  console.log(`${ownHost} is connected to ${peer}`);
});
