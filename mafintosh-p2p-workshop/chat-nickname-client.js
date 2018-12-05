// Client for ./chat-nickname-server.js.
// Start client by passing a nickname as the first argument.
const net = require('net');
const jsonStream = require('duplex-json-stream');

const nickname = process.argv[2];

const connection = net.createConnection({ port: 5000, });
const client = jsonStream(connection);

process.stdin.on('data', function (data) {
  client.write({ nickname, msg: data.toString(), });
});

client.pipe(process.stdout);
