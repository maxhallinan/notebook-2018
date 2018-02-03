// Chat server that receives a nickname and a message
// and relays the nickname/message as a formatted chat log to the other
// connected clients.
const net = require('net');
const jsonStream = require('duplex-json-stream');
const streamSet = require('stream-set');

const port = 5000;

const activeSockets = streamSet();

const server = net.createServer(function (socket) {
  const jsonSocket = jsonStream(socket);

  activeSockets.add(jsonSocket);

  jsonSocket.on('data', function ({ nickname, msg, }) {
    activeSockets.forEach(function (s) {
      if (s !== jsonSocket) {
        s.write(`<${nickname}> ${msg}`);
      }
    });
  });
});

server.listen(5000, function () {
  console.log(`Welcome to Weird Chat:`);
});
