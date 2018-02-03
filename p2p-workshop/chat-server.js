// A primitive chat application over tcp
// Notifies each socket of an incoming message except for the socket writing 
// the message.
const net = require('net');
const streamSet = require('stream-set');

const port = 5000;

const activeSockets = streamSet();

const server = net.createServer(function (socket) {
  activeSockets.add(socket);
  const index = activeSockets.size;

  socket.on('data', function (data) {
    activeSockets.forEach(function (s) {
      if (s !== socket) {
        s.write(`<${index}> `);
        s.write(data);
      }
    });
  });
});

server.listen(port, function () {
  console.log(`Chat server listening on ${port}...`);
});
