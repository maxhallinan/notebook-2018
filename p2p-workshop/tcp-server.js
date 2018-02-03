// tcp echo server
const net = require('net');

const server = net.createServer(function (socket) {
  socket.pipe(socket);
});

server.listen(5000, function () {
  console.log(`Listening on port 5000...`);
});
