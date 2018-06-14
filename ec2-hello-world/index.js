// const express = require(`express`);
const WebSocket = require(`ws`);

// const app = express();

// app.get(`/`, (req, res) => {
//   res.send(`hello world!`);
// });

// app.listen(3000, () => {
//   console.log(`server running on port 3000`);
// });
const server = new WebSocket.Server({ 
  host: '0.0.0.0',
  port: 3000, 
});
server.on(`listening`, () => {
  console.log(`server listening on port 3000`);
});
server.on(`connection`, (websocket) => {
  websocket.send(`hello world`);
});
