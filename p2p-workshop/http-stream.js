const fs = require('fs');
const http = require('http');
const pump = require('pump');


const server = http.createServer(function (_, res) {
  const file = fs.createReadStream(process.argv[2]);

  pump(file, res, function () {
    console.log(`Finished streaming: ${process.argv[2]}`);
  });
});

server.listen(5000);
