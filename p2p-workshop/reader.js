// takes a file path as the first argument
// streams file at path to stdout
const fs = require('fs');

fs.createReadStream(process.argv[2]).pipe(process.stdout);
