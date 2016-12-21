const ServerEntry = require('../src/Server.purs');
const enableDestroy = require('server-destroy');

const server = ServerEntry.main();

enableDestroy(server)

if (module.hot) {
  module.hot.accept();

  module.hot.dispose(function () {
    server.destroy();
  });
}
