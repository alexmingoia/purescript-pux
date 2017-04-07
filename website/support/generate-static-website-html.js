const scraper = require('website-scraper')
const spawn = require('child_process').spawn

const server = spawn('node', ['./dist/server.js'])

server.on('close', (code) => {
  process.exit(0);
})

const options = {
  urls: [
    '/',
    '/docs/architecture',
    '/docs/events',
    '/docs/markup',
    '/docs/rendering',
    '/docs/forms',
    '/docs/routing',
    '/docs/css',
    '/docs/react-interop',
    '/docs/learn-purescript',
    '/docs/components',
    '/404.html'
  ].map(url => ('http://localhost:3000' + url)),
  filenameGenerator: 'bySiteStructure',
  directory: './dist/www'
};

console.log('scraping website HTML...')

setTimeout(() => {
  scraper
    .scrape(options).then(result => {
      server.kill()
      console.log('finished scraping website HTML')
      process.exit(0);
    })
    .catch(console.error)
}, 5000)
