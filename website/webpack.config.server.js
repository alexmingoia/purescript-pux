var path = require('path');
var webpack = require('webpack');
var nodeExternals = require('webpack-node-externals');
var isProd = process.env.NODE_ENV === 'production';

var entries = [path.join(__dirname, 'support/server.entry.js')];
var plugins = [
  new webpack.DefinePlugin({
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
  }),
  new webpack.optimize.LimitChunkCountPlugin({ maxChunks: 1 })
];

if (!isProd) {
  entries.unshift('webpack/hot/poll?1000')
  plugins.push(
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoErrorsPlugin()
  );
}

module.exports = {
  entry: entries,
  target: 'node',
  cache: false,
  context: __dirname,
  output: {
    path: path.resolve('./dist'),
    filename: 'server.js',
    publicPath: '/',
    libraryTarget: 'commonjs2'
  },
  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        exclude: /node_modules/,
        query: {}
      }
    ],
  },
  plugins: plugins,
  externals: [nodeExternals({
    whitelist: ['webpack/hot/poll?1000']
  })],
  resolve: {
    alias: {
      'react': 'preact-compat',
      'react-dom': 'preact-compat',
      'create-react-class': 'preact-compat/lib/create-class'
    },
    modules: [
      'node_modules',
      'bower_components'
    ],
    extensions: ['.js', '.purs']
  }
};
