var path = require('path');
var webpack = require('webpack');
var HTMLPlugin = require('html-webpack-plugin');

module.exports = {
  devtool: 'source-map',
  entry: './src/app.js',
  output: {
    path: './dist',
    filename: 'main.js',
  },
  plugins: [
    new HTMLPlugin(),
  ],
  module: {
    loaders: [
      {
        test: /\.js$/,
        loaders: ['babel'],
        exclude: /node_modules/,
        include: __dirname,
      }
    ]
  }
};
