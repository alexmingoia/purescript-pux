'use strict';

var gulp = require('gulp');
var purescript = require('gulp-purescript');
var jsValidate = require('gulp-jsvalidate');
var plumber = require("gulp-plumber");

var sources = [
  'src/**/*.purs',
  'examples/basic/Main.purs',
  'bower_components/purescript-*/src/**/*.purs'
];

var foreigns = [
  'src/**/*.js',
  'bower_components/purescript-*/src/**/*.js'
];

gulp.task('jsvalidate', function () {
  return gulp.src(foreigns)
    .pipe(plumber())
    .pipe(jsValidate());
});

gulp.task('build', function() {
  return purescript.psc({
    src: sources,
    ffi: foreigns
  })
});

gulp.task('docs', function() {
  return purescript.pscDocs({
    src: sources,
    docgen: {
      'Pux': 'docs/API/Pux.md',
      'Pux.App': 'docs/API/Pux/App.md',
      'Pux.DOM': 'docs/API/Pux/DOM.md',
      'Pux.DOM.HTML.Attributes': 'docs/API/Pux/DOM/HTML/Attributes.md',
      'Pux.DOM.HTML.Elements': 'docs/API/Pux/DOM/HTML/Elements.md',
      'Pux.Render.DOM': 'docs/API/Pux/Render/DOM.md',
      'Pux.Render.HTML': 'docs/API/Pux/Render/HTML.md',
      'Pux.Router': 'docs/API/Pux/Router.md'
    }
  })
})

gulp.task('test', ['build'], function() {
  return purescript.pscBundle({ src: "output/**/*.js" });
});

gulp.task('default', ['test']);
