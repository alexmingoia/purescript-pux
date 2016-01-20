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

gulp.task('psc', function() {
  return purescript.psc({
    src: sources,
    ffi: foreigns
  })
});

gulp.task('docs', function() {
  return purescript.pscDocs({
    src: sources,
    docgen: {
      'Pux': 'docs/Pux.md',
      'Pux.App': 'docs/Pux/App.md',
      'Pux.DOM.HTML.Attributes': 'docs/Pux/DOM/HTML/Attributes.md',
      'Pux.DOM.HTML.Elements': 'docs/Pux/DOM/HTML/Elements.md',
      'Pux.Render.DOM': 'docs/Pux/Render/DOM.md',
      'Pux.Render.HTML': 'docs/Pux/Render/HTML.md',
      'Pux.Router': 'docs/Pux/Router.md',
      'Pux.View': 'docs/Pux/View.md'
    }
  })
})

gulp.task('dotPsci', function() {
  return purescript.psci({
    src: sources,
    ffi: foreigns
  })
  .pipe(gulp.dest('.'))
})

gulp.task('test', ['psc'], function() {
  return purescript.pscBundle({ src: "output/**/*.js" });
});

gulp.task('default', ['test']);
