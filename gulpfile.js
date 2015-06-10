/* jshint node: true */
"use strict";

var gulp = require("gulp");
var plumber = require("gulp-plumber");
var purescript = require("gulp-purescript");
var rimraf = require("rimraf");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("clean-docs", function (cb) {
  rimraf("docs", cb);
});

gulp.task("clean-output", function (cb) {
  rimraf("output", cb);
});

gulp.task("clean", ["clean-docs", "clean-output"]);

gulp.task("make", function() {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscMake({ ffi: foreigns }));
});

gulp.task("docs", ["clean-docs"], function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscDocs({
      docgen: {
        "Test.QuickCheck": "docs/Test/QuickCheck.md",
        "Test.QuickCheck.Arbitrary": "docs/Test/QuickCheck/Arbitrary.md",
        "Test.QuickCheck.Gen": "docs/Test/QuickCheck/Gen.md",
        "Test.QuickCheck.LCG": "docs/Test/QuickCheck/LCG.md",
        "Test.QuickCheck.Data.AlphaNumString": "docs/Test/QuickCheck/Data/AlphaNumString.md",
        "Test.QuickCheck.Data.ApproxNumber": "docs/Test/QuickCheck/Data/ApproxNumber.md"
      }
    }));
});

gulp.task("dotpsci", function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.dotPsci());
});

gulp.task("default", ["make", "docs", "dotpsci"]);