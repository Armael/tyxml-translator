"use strict";

const gulp = require('gulp');
const less = require('gulp-less');
const shell = require('gulp-shell');
const autoprefixer = require('gulp-autoprefixer');
const browserSync = require('browser-sync');
const reload = browserSync.reload;

const paths = {
    build: {
        watch: 'src/tyxml_translator_web.ml',
        html: 'static/index.html'
    },
    less: {
        src: 'src/less/style.less',
        dest: 'static/',
        watch: 'src/less/**'
    }
};

gulp.task('build', shell.task([
  'make static/tyxml_translator_web.js'
]));

gulp.task('less', function () {
    gulp.src(paths.less.src)
        .pipe(less())
        .pipe(autoprefixer({
            browsers: ['last 2 versions'],
            cascade: false
        }))
        .pipe(gulp.dest(paths.less.dest))
        .pipe(reload({stream:true}));
});

gulp.task('browser-sync', function() {
    browserSync({
        server: {
            baseDir: "static",
            directory: true
        },
        open: false
    });
});

gulp.task('bs-reload', function () {
    browserSync.reload();
});

gulp.task('build-watch', ['build'], function() {
    browserSync.reload();
});

gulp.task('html-watch', function() {
    browserSync.reload();
});

gulp.task('watch', ['build', 'less', 'browser-sync'], function () {
    gulp.watch(paths.less.watch, ['less']);
    gulp.watch(paths.build.watch, ['build-watch']);
    gulp.watch(paths.build.html, ['html-watch']);
});

gulp.task('default', ['less', 'build']);
