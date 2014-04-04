module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs.hs"
    ],
    
    clean: {
      tmp: ["tmp"],
      lib: ["js", "externs"]
    },
  
    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
  
    psc: {
      prelude: {
        options: {
          module: "PreludeTests",
          main: "PreludeTests"
        },
        src: ["examples/Prelude.purs", "<%=libFiles%>"],
        dest: "tmp/Prelude.js"
      }
    },
    
    execute: {
      prelude: "tmp/Prelude.js"
    }      
  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");
  
  grunt.registerTask("make", ["pscMake", "dotPsci"]);
  grunt.registerTask("examples", ["clean:tmp", "psc", "execute"]);
  grunt.registerTask("default", ["make", "examples"]);
};
