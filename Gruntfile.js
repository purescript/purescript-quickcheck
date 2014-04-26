module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
    ],
    
    clean: ["output"],
  
    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
    docgen: {
        readme: {
            src: "src/**/*.purs",
            dest: "README.md"
        }
    }

  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  
  grunt.registerTask("make", ["pscMake", "dotPsci", "docgen"]);
  grunt.registerTask("default", ["make"]);
};
