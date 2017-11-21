var appConstants  = {

    TRANSLATE_0 : -360,
    TRANSLATE_1 : 10040,
    SCALE : 60000

}

var geons = {};

// this file contains all the geo related objects and functions
geons.geoConfig = function() {
    this.TRANSLATE_0 = appConstants.TRANSLATE_0;
    this.TRANSLATE_1 = appConstants.TRANSLATE_1;
    this.SCALE = appConstants.SCALE;

    this.mercator = d3.geoMercator();
    this.path = d3.geoPath().projection(this.mercator);

    this.setupGeo = function() {
        var translate = this.mercator.translate();
        translate[0] = this.TRANSLATE_0;
        translate[1] = this.TRANSLATE_1;

        this.mercator.translate(translate);
        this.mercator.scale(this.SCALE);
    }
}

// geoConfig contains the configuration for the geo functions
geo = new geons.geoConfig();





/*var geometry = new THREE.BoxGeometry( 1, 1, 1 );
var material = new THREE.MeshBasicMaterial( { color: 0x00ff00 } );
var cube = new THREE.Mesh( geometry, material );
scene.add( cube );

camera.position.z = 5;


function animate() {
	requestAnimationFrame( animate );
	cube.rotation.x += 0.1;
	cube.rotation.y += 0.1;
	renderer.render( scene, camera );
}
animate();*/

// get the data



 // get the correct geo for d3s
    geo.setupGeo();
    var translate = geo.mercator.translate();
    
     var scene;
     var renderer;
     var camera;

    // get the data
    jQuery.getJSON('../data/bounds/Germany_AL4_simple.GeoJson', function(data, textStatus, jqXHR) {




        // Set up the three.js scene. This is the most basic setup without
        // any special stuff
        function initScene() {
            // set the scene size
            var WIDTH = 600, HEIGHT = 600;

            // set some camera attributes
            var VIEW_ANGLE = 45, ASPECT = WIDTH / HEIGHT, NEAR = 0.1, FAR = 10000;

            // create a WebGL renderer, camera, and a scene
            renderer = new THREE.WebGLRenderer({antialias:true});
            camera = new THREE.PerspectiveCamera(VIEW_ANGLE, ASPECT,
                                                  NEAR, FAR);
            scene = new THREE.Scene();

            // add and position the camera at a fixed position
           scene.add(camera);
            camera.position.z = 550;
            camera.position.x = 0;
            camera.position.y = 550;
            camera.lookAt( scene.position );

            // start the renderer, and black background
            renderer.setSize(WIDTH, HEIGHT);
            renderer.setClearColor(0x000);

            // add the render target to the page
           // $("#chart").append(renderer.domElement);
           document.body.appendChild( renderer.domElement );

            // add a light at a specific position
            var pointLight = new THREE.PointLight(0xFFFFFF);
            scene.add(pointLight);
            pointLight.position.x = 800;
            pointLight.position.y = 800;
            pointLight.position.z = 800;

            // add a base plane on which we'll render our map
            var planeGeo = new THREE.PlaneGeometry(10000, 10000, 10, 10);
            var planeMat = new THREE.MeshLambertMaterial({color: 0x666699});
            var plane = new THREE.Mesh(planeGeo, planeMat);

            // rotate it to correct position
            plane.rotation.x = -Math.PI/2;
            scene.add(plane);
        }


      // add the loaded gis object (in geojson format) to the map
      function addGeoObject() {
          // keep track of rendered objects
          var meshes = [];
          var averageValues = [];
          var totalValues = [];


          // keep track of min and max, used to color the objects
          var maxValueAverage = 0;
          var minValueAverage = -1;

          // keep track of max and min of total value
          var maxValueTotal = 0;
          var minValueTotal = -1;

          // convert to mesh and calculate values
          for (var i = 0 ; i < data.features.length ; i++) {
              var geoFeature = data.features[i]
              var feature = geo.path(geoFeature);
              // we only need to convert it to a three.js path
              var mesh = transformSVGPathExposed(feature);
              // add to array
              meshes.push(mesh);

              // we get a property from the json object and use it
              // to determine the color later on
              var value = parseInt(geoFeature.properties.val);
              if (value > maxValueAverage) maxValueAverage = value;
              if (value < minValueAverage || minValueAverage == -1) minValueAverage = value;
              averageValues.push(value);

              // and we get the max values to determine height later on.
              value = parseInt(geoFeature.properties.aant_inw);
              if (value > maxValueTotal) maxValueTotal = value;
              if (value < minValueTotal || minValueTotal == -1) minValueTotal = value;

              totalValues.push(value);
          }

          // we've got our paths now extrude them to a height and add a color
          for (var i = 0 ; i < averageValues.length ; i++) {

              // create material color based on average
              var scale = ((averageValues[i] - minValueAverage) / (maxValueAverage - minValueAverage)) * 255;
              var mathColor = gradient(Math.round(scale),255);
              var material = new THREE.MeshLambertMaterial({
                  color: mathColor
              });

              // create extrude based on total
              var extrude = ((totalValues[i] - minValueTotal) / (maxValueTotal - minValueTotal)) * 100;
              var shape3d = meshes[i].extrude({amount: Math.round(extrude), bevelEnabled: false});

              // create a mesh based on material and extruded shape
              var toAdd = new THREE.Mesh(shape3d, material);

              // rotate and position the elements nicely in the center
              toAdd.rotation.x = Math.PI/2;
              toAdd.translateX(-490);
              toAdd.translateZ(50);
              toAdd.translateY(extrude/2);

              // add to scene
              scene.add(toAdd);
          }
      }

        // simple gradient function
        function gradient(length, maxLength) {

            var i = (length * 255 / maxLength);
            var r = i;
            var g = 255-(i);
            var b = 0;

            var rgb = b | (g << 8) | (r << 16);
            return rgb;
        }
        
        
      
        initScene();
          requestAnimationFrame( animate );
       // addGeoObject();
        renderer.render( scene, camera );
    });

