Must have SKETCH_HOME set to the location of Sketch

To run on port 80: sudo stack exec server --allow-different-user -- -p 80

The paths will be messed up if you instead run the generated executable directly.

Note that you will need to make sure the root user also has SKETCH_HOME set up properly.

To visit, go to: localhost:port/static