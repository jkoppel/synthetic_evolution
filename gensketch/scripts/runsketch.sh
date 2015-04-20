#!/bin/bash

JAVA_CODE_GEN=`pwd`/../java_code_generator/java_code_generator.jar

cd $SKETCH_HOME/sketch-frontend
./sketch -V 0 --fe-custom-codegen $JAVA_CODE_GEN $1