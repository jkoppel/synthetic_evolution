cabal build
echo -e "[[(IntField 1,Raw)],[(IntField 2,AttributeCode Raw),(IntField 1,Raw)],[(IntField 2,AttributeCode Raw),(IntField 1,Raw),(IntField 3,AttributeCode Raw)]]" | ./dist/build/gensketch/gensketch > GenSketchOutput.sk
sketch --fe-custom-codegen ../java_code_generator/java_code_generator.jar ../gensketch/GenSketchOutput.sk

