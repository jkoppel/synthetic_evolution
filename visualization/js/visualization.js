var editor = CodeMirror.fromTextArea(document.getElementById("generated-code"), {
   lineNumbers: true
 , matchBrackets: true
 , mode: "text/x-java"
 });

var data = [];
var cursor = {};

d3.json("../getsketches/", function (result) {
  //data = {rootLabel: [], subForest: result};
  data = result;
  cursor = data;
  draw();
});

var svgWidth = 1000;
var svgHeight = 800;

var height = 50;
var padding = 10;
var width = 50;

var cursor = [];
var path = [];
var formatHistory = [];

function fetchCode() {
    d3.text("../sketch/" + JSON.stringify(path), function (code) {
        editor.setValue(JSON.parse(code));
    });
}

function val(x, d, i) {
    if (typeof(x) == "function") {
        return x(d, i);
    } else {
        return x;
    }
}

function getter(s) {
    return function(d) {
        return d[s];
    };
}

function translate(x, y) {
    return function(sel) {
        sel.attr("transform", function (d, i) {
            return "translate(" + val(x,d,i) + "," + val(y,d,i) + ")";
        });
    }
}

function makeRect(sel) {

    var heightFn = function(d,i) {return i*(height+padding);};

    var g = sel.append("g")
               .call(translate(0, heightFn));



    g.append("rect")
     .attr("class", "format-rect")
     .attr("height", height)
     .attr("width", width)
     .on("mouseover", function(d) {
         d3.select(this).style("fill", "red");
     })
     .on("mouseout", function(d) {
         d3.select(this).style("fill", "blue");
     })
     .on("click", function(d, i) {

        /*if (d.parent) {
            d.parent.subForest.forEach(function (e) { e.selected = false; })
        }*/

        //d.selected = true;
        path.push(i);
        formatHistory.push(cursor[i].rootLabel.newFormat);
        cursor = cursor[i].subForest;
        fetchCode();
        draw();
     });
     

    g.append("text")
     .attr("y", 10)
     .text(function (d) {
        return d.rootLabel.changeDesc;
     });

}



function attributeCodeField(field) {

}

function fileField(outerField) {
    var fieldID = outerField[0];

    var height = 30;
    var unitWidth = 20;

    function fieldColor() {
        switch (fieldID) {
            case 1: return "green";
            case 2: return "blue";
            case 3: return "yellow";
        }
    }

    function rawField(field) {
        function fieldName() {
            return "field" + fieldID;
        }

        function my (sel) {
            var g = sel.append("g");

            g.append("rect")
             .style("fill", fieldColor())
             .attr("width", unitWidth*my.len())
             .attr("height", height)
             .attr("class", "field-rect");

            g.append("text")
             .text(fieldName())
             .style("text-anchor", "middle")
             .attr("x", unitWidth*my.len() / 2)
             .attr("y", (height/2)+5);

            return g;
        }

        my.width = function() {
            return my.len() * unitWidth;
        };


        my.len = function() {
            return 4;
        }

        return my;
    }

    function attributeCodeField(field) {
        var innerField = fileField([field[0], field[1].AttributeCode]);

        var attributeCodeLength = 4;

        function my (sel) {

            var g = sel.append("g");

            g.append("rect")
             .style("fill", fieldColor())
             .attr("width", unitWidth * attributeCodeLength)
             .attr("height", height)
             .attr("class", "field-rect");

            g.append("text")
             .text("" + fieldID)
             .style("text-anchor", "middle")
             .attr("x", unitWidth * attributeCodeLength / 2)
             .attr("y", (height/2)+5);

            var innerG = g.append("g")
                          .call(translate(my.width() - innerField.width(), 0));

            innerField(innerG);

            return g;
        }

        my.len = function() {
            return attributeCodeLength + innerField.len();
        };

        my.width = function() {
            return my.len() * unitWidth;
        };

        return my;
    }

    if (outerField[1] == "Raw") {
        return rawField(outerField);
    } else {
        return attributeCodeField(outerField);
    }
}

function draw() {
    d3.select("svg").remove();

    /*var treeLayout = d3.layout.tree()
                              .size([svgWidth / 2, svgHeight / 2])
                              .children(function (d) {
                                if (d.selected) {
                                    return d.subForest;
                                } else {
                                    return null;
                                }
                               });

    var nodes = treeLayout.nodes(data);*/

    var svg = d3.select("#format-histories")
                .append("svg")
                .attr("width", svgWidth)
                .attr("height", svgHeight);

    var sel = svg.selectAll("rect")
        .data(cursor)
        .enter();

    makeRect(sel);

    var y = 20;

    formatHistory.forEach(function(curFormat) {
        var x = 100;
        curFormat.forEach(function (f) {
            var fld = fileField(f);
            fld(d3.select("svg")).call(translate(x, y));
            x += fld.width();
        });
        y += 50;
    });
}